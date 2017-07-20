use super::{ RawOp, RawOpArg, Atom };
use ::beam_module::{ Module, Import, Lambda };
use ::itertools::Itertools;
use ::gen_op::OpName;

use ::std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Op {
    pub reads: Vec<Source>,
    pub writes: Vec<Register>,
    pub labels: Vec<LabelId>,
    pub kind: OpKind,
}
impl Op {
    pub fn empty(kind: OpKind) -> Self {
        Op {
            reads: Vec::new(),
            writes: Vec::new(),
            labels: Vec::new(),
            kind: kind,
        }
    }
    pub fn with_reads(kind: OpKind, reads: Vec<Source>) -> Self {
        Op {
            reads: reads,
            writes: Vec::new(),
            labels: Vec::new(),
            kind: kind,
        }
    }
    pub fn with_reads_labels(kind: OpKind, reads: Vec<Source>, labels: Vec<LabelId>) -> Self {
        Op {
            reads: reads,
            writes: Vec::new(),
            labels: labels,
            kind: kind,
        }
    }
    pub fn with_reads_writes(kind: OpKind, reads: Vec<Source>, writes: Vec<Register>) -> Self {
        Op {
            reads: reads,
            writes: writes,
            labels: Vec::new(),
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpKind {

    // Info
    Line { num: u32 },
    Label { num: LabelId },
    FuncInfo { module: Atom, function: Atom, arity: u32 },
    CodeEnd,

    // General

    /// Moves r[0] to w[0]
    Move,
    /// Writes NIL to w[0]
    Init,

    // Functions
    MakeFun2 { lambda: Lambda },
    /// Read and return reg_x0 from function
    Return,
    /// Arguments in regs x[0..arity], fun in x[arity]
    CallFun { arity: u32 },
    /// mod in x[0], fun in x[1], args as list in x[2]
    /// result in x[0]
    Apply,

    // Testing
    UnaryTest { test: UnaryTest },
    BinaryTest { test: BinaryTest },
    TestHeap { heap_need: u32, live: u32 },
    /// test r[0] for r[1..] keys, jump to l[0] on fail
    HasMapFields,
    TupleArity { matches: Vec<u32> },

    // Calls
    /// Call bif with r[..] as args
    /// Put result into w[0]
    /// If l[0], go to l[0] on failure
    CallBif {
        /// If Some, gc can occur in BIF. Inner is number of live X registers.
        gc: Option<u32>,
        bif: Import,
    },
    CallExt { arity: u32, import: Import },
    CallExtOnly { arity: u32, import: Import },
    CallExtLast { arity: u32, import: Import, deallocate: u32 },
    Call { arity: u32, fun_label: LabelId },
    CallOnly { arity: u32, fun_label: LabelId },
    CallLast { arity: u32, fun_label: LabelId, deallocate: u32 },

    // Allocation
    Allocate {
        stack_need: u32,
        stack_zero: bool,
        live: u32,
        heap_need: Option<u32>,
    },
    Deallocate { num: u32 }, // N+1 for cp
    Trim { num: u32 }, // remaining value seems unused

    // Term deconstruction
    GetTupleElem { elem: u32 },
    GetMapElems { entries: Vec<Source> }, // FIXME: Add registers to read list
    /// Get list from r[0], move head into w[0], tail into w[1]
    GetList,

    // Term construction
    /// Construct a new list cell with r[0] as head and r[1] as tail
    /// Put result into w[0]
    PutList,
    PutTuple { arity: u32 },
    PutTupleElem,

    // Matching
    /// Matches on r[0]
    /// Tests against literals in matches, jump to l[n+1] if match
    /// If no matches, jump to l[0]
    SelectVal { matches: Vec<Literal> },
    Jump,

    // Messages
    Wait,
    WaitTimeout,
    Timeout,
    LoopRec,
    LoopRecEnd,
    RemoveMessage,

    // Errors
    Raise(RaiseType),
    Try,
    TryCase,
    TryEnd,
    CatchEnd,

    Unknown(RawOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RaiseType {
    /// 2 reads, error class and value
    Error,
    /// 1 read, the failed match
    BadMatch,
    IfEnd,
    /// 1 read, the failed match
    CaseEnd,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryTest {
    // Types
    Integer,
    Float,
    Number,
    Atom,
    Pid,
    Reference,
    Port,
    Nil,
    Binary,
    List,
    NonemptyList,
    Tuple,
    Map,
    Boolean,
    Function,

    Arity(u32),
    FunctionArity(u32),
    TaggedTuple {
        arity: u32,
        atom: AtomLiteral,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryTest {
    // Comparison
    IsLt,
    IsGe,
    IsEq,
    IsNe,
    IsEqExact,
    IsNeExact,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct LabelId(pub u32);

impl ::std::fmt::Debug for LabelId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "LabelId({})", self.0)
    }
}

struct MacroOpCtx {
    labels: Vec<LabelId>,
    reads: Vec<Source>,
    writes: Vec<Register>,
    arg_num: usize,
}

macro_rules! impl_op_repeat_arg {
    ($ctx:expr, $op_ctx:expr, $list:expr, (named, $name:ident)) => {
        let $name = $list;
    };
    ($ctx:expr, $op_ctx:expr, $list:expr, label) => {
        for l in $list.iter() {
            $op_ctx.labels.push(LabelId(l.fail_label()));
        }
    };
    ($ctx:expr, $op_ctx:expr, $list:expr, read) => {
        for l in $list.iter() {
            $op_ctx.reads.push(Source::from_raw(l, $ctx.1).unwrap());
        }
    };
    ($ctx:expr, $op_ctx:expr, $list:expr, write) => {
        for l in $list.iter() {
            $op_ctx.writes.push(Register::from_raw(l).unwrap());
        }
    };
}

macro_rules! impl_op_arg {
    ($ctx:expr, $op_ctx:expr, (repeat, [$a1:tt, $a2:tt])) => {
        let (l1, l2): (Vec<_>, Vec<_>) = $ctx.0.args[($op_ctx.arg_num)..].iter().tuples().unzip();

        // for error
        $op_ctx.arg_num = 9999;

        impl_op_repeat_arg!($ctx, $op_ctx, l1, $a1);
        impl_op_repeat_arg!($ctx, $op_ctx, l2, $a2);
    };
    ($ctx:expr, $op_ctx:expr, (repeat, [$a1:tt])) => {
        let l1 = &$ctx.0.args[($op_ctx.arg_num)..];

        // for error
        $op_ctx.arg_num = 9999;

        impl_op_repeat_arg!($ctx, $op_ctx, l1, $a1);
    };
    ($ctx:expr, $op_ctx:expr, (named, $name:ident)) => {
        let $name = &$ctx.0.args[$op_ctx.arg_num];
        $op_ctx.arg_num += 1;
    };
    ($ctx:expr, $op_ctx:expr, read) => {
        $op_ctx.reads.push(Source::from_raw(&$ctx.0.args[$op_ctx.arg_num], $ctx.1).unwrap());
        $op_ctx.arg_num += 1;
    };
    ($ctx:expr, $op_ctx:expr, write) => {
        $op_ctx.writes.push(Register::from_raw(&$ctx.0.args[$op_ctx.arg_num]).unwrap());
        $op_ctx.arg_num += 1;
    };
    ($ctx:expr, $op_ctx:expr, label) => {
        let label = $ctx.0.args[$op_ctx.arg_num].fail_label();
        assert!(label != 0);
        $op_ctx.labels.push(LabelId(label));
        $op_ctx.arg_num += 1;
    };
    ($ctx:expr, $op_ctx:expr, opt_label) => {
        let label = $ctx.0.args[$op_ctx.arg_num].fail_label();
        if label != 0 {
            $op_ctx.labels.push(LabelId(label));
        }
        $op_ctx.arg_num += 1;
    };
    ($ctx:expr, $op_ctx:expr, _) => {
        $op_ctx.arg_num += 1;
    };
    ($ctx:expr, $op_ctx:expr, (read_reg, $reg:expr)) => {
        $op_ctx.reads.push(Source::Register($reg));
    };
    ($ctx:expr, $op_ctx:expr, (write_reg, $reg:expr)) => {
        $op_ctx.writes.push($reg);
    };
}

macro_rules! impl_op {
    ($ctx:expr, $name:expr, [ $($dec_op:tt),* ], $enum:expr) => {
        #[allow(unused_mut)]
        {
            // TODO: Creating OpNames from strings is the slowest thing
            // in the whole program, fix it!
            if $ctx.0.opcode.name == OpName::from($name) {
                let mut op_ctx = MacroOpCtx {
                    labels: vec![],
                    reads: vec![],
                    writes: vec![],
                    arg_num: 0,
                };

                $(
                    impl_op_arg!($ctx, op_ctx, $dec_op);
                )*

                    return Op {
                        kind: $enum,
                        reads: op_ctx.reads,
                        writes: op_ctx.writes,
                        labels: op_ctx.labels,
                    }
            }
        }
    };
}

impl Op {

    pub fn is_label(&self) -> bool {
        if let OpKind::Label { .. } = self.kind {
            true
        } else {
            false
        }
    }

    pub fn has_jump(&self) -> bool {
        //self.labels.len() != 0
        match self.kind {
            OpKind::UnaryTest { .. } => true,
            OpKind::BinaryTest { .. } => true,
            OpKind::CallBif { .. } if self.labels.len() != 0 => true,
            OpKind::SelectVal { .. } => true,
            OpKind::GetMapElems { .. } => true,
            OpKind::HasMapFields { .. } => true,
            OpKind::Try { .. } => true,
            OpKind::Jump => true,
            OpKind::WaitTimeout => true,
            OpKind::LoopRec => true,
            _ => false,
        }
    }

    pub fn can_continue(&self) -> bool {
        match self.kind {
            OpKind::Raise(_) => false,
            OpKind::Return => false,
            // Special case, :erlang.error BIF never returns
            OpKind::CallExt { ref import, .. } if import.module == Atom::from("erlang")
                && import.function == Atom::from("error") => false,
            OpKind::CallExtLast { .. } => false,
            OpKind::CallExtOnly { .. } => false,
            OpKind::Jump => false,
            OpKind::SelectVal { .. } => false, // Is this right? Can label be 0?
            OpKind::CallLast { .. } => false,
            OpKind::CallOnly { .. } => false,
            OpKind::TupleArity { .. } => false,
            OpKind::Wait => false,
            OpKind::LoopRecEnd => false,
            _ => true,
        }
    }

    pub fn touches_reg(&self, reg: Register) -> bool {
        for r in &self.reads {
            if *r == Source::Register(reg) {
                return true;
            }
        }
        for w in &self.writes {
            if *w == reg {
                return true;
            }
        }
        false
    }

    pub fn from_raw(raw_op: &RawOp, module: &Module) -> Op {
        let atoms = &module.atoms;
        let imports = &module.imports;
        let lambdas = &module.lambdas;

        let ctx = (raw_op, module);


        impl_op!(&ctx, "return", [ (read_reg, Register::X(0)) ], OpKind::Return);
        impl_op!(&ctx, "move", [ read, write ], OpKind::Move);
        impl_op!(&ctx, "init", [ write ], OpKind::Init);
        impl_op!(&ctx, "line", [ (named, l) ], OpKind::Line { num: l.untagged() });
        impl_op!(&ctx, "label", [ (named, l) ], OpKind::Label { num: LabelId(l.untagged()) });
        impl_op!(&ctx, "select_val", [ read, label, (repeat, [(named, matches), label]) ],
                 OpKind::SelectVal {
                     matches: matches.iter().map(|m| Literal::from_raw(m, module).unwrap()).collect(),
                 });
        impl_op!(&ctx, "func_info", [ (named, m), (named, f), (named, a) ],
                 OpKind::FuncInfo {
                     module: atoms[m.atom() as usize - 1].clone(),
                     function: atoms[f.atom() as usize - 1].clone(),
                     arity: a.untagged(),
                 });
        impl_op!(&ctx, "has_map_fields", [ label, (repeat, [read]) ], OpKind::HasMapFields);
        impl_op!(&ctx, "select_tuple_arity", [ read, label, (repeat, [(named, m), label]) ],
                 OpKind::TupleArity { matches: m.iter().map(|m| m.untagged()).collect() });
        impl_op!(&ctx, "allocate", [ (named, s), (named, l) ],
                 OpKind::Allocate {
                     stack_need: s.untagged(),
                     live: l.untagged(),
                     stack_zero: false,
                     heap_need: None,
                 });
        impl_op!(&ctx, "deallocate", [ (named, n) ], OpKind::Deallocate { num: n.untagged() });
        impl_op!(&ctx, "allocate_heap", [ (named, s), (named, h), (named, l) ],
                 OpKind::Allocate {
                     stack_need: s.untagged(),
                     heap_need: Some(h.untagged()),
                     live: l.untagged(),
                     stack_zero: false,
                 });
        impl_op!(&ctx, "allocate_zero", [ (named, s), (named, l) ],
                 OpKind::Allocate {
                     stack_need: s.untagged(),
                     live: l.untagged(),
                     stack_zero: true,
                     heap_need: None,
                 });
        impl_op!(&ctx, "allocate_heap_zero", [ (named, s), (named, h), (named, l) ],
                 OpKind::Allocate {
                     stack_need: s.untagged(),
                     heap_need: Some(h.untagged()),
                     live: l.untagged(),
                     stack_zero: true,
                 });
        impl_op!(&ctx, "trim", [ (named, n) ], OpKind::Trim { num: n.untagged() });
        impl_op!(&ctx, "test_heap", [ (named, h), (named, l) ],
                 OpKind::TestHeap {
                     heap_need: h.untagged(),
                     live: l.untagged(),
                 });
        impl_op!(&ctx, "put_list", [ read, read, write ], OpKind::PutList);
        impl_op!(&ctx, "put_tuple", [ (named, a), write ], OpKind::PutTuple { arity: a.untagged() });
        impl_op!(&ctx, "put", [ read ], OpKind::PutTupleElem);
        impl_op!(&ctx, "get_tuple_element", [ read, (named, n), write ],
                 OpKind::GetTupleElem { elem: n.untagged() });
        impl_op!(&ctx, "get_list", [ read, write, write ], OpKind::GetList);

        impl_op!(&ctx, "is_integer", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Integer });
        impl_op!(&ctx, "is_float", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Float });
        impl_op!(&ctx, "is_number", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Number });
        impl_op!(&ctx, "is_atom", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Atom });
        impl_op!(&ctx, "is_pid", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Pid });
        impl_op!(&ctx, "is_reference", [ label, read ],
                 OpKind::UnaryTest { test: UnaryTest::Reference });
        impl_op!(&ctx, "is_port", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Port });
        impl_op!(&ctx, "is_nil", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Nil });
        impl_op!(&ctx, "is_binary", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Binary });
        impl_op!(&ctx, "is_list", [ label, read ], OpKind::UnaryTest { test: UnaryTest::List });
        impl_op!(&ctx, "is_nonempty_list", [ label, read ],
                 OpKind::UnaryTest { test: UnaryTest::NonemptyList });
        impl_op!(&ctx, "is_tuple", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Tuple });
        impl_op!(&ctx, "is_map", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Map });
        impl_op!(&ctx, "is_boolean", [ label, read ], OpKind::UnaryTest { test: UnaryTest::Boolean });
        impl_op!(&ctx, "is_function", [ label, read ],
                 OpKind::UnaryTest { test: UnaryTest::Function });
        impl_op!(&ctx, "is_function2", [ label, read, (named, arity) ],
                 OpKind::UnaryTest { test: UnaryTest::FunctionArity(arity.integer()) });
        impl_op!(&ctx, "test_arity", [ label, read, (named, a) ],
                 OpKind::UnaryTest { test: UnaryTest::Arity(a.untagged()) });
        impl_op!(&ctx, "is_tagged_tuple", [ label, read, (named, arity), (named, atom) ],
                 OpKind::UnaryTest { test: UnaryTest::TaggedTuple {
                     arity: arity.untagged(),
                     atom: AtomLiteral::from_raw(atom, module).unwrap(),
                 } });
        impl_op!(&ctx, "is_eq", [ label, read, read ], OpKind::BinaryTest { test: BinaryTest::IsEq });
        impl_op!(&ctx, "is_ne", [ label, read, read ], OpKind::BinaryTest { test: BinaryTest::IsNe });
        impl_op!(&ctx, "is_ge", [ label, read, read ], OpKind::BinaryTest { test: BinaryTest::IsGe });
        impl_op!(&ctx, "is_lt", [ label, read, read ], OpKind::BinaryTest { test: BinaryTest::IsLt });
        impl_op!(&ctx, "is_eq_exact", [ label, read, read ],
                 OpKind::BinaryTest { test: BinaryTest::IsEqExact });
        impl_op!(&ctx, "is_ne_exact", [ label, read, read ],
                 OpKind::BinaryTest { test: BinaryTest::IsNeExact });
        impl_op!(&ctx, "badmatch", [ read ], OpKind::Raise(RaiseType::BadMatch));
        impl_op!(&ctx, "if_end", [], OpKind::Raise(RaiseType::IfEnd));
        impl_op!(&ctx, "case_end", [ read ], OpKind::Raise(RaiseType::IfEnd));
        impl_op!(&ctx, "int_code_end", [], OpKind::CodeEnd);
        impl_op!(&ctx, "get_map_elements", [ label, read, (repeat, [(named, elem), write]) ],
                 OpKind::GetMapElems {
                     entries: elem.iter().map(|lit| Source::from_raw(lit, module).unwrap()).collect(),
                 });
        impl_op!(&ctx, "raise", [ read, read ], OpKind::Raise(RaiseType::Error));
        impl_op!(&ctx, "jump", [ label ], OpKind::Jump);
        impl_op!(&ctx, "apply", [ (read_reg, Register::X(0)), (read_reg, Register::X(1)),
                                   (read_reg, Register::X(2)), (write_reg, Register::X(0)) ],
                 OpKind::Apply);

        impl_op!(&ctx, "wait", [ label ], OpKind::Wait);
        impl_op!(&ctx, "loop_rec", [ label, _ ], OpKind::LoopRec);
        impl_op!(&ctx, "loop_rec_end", [ label ], OpKind::LoopRecEnd);
        impl_op!(&ctx, "remove_message", [ (write_reg, Register::X(0)) ], OpKind::RemoveMessage);
        impl_op!(&ctx, "wait_timeout", [ label, read ], OpKind::WaitTimeout);
        impl_op!(&ctx, "timeout", [], OpKind::Timeout);

        impl_op!(&ctx, "try", [ write, label ], OpKind::Try);
        impl_op!(&ctx, "try_case", [ read, (write_reg, Register::X(0)),
                                     (write_reg, Register::X(1)), (write_reg, Register::X(2)) ],
                 OpKind::TryCase);
        impl_op!(&ctx, "try_end", [ read ], OpKind::TryEnd);
        impl_op!(&ctx, "catch", [ write, label ], OpKind::Try);
        //impl_op!(&ctx, "try_case", [ read, (write_reg, Register::X(0)),
        //                             (write_reg, Register::X(1)), (write_reg, Register::X(2)) ],
        //         OpKind::TryCase);
        impl_op!(&ctx, "catch_end", [ read ], OpKind::CatchEnd);

        match &raw_op.opcode.name {
            // TODO
            n if *n == OpName::from("make_fun2") =>
                Op::empty(OpKind::MakeFun2 {
                    lambda: lambdas[raw_op.args[0].untagged() as usize].clone()
                }),
            n if *n == OpName::from("call_fun") => {
                let arity = raw_op.args[0].untagged();
                Op {
                    kind: OpKind::CallFun { arity: arity },
                    labels: vec![],
                    writes: vec![Register::X(0)], // TODO: Unsure?
                    reads: (0..(arity+1)).map(|n| Source::Register(Register::X(n))).collect(),
                }
            }
            n if *n == OpName::from("jump") => Op {
                kind: OpKind::Jump,
                labels: vec![LabelId(raw_op.args[0].fail_label())],
                reads: vec![],
                writes: vec![],
            },
            // FIXME read registers
            n if *n == OpName::from("call_ext") => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads_writes(
                    OpKind::CallExt {
                        arity: raw_op.args[0].untagged(),
                        import: imports[raw_op.args[1].untagged() as usize].clone(),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect(),
                    vec![Register::X(0)]
                )
            }
            n if *n == OpName::from("call_ext_only") => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads(
                    OpKind::CallExtOnly {
                        arity: arity,
                        import: imports[raw_op.args[1].untagged() as usize].clone(),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect()
                )
            }
            n if *n == OpName::from("call_ext_last") => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads(
                    OpKind::CallExtLast {
                        arity: arity,
                        import: imports[raw_op.args[1].untagged() as usize].clone(),
                        deallocate: raw_op.args[2].untagged(),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect()
                )
            }
            n if *n == OpName::from("call") => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads_writes(
                    OpKind::Call {
                        arity: arity,
                        fun_label: LabelId(raw_op.args[1].fail_label()),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect(),
                    vec![Register::X(0)]
                )
            }
            n if *n == OpName::from("call_only") => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads(
                    OpKind::CallOnly {
                        arity: arity,
                        fun_label: LabelId(raw_op.args[1].fail_label()),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect()
                )
            }
            n if *n == OpName::from("call_last") => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads(
                    OpKind::CallLast {
                        arity: arity,
                        fun_label: LabelId(raw_op.args[1].fail_label()),
                        deallocate: raw_op.args[2].untagged(),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect()
                )
            }
            n if *n == OpName::from("bif0") => Op {
                kind: OpKind::CallBif {
                    bif: imports[raw_op.args[0].untagged() as usize].clone(),
                    gc: None,
                },
                reads: vec![],
                writes: vec![Register::from_raw(&raw_op.args[1]).unwrap()],
                labels: vec![],
            },
            n if *n == OpName::from("bif1") => {
                let label = raw_op.args[0].fail_label();
                Op {
                    kind: OpKind::CallBif {
                        gc: None,
                        bif: imports[raw_op.args[1].untagged() as usize].clone(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[2], module).unwrap()],
                    writes: vec![Register::from_raw(&raw_op.args[3]).unwrap()],
                    labels: if label == 0 { vec![] } else { vec![LabelId(label)] },
                }
            }
            n if *n == OpName::from("bif2") => {
                let label = raw_op.args[0].fail_label();
                Op {
                    kind: OpKind::CallBif {
                        gc: None,
                        bif: imports[raw_op.args[1].untagged() as usize].clone(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[2], module).unwrap(),
                                Source::from_raw(&raw_op.args[3], module).unwrap()],
                    writes: vec![Register::from_raw(&raw_op.args[4]).unwrap()],
                    labels: if label == 0 { vec![] } else { vec![LabelId(label)] },
                }
            }
            n if *n == OpName::from("gc_bif1") => {
                let label = raw_op.args[0].fail_label();
                Op {
                    kind: OpKind::CallBif {
                        gc: Some(raw_op.args[1].untagged()),
                        bif: imports[raw_op.args[2].untagged() as usize].clone(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[3], module).unwrap()],
                    writes: vec![Register::from_raw(&raw_op.args[4]).unwrap()],
                    labels: if label == 0 { vec![] } else { vec![LabelId(label)] },
                }
            }
            n if *n == OpName::from("gc_bif2") => {
                let label = raw_op.args[0].fail_label();
                Op {
                    kind: OpKind::CallBif {
                        gc: Some(raw_op.args[1].untagged()),
                        bif: imports[raw_op.args[2].untagged() as usize].clone(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[3], module).unwrap(),
                                Source::from_raw(&raw_op.args[4], module).unwrap()],
                    writes: vec![Register::from_raw(&raw_op.args[5]).unwrap()],
                    labels: if label == 0 { vec![] } else { vec![LabelId(label)] },
                }
            }
            _ => {
                println!("Unknown instruction: {:#?}", raw_op);
                Op::empty(OpKind::Unknown(raw_op.clone()))
            }
        }
    }

}

#[derive(Clone, PartialEq, Eq)]
pub enum Source {
    Literal(Literal),
    Register(Register),
}
impl ::std::fmt::Debug for Source {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Source::Register(ref inner) => write!(f, "Register({:?})", inner),
            Source::Literal(ref inner) => write!(f, "Literal({:?})", inner),
        }
    }
}
impl Source {

    fn from_raw(raw: &RawOpArg, module: &Module) -> Option<Source> {
        Register::from_raw(raw).map(|r| Source::Register(r))
            .or_else(|| Literal::from_raw(raw, module).map(|r| Source::Literal(r)))
    }

    fn x_reg(num: u32) -> Source {
        Source::Register(Register::X(num))
    }

    pub fn get_register(&self) -> Register {
        match *self {
            Source::Register(reg) => reg,
            _ => panic!(),
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomLiteral {
    AtomNil,
    Atom(Atom),
}
impl AtomLiteral {
    fn from_raw(raw: &RawOpArg, module: &Module) -> Option<AtomLiteral> {
        match *raw {
            RawOpArg::Atom(0) => Some(AtomLiteral::AtomNil),
            RawOpArg::Atom(id) => Some(AtomLiteral::Atom(module.atoms[id as usize - 1].clone())),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Literal {
    Atom(AtomLiteral),
    Integer(u32),
    LiteralRef(u32),
}
impl ::std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Literal::Atom(ref inner) => write!(f, "Atom({:?})", inner),
            Literal::Integer(ref inner) => write!(f, "Integer({:?})", inner),
            Literal::LiteralRef(ref inner) => write!(f, "LiteralRef({:?})", inner),
        }
    }
}
impl Literal {

    fn from_raw(raw: &RawOpArg, module: &Module) -> Option<Literal> {
        match *raw {
            RawOpArg::Atom(_) => Some(Literal::Atom(AtomLiteral::from_raw(raw, module).unwrap())),
            RawOpArg::Literal(id) => Some(Literal::LiteralRef(id)),
            RawOpArg::Integer(integer) => Some(Literal::Integer(integer)),
            _ => None,
        }
    }

}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum Register {
    X(u32),
    Y(u32),
}
impl ::std::fmt::Debug for Register {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Register::X(ref inner) => write!(f, "X({:?})", inner),
            Register::Y(ref inner) => write!(f, "Y({:?})", inner),
        }
    }
}
impl Register {

    fn from_raw(raw: &RawOpArg) -> Option<Register> {
        match *raw {
            RawOpArg::XReg(reg) => Some(Register::X(reg)),
            RawOpArg::YReg(reg) => Some(Register::Y(reg)),
            _ => None,
        }
    }

}
