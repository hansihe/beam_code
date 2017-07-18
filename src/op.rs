use super::{ RawOp, RawOpArg };
use ::beam_module::{ Module, Atom, Import, Lambda };
use ::itertools::Itertools;

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

#[derive(Debug, Clone)]
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

    // Errors
    Raise(RaiseType),
    Try { exception_ctx: Register, landing_pad: LabelId },
    TryEnd { exception_ctx: Register },
    TryCase { exception_ctx: Register },

    Unknown(RawOp),
}

#[derive(Debug, Clone)]
pub enum RaiseType {
    /// 2 reads, error class and value
    Error,
    /// 1 read, the failed match
    BadMatch,
    IfEnd,
    /// 1 read, the failed match
    CaseEnd,
}

#[derive(Debug, Clone)]
pub enum BifArgs {
    Bif1(Source),
    Bif2(Source, Source),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
            _ => false,
        }
    }

    pub fn can_continue(&self) -> bool {
        match self.kind {
            OpKind::Raise(_) => false,
            OpKind::Return => false,
            // Special case, :erlang.error BIF never returns
            OpKind::CallExt { ref import, .. } if import.module.string == "erlang"
                && import.function.string == "error" => false,
            OpKind::CallExtLast { .. } => false,
            OpKind::CallExtOnly { .. } => false,
            OpKind::Jump => false,
            OpKind::SelectVal { .. } => false, // Is this right? Can label be 0?
            OpKind::CallLast { .. } => false,
            OpKind::CallOnly { .. } => false,
            OpKind::TupleArity { .. } => false,
            _ => true,
        }
    }

    pub fn from_raw(raw_op: &RawOp, module: &Module) -> Op {
        let atoms = &module.atoms;
        let imports = &module.imports;
        let lambdas = &module.lambdas;

        match &raw_op.opcode.name {
            n if n == "return" =>
                Op::with_reads(OpKind::Return, vec![Source::x_reg(0)]),
            n if n == "select_val" => {
                let matches = raw_op.args[2..].iter().tuples()
                    .map(|(val, lab)| (Literal::from_raw(val, module).unwrap(),
                                       LabelId(lab.fail_label())))
                    .collect_vec();
                Op {
                    kind: OpKind::SelectVal {
                        matches: matches.iter().map(|&(ref val, _lab)| val.clone()).collect(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[0], module).unwrap()],
                    writes: vec![],
                    labels: ::itertools::put_back(matches.iter().map(|&(ref _val, lab)| lab))
                        .with_value(LabelId(raw_op.args[1].fail_label()))
                        .collect(),
                }
            }
            n if n == "move" =>
                Op::with_reads_writes(OpKind::Move,
                                      vec![Source::from_raw(&raw_op.args[0], module).unwrap()],
                                      vec![Register::from_raw(&raw_op.args[1]).unwrap()]),
            n if n == "init" =>
                Op::with_reads_writes(OpKind::Init, vec![], vec![
                    Register::from_raw(&raw_op.args[0]).unwrap()]),
            n if n == "line" =>
                Op::empty(OpKind::Line {
                    num: raw_op.args[0].untagged(),
                }),
            n if n == "label" =>
                Op::empty(OpKind::Label {
                    num: LabelId(raw_op.args[0].untagged()),
                }),
            n if n == "func_info" =>
                Op::empty(OpKind::FuncInfo {
                    module: atoms[raw_op.args[0].atom() as usize - 1].clone(),
                    function: atoms[raw_op.args[1].atom() as usize - 1].clone(),
                    arity: raw_op.args[2].untagged(),
                }),
            n if n == "has_map_fields" =>
                Op::with_reads_labels(
                    OpKind::HasMapFields,
                    raw_op.args[1..].iter()
                        .map(|v| Source::from_raw(v, module).unwrap())
                        .collect(),
                    vec![LabelId(raw_op.args[0].fail_label())]
                ),
            n if n == "select_tuple_arity" => {
                let matches = raw_op.args[2..].iter().tuples()
                    .map(|(val, lab)| (val.untagged(),
                                       LabelId(lab.fail_label())))
                    .collect_vec();
                Op {
                    kind: OpKind::TupleArity {
                        matches: matches.iter().map(|&(val, _)| val).collect(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[0], module).unwrap()],
                    writes: vec![],
                    labels: ::itertools::put_back(matches.iter().map(|&(_, lab)| lab))
                        .with_value(LabelId(raw_op.args[1].fail_label()))
                        .collect(),
                }
            }
            // FIXME read registers
            n if n == "call_ext_only" =>
                Op::empty(OpKind::CallExtOnly {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                }),
            n if n == "call_ext" =>
                Op::empty(OpKind::CallExt {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                }),
            n if n == "call_ext_last" =>
                Op::empty(OpKind::CallExtLast {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                    deallocate: raw_op.args[2].untagged(),
                }),
            n if n == "call" => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads(
                    OpKind::Call {
                        arity: arity,
                        fun_label: LabelId(raw_op.args[1].fail_label()),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect()
                )
            }
            n if n == "call_only" => {
                let arity = raw_op.args[0].untagged();
                Op::with_reads(
                    OpKind::CallOnly {
                        arity: arity,
                        fun_label: LabelId(raw_op.args[1].fail_label()),
                    },
                    (0..arity).map(|r| Source::Register(Register::X(r))).collect()
                )
            }
            n if n == "call_last" => {
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
            n if n == "bif0" => Op {
                kind: OpKind::CallBif {
                    bif: imports[raw_op.args[0].untagged() as usize].clone(),
                    gc: None,
                },
                reads: vec![],
                writes: vec![Register::from_raw(&raw_op.args[1]).unwrap()],
                labels: vec![],
            },
            n if n == "bif1" => {
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
            n if n == "bif2" => {
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
            n if n == "gc_bif1" => {
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
            n if n == "gc_bif2" => {
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
            n if n == "allocate" =>
                Op::empty(OpKind::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                    stack_zero: false,
                    heap_need: None,
                }),
            n if n == "deallocate" =>
                Op::empty(OpKind::Deallocate { num: raw_op.args[0].untagged() }),
            n if n == "allocate_heap" =>
                Op::empty(OpKind::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    heap_need: Some(raw_op.args[1].untagged()),
                    live: raw_op.args[2].untagged(),
                    stack_zero: false,
                }),
            n if n == "allocate_zero" =>
                Op::empty(OpKind::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                    stack_zero: true,
                    heap_need: None,
                }),
            n if n == "allocate_heap_zero" =>
                Op::empty(OpKind::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    heap_need: Some(raw_op.args[1].untagged()),
                    live: raw_op.args[2].untagged(),
                    stack_zero: true,
                }),
            n if n == "trim" =>
                Op::empty(OpKind::Trim { num: raw_op.args[0].untagged() }),
            // TODO
            n if n == "make_fun2" =>
                Op::empty(OpKind::MakeFun2 {
                    lambda: lambdas[raw_op.args[0].untagged() as usize].clone()
                }),
            n if n == "call_fun" => {
                let arity = raw_op.args[0].untagged();
                Op {
                    kind: OpKind::CallFun { arity: arity },
                    labels: vec![],
                    writes: vec![Register::X(0)], // TODO: Unsure?
                    reads: (0..(arity+1)).map(|n| Source::Register(Register::X(n))).collect(),
                }
            }
            n if n == "test_heap" =>
                Op::empty(OpKind::TestHeap {
                    heap_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                }),
            n if n == "put_list" =>
                Op {
                    kind: OpKind::PutList,
                    reads: vec![Source::from_raw(&raw_op.args[0], module).unwrap(),
                                Source::from_raw(&raw_op.args[1], module).unwrap()],
                    writes: vec![Register::from_raw(&raw_op.args[2]).unwrap()],
                    labels: vec![],
                },
            n if n == "put_tuple" =>
                Op {
                    kind: OpKind::PutTuple {
                        arity: raw_op.args[0].untagged(),
                    },
                    reads: vec![],
                    writes: vec![Register::from_raw(&raw_op.args[1]).unwrap()],
                    labels: vec![],
                },
            n if n == "put" =>
                Op {
                    kind: OpKind::PutTupleElem,
                    reads: vec![Source::from_raw(&raw_op.args[0], module).unwrap()],
                    writes: vec![],
                    labels: vec![],
                },
            n if n == "get_tuple_element" =>
                Op {
                    kind: OpKind::GetTupleElem {
                        elem: raw_op.args[1].untagged(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[0], module).unwrap()],
                    writes: vec![Register::from_raw(&raw_op.args[2]).unwrap()],
                    labels: vec![],
                },
            n if n == "get_list" => {
                Op::with_reads_writes(
                    OpKind::GetList,
                    vec![Source::from_raw(&raw_op.args[0], module).unwrap()],
                    vec![Register::from_raw(&raw_op.args[1]).unwrap(),
                         Register::from_raw(&raw_op.args[2]).unwrap()]
                )
            }
            n if n == "is_integer" => make_unary_test(raw_op, UnaryTest::Integer, module),
            n if n == "is_float" => make_unary_test(raw_op, UnaryTest::Float, module),
            n if n == "is_number" => make_unary_test(raw_op, UnaryTest::Number, module),
            n if n == "is_atom" => make_unary_test(raw_op, UnaryTest::Atom, module),
            n if n == "is_pid" => make_unary_test(raw_op, UnaryTest::Pid, module),
            n if n == "is_reference" => make_unary_test(raw_op, UnaryTest::Reference, module),
            n if n == "is_port" => make_unary_test(raw_op, UnaryTest::Port, module),
            n if n == "is_nil" => make_unary_test(raw_op, UnaryTest::Nil, module),
            n if n == "is_binary" => make_unary_test(raw_op, UnaryTest::Binary, module),
            n if n == "is_list" => make_unary_test(raw_op, UnaryTest::List, module),
            n if n == "is_nonempty_list" => make_unary_test(raw_op, UnaryTest::NonemptyList, module),
            n if n == "is_tuple" => make_unary_test(raw_op, UnaryTest::Tuple, module),
            n if n == "is_map" => make_unary_test(raw_op, UnaryTest::Map, module),
            n if n == "is_boolean" => make_unary_test(raw_op, UnaryTest::Boolean, module),
            n if n == "is_function" => make_unary_test(raw_op, UnaryTest::Function, module),
            n if n == "test_arity" => make_unary_test(raw_op, UnaryTest::Arity(
                raw_op.args[2].untagged()), module),
            n if n == "is_tagged_tuple" => {
                make_unary_test(raw_op, UnaryTest::TaggedTuple {
                    arity: raw_op.args[2].untagged(),
                    atom: AtomLiteral::from_raw(&raw_op.args[3], module).unwrap(),
                }, module)
            }
            n if n == "is_function2" =>
                make_unary_test(raw_op, UnaryTest::FunctionArity(raw_op.args[2].integer()), module),
            n if n == "is_eq" => make_binary_test(raw_op, BinaryTest::IsEq, module),
            n if n == "is_ne" => make_binary_test(raw_op, BinaryTest::IsNe, module),
            n if n == "is_ge" => make_binary_test(raw_op, BinaryTest::IsGe, module),
            n if n == "is_lt" => make_binary_test(raw_op, BinaryTest::IsLt, module),
            n if n == "is_eq_exact" => make_binary_test(raw_op, BinaryTest::IsEqExact, module),
            n if n == "is_ne_exact" => make_binary_test(raw_op, BinaryTest::IsNeExact, module),
            n if n == "badmatch" =>
                Op::with_reads(
                    OpKind::Raise(RaiseType::BadMatch),
                    vec![Source::from_raw(&raw_op.args[0], module).unwrap()]
                ),
            n if n == "if_end" =>
                Op::empty(OpKind::Raise(RaiseType::IfEnd)),
            n if n == "case_end" =>
                Op::with_reads(
                    OpKind::Raise(RaiseType::CaseEnd),
                    vec![Source::from_raw(&raw_op.args[0], module).unwrap()]
                ),
            n if n == "int_code_end" =>
                Op::empty(OpKind::CodeEnd),
            n if n == "get_map_elements" => {
                let entries = raw_op.args[2..].iter().tuples()
                    .map(|(key, reg)| (Source::from_raw(key, module).unwrap(),
                                       Register::from_raw(reg).unwrap()))
                    .collect_vec();
                Op {
                    kind: OpKind::GetMapElems {
                        entries: entries.iter().map(|&(ref lit, _)| lit.clone()).collect(),
                    },
                    reads: vec![Source::from_raw(&raw_op.args[1], module).unwrap()],
                    writes: entries.iter().map(|&(_, reg)| reg).collect(),
                    labels: vec![LabelId(raw_op.args[0].fail_label())],
                }
            },
            n if n == "try" =>
                Op {
                    kind: OpKind::Try {
                        exception_ctx: Register::from_raw(&raw_op.args[0]).unwrap(),
                        landing_pad: LabelId(raw_op.args[1].fail_label()),
                    },
                    reads: vec![],
                    writes: vec![],
                    labels: vec![LabelId(raw_op.args[1].fail_label())],
                },
            n if n == "try_end" =>
                Op::empty(OpKind::TryEnd {
                    exception_ctx: Register::from_raw(&raw_op.args[0]).unwrap()
                }),
            n if n == "try_case" =>
                Op::empty(OpKind::TryCase {
                    exception_ctx: Register::from_raw(&raw_op.args[0]).unwrap()
                }),
            n if n == "raise" =>
                Op::with_reads(
                    OpKind::Raise(RaiseType::Error),
                    vec![Source::from_raw(&raw_op.args[0], module).unwrap(),
                         Source::from_raw(&raw_op.args[1], module).unwrap()]
                ),
            n if n == "jump" => Op {
                kind: OpKind::Jump,
                labels: vec![LabelId(raw_op.args[0].fail_label())],
                reads: vec![],
                writes: vec![],
            },
            _ => {
                println!("Unknown instruction: {:#?}", raw_op);
                Op::empty(OpKind::Unknown(raw_op.clone()))
            }
        }
    }

}

fn make_unary_test(raw_op: &RawOp, test: UnaryTest, module: &Module) -> Op {
    Op::with_reads_labels(
        OpKind::UnaryTest {
            test: test,
        },
        vec![Source::from_raw(&raw_op.args[1], module).unwrap()],
        vec![LabelId(raw_op.args[0].fail_label())]
    )
}
fn make_binary_test(raw_op: &RawOp, test: BinaryTest, module: &Module) -> Op {
    Op::with_reads_labels(
        OpKind::BinaryTest {
            test: test,
        },
        vec![Source::from_raw(&raw_op.args[1], module).unwrap(),
             Source::from_raw(&raw_op.args[2], module).unwrap()],
        vec![LabelId(raw_op.args[0].fail_label())]
    )
}

#[derive(Clone)]
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
