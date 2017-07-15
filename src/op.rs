use super::{ RawOp, RawOpArg };
use ::beam_module::{ Module, Atom, Import, Lambda };
use ::itertools::Itertools;

use ::std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Op {

    // Info
    Line { num: u32 },
    Label { num: LabelId },
    FuncInfo { module: Atom, function: Atom, arity: u32 },
    CodeEnd,

    // General
    Move { source: Source, dest: Register },

    // Functions
    MakeFun2 { lambda: Lambda },
    Return,

    // Testing
    UnaryTest { arg1: Source, fail: LabelId, test: UnaryTest },
    BinaryTest { arg1: Source, arg2: Source, fail: LabelId, test: BinaryTest },
    TestHeap { heap_need: u32, live: u32 },

    // Calls
    CallBif0 { bif: Import, dest: Register },
    CallBif {
        /// If Some, gc can occur in BIF. Inner is number of live X registers.
        gc: Option<u32>,
        bif: Import,
        fail: LabelId,
        args: BifArgs,
        dest: Register
    },
    CallExt { arity: u32, import: Import },
    CallExtOnly { arity: u32, import: Import },
    CallExtLast { arity: u32, import: Import, deallocate: u32 },

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
    GetTupleElem { source: Source, elem: u32, dest: Register },
    GetMapElems { source: Source, fail: LabelId, entries: Vec<(Literal, Register)> },

    // Term construction
    PutList { head: Source, tail: Source, dest: Register },
    PutTuple { arity: u32, dest: Register },
    PutTupleElem { elem: Source },

    // Matching
    SelectVal { source: Source, fail: LabelId, matches: Vec<(Literal, LabelId)> },

    // Errors
    Raise(RaiseType),
    Try { exception_ctx: Register, landing_pad: LabelId },
    TryEnd { exception_ctx: Register },
    TryCase { exception_ctx: Register },

    Unknown(RawOp),
}

#[derive(Debug, Clone)]
pub enum RaiseType {
    Error { class: Source, value: Source },
    BadMatch(Source),
    IfEnd,
    CaseEnd(Source),
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

    Arity(u32),
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
        if let Op::Label { .. } = *self {
            true
        } else {
            false
        }
    }

    pub fn has_jump(&self) -> bool {
        match *self {
            Op::UnaryTest { .. } => true,
            Op::BinaryTest { .. } => true,
            Op::CallBif { fail: label, .. } if label != LabelId(0) => true,
            Op::SelectVal { .. } => true,
            Op::GetMapElems { .. } => true,
            _ => false,
        }
    }

    pub fn jumps(&self) -> Option<Vec<LabelId>> {
        let res = match *self {
            Op::UnaryTest { fail: label, .. } => vec![label],
            Op::BinaryTest { fail: label, .. } => vec![label],
            Op::CallBif { fail: label, .. } => vec![label],
            Op::SelectVal { fail: label, ref matches, .. } => {
                let match_labels = matches.iter().map(|m| m.1);
                ::itertools::put_back(match_labels).with_value(label).collect()
            }
            Op::GetMapElems { fail: label, .. } => vec![label],
            _ => return None,
        };
        Some(res)
    }

    pub fn can_continue(&self) -> bool {
        match *self {
            Op::Raise(_) => false,
            Op::Return => false,
            Op::CallExtLast { .. } => false,
            Op::CallExtOnly { .. } => false,
            _ => true,
        }
    }

    pub fn dests(&self) -> Vec<Register> {
        match *self {
            Op::Move { dest, .. } => vec![dest],
            // MakeFun2
            Op::CallBif0 { dest, .. } => vec![dest],
            Op::CallBif { dest, .. } => vec![dest],
            // Call*
            Op::GetTupleElem { dest, .. } => vec![dest],
            Op::GetMapElems { ref entries, .. } =>
                entries.iter().map(|i| i.1).collect(),
            Op::PutList { dest, .. } => vec![dest],
            Op::PutTuple { dest, .. } => vec![dest],
            Op::Try { exception_ctx, .. } => vec![exception_ctx],
            _ => vec![],
        }
    }

    pub fn srcs(&self) -> Vec<Option<Register>> {
        fn src2reg(src: &Source) -> Option<Register> {
            match *src {
                Source::Register(reg) => Some(reg),
                _ => None,
            }
        }

        match *self {
            Op::Move { ref source, .. } => vec![src2reg(source)],
            Op::CallBif { args: BifArgs::Bif1(ref a1), .. } => vec![src2reg(a1)],
            Op::CallBif { args: BifArgs::Bif2(ref a1, ref a2), .. } =>
                vec![src2reg(a1), src2reg(a2)],
            Op::CallExt { arity: n, .. } => (0..n).map(|i| Some(Register::X(i))).collect(),
            Op::CallExtOnly { arity: n, .. } => (0..n).map(|i| Some(Register::X(i))).collect(),
            Op::CallExtLast { arity: n, .. } => (0..n).map(|i| Some(Register::X(i))).collect(),
            Op::GetTupleElem { ref source, .. } => vec![src2reg(source)],
            Op::GetMapElems { ref source, .. } => vec![src2reg(source)],
            Op::PutList { ref head, ref tail, .. } => vec![src2reg(head), src2reg(tail)],
            Op::PutTupleElem { ref elem, .. } => vec![src2reg(elem)],
            Op::SelectVal { ref source, .. } => vec![src2reg(source)],
            Op::Raise(RaiseType::Error { ref class, ref value }) =>
                vec![src2reg(class), src2reg(value)],
            Op::Raise(RaiseType::BadMatch(ref source)) => vec![src2reg(source)],
            Op::Raise(RaiseType::CaseEnd(ref source)) => vec![src2reg(source)],
            Op::TryEnd { exception_ctx } => vec![Some(exception_ctx)],
            Op::TryCase { exception_ctx } => vec![Some(exception_ctx)],
            _ => vec![],
        }
    }

    pub fn from_raw(raw_op: &RawOp, module: &Module) -> Op {
        let atoms = &module.atoms;
        let imports = &module.imports;
        let lambdas = &module.lambdas;

        match &raw_op.opcode.name {
            n if n == "return" =>
                Op::Return,
            n if n == "select_val" =>
                Op::SelectVal {
                    source: Source::from_raw(&raw_op.args[0], module).unwrap(),
                    fail: LabelId(raw_op.args[1].fail_label()),
                    matches: raw_op.args[2..].iter().tuples()
                        .map(|(val, lab)| (Literal::from_raw(val, module).unwrap(),
                                           LabelId(lab.fail_label())))
                        .collect(),
                },
            n if n == "move" =>
                Op::Move {
                    source: Source::from_raw(&raw_op.args[0], module).unwrap(),
                    dest: Register::from_raw(&raw_op.args[1]).unwrap(),
                },
            n if n == "line" =>
                Op::Line {
                    num: raw_op.args[0].untagged(),
                },
            n if n == "label" =>
                Op::Label {
                    num: LabelId(raw_op.args[0].untagged()),
                },
            n if n == "func_info" =>
                Op::FuncInfo {
                    module: atoms[raw_op.args[0].atom() as usize - 1].clone(),
                    function: atoms[raw_op.args[1].atom() as usize - 1].clone(),
                    arity: raw_op.args[2].untagged(),
                },
            n if n == "call_ext_only" =>
                Op::CallExtOnly {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                },
            n if n == "call_ext" =>
                Op::CallExt {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                },
            n if n == "call_ext_last" =>
                Op::CallExtLast {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                    deallocate: raw_op.args[2].untagged(),
                },
            n if n == "bif0" =>
                Op::CallBif0 {
                    bif: imports[raw_op.args[0].untagged() as usize].clone(),
                    dest: Register::from_raw(&raw_op.args[1]).unwrap(),
                },
            n if n == "bif1" =>
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    bif: imports[raw_op.args[1].untagged() as usize].clone(),
                    args: BifArgs::Bif1(Source::from_raw(&raw_op.args[2], module).unwrap()),
                    dest: Register::from_raw(&raw_op.args[3]).unwrap(),
                    gc: None,
                },
            n if n == "bif2" =>
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    bif: imports[raw_op.args[1].untagged() as usize].clone(),
                    args: BifArgs::Bif2(Source::from_raw(&raw_op.args[2], module).unwrap(),
                                        Source::from_raw(&raw_op.args[3], module).unwrap()),
                    dest: Register::from_raw(&raw_op.args[4]).unwrap(),
                    gc: None,
                },
            n if n == "gc_bif1" =>
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    gc: Some(raw_op.args[1].untagged()),
                    bif: imports[raw_op.args[2].untagged() as usize].clone(),
                    args: BifArgs::Bif1(Source::from_raw(&raw_op.args[3], module).unwrap()),
                    dest: Register::from_raw(&raw_op.args[4]).unwrap(),
                },
            n if n == "gc_bif2" =>
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    gc: Some(raw_op.args[1].untagged()),
                    bif: imports[raw_op.args[2].untagged() as usize].clone(),
                    args: BifArgs::Bif2(Source::from_raw(&raw_op.args[3], module).unwrap(),
                                        Source::from_raw(&raw_op.args[4], module).unwrap()),
                    dest: Register::from_raw(&raw_op.args[5]).unwrap(),
                },
            n if n == "allocate" =>
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                    stack_zero: false,
                    heap_need: None,
                },
            n if n == "deallocate" =>
                Op::Deallocate { num: raw_op.args[0].untagged() },
            n if n == "allocate_heap" =>
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    heap_need: Some(raw_op.args[1].untagged()),
                    live: raw_op.args[2].untagged(),
                    stack_zero: false,
                },
            n if n == "allocate_zero" =>
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                    stack_zero: true,
                    heap_need: None,
                },
            n if n == "allocate_heap_zero" =>
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    heap_need: Some(raw_op.args[1].untagged()),
                    live: raw_op.args[2].untagged(),
                    stack_zero: true,
                },
            n if n == "trim" =>
                Op::Trim { num: raw_op.args[0].untagged() },
            n if n == "make_fun2" =>
                Op::MakeFun2 {
                    lambda: lambdas[raw_op.args[0].untagged() as usize].clone()
                },
            n if n == "test_heap" =>
                Op::TestHeap {
                    heap_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                },
            n if n == "put_list" =>
                Op::PutList {
                    head: Source::from_raw(&raw_op.args[0], module).unwrap(),
                    tail: Source::from_raw(&raw_op.args[1], module).unwrap(),
                    dest: Register::from_raw(&raw_op.args[2]).unwrap(),
                },
            n if n == "put_tuple" =>
                Op::PutTuple {
                    arity: raw_op.args[0].untagged(),
                    dest: Register::from_raw(&raw_op.args[1]).unwrap(),
                },
            n if n == "put" =>
                Op::PutTupleElem {
                    elem: Source::from_raw(&raw_op.args[0], module).unwrap(),
                },
            n if n == "get_tuple_element" =>
                Op::GetTupleElem {
                    source: Source::from_raw(&raw_op.args[0], module).unwrap(),
                    elem: raw_op.args[1].untagged(),
                    dest: Register::from_raw(&raw_op.args[2]).unwrap(),
                },
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
            n if n == "test_arity" => make_unary_test(raw_op, UnaryTest::Arity(
                raw_op.args[2].untagged()), module),
            n if n == "is_tagged_tuple" => {
                make_unary_test(raw_op, UnaryTest::TaggedTuple {
                    arity: raw_op.args[2].untagged(),
                    atom: AtomLiteral::from_raw(&raw_op.args[3], module).unwrap(),
                }, module)
            }
            n if n == "is_eq_exact" => make_binary_test(raw_op, BinaryTest::IsEqExact, module),
            n if n == "is_ne_exact" => make_binary_test(raw_op, BinaryTest::IsNeExact, module),
            n if n == "badmatch" =>
                Op::Raise(RaiseType::BadMatch(
                    Source::from_raw(&raw_op.args[0], module).unwrap())),
            n if n == "if_end" =>
                Op::Raise(RaiseType::IfEnd),
            n if n == "case_end" =>
                Op::Raise(RaiseType::CaseEnd(
                    Source::from_raw(&raw_op.args[0], module).unwrap())),
            n if n == "int_code_end" =>
                Op::CodeEnd,
            n if n == "get_map_elements" =>
                Op::GetMapElems {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    source: Source::from_raw(&raw_op.args[1], module).unwrap(),
                    entries: raw_op.args[2..].iter().tuples()
                        .map(|(key, reg)| (Literal::from_raw(key, module).unwrap(),
                                           Register::from_raw(reg).unwrap()))
                        .collect(),
                },
            n if n == "try" =>
                Op::Try {
                    exception_ctx: Register::from_raw(&raw_op.args[0]).unwrap(),
                    landing_pad: LabelId(raw_op.args[1].fail_label()),
                },
            n if n == "try_end" =>
                Op::TryEnd {
                    exception_ctx: Register::from_raw(&raw_op.args[0]).unwrap()
                },
            n if n == "try_case" =>
                Op::TryCase {
                    exception_ctx: Register::from_raw(&raw_op.args[0]).unwrap()
                },
            n if n == "raise" =>
                Op::Raise(RaiseType::Error {
                    value: Source::from_raw(&raw_op.args[0], module).unwrap(),
                    class: Source::from_raw(&raw_op.args[1], module).unwrap(),
                }),
            _ => {
                println!("Unknown instruction: {:#?}", raw_op);
                Op::Unknown(raw_op.clone())
            }
        }
    }

}

fn make_unary_test(raw_op: &RawOp, test: UnaryTest, module: &Module) -> Op {
    Op::UnaryTest {
        test: test,
        fail: LabelId(raw_op.args[0].fail_label()),
        arg1: Source::from_raw(&raw_op.args[1], module).unwrap(),
    }
}
fn make_binary_test(raw_op: &RawOp, test: BinaryTest, module: &Module) -> Op {
    Op::BinaryTest {
        test: test,
        fail: LabelId(raw_op.args[0].fail_label()),
        arg1: Source::from_raw(&raw_op.args[1], module).unwrap(),
        arg2: Source::from_raw(&raw_op.args[2], module).unwrap(),
    }
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

}

#[derive(Debug, Clone)]
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

#[derive(Clone)]
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

#[derive(Clone, Copy)]
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
