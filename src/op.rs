use super::{ RawOp, RawOpArg };
use ::beam_module::{ Module, Atom, Import, Lambda };
use ::itertools::Itertools;

use ::std::rc::Rc;

#[derive(Debug)]
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

    // Term construction
    PutList { head: Source, tail: Source, dest: Register },
    PutTuple { arity: u32, dest: Register },
    PutTupleElem { elem: Source },

    // Matching
    SelectVal { source: Source, fail: LabelId, matches: Vec<(Literal, LabelId)> },

    // Errors
    Raise(RaiseType),

    Unknown(RawOp),
}

#[derive(Debug)]
pub enum RaiseType {
    BadMatch(Source),
    IfEnd,
    CaseEnd(Source),
}

#[derive(Debug)]
pub enum BifArgs {
    Bif1(Source),
    Bif2(Source, Source),
}

#[derive(Debug)]
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

    Arity(u32),
    TaggedTuple {
        arity: u32,
        atom: AtomLiteral,
    }
}

#[derive(Debug)]
pub enum BinaryTest {
    // Comparison
    IsLt,
    IsGe,
    IsEq,
    IsNe,
    IsEqExact,
    IsNeExact,
}

#[derive(Copy, Clone)]
pub struct LabelId(u32);

impl ::std::fmt::Debug for LabelId {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "LabelId({})", self.0)
    }
}

impl Op {

    pub fn jumps(&self) -> Option<Vec<LabelId>> {
        let res = match *self {
            Op::UnaryTest { fail: label, .. } => vec![label],
            Op::BinaryTest { fail: label, .. } => vec![label],
            Op::CallBif { fail: label, .. } => vec![label],
            Op::SelectVal { fail: label, ref matches, .. } => {
                let match_labels = matches.iter().map(|m| m.1);
                ::itertools::put_back(match_labels).with_value(label).collect()
            }
            _ => return None,
        };
        Some(res)
    }

    pub fn from_raw(raw_op: &RawOp, module: &Module) -> Op {
        let atoms = &module.atoms;
        let imports = &module.imports;
        let lambdas = &module.lambdas;

        match &raw_op.opcode.name {
            n if n == "return" => Op::Return,
            n if n == "select_val" => {
                Op::SelectVal {
                    source: Source::from_raw(&raw_op.args[0], atoms).unwrap(),
                    fail: LabelId(raw_op.args[1].fail_label()),
                    matches: raw_op.args[2..].iter().tuples()
                        .map(|(val, lab)| (Literal::from_raw(val, atoms).unwrap(),
                                           LabelId(lab.fail_label())))
                        .collect(),
                }
            }
            n if n == "move" => {
                Op::Move {
                    source: Source::from_raw(&raw_op.args[0], atoms).unwrap(),
                    dest: Register::from_raw(&raw_op.args[1]).unwrap(),
                }
            }
            n if n == "line" => {
                Op::Line {
                    num: raw_op.args[0].untagged(),
                }
            }
            n if n == "label" => {
                Op::Label {
                    num: LabelId(raw_op.args[0].untagged()),
                }
            }
            n if n == "func_info" => {
                Op::FuncInfo {
                    module: atoms[raw_op.args[0].atom() as usize - 1].clone(),
                    function: atoms[raw_op.args[1].atom() as usize - 1].clone(),
                    arity: raw_op.args[2].untagged(),
                }
            }
            n if n == "call_ext_only" => {
                Op::CallExtOnly {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                }
            }
            n if n == "call_ext" => {
                Op::CallExt {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                }
            }
            n if n == "call_ext_last" => {
                Op::CallExtLast {
                    arity: raw_op.args[0].untagged(),
                    import: imports[raw_op.args[1].untagged() as usize].clone(),
                    deallocate: raw_op.args[2].untagged(),
                }
            }
            n if n == "bif0" => {
                Op::CallBif0 {
                    bif: imports[raw_op.args[0].untagged() as usize].clone(),
                    dest: Register::from_raw(&raw_op.args[1]).unwrap(),
                }
            }
            n if n == "bif1" => {
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    bif: imports[raw_op.args[1].untagged() as usize].clone(),
                    args: BifArgs::Bif1(Source::from_raw(&raw_op.args[2], atoms).unwrap()),
                    dest: Register::from_raw(&raw_op.args[3]).unwrap(),
                    gc: None,
                }
            }
            n if n == "bif2" => {
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    bif: imports[raw_op.args[1].untagged() as usize].clone(),
                    args: BifArgs::Bif2(Source::from_raw(&raw_op.args[2], atoms).unwrap(),
                                        Source::from_raw(&raw_op.args[3], atoms).unwrap()),
                    dest: Register::from_raw(&raw_op.args[4]).unwrap(),
                    gc: None,
                }
            }
            n if n == "gc_bif1" => {
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    gc: Some(raw_op.args[1].untagged()),
                    bif: imports[raw_op.args[2].untagged() as usize].clone(),
                    args: BifArgs::Bif1(Source::from_raw(&raw_op.args[3], atoms).unwrap()),
                    dest: Register::from_raw(&raw_op.args[4]).unwrap(),
                }
            }
            n if n == "gc_bif2" => {
                Op::CallBif {
                    fail: LabelId(raw_op.args[0].fail_label()),
                    gc: Some(raw_op.args[1].untagged()),
                    bif: imports[raw_op.args[2].untagged() as usize].clone(),
                    args: BifArgs::Bif2(Source::from_raw(&raw_op.args[3], atoms).unwrap(),
                                        Source::from_raw(&raw_op.args[4], atoms).unwrap()),
                    dest: Register::from_raw(&raw_op.args[5]).unwrap(),
                }
            }
            n if n == "allocate" => {
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                    stack_zero: false,
                    heap_need: None,
                }
            }
            n if n == "deallocate" => {
                Op::Deallocate { num: raw_op.args[0].untagged() }
            }
            n if n == "allocate_heap" => {
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    heap_need: Some(raw_op.args[1].untagged()),
                    live: raw_op.args[2].untagged(),
                    stack_zero: false,
                }
            }
            n if n == "allocate_zero" => {
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                    stack_zero: true,
                    heap_need: None,
                }
            }
            n if n == "allocate_heap_zero" => {
                Op::Allocate {
                    stack_need: raw_op.args[0].untagged(),
                    heap_need: Some(raw_op.args[1].untagged()),
                    live: raw_op.args[2].untagged(),
                    stack_zero: true,
                }
            }
            n if n == "trim" => {
                Op::Trim { num: raw_op.args[0].untagged() }
            }
            n if n == "make_fun2" => {
                Op::MakeFun2 { lambda: lambdas[raw_op.args[0].untagged() as usize].clone() }
            }
            n if n == "test_heap" => {
                Op::TestHeap {
                    heap_need: raw_op.args[0].untagged(),
                    live: raw_op.args[1].untagged(),
                }
            }
            n if n == "put_list" => {
                Op::PutList {
                    head: Source::from_raw(&raw_op.args[0], atoms).unwrap(),
                    tail: Source::from_raw(&raw_op.args[1], atoms).unwrap(),
                    dest: Register::from_raw(&raw_op.args[2]).unwrap(),
                }
            }
            n if n == "put_tuple" => {
                Op::PutTuple {
                    arity: raw_op.args[0].untagged(),
                    dest: Register::from_raw(&raw_op.args[1]).unwrap(),
                }
            }
            n if n == "put" => {
                Op::PutTupleElem {
                    elem: Source::from_raw(&raw_op.args[0], atoms).unwrap(),
                }
            }
            n if n == "get_tuple_element" => {
                Op::GetTupleElem {
                    source: Source::from_raw(&raw_op.args[0], atoms).unwrap(),
                    elem: raw_op.args[1].untagged(),
                    dest: Register::from_raw(&raw_op.args[2]).unwrap(),
                }
            }
            n if n == "is_integer" => make_unary_test(raw_op, UnaryTest::Integer, atoms),
            n if n == "is_float" => make_unary_test(raw_op, UnaryTest::Float, atoms),
            n if n == "is_number" => make_unary_test(raw_op, UnaryTest::Number, atoms),
            n if n == "is_atom" => make_unary_test(raw_op, UnaryTest::Atom, atoms),
            n if n == "is_pid" => make_unary_test(raw_op, UnaryTest::Pid, atoms),
            n if n == "is_reference" => make_unary_test(raw_op, UnaryTest::Reference, atoms),
            n if n == "is_port" => make_unary_test(raw_op, UnaryTest::Port, atoms),
            n if n == "is_nil" => make_unary_test(raw_op, UnaryTest::Nil, atoms),
            n if n == "is_binary" => make_unary_test(raw_op, UnaryTest::Binary, atoms),
            n if n == "is_list" => make_unary_test(raw_op, UnaryTest::List, atoms),
            n if n == "is_nonempty_list" => make_unary_test(raw_op, UnaryTest::NonemptyList, atoms),
            n if n == "is_tuple" => make_unary_test(raw_op, UnaryTest::Tuple, atoms),
            n if n == "test_arity" => make_unary_test(raw_op, UnaryTest::Arity(
                raw_op.args[2].untagged()), atoms),
            n if n == "is_tagged_tuple" => {
                make_unary_test(raw_op, UnaryTest::TaggedTuple {
                    arity: raw_op.args[2].untagged(),
                    atom: AtomLiteral::from_raw(&raw_op.args[3], atoms).unwrap(),
                }, atoms)
            }
            n if n == "badmatch" => {
                Op::Raise(RaiseType::BadMatch(Source::from_raw(&raw_op.args[0], atoms).unwrap()))
            }
            n if n == "if_end" => {
                Op::Raise(RaiseType::IfEnd)
            }
            n if n == "case_end" => {
                Op::Raise(RaiseType::CaseEnd(Source::from_raw(&raw_op.args[0], atoms).unwrap()))
            }
            n if n == "int_code_end" => Op::CodeEnd,
            _ => Op::Unknown(raw_op.clone()),
        }
    }

}

fn make_unary_test(raw_op: &RawOp, test: UnaryTest, atoms: &[Atom]) -> Op {
    Op::UnaryTest {
        test: test,
        fail: LabelId(raw_op.args[0].fail_label()),
        arg1: Source::from_raw(&raw_op.args[1], atoms).unwrap(),
    }
}

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

    fn from_raw(raw: &RawOpArg, atoms: &[Atom]) -> Option<Source> {
        Register::from_raw(raw).map(|r| Source::Register(r))
            .or_else(|| Literal::from_raw(raw, atoms).map(|r| Source::Literal(r)))
    }

}

#[derive(Debug)]
pub enum AtomLiteral {
    AtomNil,
    Atom(Atom),
}
impl AtomLiteral {
    fn from_raw(raw: &RawOpArg, atoms: &[Atom]) -> Option<AtomLiteral> {
        match *raw {
            RawOpArg::Atom(0) => Some(AtomLiteral::AtomNil),
            RawOpArg::Atom(id) => Some(AtomLiteral::Atom(atoms[id as usize - 1].clone())),
            _ => None,
        }
    }
}

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

    fn from_raw(raw: &RawOpArg, atoms: &[Atom]) -> Option<Literal> {
        match *raw {
            RawOpArg::Atom(_) => Some(Literal::Atom(AtomLiteral::from_raw(raw, atoms).unwrap())),
            RawOpArg::Literal(id) => Some(Literal::LiteralRef(id)),
            RawOpArg::Integer(integer) => Some(Literal::Integer(integer)),
            _ => None,
        }
    }

}

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
