use ::beam_module::Atom;
use ::op::{ OpKind, Literal };

use ::std::collections::HashMap;
use ::std::hash::Hash;

#[derive(Debug, Clone)]
pub struct SSAOp {
    pub kind: OpKind,
    pub reads: Vec<SSASource>,
    pub writes: Vec<SSARegister>,
}

#[derive(Debug, Clone)]
pub struct SSAFunction {
    pub name: Atom,
    pub arity: u32,
    pub num_free: u32,
    pub entry: ExtLabel,
    pub args: Vec<SSARegister>,
    pub blocks: HashMap<ExtLabel, SSABasicBlock>,
}

#[derive(Debug, Clone)]
pub struct SSABasicBlock {
    pub label: ExtLabel,
    pub continuation: Option<ExtLabel>,
    pub jumps: Vec<ExtLabel>,
    pub ops: Vec<SSAOp>,
    pub phi_nodes: Vec<PhiNode>,
}

#[derive(Debug, Clone)]
pub struct PhiNode {
    pub output: SSARegister,
    pub inputs: HashMap<ExtLabel, SSARegister>,
    pub dead: bool,
}

#[derive(Copy, Clone)]
pub struct SSARegister(pub u32);
impl ::std::fmt::Debug for SSARegister {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "SSARegister({})", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum SSASource {
    Register(SSARegister),
    Literal(Literal),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ExtLabel {
    Original(u32),
    Added(u32),
}
impl ::std::fmt::Debug for ExtLabel {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            ExtLabel::Original(i) => write!(f, "Original({})", i),
            ExtLabel::Added(i) => write!(f, "Added({})", i),
        }
    }
}
impl ExtLabel {
    pub fn name(self) -> String {
        match self {
            ExtLabel::Original(l) => format!("o{}", l),
            ExtLabel::Added(l) => format!("a{}", l),
        }
    }
}
