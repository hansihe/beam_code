use ::Atom;
use ::op::{ OpKind, Literal, Register, Source, AtomLiteral };

use ::std::collections::HashMap;
use ::std::hash::Hash;

#[derive(Debug, Clone)]
pub struct SSAOp {
    pub kind: OpKind,
    pub reads: Vec<SSASource>,
    pub writes: Vec<SSARegister>,
    pub w: Vec<Register>,
    pub r: Vec<Source>,
}

#[derive(Debug, Clone)]
pub struct SSAFunction {
    pub name: Atom,
    pub arity: u32,
    pub fun_type: FunDefType,
    pub entry: ExtLabel,
    pub args: Vec<SSARegister>,
    pub blocks: HashMap<ExtLabel, SSABasicBlock>,
}

#[derive(Debug, Copy, Clone)]
pub enum FunDefType {
    Lambda(u32),
    Public,
    Private,
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
    pub inputs: HashMap<ExtLabel, SSASource>,
    pub dead: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SSARegister(pub u32);
impl ::std::fmt::Debug for SSARegister {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "SSARegister({})", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SSASource {
    Register(SSARegister),
    Literal(Literal),
}
impl SSASource {
    pub fn get_register(&self) -> SSARegister {
        match *self {
            SSASource::Register(reg) => reg,
            _ => panic!(),
        }
    }
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

impl SSAFunction {

    pub fn replace_reads(&mut self, orig: SSARegister, replace: &SSASource) -> bool {
        let mut changed = false;
        for (_, block) in self.blocks.iter_mut() {
            for phi in &mut block.phi_nodes {
                for (_, read) in phi.inputs.iter_mut() {
                    if *read == SSASource::Register(orig) {
                        *read = replace.clone();
                        changed = true;
                    }
                }
            }
            for op in &mut block.ops {
                for read in &mut op.reads {
                    if let SSASource::Register(reg) = *read {
                        if reg == orig {
                            *read = replace.clone();
                            changed = true;
                        }
                    }
                }
            }
        }
        changed
    }

    pub fn clean(&mut self) {

        let mut renames: Vec<(SSARegister, SSASource)> = Vec::new();
        for (_, block) in &mut self.blocks {
            block.ops.retain(|ref op| {
                if let OpKind::Move = op.kind {
                    renames.push((op.writes[0], op.reads[0].clone()));
                    return false;
                }
                if let OpKind::Init = op.kind {
                    renames.push((op.writes[0], SSASource::Literal(
                        Literal::Atom(AtomLiteral::AtomNil))));
                    return false;
                }
                true
            })
        }

        //if self.name == Atom::from("into") && self.arity == 4 {
        //    println!("{:?}", renames);
        //}

        // TODO: Make this more efficient
        let mut changed = true;
        while changed {
            changed = false;
            for &(orig, ref replace) in &renames {
                if self.replace_reads(orig, replace) {
                    changed = true;
                }
            }
        }

    }

}
