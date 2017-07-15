use ::op::{ Op, LabelId, Register };
use ::beam_module::Module;

use ::itertools::Itertools;

#[derive(Debug)]
pub struct BasicBlock {
    label: ExtLabel,
    continuation: Option<ExtLabel>,
    jumps: Vec<LabelId>,
    ops: Vec<ExtOp>,
}

#[derive(Debug)]
pub struct SSARegister(u32);

#[derive(Debug)]
pub struct ExtOp {
    op: Op,
    dests: Vec<(Register, SSARegister)>,
    srcs: Vec<Option<(Register, SSARegister)>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ExtLabel {
    Original(u32),
    Added(u32),
}
impl ExtLabel {
    pub fn name(self) -> String {
        match self {
            ExtLabel::Original(l) => format!("o{}", l),
            ExtLabel::Added(l) => format!("a{}", l),
        }
    }
}

pub fn code_to_basic_blocks(code: &[Op], module: &Module) -> Vec<BasicBlock> {
    let mut basic_blocks: Vec<BasicBlock> = code.iter()
        .scan((false, 0u32), |&mut (ref mut last_label, ref mut id), op| {
            let res = match (op.is_label(), op.has_jump(), *last_label) {
                (a, b, _) if a && b => unreachable!(),
                (true, _, true) => panic!(),
                (true, _, false) => {
                    *last_label = true;
                    *id += 1;
                    *id
                },
                (_, true, true) => *id,
                (_, true, false) => {
                    let r = *id;
                    *id += 1;
                    r
                },
                (false, false, _) => *id,
            };
            *last_label = false;

            Some((res, op))
        })
        .group_by(|&(id, _op)| id).into_iter()
        .map(|(_id, ops)|
             ops.into_iter()
             .map(|(_id, op)| {
                 let mut dests_i = op.dests();
                 let dests = dests_i.drain(0..).map(|i| {
                     (i, SSARegister(0))
                 }).collect();

                 let mut srcs_i = op.srcs();
                 let srcs = srcs_i.drain(0..).map(|i| {
                     i.map(|r| (r, SSARegister(0)))
                 }).collect();

                 ExtOp {
                     srcs: srcs,
                     dests: dests,
                     op: op.clone(),
                 }
             })
        )
        .enumerate()
        .map(|(added_label, ops)| BasicBlock {
            label: ExtLabel::Added(added_label as u32),
            continuation: None,
            jumps: Vec::new(),
            ops: ops.collect_vec(),
        })
        .collect_vec();

    for mut block in &mut basic_blocks {
        if let Op::Label { num: id } = block.ops.first().unwrap().op {
            block.label = ExtLabel::Original(id.0);
            block.ops.remove(0);
        }
    }

    // Sanity check
    for block in &basic_blocks {
        assert!(block.ops.len() > 0, "Basic block containing only label found!!");
        for op in &block.ops {
            if let Op::Label { .. } = op.op {
                unreachable!("Undetected label in basic block!!");
            }
        }
    }

    // Assign jumps and continuations
    let mut ssa_register_num: u32 = 0;
    for block_num in 0..(basic_blocks.len() - 1) {
        let elems = &mut basic_blocks[block_num..block_num + 2];

        if elems[0].ops.last().unwrap().op.can_continue() {
            elems[0].continuation = Some(elems[1].label);
            if let Some(jumps) = elems[0].ops.last().unwrap().op.jumps() {
                elems[0].jumps = jumps;
            }
        }

        for mut op in elems[0].ops.iter_mut() {
            for &mut (_, ref mut ssa_reg) in op.dests.iter_mut() {
                ssa_register_num += 1;
                *ssa_reg = SSARegister(ssa_register_num);
            }
        }
    }

    basic_blocks
}

const DOT_BREAK: &str = "<br align=\"left\" />";

use std::io::Write;
pub fn basic_blocks_to_dot(blocks: &[BasicBlock], w: &mut Write) -> ::std::io::Result<()> {
    write!(w, "digraph g {{\n")?;
    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
    write!(w, "edge [fontname=\"Courier New\", fontsize=9]\n\n")?;

    for block in blocks {
        let block_name = block.label.name();

        write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;
        for op in &block.ops {
            let op_line = format!("{:#?}", op).replace("{", "\\{").replace("}", "\\}")
                .replace("\n", DOT_BREAK);
            write!(w, "{}{}", DOT_BREAK, op_line)?;
        }
        write!(w, "> ];\n")?;

        if let Some(label) = block.continuation {
            write!(w, "blk_{} -> blk_{} [ label=cont ];\n", block_name, label.name())?;
        }
        for arg in &block.jumps {
            write!(w, "blk_{} -> blk_o{} [  ];\n", block_name, arg.0)?;
        }
        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
