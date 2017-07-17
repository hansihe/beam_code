use ::std::collections::HashSet;
use ::std::collections::HashMap;

use ::op::{ Op, LabelId, Register, OpKind, Source };
use ::beam_module::{ Module, Atom };

use ::itertools::Itertools;

#[derive(Debug)]
pub struct Function {
    name: Atom,
    arity: u32,
    num_free: u32,
    entry: ExtLabel,
    blocks: HashMap<ExtLabel, BasicBlock>,
    args: Vec<(Register, SSARegister)>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    label: ExtLabel,
    continuation: Option<ExtLabel>,
    jumps: Vec<LabelId>,
    ops: Vec<ExtOp>,
    phi_nodes: HashMap<Register, PhiNode>,
}

#[derive(Debug, Clone)]
struct PhiNode {
    output: SSARegister,
    inputs: HashMap<ExtLabel, SSARegister>,
    dead: bool,
}

#[derive(Copy, Clone)]
pub struct SSARegister(u32);
impl ::std::fmt::Debug for SSARegister {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "SSARegister({})", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct ExtOp {
    op: Op,
    writes: Vec<SSARegister>,
    reads: Vec<Option<SSARegister>>,
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

pub fn code_to_functions(code: &[Op], module: &Module) -> Vec<Function> {
    // Split bytecode into opcodes
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
                 let dests = op.writes.iter().map(|i| {
                     SSARegister(0)
                 }).collect();

                 let srcs = op.reads.iter().map(|i| {
                     if let Source::Register(reg) = *i {
                         Some(SSARegister(0))
                     } else {
                         None
                     }
                 }).collect();

                 ExtOp {
                     reads: srcs,
                     writes: dests,
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
            phi_nodes: HashMap::new(),
        })
        .collect_vec();

    // Label basic blocks
    for mut block in &mut basic_blocks {
        if let OpKind::Label { num: id } = block.ops.first().unwrap().op.kind {
            block.label = ExtLabel::Original(id.0);
            block.ops.remove(0);
        }
    }

    // Sanity check
    for block in &basic_blocks {
        assert!(block.ops.len() > 0, "Basic block containing only label found!!");
        for op in &block.ops {
            if let OpKind::Label { .. } = op.op.kind {
                unreachable!("Undetected label in basic block!!");
            }
        }
    }

    // Assign continuations
    for block_num in 0..(basic_blocks.len() - 1) {
        let elems = &mut basic_blocks[block_num..block_num + 2];

        if elems[0].ops.last().unwrap().op.can_continue() {
            elems[0].continuation = Some(elems[1].label);
        }
    }

    // Assign jumps
    for mut block in &mut basic_blocks {
        block.jumps = block.ops.last().unwrap().op.labels.clone();
    }

    let resolve_label = |label_id| {
        let mut blocks: HashMap<ExtLabel, BasicBlock> = HashMap::new();

        let mut to_add: Vec<ExtLabel> = vec![label_id];

        while to_add.len() != 0 {
            let to_add_label = to_add.pop().unwrap();
            if blocks.contains_key(&to_add_label) {
                continue;
            }

            let block = basic_blocks.iter().find(|b| b.label == to_add_label).unwrap();
            blocks.insert(to_add_label, block.clone());

            if let Some(continuation) = block.continuation {
                to_add.push(continuation);
            }
            for label in &block.jumps {
                to_add.push(ExtLabel::Original(label.0));
            }
        }

        blocks
    };

    // Divide graph into separate functions
    let mut functions = Vec::new();

    for export in &module.exports {
        let label = ExtLabel::Original(export.label);
        functions.push(Function {
            name: export.function.clone(),
            arity: export.arity,
            num_free: 0,
            entry: label,
            blocks: resolve_label(label),
            args: Vec::new(),
        });
    }
    for lambda in &module.lambdas {
        let label = ExtLabel::Original(lambda.label);
        functions.push(Function {
            name: lambda.function.clone(),
            arity: lambda.arity,
            num_free: lambda.num_free,
            entry: label,
            blocks: resolve_label(label),
            args: Vec::new(),
        })
    }

    for function in &mut functions {
        function_propagate_ssa(function);
    }

    functions
}

fn function_dominator_frontiers(fun: &Function) -> HashMap<ExtLabel, Vec<ExtLabel>> {
    let get_child_num = |node, num| {
        let block: &BasicBlock = fun.blocks.get(&node).unwrap();

        if let Some(cont) = block.continuation {
            if num == 0 {
                Some(cont)
            } else {
                let jump: Option<&LabelId> = block.jumps.get(num-1);
                jump.map(|l| ExtLabel::Original(l.0))
            }
        } else {
            let jump: Option<&LabelId> = block.jumps.get(num);
            jump.map(|l| ExtLabel::Original(l.0))
        }
    };

    let post_dfs = ::graph::get_dfs(fun.entry, &get_child_num, ::graph::Kind::Post);
    let predecessors = ::graph::node_list_to_predecessors(&post_dfs, &get_child_num);
    let i_dominators = ::graph::immediate_dominators(fun.entry, &post_dfs, &predecessors);
    let frontiers = ::graph::dominance_frontiers(&i_dominators, &predecessors);

    frontiers
}

fn function_propagate_ssa(fun: &mut Function) {

    // Place PHI nodes
    let frontiers = function_dominator_frontiers(fun);
    let keys = fun.blocks.keys().cloned().collect_vec();
    let mut changed = true;
    while changed {
        changed = false;

        for key in keys.iter() {
            let frontiers = &frontiers[key];
            let phi_nodes = fun.blocks[key].phi_nodes.clone();

            let mut writes: Vec<Register> = Vec::new();
            for op in &fun.blocks[key].ops {
                for write in &op.op.writes {
                    writes.push(*write);
                }
            }

            for frontier_key in frontiers {
                for write in &writes {
                    let block = fun.blocks.get_mut(frontier_key).unwrap();
                    if !block.phi_nodes.contains_key(write) {
                        block.phi_nodes.insert(*write, PhiNode {
                            inputs: HashMap::new(),
                            output: SSARegister(0),
                            dead: false,
                        });
                        changed = true;
                    }
                }
                for node in &phi_nodes {
                    let block = fun.blocks.get_mut(frontier_key).unwrap();
                    if !block.phi_nodes.contains_key(node.0) {
                        block.phi_nodes.insert(*node.0, PhiNode {
                            inputs: HashMap::new(),
                            output: SSARegister(0),
                            dead: false,
                        });
                        changed = true;
                    }
                }
            }
        }
    }

    // Assign SSARegisters to writes
    let mut ssa_register_num: u32 = 0;
    for arg_num in 0..fun.arity {
        ssa_register_num += 1;
        fun.args.push((Register::X(arg_num), SSARegister(ssa_register_num)));
    }
    for mut block in fun.blocks.values_mut() {
        for (_, phi) in block.phi_nodes.iter_mut() {
            ssa_register_num += 1;
            phi.output = SSARegister(ssa_register_num);
        }
        for mut op in block.ops.iter_mut() {
            for ssa_reg in op.writes.iter_mut() {
                ssa_register_num += 1;
                *ssa_reg = SSARegister(ssa_register_num);
            }
        }
    }

    // Perform register read replacement
    fn propagate_reads(changed: &mut bool, blocks: &mut HashMap<ExtLabel, BasicBlock>,
                       visited: &mut HashSet<ExtLabel>, mut state: ReadPropState,
                       last: ExtLabel, entry: ExtLabel) {

        {
            let block = blocks.get_mut(&entry).unwrap();
            for (register, phi) in block.phi_nodes.iter_mut() {
                if !state.assignments.contains_key(&register) {
                    // input to phi not assigned, this phi node can't exist!
                    phi.dead = true;
                } else {
                    phi.inputs.insert(last, state.assignments[register]);
                    *state.assignments.get_mut(register).unwrap() = phi.output;
                }
            }
            for op in block.ops.iter_mut() {
                for (idx, source) in op.op.reads.iter().enumerate() {
                    if let &Source::Register(reg) = source {
                        op.reads[idx] = Some(state.assignments[&reg]);
                    } else {
                        op.reads[idx] = None;
                    }
                }
                for (idx, register) in op.op.writes.iter().enumerate() {
                    state.assignments.insert(*register, op.writes[idx]);
                }
            }
        }

        if visited.contains(&entry) {
            return;
        }
        visited.insert(entry);

        let mut children = Vec::new();
        if let Some(i) = blocks[&entry].continuation {
            children.push(i);
        }
        for jump in &blocks[&entry].jumps {
            children.push(ExtLabel::Original(jump.0));
        }

        for child in children {
            propagate_reads(changed, blocks, visited, state.clone(), entry, child);
        }

    }

    let mut changed = true;
    let mut read_prop_state = ReadPropState {
        assignments: HashMap::new(),
    };
    for arg in &fun.args {
        read_prop_state.assignments.insert(arg.0, arg.1);
    }
    while changed {
        changed = false;
        let mut visited = HashSet::new();
        propagate_reads(&mut changed, &mut fun.blocks, &mut visited,
                        read_prop_state.clone(), fun.entry, fun.entry);
    }

}

#[derive(Debug, Clone)]
struct ReadPropState {
    assignments: HashMap<Register, SSARegister>,
}

const DOT_BREAK: &str = "<br align=\"left\" />";

fn format_label(label: &str) -> String {
    label.replace("{", "\\{").replace("}", "\\}").replace("\n", DOT_BREAK)
}

use std::io::Write;
pub fn function_to_dot(function: &Function, w: &mut Write) -> ::std::io::Result<()> {
    write!(w, "digraph g {{\n")?;
    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
    write!(w, "edge [fontname=\"Courier New\", fontsize=9]\n\n")?;

    let fun_name = format_label(&format!("{:?}/{}", function.name, function.arity));
    write!(w, "entry [ label=<entry|fun: {} free: {}> ];\n", fun_name, function.num_free)?;
    write!(w, "entry -> blk_{};\n\n", function.entry.name())?;

    for (_, block) in &function.blocks {
        let block_name = block.label.name();

        write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;

        let phis = format_label(&format!("{:#?}", block.phi_nodes));
        write!(w, "phi: {}", phis)?;

        for op in &block.ops {
            let op_line = format_label(&format!("{:#?}", op));
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
