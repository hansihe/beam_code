use ::std::collections::HashSet;
use ::std::collections::HashMap;

use ::Atom;
use ::op::{ Op, LabelId, Register, OpKind, Source, Literal };
use ::beam_module::{ Module };

use ::itertools::Itertools;

mod representation;
pub use self::representation::{
    SSAOp,
    SSAFunction,
    SSABasicBlock,
    PhiNode,
    SSARegister,
    SSASource,
    ExtLabel,
    FunDefType,
};

#[derive(Debug)]
pub struct IntFunction {
    name: Atom,
    arity: u32,
    entry: ExtLabel,
    blocks: HashMap<ExtLabel, IntBasicBlock>,
    args: Vec<(Register, SSARegister)>,
    fun_type: FunDefType,
}

#[derive(Debug, Clone)]
pub struct IntBasicBlock {
    label: ExtLabel,
    continuation: Option<ExtLabel>,
    jumps: Vec<LabelId>,
    ops: Vec<ExtOp>,
    phi_nodes: HashMap<Register, PhiNode>,
}


#[derive(Debug, Clone)]
pub struct ExtOp {
    op: Op,
    writes: Vec<SSARegister>,
    reads: Vec<SSASource>,
}


#[derive(Debug)]
struct FunDef {
    module: Atom,
    name: Atom,
    arity: u32,
    label: LabelId,
    fun_type: FunDefType,
}

pub fn code_to_functions(code: &[Op], module: &Module) -> Vec<SSAFunction> {

    let lambdas: HashMap<(Atom, u32), (LabelId, u32)> = module.lambdas.iter()
        .map(|l| ((l.function.clone(), l.arity), (LabelId(l.label), l.num_free))).collect();
    let exports: HashMap<(Atom, u32), LabelId> = module.exports.iter()
        .map(|l| ((l.function.clone(), l.arity), LabelId(l.label))).collect();

    // Collect function definitions
    let fun_defs = code.iter().scan(LabelId(0), |state, op| {
        if let OpKind::Label { num } = op.kind {
            *state = num;
        }
        Some((*state, op))
    }).filter_map(|(label, op)| {
        if let OpKind::FuncInfo { ref module, ref function, arity, .. } = op.kind {
            if label == LabelId(0) {
                panic!("FuncInfo without label");
            }

            let key = (function.clone(), arity);
            let is_lambda = lambdas.get(&key);
            let is_export = exports.get(&key);

            let typ = match (is_lambda, is_export) {
                (None, None) => FunDefType::Private,
                (Some(&(id, num_free)), None) => {
                    assert!(label.0 == id.0-1);
                    FunDefType::Lambda(num_free)
                }
                (None, Some(id)) => {
                    assert!(label.0 == id.0-1);
                    FunDefType::Public
                }
                _ => panic!(),
            };

            Some(FunDef {
                module: module.clone(),
                name: function.clone(),
                arity: arity,
                label: label,
                fun_type: typ,
            })
        } else {
            None
        }
    }).collect_vec();

    // Split bytecode into opcodes
    let mut basic_blocks: Vec<IntBasicBlock> = code.iter()
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
                 let dests = op.writes.iter().map(|_| {
                     SSARegister(0)
                 }).collect();

                 let srcs = op.reads.iter().map(|i| {
                     match *i {
                         Source::Register(_) => SSASource::Register(SSARegister(0)),
                         Source::Literal(ref lit) => SSASource::Literal(lit.clone()),
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
        .map(|(added_label, ops)| IntBasicBlock {
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

        // If the last instruction in a block is a jump,
        // assign that as the continuation..
        if let OpKind::Jump = elems[0].ops.last().unwrap().op.kind {
            elems[0].continuation = Some(ExtLabel::Original(
                elems[0].ops.last().unwrap().op.labels[0].0));
            elems[0].ops.pop();
        }
        // ..if not, assign the following block if the last op
        // can fall through
        else if elems[0].ops.last().unwrap().op.can_continue() {
            elems[0].continuation = Some(elems[1].label);
        }
    }

    // Assign jumps
    for mut block in &mut basic_blocks {
        if let Some(last_op) = block.ops.last() {
            block.jumps = last_op.op.labels.clone();
        }
    }

    // Reorder moves arund try instructions
    let mut reorders = Vec::new();
    // Collect blocks that should be reordered around
    for (idx, block) in basic_blocks.iter().enumerate() {
        if let Some(last_op) = block.ops.last() {
            // FIXME: move exception_ctx into writes?
            if let OpKind::Try = last_op.op.kind {
                if let ExtLabel::Added(_) = block.continuation.unwrap() {
                    reorders.push((idx, last_op.op.writes[0]));
                }
            }
        }
    }
    // Perform reordering
    for &(first_idx, clobbered_reg) in &reorders {
        // Collect instructions from second block to move to first
        let num = basic_blocks[first_idx+1].ops.iter()
            .take_while(|b| b.op.kind == OpKind::Move && !b.op.touches_reg(clobbered_reg))
            .count();
        let mut removed = basic_blocks.get_mut(first_idx+1).unwrap().ops.drain(0..num).collect_vec();

        // Insert into previous block
        let bb = basic_blocks.get_mut(first_idx).unwrap();
        // TODO: Make insertion more efficient
        for r in removed.drain(..) {
            let elem_num = bb.ops.len() - 1;
            bb.ops.insert(elem_num, r);
        }
    }

    let resolve_label = |label_id| {
        let mut blocks: HashMap<ExtLabel, IntBasicBlock> = HashMap::new();

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

    for fun in &fun_defs {
        let label = ExtLabel::Original(fun.label.0);
        functions.push(IntFunction {
            name: fun.name.clone(),
            arity: fun.arity,
            entry: label,
            blocks: resolve_label(label),
            args: Vec::new(),
            fun_type: fun.fun_type,
        });
    }
    for function in &mut functions {
        function_propagate_ssa(function);
    }

    // Clean up
    let cleaned = functions.iter().map(|f| {
        let mut ssa_fun = SSAFunction {
            name: f.name.clone(),
            arity: f.arity,
            fun_type: f.fun_type,
            entry: f.entry,
            args: f.args.iter().map(|&(_, arg)| arg).collect(),
            blocks: f.blocks.iter().map(|(label, block)| {
                let res = SSABasicBlock {
                    label: *label,
                    continuation: block.continuation,
                    jumps: block.jumps.iter()
                        .map(|l| ExtLabel::Original(l.0)).collect(),
                    phi_nodes: block.phi_nodes.values().cloned().collect(),
                    ops: block.ops.iter().enumerate().map(|(op_num, op)| {
                        if op_num != block.ops.len()-1 {
                            //println!("{:?}", op);
                            assert!(op.op.labels.len() == 0);
                        }
                        SSAOp {
                            kind: op.op.kind.clone(),
                            reads: op.reads.clone(),
                            writes: op.writes.clone(),
                            w: op.op.writes.clone(),
                            r: op.op.reads.clone(),
                        }
                    }).collect(),
                };
                (*label, res)
            }).collect(),
        };

        ssa_fun.clean();

        ssa_fun
    }).collect();

    cleaned
}

fn function_dominator_frontiers(fun: &IntFunction) ->
    HashMap<ExtLabel, Vec<ExtLabel>> {

    let get_child_num = |node, num| {
        let block: &IntBasicBlock = fun.blocks.get(&node).unwrap();

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

fn function_propagate_ssa(fun: &mut IntFunction) {
    //println!("{:?}/{} {:?}", fun.name, fun.arity, fun.fun_type);

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
    fn propagate_reads(level: usize, changed: &mut bool,
                       blocks: &mut HashMap<ExtLabel, IntBasicBlock>,
                       visited: &mut HashSet<ExtLabel>, mut state: ReadPropState,
                       last: ExtLabel, entry: ExtLabel) {

        //println!("{}: {:?} {:?}", level, entry, state);

        {
            let block = blocks.get_mut(&entry).unwrap();
            for (register, phi) in block.phi_nodes.iter_mut() {
                if !state.assignments.contains_key(&register) {
                    // input to phi not assigned, this phi node can't exist!
                    phi.dead = true;
                } else {
                    phi.inputs.insert(last, SSASource::Register(state.assignments[register]));
                    *state.assignments.get_mut(register).unwrap() = phi.output;
                }
            }
            for op in block.ops.iter_mut() {
                for (idx, source) in op.op.reads.iter().enumerate() {
                    if let &Source::Register(reg) = source {
                        //println!("{:?}, {:?}, {:?}, {:?}, {:?}", reg, op.op.kind, op.op.reads, op.op.writes, state.assignments);
                        //println!("read: {:?}", reg);
                        op.reads[idx] = SSASource::Register(state.assignments[&reg]);
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
            propagate_reads(level+1, changed, blocks, visited, state.clone(), entry, child);
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
        propagate_reads(0, &mut changed, &mut fun.blocks, &mut visited,
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
pub fn function_to_dot(function: &SSAFunction, w: &mut Write) -> ::std::io::Result<()> {
    write!(w, "digraph g {{\n")?;
    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
    write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;

    let fun_name = format_label(&format!("{:?}/{}", function.name, function.arity));
    write!(w, "entry [ label=<entry|fun: {} free: {:?} write[{:?}]> ];\n",
           fun_name, function.fun_type, function.args)?;
    write!(w, "entry -> blk_{};\n\n", function.entry.name())?;

    for (_, block) in &function.blocks {
        let block_name = block.label.name();

        write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;

        for phi in &block.phi_nodes {
            if phi.dead {
                continue;
            }
            let fmt = format_label(&format!("${}, = PHI[{:?}]\n",
                                            phi.output.0, phi.inputs));
            write!(w, "{}", fmt)?;
        }

        for op in block.ops.iter() {
            if op.writes.len() > 0 {
                for write in &op.writes {
                    write!(w, "${}, ", write.0)?;
                }
                write!(w, "= ")?;
            }

            let body = format_label(&format!("{:?} ", op.kind));
            write!(w, "{}", body)?;

            if op.reads.len() > 0 {
                write!(w, "read[")?;
                for read in op.reads.iter() {
                    match *read {
                        SSASource::Register(reg) => write!(w, "${}, ", reg.0)?,
                        SSASource::Literal(ref lit) => write!(w, "{}, ", format_label(
                            &format!("{:?}", lit)))?,
                    }
                }
                write!(w, "] ")?;
            }

            //write!(w, "r{:?}", op.r)?;
            //write!(w, " w{:?}", op.w)?;

            write!(w, "{}", DOT_BREAK)?;
        }

        //write!(w, "jumps[")?;
        //for label in block.jumps.iter() {
        //    write!(w, "{}, ", label.name())?;
        //}
        //write!(w, "] ")?;

        write!(w, "> ];\n")?;

        if let Some(label) = block.continuation {
            write!(w, "blk_{} -> blk_{} [ label=cont ];\n", block_name, label.name())?;
        }
        for (idx, arg) in block.jumps.iter().enumerate() {
            write!(w, "blk_{} -> blk_{} [ label={} ];\n", block_name, arg.name(), idx)?;
        }
        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
