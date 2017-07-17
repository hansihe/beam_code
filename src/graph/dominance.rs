use ::itertools::Itertools;
use ::std::collections::{ HashMap, HashSet };
use ::std::hash::Hash;

use super::{ GraphDfs, GraphPredecessors };

pub fn immediate_dominators<NodeId>(start: NodeId,
                                    index_to_node_id: &GraphDfs<NodeId>,
                                    predecessors: &GraphPredecessors<NodeId>)
                                    -> HashMap<NodeId, NodeId> where
    NodeId: Eq + Hash + Copy {

    let node_id_to_index: HashMap<NodeId, usize> = index_to_node_id.iter()
        .cloned().enumerate().map(|(a, b)| (b, a)).collect();

    let mut immediate_dominators: HashMap<NodeId, NodeId> = HashMap::new();
    immediate_dominators.insert(start, start);

    let mut changed = true;
    while changed {
        changed = false;

        for u in index_to_node_id.iter().rev().skip(1) {
            let new_idom = predecessors[&u].iter()
                .filter(|n| immediate_dominators.contains_key(n))
                .cloned()
                .fold1(|mut u, mut v| {
                    while u != v {
                        while node_id_to_index[&u] < node_id_to_index[&v] {
                            u = immediate_dominators[&u];
                        }
                        while node_id_to_index[&u] > node_id_to_index[&v] {
                            v = immediate_dominators[&v];
                        }
                    }
                    u
                })
                .unwrap();
            let dom = immediate_dominators.get(u).cloned();

            if dom != Some(new_idom) {
                immediate_dominators.insert(*u, new_idom);
                changed = true;
            }
        }
    }

    immediate_dominators
}

pub fn dominance_frontiers<NodeId>(immediate_dominators: &HashMap<NodeId, NodeId>,
                                   predecessors: &GraphPredecessors<NodeId>)
                                   -> HashMap<NodeId, Vec<NodeId>>
    where NodeId: Eq + PartialEq + Hash + Copy {

    let mut df: HashMap<NodeId, Vec<NodeId>> = immediate_dominators.keys()
        .map(|key| (*key, Vec::new())).collect();

    for u in immediate_dominators.keys() {
        let loops_self = predecessors[u].contains(u);
        if predecessors[u].len() - if loops_self { 1 } else { 0 } >= 2 {
            let mut p = HashSet::new();
            for v_r in &predecessors[u] {
                let mut v = *v_r;
                while v != immediate_dominators[u] && !p.contains(&v) {
                    p.insert(v);
                    v = immediate_dominators[&v];
                }
            }
            p.remove(u);
            for v in p.iter() {
                df.get_mut(v).unwrap().push(*u);
            }
        }
    }

    df
}
