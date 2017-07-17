use ::std::collections::HashMap;
use ::std::hash::Hash;

mod dfs;
pub use self::dfs::get_dfs;
pub use self::dfs::Kind;

mod dominance;
pub use self::dominance::immediate_dominators;
pub use self::dominance::dominance_frontiers;

/// Easy to implement function that allows full access to a graph.
/// In most cases this is not optimal, but it makes things easy.
pub type GraphAccessor<NodeId> = Fn(NodeId, usize) -> Option<NodeId>;

pub type GraphPredecessors<NodeId> = HashMap<NodeId, Vec<NodeId>>;
pub type GraphDfs<NodeId> = Vec<NodeId>;

pub fn node_list_to_predecessors<NodeId, F>(nodes: &[NodeId], access: &F) -> GraphPredecessors<NodeId> where NodeId: Hash + Eq + Copy, F: Fn(NodeId, usize) -> Option<NodeId> {
    let mut predecessors: HashMap<NodeId, Vec<NodeId>> = HashMap::new();

    for node in nodes {
        predecessors.insert(*node, Vec::new());
    }

    for node in nodes {
        let mut num = 0;
        while let Some(child) = access(*node, num) {
            predecessors.get_mut(&child).unwrap().push(*node);
            num += 1;
        }
    }

    predecessors
}
