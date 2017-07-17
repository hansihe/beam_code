use ::std::hash::Hash;
use ::std::fmt::Debug;
use ::std::collections::HashSet;

use super::{ GraphAccessor, GraphDfs };

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Kind {
    Pre,
    Post,
}

pub fn get_dfs<Idx, F>(start: Idx, get_child_num: &F, kind: Kind)
                       -> GraphDfs<Idx> where Idx: Hash + Copy + Eq,
                                              F: Fn(Idx, usize) -> Option<Idx> {

    let mut discovered = HashSet::new();
    let mut dfs = Vec::new();

    dfs_inner(start, get_child_num, &mut discovered, &mut dfs, kind);

    dfs
}

fn dfs_inner<Idx, F>(node: Idx, get_child_num: &F,
                  discovered: &mut HashSet<Idx>, dfs: &mut Vec<Idx>, kind: Kind)
    where Idx: Hash + Copy + Eq, F: Fn(Idx, usize) -> Option<Idx> {

    discovered.insert(node);
    if kind == Kind::Pre {
        dfs.push(node);
    }

    let mut num = 0;
    while let Some(next) = get_child_num(node, num) {
        if !discovered.contains(&next) {
            dfs_inner(next, get_child_num, discovered, dfs, kind);
        }
        num += 1;
    }

    if kind == Kind::Post {
        dfs.push(node);
    }

}
