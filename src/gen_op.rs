use ::std::fs::File;
use ::std::io::{ BufReader, BufRead };

use ::regex::Regex;

#[derive(Debug)]
pub struct GenOpTable {
    pub ops: Vec<Option<GenOp>>,
}

#[derive(Debug, Clone)]
pub struct GenOp {
    pub id: u32,
    pub name: String,
    pub arity: u32,
    pub doc: String,
}

//#[derive(Debug, Clone)]
//enum Op {
//    Noop,
//}

impl GenOpTable {

    pub fn from_str(string: &str) -> Self {
        let doc_regex = Regex::new(r"^##.+").unwrap();
        let op_regex = Regex::new(r"^(\d+): ([^\\]+)/(\d+)\s*$").unwrap();

        let mut ops = vec![None; 256];

        let mut docs = String::new();
        for line in string.lines() {
            if doc_regex.is_match(&line) {
                docs.push_str(&line);
                docs.push('\n');
            } else if let Some(capts) = op_regex.captures(&line) {
                let id: usize = capts.get(1).unwrap().as_str().parse().unwrap();
                let name: &str = capts.get(2).unwrap().as_str();
                let arity: u32 = capts.get(3).unwrap().as_str().parse().unwrap();

                ops[id] = Some(GenOp {
                    id: id as u32,
                    name: name.to_owned(),
                    arity: arity,
                    doc: docs,
                });
                docs = String::new();
            }
        }

        GenOpTable {
            ops: ops,
        }
    }

    pub fn from_file(path: &str) -> Self {
        let doc_regex = Regex::new(r"^##.+").unwrap();
        let op_regex = Regex::new(r"^(\d+): ([^\\]+)/(\d+)\s*$").unwrap();

        let file = File::open(path).unwrap();
        let buf = BufReader::new(file);

        let mut ops = vec![None; 256];

        let mut docs = String::new();
        for line_res in buf.lines() {
            let line = line_res.unwrap();

            if doc_regex.is_match(&line) {
                docs.push_str(&line);
                docs.push('\n');
            } else if let Some(capts) = op_regex.captures(&line) {
                let id: usize = capts.get(1).unwrap().as_str().parse().unwrap();
                let name: &str = capts.get(2).unwrap().as_str();
                let arity: u32 = capts.get(3).unwrap().as_str().parse().unwrap();

                ops[id] = Some(GenOp {
                    id: id as u32,
                    name: name.to_owned(),
                    arity: arity,
                    doc: docs,
                });
                docs = String::new();
            }
        }

        GenOpTable {
            ops: ops,
        }
    }
}
