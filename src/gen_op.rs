use ::std::fs::File;
use ::std::io::{ BufReader, BufRead };
use ::std::str::FromStr;

use ::string_intern::{ Validator, Symbol };

pub struct OpNameSymbol;
impl Validator for OpNameSymbol {
    type Err = ::std::string::ParseError;
    fn validate_symbol(_val: &str) -> Result<(), Self::Err> {
        Ok(())
    }
}
pub type OpName = Symbol<OpNameSymbol>;

use ::regex::Regex;

#[derive(Debug)]
pub struct GenOpTable {
    pub ops: Vec<Option<GenOp>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenOp {
    pub id: u32,
    pub name: OpName,
    pub arity: u32,
    pub doc: String,
}

impl GenOpTable {

    pub fn from_file(path: &str) -> Self {
        let file = File::open(path).unwrap();
        GenOpTable::from_read(BufReader::new(file))
    }

    pub fn from_read<R>(read: R) -> Self where R: BufRead {
        let doc_regex = Regex::new(r"^##.+").unwrap();
        let op_regex = Regex::new(r"^(\d+): ([^\\]+)/(\d+)\s*$").unwrap();

        let mut ops = vec![None; 256];

        let mut docs = String::new();
        for line_r in read.lines() {
            let line = line_r.unwrap();

            if doc_regex.is_match(&line) {
                docs.push_str(&line);
                docs.push('\n');
            } else if let Some(capts) = op_regex.captures(&line) {
                let id: usize = capts.get(1).unwrap().as_str().parse().unwrap();
                let name: &str = capts.get(2).unwrap().as_str();
                let arity: u32 = capts.get(3).unwrap().as_str().parse().unwrap();

                ops[id] = Some(GenOp {
                    id: id as u32,
                    name: OpName::from_str(name).unwrap(),
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
