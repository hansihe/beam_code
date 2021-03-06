extern crate eetf;
extern crate itertools;
extern crate regex;
extern crate byteorder;
extern crate string_intern;

extern crate beam_file;
use beam_file::RawBeamFile;

use ::std::io::{ BufReader, Cursor };

mod gen_op;
pub use gen_op::{GenOp, GenOpTable};

mod op;
use op::Op;

mod raw_op;
use raw_op::{ RawOp, RawOpArg };

mod beam_module;
use beam_module::Module;

pub mod ssa;

mod graph;

mod atom;
pub use atom::Atom;

use ::std::io::Read;

const OP_TAB: &'static str = include_str!("../test_data/genop.tab");

pub fn read_beam<R>(r: &mut R) -> Module where R: Read {
    let op_tab_reader = BufReader::new(Cursor::new(OP_TAB));
    let op_table = GenOpTable::from_read(op_tab_reader);
    let file = RawBeamFile::from_reader(r).unwrap();
    let module = Module::from_beam_file(&file, &op_table);
    module
}

pub fn module_to_functions(module: &Module) -> Vec<ssa::SSAFunction> {
    let code: Vec<Op> = module.code.iter()
        .map(|o| op::Op::from_raw(o, &module))
        .collect();

    let functions = ssa::code_to_functions(&code, module);

    functions
}
