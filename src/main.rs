extern crate eetf;
extern crate itertools;
extern crate regex;
extern crate byteorder;

extern crate beam_file;
use beam_file::RawBeamFile;


mod gen_op;
use gen_op::{GenOp, GenOpTable};

mod op;
use op::Op;

mod raw_op;
use raw_op::{ RawOp, RawOpArg };

mod beam_module;
use beam_module::Module;

fn main() {
    let gen_op_table = GenOpTable::from_file("test_data/genop.tab");

    let file = RawBeamFile::from_file("test_data/Elixir.Test.beam").unwrap();
    let module = Module::from_beam_file(&file, &gen_op_table);
    let code: Vec<Op> = module.code.iter()
        .map(|o| op::Op::from_raw(o, &module))
        .collect();

    println!("{:#?}", code);
}