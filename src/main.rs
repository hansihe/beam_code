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

mod ssa;

mod graph;

fn main() {
    let gen_op_table = GenOpTable::from_file("test_data/genop.tab");

    let file = RawBeamFile::from_file("test_data/Elixir.Test.beam").unwrap();
    let module = Module::from_beam_file(&file, &gen_op_table);
    //println!("{:#?}", module);

    let code: Vec<Op> = module.code.iter()
        .map(|o| op::Op::from_raw(o, &module))
        .collect();
    //println!("{:#?}", code);

    let functions = ssa::code_to_functions(&code, &module);
    println!("{:?}", functions);

    let mut dot_out = std::fs::File::create("cfg.dot").unwrap();
    ssa::function_to_dot(&functions[4], &mut dot_out).unwrap(); // 34


}
