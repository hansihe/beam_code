extern crate eetf;
extern crate itertools;
extern crate regex;
extern crate byteorder;
extern crate string_intern;

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

mod atom;
pub use atom::Atom;

fn main() {
    let gen_op_table = GenOpTable::from_file("test_data/genop.tab");

    let file = RawBeamFile::from_file("test_data/gen_server.beam").unwrap();
    let module = Module::from_beam_file(&file, &gen_op_table);
    //println!("{:#?}", module);

    let code: Vec<Op> = module.code.iter()
        .map(|o| op::Op::from_raw(o, &module))
        .collect();
    //println!("{:#?}", code);

    let functions = ssa::code_to_functions(&code, &module);
    //println!("{:?}", functions);

    //println!("{:#?}", functions[4]);

    let fun_atom = Atom::from("loop");
    let fun = functions.iter().find(|f| {
        f.name == fun_atom && f.arity == 7
    }).unwrap();

    let mut dot_out = std::fs::File::create("cfg.dot").unwrap();
    ssa::function_to_dot(fun, &mut dot_out).unwrap(); // 34 // 4


}
