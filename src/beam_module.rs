use std::str::from_utf8;
use std::rc::Rc;
use std::io::Cursor;
use std::io::Read;

use ::beam_file::RawBeamFile;
use ::raw_op::RawOp;
use ::gen_op::GenOpTable;

use ::byteorder::{ ReadBytesExt, BigEndian };

#[derive(Debug)]
pub struct Module {
    pub atoms: Vec<Atom>,
    pub exports: Vec<Export>,
    pub imports: Vec<Import>,
    pub lambdas: Vec<Lambda>,
    pub code_header: CodeHeader,
    pub code: Vec<RawOp>,
}

impl Module {

    pub fn from_beam_file(file: &RawBeamFile, gen_op_table: &GenOpTable) -> Self {

        for chunk in &file.chunks {
            println!("Chunk: {}", from_utf8(&chunk.id).unwrap());
        }

        let atoms_raw = file.chunks.iter().find(|c| &c.id == b"AtU8").unwrap();
        let atoms = read_atoms(&atoms_raw.data);

        let exports_raw = file.chunks.iter().find(|c| &c.id == b"ExpT").unwrap();
        let exports = read_exports(&exports_raw.data, &atoms);

        let imports_raw = file.chunks.iter().find(|c| &c.id == b"ImpT").unwrap();
        let imports = read_imports(&imports_raw.data, &atoms);

        let lambdas_raw_opt = file.chunks.iter().find(|c| &c.id == b"FunT");
        let lambdas = if let Some(lambdas_raw) = lambdas_raw_opt {
            read_lambdas(&lambdas_raw.data, &atoms)
        } else {
            vec![]
        };

        let lit_raw = file.chunks.iter().find(|c| &c.id == b"LitT").unwrap();
        println!("{:?}", lit_raw);

        // https://github.com/erlang/otp/blob/master/erts/emulator/beam/beam_load.c
        //let literals_raw = file.chunks.iter().find(|c| &c.id == b"LitT").unwrap();
        //let lines_raw = file.chunks.iter().find(|c| &c.id == b"Line").unwrap();

        let code_raw = file.chunks.iter().find(|c| &c.id == b"Code").unwrap();
        let (code_header, code) = read_code(&code_raw.data, gen_op_table);
        //let code_proc = code.iter()
        //    .map(|op| ::op::Op::from_raw(op, &atoms, &imports, &lambdas))
        //    .collect();

        Module {
            atoms,
            exports,
            imports,
            lambdas,
            code_header,
            code,
        }
    }

}

pub type Atom = Rc<AtomInner>;
pub struct AtomInner {
    id: u32,
    string: String,
}
impl ::std::fmt::Debug for AtomInner {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "Atom({}, {:?})", self.id, self.string)
    }
}

pub type Export = Rc<ExportInner>;
#[derive(Debug)]
pub struct ExportInner {
    id: u32,
    function: Atom,
    arity: u32,
    label: u32,
}

pub type Import = Rc<ImportInner>;
#[derive(Debug)]
pub struct ImportInner {
    id: u32,
    module: Atom,
    function: Atom,
    arity: u32,
}

pub type Lambda = Rc<LambdaInner>;
#[derive(Debug)]
pub struct LambdaInner {
    id: u32,
    function: Atom,
    arity: u32,
    label: u32,
    index: u32,
    num_free: u32,
}

#[derive(Debug)]
pub struct CodeHeader {
    pub instruction_version: u32,
    pub highest_opcode: u32,
    pub num_labels: u32,
    pub num_functions: u32,
}

fn read_code(data: &[u8], gen_op_table: &GenOpTable) -> (CodeHeader, Vec<RawOp>) {
    let mut reader = Cursor::new(data);

    let head_size = reader.read_u32::<BigEndian>().unwrap();
    let instruction_version = reader.read_u32::<BigEndian>().unwrap();
    let highest_opcode = reader.read_u32::<BigEndian>().unwrap();
    let num_labels = reader.read_u32::<BigEndian>().unwrap();
    let num_functions = reader.read_u32::<BigEndian>().unwrap();

    let header = CodeHeader {
        instruction_version,
        highest_opcode,
        num_labels,
        num_functions,
    };

    println!("header_size: {}", head_size);
    println!("header: {:?}", header);

    let mut ops: Vec<RawOp> = Vec::new();

    while reader.position() as usize != data.len() {
        ops.push(RawOp::read(&mut reader, gen_op_table));
    }

    (header, ops)
}

fn read_lambdas(data: &[u8], atoms: &[Atom]) -> Vec<Lambda> {
    let mut reader = Cursor::new(data);
    let num_lambdas = reader.read_u32::<BigEndian>().unwrap();

    let mut lambdas = Vec::with_capacity(num_lambdas as usize);
    for lambda_id in 0..num_lambdas {
        let function_atom = reader.read_u32::<BigEndian>().unwrap();
        let arity = reader.read_u32::<BigEndian>().unwrap();
        let label = reader.read_u32::<BigEndian>().unwrap();
        let index = reader.read_u32::<BigEndian>().unwrap();
        let num_free = reader.read_u32::<BigEndian>().unwrap();
        let _old_uniq = reader.read_u32::<BigEndian>().unwrap();

        lambdas.push(Rc::new(LambdaInner {
            id: lambda_id,
            function: atoms[function_atom as usize - 1].clone(),
            arity,
            label,
            index,
            num_free,
        }));
    }

    lambdas
}

fn read_imports(data: &[u8], atoms: &[Atom]) -> Vec<Import> {
    let mut reader = Cursor::new(data);
    let num_imports = reader.read_u32::<BigEndian>().unwrap();

    let mut imports = Vec::with_capacity(num_imports as usize);
    for import_id in 0..num_imports {
        let module_atom = reader.read_u32::<BigEndian>().unwrap();
        let function_atom = reader.read_u32::<BigEndian>().unwrap();
        let arity = reader.read_u32::<BigEndian>().unwrap();

        imports.push(Rc::new(ImportInner {
            id: import_id,
            module: atoms[module_atom as usize - 1].clone(),
            function: atoms[function_atom as usize - 1].clone(),
            arity: arity,
        }));
    }

    imports
}

fn read_exports(data: &[u8], atoms: &[Atom]) -> Vec<Export> {
    let mut reader = Cursor::new(data);

    let num_exports = reader.read_u32::<BigEndian>().unwrap();

    let mut exports = Vec::with_capacity(num_exports as usize);
    for export_id in 0..num_exports {
        let function_atom = reader.read_u32::<BigEndian>().unwrap();
        let arity = reader.read_u32::<BigEndian>().unwrap();
        let label = reader.read_u32::<BigEndian>().unwrap();

        exports.push(Rc::new(ExportInner {
            id: export_id,
            function: atoms[function_atom as usize - 1].clone(),
            arity: arity,
            label: label,
        }));
    }

    exports
}

fn read_atoms(data: &[u8]) -> Vec<Atom> {
    let mut reader = Cursor::new(data);

    let len = reader.read_u32::<BigEndian>().unwrap();
    let mut atoms = Vec::with_capacity(len as usize);

    for atom_num in 0..len {
        let atom_len = reader.read_u8().unwrap() as usize;

        let mut atom_raw = vec![0; atom_len];
        reader.read_exact(&mut atom_raw).unwrap();

        atoms.push(Rc::new(AtomInner {
            id: atom_num,
            string: String::from_utf8(atom_raw).unwrap(),
        }));
    }

    atoms
}
