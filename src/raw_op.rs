use ::GenOp;
use ::GenOpTable;
use ::byteorder::ReadBytesExt;

use ::std::io::Read;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawOp {
    pub opcode: GenOp,
    pub args: Vec<RawOpArg>,
}
impl RawOp {

    pub fn read<R>(mut reader: &mut R, gen_op_table: &GenOpTable) -> Self where R: Read {
        let opcode = reader.read_u8().unwrap();
        let op_entry = gen_op_table.ops[opcode as usize].as_ref().unwrap();
        let arity = op_entry.arity;

        let mut args: Vec<RawOpArg> = Vec::with_capacity(arity as usize);

        let mut ext_arity = arity;
        let mut arg_num = 0;
        while arg_num < ext_arity {
            let (arg_typ, arg_val) = get_tag_val(&mut reader);

            match arg_typ {
                0 => args.push(RawOpArg::Untagged(arg_val)),
                1 => args.push(RawOpArg::Integer(arg_val)),
                2 => args.push(RawOpArg::Atom(arg_val)),
                3 => args.push(RawOpArg::XReg(arg_val)),
                4 => args.push(RawOpArg::YReg(arg_val)),
                5 => args.push(RawOpArg::FailLabel(arg_val)),
                7 => {
                    match arg_val {
                        0 => panic!(), // Float, not generated anymore, maybe support later
                        1 => { // list
                            assert!(arg_num+1 == arity); // List must be last
                            let (l_arg_typ, l_arg_val) = get_tag_val(&mut reader);
                            assert!(l_arg_typ == 0); // == TAG_u
                            ext_arity += l_arg_val;
                        },
                        2 => { // float_register
                            let (l_arg_typ, l_arg_val) = get_tag_val(&mut reader);
                            assert!(l_arg_typ == 0); // == TAG_u
                            args.push(RawOpArg::FloatReg(l_arg_val));
                        },
                        4 => { // literal
                            let (l_arg_typ, l_arg_val) = get_tag_val(&mut reader);
                            assert!(l_arg_typ == 0); // == TAG_u
                            args.push(RawOpArg::Literal(l_arg_val));
                        },
                        _ => panic!("{}", arg_val),
                    }
                },
                _ => panic!("{}", arg_typ),
            }

            arg_num += 1;
        }

        RawOp {
            opcode: op_entry.clone(),
            args,
        }
    }

}

fn get_tag_val<R>(reader: &mut R) -> (u32, u32) where R: Read {
    let arg_base = reader.read_u8().unwrap() as u32;

    let arg_type = arg_base & 0b00000111;
    let arg_expanded = arg_base & 0b00001000;
    let arg_adv_expanded = arg_base & 0b00010000;

    let mut val;

    if arg_expanded == 0 {
        val = arg_base >> 4;
    } else if arg_adv_expanded == 0 {
        let expand_val = reader.read_u8().unwrap();
        val = (arg_base >> 5) << 8;
        val |= expand_val as u32;
    } else {
        panic!("adv expand not implemented");
    }

    (arg_type, val)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RawOpArg {
    Untagged(u32),
    Integer(u32),
    Atom(u32),
    XReg(u32),
    YReg(u32),
    FailLabel(u32),
    FloatReg(u32),
    Literal(u32),
}
impl RawOpArg {
    pub fn integer(&self) -> u32 {
        match *self {
            RawOpArg::Untagged(i) => i,
            RawOpArg::Integer(i) => i,
            _ => panic!(),
        }
    }
    pub fn untagged(&self) -> u32 {
        match *self {
            RawOpArg::Untagged(i) => i,
            _ => panic!(),
        }
    }
    pub fn atom(&self) -> u32 {
        match *self {
            RawOpArg::Atom(i) => i,
            _ => panic!(),
        }
    }
    pub fn fail_label(&self) -> u32 {
        match *self {
            RawOpArg::FailLabel(i) => i,
            _ => panic!(),
        }
    }
}
