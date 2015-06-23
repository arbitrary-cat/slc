// Copyright (c) 2015, Sam Payson
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
// associated documentation files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge, publish, distribute,
// sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
// NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#![allow(non_snake_case)]

use std::collections::HashMap;
use std::io::{self, Write};
use std::mem;
use std::ops::Range;

use cats;

use syntax::{self, Node};
use types::{self, Type};

const HEX: cats::FormattedInt<'static> = cats::FormattedInt {
    prefix:  "",
    suffix:  "",
    digits:  &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'],
    min_len: 3,
    sign:    cats::SignPolicy::Empty,
};

#[derive(Clone,Copy)]
enum BufID {
    Preamble,
    StructFwd,
    StructDefn,
    Typedefs,
    FnProto,
    Code,
    ExprGen,

    Temp,
}

const GEN_PREFIX: &'static str = "X";

// C doesn't allow compound expressions, so we need to use temporary variables to store intermediate
// expressions. This is a stack-based writer which should avoid excessive allocations.
struct ExprGenerator {
    buf:   Vec<u8>,

    // Invariant: this vec always contains at least 1 element.
    stack: Vec<Range<usize>>,
}

impl ExprGenerator {
    fn new()
    -> ExprGenerator
    {
        ExprGenerator {
            buf:   Vec::new(),
            stack: vec![0..0],
        }
    }

    fn push(&mut self)
    {
        let top = self.stack.last().cloned().expect("ExprGenerator stack was empty!");

        self.stack.push(top.end..top.end);
     }

    fn pop_into(&mut self, v: &mut Vec<u8>)
    {
        let range = self.stack.pop().expect("ExprGenerator stack was empty!");
        for b in self.buf.drain(range) {
            v.push(b)
        }
        if self.stack.is_empty() {
            self.stack.push(0..0)
        }
    }
}

impl io::Write for ExprGenerator {
    fn write(&mut self, buf: &[u8])
    -> io::Result<usize>
    {
        let size = try!(self.buf.write(buf));
        self.stack.last_mut().expect("ExprGenerator stack was empty!").end += size;

        Ok(size)
    }

    fn flush(&mut self)
    -> io::Result<()>
    {
        self.buf.flush()
    }
}

pub struct CGenerator<'ctx>
{
    // A mapping from syntax::Node's to Types
    tys: types::TypeTable<'ctx>,

    // A mapping from types to a C typedef identifier.
    td_map: HashMap<&'ctx Type<'ctx>, String>,

    // A generator for unique integers, used for creating unique identifiers.
    next_uniq: usize,

    // Each of the following Vec<u8>s corresponds to a different section of the C output text. They
    // will be emitted to the final C file in the order that they appear in this struct.

    // The first section of the C file, for comments and includes.
    preamble: Vec<u8>,

    // The text for all C-language struct forward declarations
    struct_fwd: Vec<u8>,

    // The text for all C-language struct definitions.
    struct_defn: Vec<u8>,

    // The text for all C-language typedefs.
    typedefs: Vec<u8>,

    // The text for all C-language function prototypes.
    fn_proto: Vec<u8>,

    // The text for all C-language function implementations.
    code: Vec<u8>,

    // A specialized writer which helps us convert compound expressions to C.
    expr_gen: ExprGenerator,

    // A mapping from expressions to variable ids, so that we don't recompute the same subexpression
    // multiple times. The if the usize is N, then the variable is "${GEN_PREFIX}tmpN".
    exprs: syntax::NodeTable<'ctx, usize>,

    // This buffer can be used to hold text which is only needed temporarily (i.e. to be mutated and
    // then copied to another buffer).
    temp: Vec<u8>,

    // String of tabs, which gives the program's current indentation level.
    indent: String,
}

macro_rules! buf {
    ($gen:expr, $id:expr) => {
        match $id {
            BufID::Preamble   => &mut $gen.preamble     as &mut io::Write,
            BufID::StructFwd  => &mut $gen.struct_fwd   as &mut io::Write,
            BufID::StructDefn => &mut $gen.struct_defn  as &mut io::Write,
            BufID::Typedefs   => &mut $gen.typedefs     as &mut io::Write,
            BufID::FnProto    => &mut $gen.fn_proto     as &mut io::Write,
            BufID::Code       => &mut $gen.code         as &mut io::Write,
            BufID::ExprGen    => &mut $gen.expr_gen     as &mut io::Write,

            BufID::Temp       => &mut $gen.temp         as &mut io::Write,
        }
    }
}

struct CIdentMap {
    pre: Option<char>,
    buf: [char; 8],
    idx: usize,
}

const HEX_DIGITS: &'static [char] = &['0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

impl CIdentMap {
    fn new(c: char) -> CIdentMap {
        let x = c as usize;

        match c {
            'a' ... 'z' | 'A' ... 'Z' | '0' ... '9' | '_' => CIdentMap {
                pre: None,
                buf: ['.', '.', '.', '.', '.', '.', '.', c],
                idx: 7,
            },
            _                               => CIdentMap {
                pre: Some('U'),
                buf: [
                    HEX_DIGITS[(x >> 28) & 0xF],
                    HEX_DIGITS[(x >> 24) & 0xF],
                    HEX_DIGITS[(x >> 20) & 0xF],
                    HEX_DIGITS[(x >> 16) & 0xF],
                    HEX_DIGITS[(x >> 12) & 0xF],
                    HEX_DIGITS[(x >>  8) & 0xF],
                    HEX_DIGITS[(x >>  4) & 0xF],
                    HEX_DIGITS[(x >>  0) & 0xF],
                ],
                idx: match x {
                    x if x > (0xF << 24) => 0,
                    x if x > (0xF << 20) => 1,
                    x if x > (0xF << 16) => 2,
                    x if x > (0xF << 12) => 3,
                    x if x > (0xF << 8)  => 4,
                    x if x > (0xF << 4)  => 5,
                    x if x > (0xF << 0)  => 6,
                    _                    => 7
                },
            }
        }
    }
}

impl Iterator for CIdentMap {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let c = mem::replace(&mut self.pre, None).or_else(|| {
            self.idx += 1;
            self.buf.get(self.idx - 1).cloned()
        });
        c
    }
}

fn cid(s: &str) -> String {
    s.chars().flat_map(|c| CIdentMap::new(c)).collect()
}

impl<'ctx> CGenerator<'ctx> {
    fn inc_indent(&mut self) { self.indent.push('\t'); }
    fn dec_indent(&mut self) { self.indent.pop(); }

    pub fn new(tys: types::TypeTable<'ctx>)
        -> CGenerator<'ctx>
    {
        CGenerator {
            tys:         tys,
            td_map:      HashMap::new(),
            next_uniq:   0,
            preamble:    Vec::new(),
            struct_fwd:  Vec::new(),
            struct_defn: Vec::new(),
            typedefs:    Vec::new(),
            fn_proto:    Vec::new(),
            code:        Vec::new(),
            expr_gen:    ExprGenerator::new(),
            exprs:       syntax::NodeTable::new(),
            temp:        Vec::new(),
            indent:      String::new(),
        }
    }

    pub fn generate<W>(&mut self, w: &mut W, tu: &'ctx Node<'ctx>)
    -> io::Result<()>
    where W: io::Write,
    {
        #![allow(unused_must_use)]

        fcatln!(self.struct_fwd,  "\n// Struct Forward Declarations:\n").ok();
        fcatln!(self.struct_defn, "\n// Struct Definitions:\n").ok();
        fcatln!(self.typedefs,    "\n// Typedefs:\n").ok();
        fcatln!(self.fn_proto,    "\n// Function Prototypes:\n").ok();
        fcatln!(self.code,        "\n// Function Implementations (code):\n").ok();

        self.gen_translation_unit(tu);

        try!(w.write_all(&self.preamble));
        try!(w.write_all(&self.struct_fwd));
        try!(w.write_all(&self.struct_defn));
        try!(w.write_all(&self.typedefs));
        try!(w.write_all(&self.fn_proto));
        try!(w.write_all(&self.code));

        Ok(())
    }


    fn get_uniq(&mut self)
    -> usize
    {
        self.next_uniq += 1;
        self.next_uniq
    }

    fn gen_translation_unit(&mut self, tu: &'ctx Node<'ctx>)
    {
        if let &Node::TranslationUnit { ref fn_decls } = tu {
            for &decl in fn_decls.iter() {
                self.gen_fn_decl(decl)
            }
        } else { unreachable!() }
    }

    fn emit_C_type(&mut self, id: BufID, ty: &'ctx Type<'ctx>) {
        if let Some(s) = self.td_map.get(ty) {
            fcat!(buf!(self, id), s).ok();
            return
        }

        self.gen_typedef(ty);
        self.emit_C_type(id, ty);
    }

    fn gen_typedef(&mut self, ty: &'ctx Type<'ctx>) {
        match ty {
            &Type::Int  => { self.td_map.insert(ty, String::from("int")); },
            &Type::Bool => { self.td_map.insert(ty, String::from("int")); },
            &Type::Func { to, from } => {
                let uniq = self.get_uniq();
                self.td_map.insert(ty, scat!(GEN_PREFIX, "fn", HEX;uniq, "_t"));

                fcatln!(self.typedefs, "// ", ty).ok();

                fcat!(self.typedefs, "typedef ").ok();

                self.emit_C_type(BufID::Typedefs, to);

                fcat!(self.typedefs, " (*", GEN_PREFIX, "fn", HEX;uniq, "_t)(").ok();

                match from {
                    &Type::Tuple { ref elems } => {
                        for (idx, &ty) in elems.iter().enumerate() {
                            fcat!(self.typedefs, if idx > 0 { ", " } else { "" }).ok();
                            self.emit_C_type(BufID::Typedefs, ty);
                        }
                    }
                    scalar => {
                        self.emit_C_type(BufID::Typedefs, scalar);
                    }
                }

                fcatln!(self.typedefs, ");\n").ok();
            },
            &Type::Tuple { ref elems } => {
                let uniq = self.get_uniq();
                self.td_map.insert(ty, scat!(GEN_PREFIX, "tpl", HEX;uniq, "_t"));

                fcatln!(self.typedefs, "// ", ty).ok();

                fcatln!(self.typedefs, "typedef struct ", GEN_PREFIX, "tpl", HEX;uniq, "_t {").ok();
                self.inc_indent();
                for (idx, &elem) in elems.iter().enumerate() {
                    fcat!(self.typedefs, self.indent).ok();
                    self.emit_C_type(BufID::Typedefs, elem);
                    fcatln!(self.typedefs, " _", idx, ";").ok();
                }
                self.dec_indent();
                fcatln!(self.typedefs, "} ", GEN_PREFIX, "tpl", HEX;uniq, "_t;\n").ok();
            },
        }
    }

    fn emit_C_fn_sig(&mut self, id: BufID, decl: &'ctx Node<'ctx>)
    {
        if let &Node::FnDecl { name, ty, ref args, ..  } = decl {
            if let &Type::Func { from, to } = ty {
                self.emit_C_type(id, to);
                fcat!(buf!(self, id), " ", cid(name.text()), "(").ok();

                match from {
                    &Type::Tuple { ref elems } => {
                        for (idx, (&arg, &ty)) in args.iter().zip(elems.iter()).enumerate() {
                            fcat!(buf!(self, id), if idx > 0 { ", " } else { "" }).ok();
                            self.emit_C_type(id, ty);
                            fcat!(buf!(self, id), " ", cid(arg.text())).ok();
                        }
                    }
                    scalar => {
                        self.emit_C_type(id, scalar);
                        fcat!(buf!(self, id), " ", cid(args[0].text())).ok();
                    }
                }

                fcat!(buf!(self, id), ")").ok();
            } else { unreachable!() }
        } else { unreachable!() }
    }

    fn gen_fn_decl(&mut self, decl: &'ctx Node<'ctx>)
    {
        if let &Node::FnDecl { body, ..  } = decl {
            self.emit_C_fn_sig(BufID::FnProto, decl);
            fcatln!(self.fn_proto, ";\n").ok();

            self.emit_C_fn_sig(BufID::Code, decl);
            fcatln!(self.code, " {").ok();
            self.inc_indent();
            self.emit_expr(body);
            fcat!(self.code, self.indent, "return ").ok();
            self.expr_gen.pop_into(&mut self.code);
            self.dec_indent();
            fcatln!(self.code, ";\n", self.indent, "}\n").ok();
        } else { unreachable!() }
    }

    fn emit_expr(&mut self, expr: &'ctx Node<'ctx>)
    {
        match expr {
            id @ &Node::Ident{ .. } => {
                fcat!(self.expr_gen, cid(id.text())).ok();
                return;
            },
            &Node::IntLit { val } => {
                fcat!(self.expr_gen, val).ok();
                return;
            },
            _ => (),
        }

        let id =match self.exprs.get(expr) {
            Some(id) => id,
            None     => {
                let uniq = self.get_uniq();
                let ty   = self.tys.get(expr).expect("Expr not annotated with type!");

                self.expr_gen.push();

                fcat!(self.expr_gen, self.indent).ok();

                self.emit_C_type(BufID::Temp, ty);
                self.expr_gen.write_all(&self.temp[..]).ok();
                self.temp.clear();

                fcat!(self.expr_gen, " ", GEN_PREFIX, "tmp", HEX;uniq).ok();

                match expr {
                    &Node::BinOp { lhs, op, rhs } => {
                        fcat!(self.expr_gen, " = (").ok();
                        self.emit_expr(lhs);
                        fcat!(self.expr_gen, " ", op.text(), " ").ok();
                        self.emit_expr(rhs);
                        fcatln!(self.expr_gen, ");").ok();
                    },
                    &Node::FnCall { fun, arg } => {
                        fcat!(self.expr_gen, " = ").ok();

                        self.emit_expr(fun);

                        fcat!(self.expr_gen, "(").ok();
                        if let &Node::Tuple { ref elems } = arg {
                            for (idx, &elem) in elems.iter().enumerate() {
                                fcat!(self.expr_gen, if idx > 0 { ", " } else { "" }).ok();
                                self.emit_expr(elem);
                            }
                        } else {
                            match self.tys.get(arg).expect("Function argument not annotated with type") {
                                &Type::Tuple { ref elems } => {
                                    for (idx, _) in elems.iter().enumerate() {
                                        fcat!(self.expr_gen, if idx > 0 { ", " } else { "" }).ok();
                                        self.emit_expr(arg);
                                        fcat!(self.expr_gen, "._", idx).ok();
                                    }
                                },
                                _ => self.emit_expr(arg),
                            }
                        }
                        fcatln!(self.expr_gen, ");\n").ok();
                    },
                    &Node::Tuple { ref elems } => {
                        fcatln!(self.expr_gen, ";").ok();
                        for (idx, &elem) in elems.iter().enumerate() {
                            fcat!(self.expr_gen, self.indent, GEN_PREFIX, "tmp", HEX;uniq, "._", idx, " = ").ok();
                            self.emit_expr(elem);
                            fcatln!(self.expr_gen, ";").ok();
                        }

                        self.expr_gen.pop_into(&mut self.code);
                    },
                    &Node::IfElse { cond, yes, no: Some(no), .. } => {
                        fcatln!(self.expr_gen, ";").ok();
                        fcat!(self.expr_gen, self.indent,  "if (").ok();
                        self.emit_expr(cond);
                        fcatln!(self.expr_gen, ") {").ok();
                        self.inc_indent();
                        self.expr_gen.pop_into(&mut self.code);
                        self.expr_gen.push();
                        fcat!(self.expr_gen, self.indent, GEN_PREFIX, "tmp", HEX;uniq, " = ").ok();
                        self.emit_expr(yes);
                        fcatln!(self.expr_gen, ";").ok();
                        self.dec_indent();
                        fcatln!(self.expr_gen, self.indent, "} else {").ok();
                        self.inc_indent();
                        self.expr_gen.pop_into(&mut self.code);
                        self.expr_gen.push();
                        fcat!(self.expr_gen, self.indent, GEN_PREFIX, "tmp", HEX;uniq, " = ").ok();
                        self.emit_expr(no);
                        fcatln!(self.expr_gen, ";").ok();
                        self.dec_indent();
                        fcatln!(self.expr_gen, self.indent, "}").ok();
                    },
                    _ => unreachable!(),
                };

                self.exprs.insert(expr, uniq);

                uniq
            },
        };

        self.expr_gen.pop_into(&mut self.code);

        fcat!(self.expr_gen, GEN_PREFIX, "tmp", HEX;id).ok();
    }
}
