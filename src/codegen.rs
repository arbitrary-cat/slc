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

use std::io;

use compiler::CtxRef;
use error;
use syntax::{self, Node, GenNode};
use types::Type;
use util;

const PREFIX: &'static str = "SL";

type TempMap<'ctx> = util::TagMap<'ctx, &'ctx str>;

type TypedefMap<'ctx> = util::TagMap<'ctx, &'ctx str>;

/// A buffer which can be used to build a C file.
pub struct CBuffer<'ctx> {
    next_uniq: usize,

    // A string to use as scratch space for constructing identifiers (which can then be stored using
    // the StrArena).
    scratch: String,

    // A mapping from expression nodes to temporary variable names (used to store sub-expressions).
    tmp_map: TempMap<'ctx>,

    // A mapping from SL types to their C typedef names.
    typedef_map: TypedefMap<'ctx>,

    // Text for all C typedef code.
    typedefs_txt: Vec<u8>,

    // Text for C function prototypes.
    fn_protos_txt: Vec<u8>,

    // Text for C function implementations (the actual code!).
    fn_impls_txt: Vec<u8>,
}

impl<'ctx> Type<'ctx> {
    // Emit a C-syntax typedef to the typedefs_txt section.
    fn emit_typedef(&'ctx self, td: &str, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        fcatln!(c.typedefs_txt,"// ", self).ok();
        match self {
            &Type::Func { from, to } => {
                let ret_t = try!(c.typedef(ctx, to));
                fcat!(c.typedefs_txt, "typedef ", ret_t, " (*", td, ")(").ok();
                match from {
                    &Type::Tuple { ref elems } => {
                        let mut args_t = Vec::with_capacity(elems.len());
                        for &elem in elems.iter() {
                            args_t.push(try!(c.typedef(ctx, elem)))
                        }

                        for (idx, &arg_t) in args_t.iter().enumerate() {
                            fcat!(c.typedefs_txt, if idx > 0 { ", " } else { "" }, arg_t).ok();
                        }
                    }
                    ty => {
                        let c_name = try!(c.typedef(ctx, ty));
                        fcat!(c.typedefs_txt, c_name).ok();
                    },
                }
                fcatln!(c.typedefs_txt, ");\n").ok();
            }
            &Type::Tuple { ref elems } => {
                fcatln!(c.typedefs_txt, "typedef struct ", td, " {\n").ok();
                for (idx, &elem) in elems.iter().enumerate() {
                    let elem_t = try!(c.typedef(ctx, elem));
                    fcatln!(c.typedefs_txt, "\t// ", elem_t).ok();
                    fcatln!(c.typedefs_txt, "\t", elem_t, " _", idx, ";\n").ok();
                }
                fcatln!(c.typedefs_txt, "} ", td, ";\n").ok();
            }
            &Type::Int | &Type::Bool => {
                fcatln!(c.typedefs_txt, "typedef int ", td, ";\n").ok();
            }
        }

        Ok(())
    }
}

impl<'ctx> CBuffer<'ctx> {
    pub fn new()
    -> CBuffer<'ctx>
    {
        CBuffer {
            next_uniq:     0,
            scratch:       String::new(),
            tmp_map:       TempMap::new(),
            typedef_map:   TypedefMap::new(),
            typedefs_txt:  Vec::new(),
            fn_protos_txt: Vec::new(),
            fn_impls_txt:  Vec::new(),
        }
    }

    pub fn write<W: io::Write>(&self, w: &mut W)
    -> io::Result<()> {
        try!(fcatln!(w, "// Generated by the SL compiler\n"));
        try!(fcatln!(w, "\n// C Typedefs:\n"));
        try!(w.write_all(&self.typedefs_txt[..]));
        try!(fcatln!(w, "\n// C Function Prototypes:\n"));
        try!(w.write_all(&self.fn_protos_txt[..]));

        try!(fcatln!(w, "\n// C Function Implementations:\n"));
        w.write_all(&self.fn_impls_txt[..])

    }

    pub fn get_uniq(&mut self)
    -> usize
    {
        let uniq = self.next_uniq;
        self.next_uniq = self.next_uniq + 1;

        uniq
    }

    pub fn typedef(&mut self, ctx: CtxRef<'ctx>, ty: &'ctx Type<'ctx>)
    -> error::Result<'ctx, &'ctx str>
    {
        if let Some(typedef_name) = self.typedef_map.get(ty) {
            return Ok(typedef_name);
        } 

        self.scratch.clear();
        let uniq = self.get_uniq();

        match ty {
            &Type::Int  => strcat!(self.scratch, PREFIX, "int_t"),
            &Type::Bool => strcat!(self.scratch, PREFIX, "bool_t"),
            &Type::Func { .. }  => strcat!(self.scratch, PREFIX, "fn", uniq, "_t"),
            &Type::Tuple { .. } => strcat!(self.scratch, PREFIX, "tuple", uniq, "_t"),
        }

        let name = ctx.strings.alloc(&self.scratch[..]);

        try!(ty.emit_typedef(name, ctx, self));

        self.typedef_map.insert(ty, name);

        Ok(name)
    }
}

/// A trait for nodes which can be emitted as C code.
pub trait EmitC<'ctx> {
    /// Emit the code for this node.
    fn emit(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    ;

    /// Emit any C-code which must precede this. The default implementation does nothing.
    fn depends(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        #![allow(unused_variables)]
        Ok(())
    }
}


impl<'ctx> EmitC<'ctx> for syntax::Node<'ctx> {
    fn depends(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        if let Some(..) = c.tmp_map.get(self) { return Ok(()) }

        match self {
            &TranslationUnit(ref translation_unit) => translation_unit.depends(ctx, c),
            &FnDecl(ref fn_decl)                   => fn_decl.depends(ctx, c),
            _ => return Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("Node type `", self, "' cannot be emitted as C."),
            }),
        }
    }

    fn emit(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        // For expressions with a temporary already generated, we simply emit them by emitting their
        // assigned identifier. This will always happen in the `fn_impls_txt` section.
        if let Some(tmp) = c.tmp_map.get(self) {
            fcat!(c.fn_impls_txt, tmp).ok();

            return Ok(());
        }

        match self {
            &TranslationUnit(ref translation_unit) => translation_unit.emit(ctx, c),
            &FnDecl(ref fn_decl)                   => fn_decl.emit(ctx, c),
            _ => return Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("Node type `", self, "' cannot be emitted as C."),
            }),
        }
    }
}

impl<'ctx> EmitC<'ctx> for syntax::TranslationUnit<'ctx> {
    fn emit(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        for &fn_decl in self.fn_decls.iter() {
            try!(fn_decl.depends(ctx, c));
            try!(fn_decl.emit(ctx, c));
        }

        Ok(())
    }
}

// Where should a function signature be emitted?
//
// This type is just for use with FnDecl::print_sig.
#[derive(Clone,Copy)] enum SigLoc {
    Proto,
    Impl,
}

impl SigLoc {
    fn get_buffer<'x>(self, c: &'x mut CBuffer)
    -> &'x mut Vec<u8>
    {
        match self {
            SigLoc::Proto => &mut c.fn_protos_txt,
            SigLoc::Impl  => &mut c.fn_impls_txt,
        }
    }
}

impl<'ctx> syntax::FnDecl<'ctx> {
    fn print_sig(&'ctx self, sig_loc: SigLoc, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        let mut comma = None;

        let ret_t: &'ctx str = try!(c.typedef(ctx, self.to));

        fcat!(sig_loc.get_buffer(c), ret_t, " ", self.name.text(), "(").ok();

        for &n in self.args.iter() {
            let var_decl: &'ctx syntax::VarDecl<'ctx> = n.as_ref();
            for &name in var_decl.names.iter() {
                let c_id = name.text();
                let c_ty = try!(c.typedef(ctx, var_decl.ty));
                fcat!(sig_loc.get_buffer(c), comma, c_ty, " ", c_id).ok();
                comma = Some(", ");
            }
        }

        fcat!(sig_loc.get_buffer(c), ")").ok();

        Ok(())
    }
}

impl<'ctx> EmitC<'ctx> for syntax::FnDecl<'ctx> {
    fn emit(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        try!(self.print_sig(SigLoc::Proto, ctx, c));
        fcatln!(c.fn_protos_txt, ";\n").ok();

        try!(self.print_sig(SigLoc::Impl, ctx, c));
        fcatln!(c.fn_impls_txt, " {").ok();
        try!(self.body.depends(ctx, c));

        fcatln!(c.fn_impls_txt, "\treturn ").ok();
        try!(self.body.emit(ctx, c));

        fcatln!(c.fn_impls_txt, ";\n}\n").ok();

        Ok(())
    }
}
