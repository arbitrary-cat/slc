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

use codegen::CBuffer;
use compiler::CtxRef;
use error;
use expr;
use semantic::Scope;
use syntax::{self, GenNode};
use types::{Type, Ty};


pub trait Pattern<'ctx> {
    /// Declare in `scope` all identifiers introduced by a pattern to which an expression of type
    /// `ty` will be assigned.
    fn decl(&'ctx self, ty: Ty<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    ;

    /// Emit the C code to assign variables from this pattern. The text of an expression (which it
    /// is safe to evaluate multiple times) for the value being assigned is stored in `c.field`.
    ///
    /// See the implementation for `syntax::TuplePattern` for a good example.
    fn assign(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    ;
}

impl<'ctx> Pattern<'ctx> for syntax::Node<'ctx> {
    fn decl(&'ctx self, ty: Ty<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        match self {
            &Ident(ref ident)                => ident.decl(ty, ctx, scope),
            &TuplePattern(ref tuple_pattern) => tuple_pattern.decl(ty, ctx, scope),

            _ => Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("Node type `", self, "' is not a pattern"),
            }),
        }
    }
    fn assign(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        match self {
            &Ident(ref ident)                => ident.assign(ctx, c),
            &TuplePattern(ref tuple_pattern) => tuple_pattern.assign(ctx, c),

            _ => Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("Node type `", self, "' is not a pattern"),
            }),
        }
    }
}

impl<'ctx> Pattern<'ctx> for syntax::Ident<'ctx> {
    fn decl(&'ctx self, ty: Ty<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        // Annotate the identifier with its type.
        try!(ctx.set_annot(expr::TypeAnnot, self, ty));

        Ok(if self.text() != "_" { try!(scope.decl(self, ty)) })
    }

    fn assign(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        if self.text() == "_" { return Ok(()) }

        let c_t = {
            let ty = try!(ctx.get_annot(self.loc(), expr::TypeAnnot, self));
            try!(c.typedef(ctx, ty))
        };

        fcatln!(c.fn_impls_txt, c.indent, c_t, " ", self.text(), " = ", c.field, ";").ok();

        Ok(())
    }
}

impl<'ctx> Pattern<'ctx> for syntax::TuplePattern<'ctx> {
    fn decl(&'ctx self, ty: Ty<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        if let &Type::Tuple { ref elems } = ty {
            if elems.len() != self.elems.len() {
                return Err(error::Error::TuplePatternCountMismatch {
                    pat_loc: self.loc(),
                    ty:      ty,
                });
            }

            for (&pat, &elem_ty) in self.elems.iter().zip(elems.iter()) {
                try!(pat.decl(elem_ty, ctx, scope));
            }

            Ok(())
        } else {
            Err(error::Error::PatternTypeMismatch {
                pat_loc:  self.loc(),
                pat_desc: "tuple pattern",
                ty:       ty,
            })
        }
    }

    fn assign(&'ctx self, ctx: CtxRef<'ctx>, c: &mut CBuffer<'ctx>)
    -> error::Result<'ctx, ()>
    {
        let old_len = c.field.len();

        for (idx, &elem) in self.elems.iter().enumerate() {
            // Push a selector onto the field string.
            strcat!(c.field, "._", idx);

            // Recurse into the next pattern.
            try!(elem.assign(ctx, c));

            // Pop the selector off.
            c.field.truncate(old_len);
        }

        Ok(())
    }
}
