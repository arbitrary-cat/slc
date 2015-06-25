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

use compiler::CtxRef;
use error;
use semantic::Scope;
use syntax::{self, GenNode};
use types::Type;


pub trait Pattern<'ctx> {
    /// Declare in `scope` all identifiers introduced by a pattern to which an expression of type
    /// `ty` will be assigned.
    fn decl(&'ctx self, ty: &'ctx Type<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    ;
}

impl<'ctx> Pattern<'ctx> for syntax::Node<'ctx> {
    fn decl(&'ctx self, ty: &'ctx Type<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
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
}

impl<'ctx> Pattern<'ctx> for syntax::Ident<'ctx> {
    fn decl(&'ctx self, ty: &'ctx Type<'ctx>, _ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        Ok(if self.text() != "_" { try!(scope.decl(self, ty)) })
    }
}

impl<'ctx> Pattern<'ctx> for syntax::TuplePattern<'ctx> {
    fn decl(&'ctx self, ty: &'ctx Type<'ctx>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
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
}
