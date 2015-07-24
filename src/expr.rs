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

use compiler::{self, CtxRef};
use types::Ty;
use util;

use cats;

use std::io;

/// Compiler context for processing expressions.
pub struct Context<'ctx> {
    ty_map: util::TagMap<'ctx, Ty<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Context<'ctx> {
        Context {
            ty_map: util::TagMap::new(),
        }
    }
}

/// TypeAnnot is the tag for type-annotations on expression nodes.
pub struct TypeAnnot;

impl cats::Show for TypeAnnot {
    fn len(&self)
    -> usize
    {
        cat_len!("expr::TypeAnnot")
    }

    fn write<W: io::Write>(&self, w: &mut W)
    -> io::Result<usize>
    {
        cat_write!(w, "expr::TypeAnnot")
    }
}

impl<'ctx> compiler::Annotation<'ctx> for TypeAnnot {
    type Data = Ty<'ctx>;

    fn get_map(&self, ctx: CtxRef<'ctx>)
    -> &'ctx util::TagMap<'ctx, Ty<'ctx>>
    {
        &ctx.expr.ty_map
    }
}
