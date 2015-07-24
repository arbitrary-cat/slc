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

use error;
use semantic;
use source;
use syntax;
use types;
use util;

use cats;

pub struct Context<'ctx> {
    pub file:     &'ctx source::File,
    pub syntax:   syntax::Context<'ctx>,
    pub types:    types::Context<'ctx>,
    pub semantic: semantic::Context<'ctx>,

    pub scopes:  semantic::ScopeMap<'ctx>,
    pub strings: util::StrArena,
}

/// An annotation on a compiler entity.
///
/// See `semantic::TypeAnnot` for an example implementation of this trait.
pub trait Annotation<'ctx>: cats::Show
{
    type Data: Clone;

    /// Return a tag-map (probably stored at `ctx`) which contains these annotations.
    fn get_map(&self, ctx: CtxRef<'ctx>)
    -> &'ctx util::TagMap<'ctx, Self::Data>
    ;
}

/// Convenience type to avoid writing the same typename twice.
pub type CtxRef<'ctx> = &'ctx Context<'ctx>;

impl<'ctx> Context<'ctx> {
    pub fn new(f: &'ctx source::File) -> Context<'ctx> {
        Context {
            file:    f,
            scopes:  semantic::ScopeMap::new(),
            strings: util::StrArena::new(),

            syntax:   syntax::Context::new(),
            types:    types::Context::new(),
            semantic: semantic::Context::new(),
        }
    }

    /// Annotate a compiler object (anything that implements `util::Tag`, such as No/Ty).
    pub fn set_annot<T, A, K>(&'ctx self, t: T, key: &'ctx K, a: A)
    -> error::Result<'ctx, Option<A>>
    where T: Annotation<'ctx, Data=A>,
          A: Clone + 'ctx,
          K: util::Tagged<'ctx>,
    {
        Ok(t.get_map(self).insert(key, a))
    }

    /// Get the annotation for a compiler object (anything that implements `util::Tag`, such as
    /// No/Ty).
    pub fn get_annot<T, A, K>(&'ctx self, loc: source::Loc<'ctx>, t: T, key: &'ctx K)
    -> error::Result<'ctx, A>
    where T: Annotation<'ctx, Data=A>,
          A: Clone + 'ctx,
          K: util::Tagged<'ctx>,
    {
        match t.get_map(self).get(key) {
            Some(a) => Ok(a),
            None    => Err(error::Error::InternalError {
                loc: Some(loc),
                msg: scat!("missing `", t, "' annotation"),
            })
        }
    }
}
