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

use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

use cats;

use compiler::{self, CtxRef};
use error::{self, Error};
use syntax::{self, Ident, GenNode, No};
use types::{Ty, Type};
use util;

#[derive(Clone)]
pub struct Symbol<'ctx> {
    pub decl: &'ctx Ident<'ctx>,
    pub ty:   Ty<'ctx>,
}

pub struct ScopeInner<'ctx> {
    parent: Option<Scope<'ctx>>,
    table:  HashMap<&'ctx str, Symbol<'ctx>>,
}

#[derive(Clone)]
pub struct Scope<'ctx> {
    inner: Rc<RefCell<ScopeInner<'ctx>>>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new() -> Scope<'ctx> {
        Scope {
            inner: Rc::new(RefCell::new(ScopeInner {
                parent: None,
                table:  HashMap::new(),
            })),
        }
    }

    pub fn decl(&mut self, id: &'ctx Ident<'ctx>, ty: Ty<'ctx>)
    -> error::Result<'ctx, ()>
    {

        let mut borrow = self.inner.borrow_mut();

        if let Some(&Symbol { decl, .. }) = borrow.table.get(id.text()) {
            Err(Error::AlreadyDeclared { decl: decl, ident: id })
        } else {
            borrow.table.insert(id.text(), Symbol { decl: id, ty: ty });
            Ok(())
        }
    }

    pub fn child(&self)
    -> Scope<'ctx>
    {
        Scope {
            inner: Rc::new(RefCell::new(ScopeInner {
                parent: Some(self.clone()),
                table:  HashMap::new(),
            })),
        }
    }

    pub fn parent(&self) -> Option<Scope<'ctx>> { self.inner.borrow().parent.clone() }

    /// Lookup a symbol based on the name of a given identifier.
    pub fn lookup(&self, id: &'ctx Ident<'ctx>)
    -> error::Result<'ctx, Symbol<'ctx>>
    {
        let r = self.inner.borrow();
        if let Some(sym) = r.table.get(id.text()) {
            Ok(sym.clone())
        } else if let Some(p) = r.parent.as_ref() {
            p.lookup(id)
        } else {
            Err(Error::NotDeclared { ident: id })
        }
    }
}

/// A mapping from nodes to the scopes that contain them.
///
/// # Notes
///
/// This is kind of sketchy at the moment, which nodes should be annotated with scopes? Right now
/// we're just doing function bodies and let-expr bodies.
pub type ScopeMap<'ctx> = util::TagMap<'ctx, Scope<'ctx>>;

/// A trait for nodes that can be checked for semantic validity.
pub trait Check<'ctx> {
    /// The methods of this trait each constitute a *compiler pass*, and they should be run in the
    /// order that they are listed here:

    /// Declare identifiers with (possibly unresolved) types for scopes within this syntax element.
    fn decl(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    ;

    /// Resolve the type of this syntax element and/or any typed children it might have.
    fn resolve(&'ctx self, hint: Option<Ty<'ctx>>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    ;

    /// Verify that this syntax element and all of its children are semantically sound.
    fn check(&'ctx self, ctx: CtxRef<'ctx>)
    -> error::Result<'ctx, ()>
    ;
}

impl<'ctx> Check<'ctx> for No<'ctx> {

    fn decl(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        match self {

            _ => return Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("No semantic::Check::decl pass for node type `", self, "'"),
            }),
        }
    }

    fn resolve(&'ctx self, hint: Option<Ty<'ctx>>, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        match self {

            _ => return Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("No semantic::Check::resolve pass for node type `", self, "'"),
            }),
        }
    }

    fn check(&'ctx self, ctx: CtxRef<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        match self {

            _ => return Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("No semantic::Check::check pass for node type `", self, "'"),
            }),
        }
    }
}

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
        cat_len!("semantic::TypeAnnot")
    }

    fn write<W: io::Write>(&self, w: &mut W)
    -> io::Result<usize>
    {
        cat_write!(w, "semantic::TypeAnnot")
    }
}

impl<'ctx> compiler::Annotation<'ctx> for TypeAnnot {
    type Data = Ty<'ctx>;

    fn get_map(&self, ctx: CtxRef<'ctx>)
    -> &'ctx util::TagMap<'ctx, Ty<'ctx>>
    {
        &ctx.semantic.ty_map
    }
}
