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
use std::rc::Rc;

use compiler::CtxRef;
use error::{self, Error};
use expr::Expr;
use pattern::Pattern;
use syntax::{self, Ident, GenNode};
use types::Type;
use util;

#[derive(Clone)]
pub struct Symbol<'ctx> {
    pub decl: &'ctx Ident<'ctx>,
    pub ty:   &'ctx Type<'ctx>,
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

    pub fn decl(&mut self, id: &'ctx Ident<'ctx>, ty: &'ctx Type<'ctx>)
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
    /// This method will be called exactly once per node. It should declare variables, set up
    /// scopes, and annotate nodes.
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    ;
}

impl<'ctx> Check<'ctx> for syntax::Node<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        use syntax::Node::*;

        match self {
            &TranslationUnit(ref translation_unit) => translation_unit.check(ctx, scope),
            &FnDecl(ref fn_decl)                   => fn_decl.check(ctx, scope),
            &BinOp(ref bin_op)                     => bin_op.check(ctx, scope),
            &Ident(ref ident)                      => ident.check(ctx, scope),
            &Tuple(ref tuple)                      => tuple.check(ctx, scope),
            &FnCall(ref fn_call)                   => fn_call.check(ctx, scope),
            &IfExpr(ref if_expr)                   => if_expr.check(ctx, scope),
            &IntLit(ref int_lit)                   => int_lit.check(ctx, scope),
            &LetExpr(ref let_expr)                 => let_expr.check(ctx, scope),

            _ => return Err(error::Error::InternalError {
                loc: Some(self.loc()),
                msg: scat!("No semantic checker for node type `", self, "'"),
            }),
        }
    }
}

impl<'ctx> Check<'ctx> for syntax::TranslationUnit<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        for &fn_decl in self.fn_decls.iter() {
            try!(fn_decl.check(ctx, scope));
        }

        Ok(())
    }
}

impl<'ctx> Check<'ctx> for syntax::FnDecl<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        // Count the number of arguments the function has
        let n_args = self.args.iter()
                              .fold(0, |n, &arg| {
                                  let var_decl: &syntax::VarDecl<'ctx> = arg.as_ref();
                                  var_decl.names.len() + n
                              });

        let mut body_scope = scope.child();

        // Construct the function's argument type
        let from = {
            let mut elems = Vec::with_capacity(n_args);

            for var_decl in self.args.iter().map(|a| -> &'ctx syntax::VarDecl<'ctx> { a.as_ref() }) {
                for &name in var_decl.names.iter() {
                    try!(body_scope.decl(name.as_ref(), var_decl.ty));
                    elems.push(var_decl.ty);
                }
            }

            ctx.types.mk_type(Type::Tuple { elems: elems })
        };

        // Declare the function in its parent scope.
        let fn_t = ctx.types.mk_type(Type::Func { from: from, to: self.to });
        try!(scope.decl(self.name.as_ref(), fn_t));

        // Annotate the function body with its scope.
        ctx.scopes.insert(self.body, body_scope.clone());


        try!(self.body.check(ctx, &mut body_scope));

        let body_t = try!(self.body.type_in(ctx, &mut body_scope));

        if body_t != self.to {
            return Err(error::Error::FnBodyTypeMismatch {
                 name:  self.name.as_ref(),
                 exp_t: self.to,
                 got_t: body_t,
            })
        }

        Ok(())
    }
}


impl<'ctx> Check<'ctx> for syntax::Ident<'ctx> {
    /// `check` should only be called on an Ident in an expression context.
    fn check(&'ctx self, _ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        try!(scope.lookup(self));
        Ok(())
    }
}

impl<'ctx> Check<'ctx> for syntax::BinOp<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        try!(self.lhs.check(ctx, scope));
        try!(self.rhs.check(ctx, scope));

        let lhs_t = try!(self.lhs.type_in(ctx, scope));
        let rhs_t = try!(self.rhs.type_in(ctx, scope));

        if lhs_t != rhs_t {
            Err(error::Error::BinOpTypeMismatch {
                op:    self.op,
                lhs_t: lhs_t,
                rhs_t: rhs_t,
            })
        } else { Ok(()) }
    }
}

impl<'ctx> Check<'ctx> for syntax::Tuple<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        for &elem in self.elems.iter() {
            try!(elem.check(ctx, scope));
            try!(elem.type_in(ctx, scope));
        }

        Ok(())
    }
}

impl<'ctx> Check<'ctx> for syntax::FnCall<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        try!(self.fun.check(ctx, scope));
        try!(self.arg.check(ctx, scope));

        let from_t = match try!(self.fun.type_in(ctx, scope)) {
            &Type::Func { from, .. } => from,
            ty => return Err(error::Error::NonFnCalled {
                ty:   ty,
                site: self.fun,
            }),
        };

        let arg_t = try!(self.arg.type_in(ctx, scope));

        if from_t != arg_t {
            Err(error::Error::FnArgTypeMismatch {
                arg:   self.arg,
                exp_t: from_t,
                arg_t: arg_t,
            })
        } else { Ok(()) }
    }
}

impl<'ctx> Check<'ctx> for syntax::IfExpr<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        let no = match self.no {
            Some(no) => no,
            None => return Err(error::Error::Unsupported {
                msg:  "if-exprs without a matching else-clause are currently unsupported",
                site: self.if_loc,
            }),
        };

        try!(self.cond.check(ctx, scope));
        try!(self.yes.check(ctx, scope));
        try!(no.check(ctx, scope));

        let cond_t = try!(self.cond.type_in(ctx, scope));

        if cond_t != &Type::Bool {
            return Err(error::Error::IfCondNotBool {
                if_loc: self.if_loc,
                cond_t: cond_t,
            });
        }

        let yes_t  = try!(self.yes.type_in(ctx, scope));
        let no_t   = try!(no.type_in(ctx, scope));

        if yes_t != no_t {
            Err(error::Error::IfElseTypeMismatch {
                if_loc: self.if_loc,
                yes_t:  yes_t,
                no_t:   no_t,
            })
        } else { Ok(()) }
    }
}

impl<'ctx> Check<'ctx> for syntax::IntLit<'ctx> {
    fn check(&'ctx self, _ctx: CtxRef<'ctx>, _scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        Ok(())
    }
}

impl<'ctx> Check<'ctx> for syntax::LetExpr<'ctx> {
    fn check(&'ctx self, ctx: CtxRef<'ctx>, scope: &mut Scope<'ctx>)
    -> error::Result<'ctx, ()>
    {
        try!(self.val.check(ctx, scope));

        let val_t = try!(self.val.type_in(ctx, scope));

        let mut inner_scope = scope.child();

        try!(self.pat.decl(val_t, ctx, &mut inner_scope));

        ctx.scopes.insert(self.body, inner_scope.clone());

        self.body.check(ctx, &mut inner_scope)
    }
}
