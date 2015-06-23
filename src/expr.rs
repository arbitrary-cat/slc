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
use syntax::{self, GenNode, Node, Ident};
use semantic::Scope;
use types::Type;
use pattern::Pattern;

/// The table used to annotate expression nodes with their type.
pub type TypeMap<'ctx> = syntax::NodeMap<'ctx, &'ctx Type<'ctx>>;

/// A trait for types that can be an expression in the syntax tree.
pub trait Expr<'ctx> {
    /// Return the type of this node in the given context.
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    ;
}

impl<'ctx> Expr<'ctx> for Node<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        use syntax::Node::*;

        if let Some(ty) = ctx.ty_map.get(self) {
            Ok(ty)
        } else {
            let ty = match self {
                &Ident(ref ident)      => try!(ident.type_in(ctx, scope)),
                &IntLit(ref int_lit)   => try!(int_lit.type_in(ctx, scope)),
                &LogicOp(ref logic_op) => try!(logic_op.type_in(ctx, scope)),
                &BinOp(ref bin_op)     => try!(bin_op.type_in(ctx, scope)),
                &FnCall(ref fn_call)   => try!(fn_call.type_in(ctx, scope)),
                &IfExpr(ref if_expr)   => try!(if_expr.type_in(ctx, scope)),
                &LetExpr(ref let_expr) => try!(let_expr.type_in(ctx, scope)),
                &Tuple(ref tuple)      => try!(tuple.type_in(ctx, scope)),

                _ => return Err(error::Error::InternalError {
                    loc: Some(self.loc()),
                    msg: scat!("Node type `", self, "' is not an expression"),
                }),
            };

            ctx.ty_map.insert(self, ty);

            Ok(ty)
        }
    }
}

impl<'ctx> Expr<'ctx> for syntax::Ident<'ctx> {
    fn type_in(&'ctx self, _ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        scope.lookup(self).map(|sym| sym.ty)
    }
}

impl<'ctx> Expr<'ctx> for syntax::IntLit<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, _scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        Ok(ctx.types.mk_type(Type::Int))
    }
}

impl<'ctx> Expr<'ctx> for syntax::LogicOp<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        if try!(self.first.type_in(ctx, scope)) != &Type::Bool {
            Err(error::Error::LogicOperandNotBool {
                op_loc:   self.first.loc(),
                op_t:     try!(self.first.type_in(ctx, scope)),
                operator: self.op,
            })
        } else if try!(self.first.type_in(ctx, scope)) != &Type::Bool {
            Err(error::Error::LogicOperandNotBool {
                op_loc:   self.second.loc(),
                op_t:     try!(self.second.type_in(ctx, scope)),
                operator: self.op,
            })
        } else {
            Ok(ctx.types.mk_type(Type::Bool))
        }
    }
}


impl<'ctx> Expr<'ctx> for syntax::BinOp<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        let lhs_t = try!(self.lhs.type_in(ctx, scope));
        let rhs_t = try!(self.rhs.type_in(ctx, scope));

        if lhs_t != rhs_t {
            Err(error::Error::BinOpTypeMismatch {
                op:    self.op,
                lhs_t: lhs_t,
                rhs_t: rhs_t,
            })
        } else {
            Ok(lhs_t)
        }
    }
}


impl<'ctx> Expr<'ctx> for syntax::FnCall<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        if let &Type::Func { from, to } = try!(self.fun.type_in(ctx, scope)) {
            let arg_t = try!(self.arg.type_in(ctx, scope));
            if from != arg_t {
                // The function's argument is of the wrong type.
                Err(error::Error::FnArgTypeMismatch {
                    arg:   self.arg,
                    exp_t: from,
                    arg_t: arg_t,
                })
            } else {
                Ok(to)
            }
        } else {
            // `fun` isn't a function.
            Err(error::Error::NonFnCalled { site: self.fun })
        }
    }
}


impl<'ctx> Expr<'ctx> for syntax::IfExpr<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        let cond_t = try!(self.cond.type_in(ctx, scope));
        let yes_t  = try!(self.yes.type_in(ctx, scope));
        let no_t   = if let Some(no) = self.no {
            try!(no.type_in(ctx, scope))
        } else {
            return Err(error::Error::Unsupported {
                msg:  "if expressions without a corresponding else are currently unsupported",
                site: self.if_loc,
            });
        };

        if cond_t != &Type::Bool {
            Err(error::Error::IfCondNotBool {
                if_loc: self.if_loc,
                cond_t: cond_t,
            })
        } else if yes_t != no_t {
            Err(error::Error::IfElseTypeMismatch {
                if_loc: self.if_loc,
                yes_t:  yes_t,
                no_t:   no_t,
            })
        } else {
            Ok(yes_t)
        }
    }
}


impl<'ctx> Expr<'ctx> for syntax::LetExpr<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        let val_t = try!(self.val.type_in(ctx, scope));

        let expr_scope = if let Some(expr_scope) = ctx.scopes.get(self.body) {
            expr_scope
        } else {
            let mut expr_scope = scope.child();
            try!(self.pat.decl(val_t, ctx, &mut expr_scope));
            ctx.scopes.insert(self.body, expr_scope.clone());

            expr_scope
        };

        self.body.type_in(ctx, &expr_scope)
    }
}


impl<'ctx> Expr<'ctx> for syntax::Tuple<'ctx> {
    fn type_in(&'ctx self, ctx: CtxRef<'ctx>, scope: &Scope<'ctx>)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    {
        let mut elems_t = Vec::new();

        for &elem in self.elems.iter() {
            elems_t.push(try!(elem.type_in(ctx, scope)));
        }

        Ok(ctx.types.mk_type(Type::Tuple { elems: elems_t }))
    }
}
