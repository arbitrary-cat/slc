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

use std::result;

use source::{self, Token};
use syntax::{Node, GenNode, Ident};
use types::Type;

macro_rules! sl_err {
    ($loc:expr, $($args:tt)*) => {
        ecatln!(ERROR_PREFIX, ": ", $loc, ": ", MSG_BEGIN, $($args)*, MSG_END);
        ecatln!($loc.in_context());
    }
}

macro_rules! sl_info {
    ($loc:expr, $($args:tt)*) => {
        ecatln!(MSG_BEGIN, "info: ", $loc, ": ", $($args)*, MSG_END);
        ecatln!($loc.in_context());
    }
}

pub type Result<'ctx, T> = result::Result<T, Error<'ctx>>;

pub enum Error<'ctx> {
    InternalError {
        loc: Option<source::Loc<'ctx>>,
        msg: String,
    },
    Syntax {
        exp: String,
        got: Token<'ctx>,
    },
    EOF {
        exp: String,
    },
    AlreadyDeclared {
        /// Location where redeclaration was attemped.
        ident: &'ctx Ident<'ctx>,

        /// Location where the original declaration occurred.
        decl:  &'ctx Ident<'ctx>,
    },
    NotDeclared {
        ident: &'ctx Ident<'ctx>,
    },
    BinOpTypeMismatch {
        op:    &'ctx Node<'ctx>,
        lhs_t: &'ctx Type<'ctx>,
        rhs_t: &'ctx Type<'ctx>,
    },
    FnBodyTypeMismatch {
        name:  &'ctx Ident<'ctx>,
        exp_t: &'ctx Type<'ctx>,
        got_t: &'ctx Type<'ctx>,
    },
    FnArgTypeMismatch {
        arg:   &'ctx Node<'ctx>,
        exp_t: &'ctx Type<'ctx>,
        arg_t: &'ctx Type<'ctx>,
    },
    NonFnCalled {
        ty:   &'ctx Type<'ctx>,
        site: &'ctx Node<'ctx>,
    },
    Unsupported {
        msg:  &'static str,
        site: source::Loc<'ctx>,
    },
    IfElseTypeMismatch {
        if_loc: source::Loc<'ctx>,
        yes_t:  &'ctx Type<'ctx>,
        no_t:   &'ctx Type<'ctx>,
    },
    IfCondNotBool {
        if_loc: source::Loc<'ctx>,
        cond_t: &'ctx Type<'ctx>,
    },
    LogicOperandNotBool {
        op_loc:   source::Loc<'ctx>,
        operator: &'ctx Node<'ctx>,
        op_t:     &'ctx Type<'ctx>,
    },
    PatternTypeMismatch {
        pat_loc:  source::Loc<'ctx>,
        pat_desc: &'static str,
        ty:       &'ctx Type<'ctx>,
    },
    CannotResolveType {
        loc: source::Loc<'ctx>,
    },
    TuplePatternCountMismatch {
        pat_loc:  source::Loc<'ctx>,
        ty:       &'ctx Type<'ctx>,
    },
}


const ERROR_PREFIX: &'static str = "\x1B[1m\x1B[31merror\x1B[0m";
const MSG_BEGIN:    &'static str = "\x1B[3m";
const MSG_END:      &'static str = "\x1B[0m";

impl<'ctx> Error<'ctx> {
    // Print an error to stdout.
    pub fn display(&self) {
        // Don't care if we silently fail to print to stderr. That's a crazy scenario anyway.
        #![allow(unused_must_use)]

        use self::Error::*;

        match self {
            &InternalError { loc: Some(loc), ref msg } => {
                const INTERNAL_ERROR_PREFIX: &'static str = "\x1B[1m\x1B[31minternal compiler error\x1B[0m";

                ecatln!(INTERNAL_ERROR_PREFIX, ": ", loc, ": ", MSG_BEGIN, msg, MSG_END);
                ecatln!(loc.in_context());
            },
            &InternalError { loc: None, ref msg } => {
                const INTERNAL_ERROR_PREFIX: &'static str = "\x1B[1m\x1B[31minternal compiler error\x1B[0m";
                ecatln!(INTERNAL_ERROR_PREFIX, ": ", MSG_BEGIN, msg, MSG_END);
            },
            &Syntax { ref exp, ref got } => {
                sl_err!(got.loc, "unexpected ", got.data, ", expecting ", exp);
            },
            &EOF { ref exp } => {
                ecatln!(ERROR_PREFIX, ": ", MSG_BEGIN, "unexpected EOF, expecting ", exp, MSG_END);
            },
            &AlreadyDeclared { decl, ident } => {
                sl_err!(ident.loc(), "variable `", ident.text(), "' already declared");

                sl_info!(decl.loc(), "original declaration here");
            },
            &NotDeclared { ident } => {
                sl_err!(ident.loc(), "variable `", ident.text(), "' not declared");
            },
            &BinOpTypeMismatch { op, lhs_t, rhs_t } => {
                sl_err!(op.loc(),  "type mismatch for operator `", op.text(), "' ",
                    "(`", lhs_t, "' != `", rhs_t, "')");
            },
            &FnBodyTypeMismatch { name, exp_t, got_t } => {
                sl_err!(name.loc(), "wrong type for function body, declaration gives type `", exp_t,
                    "', but body has type `", got_t, "'");
            },
            &FnArgTypeMismatch { arg, exp_t, arg_t } => {
                sl_err!(arg.loc(), "wrong type for function argument, expected `", exp_t, "' but ",
                    "argument has type `", arg_t, "'");
            },
            &NonFnCalled { site, ty } => {
                sl_err!(site.loc(), "attempt to call non-function expression (has type `", ty, "')");
            }
            &Unsupported { msg, site } => {
                sl_err!(site, msg);
            }
            &IfElseTypeMismatch { if_loc, yes_t, no_t } => {
                sl_err!(if_loc, "if/else branch types don't match (`", yes_t, "' != `", no_t, "')");
            }
            &IfCondNotBool { if_loc, cond_t } => {
                sl_err!(if_loc, "if-expression condition isn't boolean (type is `", cond_t, "')");
            }
            &LogicOperandNotBool { op_loc, op_t, operator } => {
                sl_err!(op_loc, "operand to `", operator.text(), "' isn't boolean (type is `", op_t,
                    "')");
            }
            &PatternTypeMismatch { pat_loc, pat_desc, ty } => {
                sl_err!(pat_loc, "cannot match type `", ty, "' against ", pat_desc);
            }
            &CannotResolveType { loc } => {
                sl_err!(loc, "cannot resolve type for `nil' alone, need a hint.");
            }
            &TuplePatternCountMismatch { pat_loc, ty } => {
                sl_err!(pat_loc, "incorrect number of tuple elements for type `", ty, "'");
            }
        }
    }
}
