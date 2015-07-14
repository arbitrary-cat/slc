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

// The parser macros sometimes create unused variables
#![allow(unused_variables)]

use std::convert::Into;
use std::io;
use std::mem;

use cats;

use compiler;
use error;
use source;
use source::TokenData::*;
use types::Type;
use util;

use arena::TypedArena;


/// An identifier.
pub struct Ident<'ctx> {
    /// The token which corresponds to this identifier in the source.
    pub tok: source::Token<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for Ident<'ctx> {}

impl<'ctx> AsRef<Ident<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &Ident<'ctx> {
        match self {
            &Node::Ident(ref id) => id,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for Ident<'ctx> {
    fn into(self) -> Node<'ctx> { Node::Ident(self) }
}

impl<'ctx> GenNode<'ctx> for Ident<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.tok.loc
    }

    fn text(&self)
    -> &'ctx str
    {
        if let source::TokenData::Id(txt) = self.tok.data {
            txt
        } else { unreachable!() }
    }
}


/// An integer literal.
pub struct IntLit<'ctx> {
    /// The value of this literal.
    pub tok: source::Token<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for IntLit<'ctx> {}

impl<'ctx> AsRef<IntLit<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &IntLit<'ctx> {
        match self {
            &Node::IntLit(ref i) => i,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for IntLit<'ctx> {
    fn into(self) -> Node<'ctx> { Node::IntLit(self) }
}

impl<'ctx> GenNode<'ctx> for IntLit<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.tok.loc
    }

    fn text(&self)
    -> &'ctx str
    {
        "<int literal>"
    }
}

/// An constructor statement for an optional type (e.g. `? int`).
pub struct OptCons<'ctx> {
    /// The location of the `some` or `nil` keyword.
    pub kw: source::Token<'ctx>,

    /// If `kw` has data `Kw("some")` then this will be the expression being wrapped with some.
    /// Otherwise it will be `None`.
    pub val: Option<&'ctx Node<'ctx>>,
}

impl<'ctx> util::Tagged<'ctx> for OptCons<'ctx> {}

impl<'ctx> AsRef<OptCons<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &OptCons<'ctx> {
        match self {
            &Node::OptCons(ref opt_cons) => opt_cons,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for OptCons<'ctx> {
    fn into(self) -> Node<'ctx> { Node::OptCons(self) }
}

impl<'ctx> GenNode<'ctx> for OptCons<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.kw.loc
    }

    fn text(&self)
    -> &'ctx str
    {
        if let source::TokenData::Kw(txt) = self.kw.data {
            txt
        } else { unreachable!() }
    }
}

/// An operator, as would be found in an expression.
pub struct Operator<'ctx> {
    /// The token which corresponds to this operator in the source.
    pub tok: source::Token<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for Operator<'ctx> {}

impl<'ctx> AsRef<Operator<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &Operator<'ctx> {
        match self {
            &Node::Operator(ref op) => op,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for Operator<'ctx> {
    fn into(self) -> Node<'ctx> { Node::Operator(self) }
}

impl<'ctx> GenNode<'ctx> for Operator<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.tok.loc
    }

    fn text(&self)
    -> &'ctx str
    {
        if let source::TokenData::Sym(txt) = self.tok.data {
            txt
        } else { unreachable!() }
    }
}


/// A translation unit, a single contiguous SL source file.
pub struct TranslationUnit<'ctx> {
    /// The source location of the beginning of the source file.
    pub start: source::Loc<'ctx>,

    /// All functions declared in this file, in the order that they were declared.
    pub fn_decls: Vec<&'ctx Node<'ctx>>,
}

impl<'ctx> util::Tagged<'ctx> for TranslationUnit<'ctx> {}

impl<'ctx> AsRef<TranslationUnit<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &TranslationUnit<'ctx> {
        match self {
            &Node::TranslationUnit(ref tu) => tu,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for TranslationUnit<'ctx> {
    fn into(self) -> Node<'ctx> { Node::TranslationUnit(self) }
}

impl<'ctx> GenNode<'ctx> for TranslationUnit<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.start
    }

    fn text(&self)
    -> &'ctx str
    {
        "<translation unit>"
    }
}

/// A list of variables being declared by name.
pub struct VarDecl<'ctx> {
    /// Identifiers which name the variables being declared.
    ///
    /// # Invariants
    ///
    /// `self.names.len() != 0`
    pub names: Vec<&'ctx Node<'ctx>>,

    /// The type of the variables being declared.
    pub ty: &'ctx Type<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for VarDecl<'ctx> {}

impl<'ctx> AsRef<VarDecl<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &VarDecl<'ctx> {
        match self {
            &Node::VarDecl(ref var_decl) => var_decl,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for VarDecl<'ctx> {
    fn into(self) -> Node<'ctx> { Node::VarDecl(self) }
}

impl<'ctx> GenNode<'ctx> for VarDecl<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.names.first().unwrap().loc()
    }

    fn text(&self)
    -> &'ctx str
    {
        "<var decl>"
    }
}

/// A function declaration.
pub struct FnDecl<'ctx> {
    /// The name of this function, at the location where it was declared.
    pub name: &'ctx Node<'ctx>,

    /// The VarDecls which declare the arguments to this function.
    pub args: Vec<&'ctx Node<'ctx>>,

    /// Type that the function outputs.
    pub to: &'ctx Type<'ctx>,

    /// The body of this function (an expression).
    pub body: &'ctx Node<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for FnDecl<'ctx> {}

impl<'ctx> AsRef<FnDecl<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &FnDecl<'ctx> {
        match self {
            &Node::FnDecl(ref fn_decl) => fn_decl,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for FnDecl<'ctx> {
    fn into(self) -> Node<'ctx> { Node::FnDecl(self) }
}

impl<'ctx> GenNode<'ctx> for FnDecl<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.name.loc()
    }

    fn text(&self)
    -> &'ctx str
    {
        "<fn decl>"
    }
}


pub struct LogicOp<'ctx> {
    /// The expression which will be tested first. Must have type `Bool`.
    pub first:  &'ctx Node<'ctx>,

    /// The operator, either `&&` or `||`
    pub op:     &'ctx Node<'ctx>,

    /// The expression which will be tested second. Must have type `Bool`.
    pub second: &'ctx Node<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for LogicOp<'ctx> {}

impl<'ctx> AsRef<LogicOp<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &LogicOp<'ctx> {
        match self {
            &Node::LogicOp(ref logic_op) => logic_op,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for LogicOp<'ctx> {
    fn into(self) -> Node<'ctx> { Node::LogicOp(self) }
}

impl<'ctx> GenNode<'ctx> for LogicOp<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.op.loc()
    }

    fn text(&self)
    -> &'ctx str
    {
        "<logic op>"
    }
}


/// A binary expression.
pub struct BinOp<'ctx> {
    /// The left-hand side of the expression.
    pub lhs: &'ctx Node<'ctx>,

    /// The operator.
    pub op:  &'ctx Node<'ctx>,

    /// The right-hand side of the expression.
    pub rhs: &'ctx Node<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for BinOp<'ctx> {}

impl<'ctx> AsRef<BinOp<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &BinOp<'ctx> {
        match self {
            &Node::BinOp(ref bin_op) => bin_op,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for BinOp<'ctx> {
    fn into(self) -> Node<'ctx> { Node::BinOp(self) }
}

impl<'ctx> GenNode<'ctx> for BinOp<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.op.loc()
    }

    fn text(&self)
    -> &'ctx str
    {
        "<binary op>"
    }
}


/// A function call.
pub struct FnCall<'ctx> {
    /// An expression which evaluates to a function.
    pub fun: &'ctx Node<'ctx>,

    /// The argument to this function.
    pub arg: &'ctx Node<'ctx>,
}

impl<'ctx> util::Tagged<'ctx> for FnCall<'ctx> {}

impl<'ctx> AsRef<FnCall<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &FnCall<'ctx> {
        match self {
            &Node::FnCall(ref fn_call) => fn_call,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for FnCall<'ctx> {
    fn into(self) -> Node<'ctx> { Node::FnCall(self) }
}

impl<'ctx> GenNode<'ctx> for FnCall<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.fun.loc()
    }

    fn text(&self)
    -> &'ctx str
    {
        "<fn call>"
    }
}


/// An if/else expression.
pub struct IfExpr<'ctx> {
    /// The location of the `if` token, for use in error messages.
    pub if_loc: source::Loc<'ctx>,

    /// The condition, this must be an expression of boolean type.
    pub cond: &'ctx Node<'ctx>,

    /// The expression which will be evaluated if the condition is true.
    pub yes: &'ctx Node<'ctx>,

    /// The expression which will be evaluated if the condition is false.
    pub no: Option<&'ctx Node<'ctx>>,
}

impl<'ctx> util::Tagged<'ctx> for IfExpr<'ctx> {}

impl<'ctx> AsRef<IfExpr<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &IfExpr<'ctx> {
        match self {
            &Node::IfExpr(ref if_expr) => if_expr,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for IfExpr<'ctx> {
    fn into(self) -> Node<'ctx> { Node::IfExpr(self) }
}

impl<'ctx> GenNode<'ctx> for IfExpr<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.if_loc
    }

    fn text(&self)
    -> &'ctx str
    {
        "<if expr>"
    }
}


/// A tuple expression.
pub struct Tuple<'ctx> {
    /// The location of the tuple's first open paren, for error messages.
    pub open_paren: source::Loc<'ctx>,

    /// The elements of the tuple (expressions).
    pub elems: Vec<&'ctx Node<'ctx>>,
}

impl<'ctx> util::Tagged<'ctx> for Tuple<'ctx> {}

impl<'ctx> AsRef<Tuple<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &Tuple<'ctx> {
        match self {
            &Node::Tuple(ref tuple) => tuple,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for Tuple<'ctx> {
    fn into(self) -> Node<'ctx> { Node::Tuple(self) }
}

impl<'ctx> GenNode<'ctx> for Tuple<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.open_paren
    }

    fn text(&self)
    -> &'ctx str
    {
        "<tuple>"
    }
}


/// A let expression.
pub struct LetExpr<'ctx> {
    /// The location of the `let` token, for use in error messages.
    pub let_loc: source::Loc<'ctx>,

    /// The pattern being assigned to.
    pub pat: &'ctx Node<'ctx>,

    /// The value being destructured into the pattern.
    pub val: &'ctx Node<'ctx>,

    /// The expression to be evaluated with this binding. If this field is `None` then the bindings
    /// are declared in the containing scope.
    pub body: Option<&'ctx Node<'ctx>>,
}

impl<'ctx> util::Tagged<'ctx> for LetExpr<'ctx> {}

impl<'ctx> AsRef<LetExpr<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &LetExpr<'ctx> {
        match self {
            &Node::LetExpr(ref let_expr) => let_expr,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for LetExpr<'ctx> {
    fn into(self) -> Node<'ctx> { Node::LetExpr(self) }
}

impl<'ctx> GenNode<'ctx> for LetExpr<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.let_loc
    }

    fn text(&self)
    -> &'ctx str
    {
        "<let expr>"
    }
}


/// A pattern to destructure a tuple.
pub struct TuplePattern<'ctx> {
    pub open_paren: source::Loc<'ctx>,

    /// Patterns for each element of the tuple.
    pub elems:  Vec<&'ctx Node<'ctx>>,
}

impl<'ctx> util::Tagged<'ctx> for TuplePattern<'ctx> {}

impl<'ctx> AsRef<TuplePattern<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &TuplePattern<'ctx> {
        match self {
            &Node::TuplePattern(ref tuple_pattern) => tuple_pattern,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for TuplePattern<'ctx> {
    fn into(self) -> Node<'ctx> { Node::TuplePattern(self) }
}

impl<'ctx> GenNode<'ctx> for TuplePattern<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.open_paren
    }

    fn text(&self)
    -> &'ctx str
    {
        "<tuple pattern>"
    }
}


/// A series of expressions to be evaluated in order, whose value is that of the last expression in
/// the block.
pub struct Block<'ctx> {
    pub open_curly: source::Loc<'ctx>,

    pub exprs: Vec<&'ctx Node<'ctx>>,

    /// This bool will be true of the `Block` has unit type, meaning that it will only be evaluated
    /// for its side-effects.
    pub unit: bool,
}

impl<'ctx> util::Tagged<'ctx> for Block<'ctx> {}

impl<'ctx> AsRef<Block<'ctx>> for Node<'ctx> {
    fn as_ref(&self) -> &Block<'ctx> {
        match self {
            &Node::Block(ref block) => block,
            _ => unreachable!(),
        }
    }
}

impl<'ctx> Into<Node<'ctx>> for Block<'ctx> {
    fn into(self) -> Node<'ctx> { Node::Block(self) }
}

impl<'ctx> GenNode<'ctx> for Block<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        self.open_curly
    }

    fn text(&self)
    -> &'ctx str
    {
        "<block>"
    }
}


pub struct Context<'ctx> {
    mem: TypedArena<Node<'ctx>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Context<'ctx> {
        Context { mem: TypedArena::new() }
    }

    pub fn mk_node(&'ctx self, n: Node<'ctx>) -> &'ctx Node<'ctx> {
        self.mem.alloc(n)
    }
}


struct Parser<'ctx> {
    ctx:     &'ctx compiler::Context<'ctx>,
    next:    Option<source::Token<'ctx>>,
    scanner: source::Scanner<'ctx>,
}

macro_rules! p_match {
    ($parser:expr, $desc:expr, $exp:pat) => {
        match mem::replace(&mut $parser.next, None) {
            Some(tok @ source::Token { data: $exp, .. }) => {
                mem::replace(&mut $parser.next, $parser.scanner.next());
                tok
            },
            tok => {
                return expecting($desc, tok);
            },
        }
    }
}

macro_rules! p_peek {
    ($tok:ident in $parser:expr { $($($exp:pat),* => $action:expr),* } else $def:expr) => {
        match mem::replace(&mut $parser.next, None) {
            $( $(Some($tok @ source::Token { data: $exp, .. }) => {
                mem::replace(&mut $parser.next, Some($tok));
                $action
            },)* )*
            $tok => {
                mem::replace(&mut $parser.next, $tok);
                $def
            },
        }
    }
}

macro_rules! p_try {
    ($tok:ident in $parser:expr { $($($exp:pat),* => $action:expr),* } else $def:expr) => {
        match mem::replace(&mut $parser.next, None) {
            $( $(Some($tok @ source::Token { data: $exp, .. }) => {
                mem::replace(&mut $parser.next, $parser.scanner.next());
                $action
            },)* )*
            $tok => {
                mem::replace(&mut $parser.next, $tok);
                $def
            },
        }
    }
}

macro_rules! p_delim {
    ($parser:expr, $exp:pat) => {
        p_try!(delim in $parser {
            $exp => true
        } else false )
    }
}

impl<'ctx> Parser<'ctx> {
    fn mk_node<N>(&self, n: N)
    -> &'ctx Node<'ctx>
    where N: Into<Node<'ctx>>
    {
        self.ctx.syntax.mk_node(n.into())
    }

    fn mk_type(&self, t: Type<'ctx>)
    -> &'ctx Type<'ctx>
    {
        self.ctx.types.mk_type(t)
    }

    fn int_lit(&self, tok: source::Token<'ctx>)
    -> &'ctx Node<'ctx>
    {
        self.mk_node(IntLit { tok: tok })
    }

    fn ident(&self, tok: source::Token<'ctx>)
    -> &'ctx Node<'ctx>
    {
        self.mk_node(Ident { tok: tok })
    }

    fn operator(&self, tok: source::Token<'ctx>)
    -> &'ctx Node<'ctx>
    {
        self.mk_node(Operator { tok: tok })
    }

    // TranslationUnit = { FnDecl } .
    fn parse_translation_unit<'x>(&'x mut self) -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x {

        let mut fn_decls = Vec::new();
        while let Some(..) = self.next {
            fn_decls.push(try!(self.parse_fn_decl()));
        }

        Ok(self.mk_node(TranslationUnit {
            start:    self.ctx.file.start_loc(),
            fn_decls: fn_decls,
        }))
    }

    // FnDecl = 'fn' Ident '(' ArgList ')' [ ':' Type ] Block
    fn parse_fn_decl(&mut self)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {
        p_match!(self, "`fn'", Kw("fn"));
        let name = {
            let tok = p_match!(self, "identifier", Id(..));
            self.ident(tok)
        };

        p_match!(self, "`('", Sym("("));
        let args = try!(self.parse_arg_list());
        p_match!(self,
            if args.len() > 0 { "`,' or `)'" } else { "identifier or `)'" },
            Sym(")"));

        let to = if p_delim!(self, Sym(":")) {
            try!(self.parse_type())
        } else {
            self.ctx.types.unit()
        };

        let curly = p_match!(self, "`{'", Sym("{"));
        let body  = try!(self.parse_block(curly.loc));

        let fn_decl = self.mk_node(FnDecl {
            name: name,
            args: args,
            to:   to,
            body: body,
        });

        Ok(fn_decl)
    }

    // ArgList = { VarDecl ',' } [ VarDecl ]
    fn parse_arg_list(&mut self)
    -> error::Result<'ctx, Vec<&'ctx Node<'ctx>>>
    {
        let mut args   = Vec::new();
        p_peek!(tok in self {
            // VarDecl
            Id(..) => {
                while !p_delim!(self, Sym(")")) {
                    args.push(try!(self.parse_var_decl()));
                    if !p_delim!(self, Sym(",")) { break }
                }
            }
        } else { () });

        Ok(args)
    }

    fn parse_var_decl( &mut self)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {
        let mut names = Vec::new();

        let var_id = p_match!(self, "identifier", Id(..));
        names.push(self.ident(var_id));

        while p_delim!(self, Sym(",")) {
            let var_id = p_match!(self, "identifier", Id(..));
            names.push(self.ident(var_id));
        }

        p_match!(self, "`:'", Sym(":"));

        let ty = try!(self.parse_type());

        Ok(self.mk_node(VarDecl {
            names: names,
            ty:    ty,
        }))
    }

    // Type = 'int'
    //      | 'bool'
    //      | '(' { Type ',' } [ Type ] ')'
    //      | 'fn' Type '->' Type
    //      | '?' Type
    //      .
    fn parse_type<'x>(&'x mut self)
    -> error::Result<'ctx, &'ctx Type<'ctx>>
    where 'ctx: 'x
    {
        Ok( p_try!( tok in self {

            Sym("(") => {
                let mut elems = Vec::new();
                while !p_delim!(self, Sym(")")) {
                    elems.push(try!(self.parse_type()));

                    if !p_delim!(self, Sym(",")) {
                        p_match!(self, "`)' or `,'", Sym(")"));
                        break;
                    }
                }

                self.mk_type(Type::Tuple { elems: elems })
            },

            Kw("fn") => {
                let from = try!(self.parse_type());
                p_match!(self, "`->'", Sym("->"));
                let to = try!(self.parse_type());

                self.mk_type(Type::Func {
                    from: from,
                    to:   to,
                })
            },

            Sym("?") => {
                let t = try!(self.parse_type());
                self.mk_type(Type::Opt(t))
            },

            Id("int") => self.mk_type(Type::Int),

            Id("bool") => self.mk_type(Type::Bool)

        } else {
            return expecting("type", tok);
        }))
    }

    // Expr = LogicExpr .
    fn parse_expr<'x>(&'x mut self)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x {
        self.parse_logic_expr()
    }

    // LogicExpr = CmpExpr [ ( '&&' | '||' ) LogicExpr ] .
    fn parse_logic_expr<'x>(&'x mut self) -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x
    {
        // CmpExpr
        let first = try!(self.parse_cmp_expr());

        Ok(p_try!(tok in self {

            // ( '&&' | '||' ) LogicExpr .
            Sym("||"), Sym("&&") => {
                let second = try!(self.parse_cmp_expr());
                let op = self.operator(tok);

                self.mk_node(LogicOp {
                    first:  first,
                    op:     op,
                    second: second,
                })
            }

        } else {
            // .
            first
        }))
    }

    // CmpExpr = AddExpr [ ( '<' | '>' | '<=' | '>=' | '== ' | '!=' ) CmpExpr ] .
    fn parse_cmp_expr<'x>(&'x mut self) -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x
    {
        // AddExpr
        let lhs = try!(self.parse_add_expr());

        Ok(p_try!(tok in self {

            // ( '<' | '>' | '<=' | '>=' | '==' | '!=' ) CmpExpr .
            Sym("<"), Sym(">"), Sym("<="), Sym(">="), Sym("=="), Sym("!=") => {
                let rhs = try!(self.parse_cmp_expr());
                let op = self.operator(tok);

                self.mk_node(BinOp {
                    lhs: lhs,
                    op:  op,
                    rhs: rhs,
                })
            }

        } else {
            // .
            lhs
        }))
    }

    // AddExpr = MulExpr [ ( '+' | '-' ) AddExpr ] .
    fn parse_add_expr<'x>(&'x mut self) -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x {
        // MulExpr
        let lhs = try!(self.parse_mul_expr());

        Ok(p_try!(tok in self {

            // ( '+' | '-' ) AddExpr .
            Sym("+"), Sym("-") => {
                let rhs = try!(self.parse_add_expr());
                let op = self.operator(tok);

                self.mk_node(BinOp {
                    lhs: lhs,
                    op:  op,
                    rhs: rhs,
                })
            }

        } else {
            // .
            lhs
        }))
    }

    // MulExpr = FnExpr [ ( '*' | '/' | '%' ) MulExpr ] .
    fn parse_mul_expr<'x>(&'x mut self) -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x {
        // FnExpr
        let lhs = try!(self.parse_fn_expr());

        Ok(p_try!(tok in self {

            // ( '*' | '/' | '%' ) MulExpr .
            Sym("*"), Sym("/"), Sym("%") => {
                let rhs = try!(self.parse_mul_expr());
                let op  = self.operator(tok);

                self.mk_node(BinOp {
                    lhs: lhs,
                    op:  op,
                    rhs: rhs,
                })
            }

        } else {
            // .
            lhs
        }))
    }

    // FnExpr = Factor [ FnExpr ] .
    fn parse_fn_expr(&mut self)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {

        let fun = try!(self.parse_factor_expr());

        Ok(p_peek!(tok in self {
            Sym("("), Id(..), Int(..) => {
                let arg = try!(self.parse_fn_expr());

                self.mk_node(FnCall {
                    fun: fun,
                    arg: arg,
                })
            }
        } else {
            fun
        }))
    }

    // Factor = '(' ExprList ')'
    //        | LetExpr
    //        | IfExpr
    //        | Ident
    //        | IntLit
    //        .
    fn parse_factor_expr<'x>(&'x mut self) -> error::Result<'ctx, &'ctx Node<'ctx>>
    where 'ctx: 'x {
        Ok(p_try!(tok in self {

            // Ident
            Id(..) => self.ident(tok),

            // IntLit
            Int(..)  => self.int_lit(tok),

            // '(' ExprList ')' .
            Sym("(") => {
                let mut elems = try!(self.parse_expr_list());
                p_match!(self,
                    if elems.len() > 0 { "`)' or `,'" } else { "`)' or expression" },
                    Sym(")"));

                if elems.len() == 1 {
                    elems.pop().unwrap()
                } else {
                    self.mk_node(Tuple { open_paren: tok.loc, elems: elems })
                }
            },

            Kw("some") => {
                let val = try!(self.parse_factor_expr()); 
                self.mk_node(OptCons {
                    kw: tok,
                    val: Some(val),
                })
            },

            Kw("nil") => self.mk_node(OptCons { kw: tok, val: None }),

            Kw("let") => try!(self.parse_let_expr(tok.loc)),

            Kw("if") => try!(self.parse_if_expr(tok.loc)),

            Sym("{") => try!(self.parse_block(tok.loc))

        } else {
            return expecting("`(', `if', `let', integer, or identifier", tok);
        }))
    }

    fn parse_block(&mut self, loc: source::Loc<'ctx>)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {
        let mut exprs = Vec::new();
        let mut unit  = true;

        while !p_delim!(self, Sym("}")) {
            exprs.push(try!(self.parse_expr()));
            if !p_delim!(self, Sym(";")) {
                unit = false;
                p_match!(self, "`}'", Sym("}"));
                break;
            }
        }

        Ok(self.mk_node(Block {
            open_curly: loc,
            exprs:       exprs,
            unit:        unit,
        }))
    }

    // This function expects that a `Kw("let")` token has just been consumed.
    //
    // LetExpr = 'let' Pattern '=' Expr 'in' Expr
    fn parse_let_expr(&mut self, loc: source::Loc<'ctx>)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {
        let pat = try!(self.parse_pattern());
        p_match!(self, "`='", Sym("="));
        let val = try!(self.parse_expr());
        let body = p_peek!(tok in self {
            Sym(";") => None
        } else {
            p_match!(self, "`in' or ';'", Kw("in"));
            Some(try!(self.parse_expr()))
        });

        Ok(self.mk_node(LetExpr {
            let_loc: loc,
            pat:     pat,
            val:     val,
            body:    body,
        }))
    }

    // Pattern = '(' { Pattern ',' } [ Pattern ] ')'
    //         | Ident
    //         .
    fn parse_pattern(&mut self)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {
        Ok(p_try!(tok in self {
            Sym("(") => {
                let mut pat_ls = Vec::new();

                while !p_delim!(self, Sym(")")) {
                    pat_ls.push(try!(self.parse_pattern()));
                    if !p_delim!(self, Sym(",")) { break }
                }

                p_match!(self, "pattern or `)'", Sym(")"));

                self.mk_node( TuplePattern {
                    open_paren: tok.loc,
                    elems:      pat_ls,
                })
            },

            Id(..) => self.ident(tok)
        } else {
            return expecting("pattern", tok);
        }))
    }

    // This function expects that a `Kw("if")` token has just been consumed.
    //
    // IfExpr = 'if' Expr '{' Expr '}' [ ElseExpr ] .
    //
    // ElseExpr = 'else' IfExpr
    //          | 'else' '{' Expr '}'
    //          .
    fn parse_if_expr(&mut self, loc: source::Loc<'ctx>)
    -> error::Result<'ctx, &'ctx Node<'ctx>>
    {
        let cond = try!(self.parse_expr());

        let curly = p_match!(self, "`{'", Sym("{"));
        let yes   = try!(self.parse_block(curly.loc));

        let no  = if p_delim!(self, Kw("else")) {
            Some(p_try!(tok in self {
                Kw("if") => try!(self.parse_if_expr(tok.loc)),
                Sym("{") => try!(self.parse_block(tok.loc))
            } else {
                return expecting("`if' or `{'", tok);
            }))
        } else { None };

        Ok(self.mk_node(IfExpr {
            if_loc: loc,
            cond:   cond,
            yes:    yes,
            no:     no,
        }))
    }

    // ExprList = { Expr ',' } [ Expr ]
    fn parse_expr_list<'x>(&'x mut self) -> error::Result<'ctx, Vec<&'ctx Node<'ctx>>>
    where 'ctx: 'x {
        let mut expr_ls = Vec::new();

        // This will only work if all ExprList's are inside of parens. If that changes, this
        // function will require repair.
        while !p_delim!(self, Sym(")")) {
            expr_ls.push(try!(self.parse_expr()));
            if !p_delim!(self, Sym(",")) { break }
        }

        Ok(expr_ls)
    }
}

pub fn parse<'ctx>(ctx: &'ctx compiler::Context<'ctx>) -> error::Result<'ctx, &'ctx Node<'ctx>> {

    let mut scanner = ctx.file.scan();
    let mut parser  = Parser {
        ctx:     ctx,
        next:    scanner.next(),
        scanner: scanner,
    };

    parser.parse_translation_unit()
}

fn expecting<'ctx, S, T>(exp: S, tok: Option<source::Token<'ctx>>)
-> error::Result<'ctx, T>
where String: From<S>
{
    if let Some(got) = tok {
        Err(error::Error::Syntax { exp: String::from(exp), got: got })
    } else {
        Err(error::Error::EOF { exp: String::from(exp) })
    }
}

/// A node in the syntax tree. This enum wraps each possible node type to allow for a sort of
/// reflection. The `Node` type provides all caching and annotation methods on top of the individual
/// methods, including ICE generation if the wrong operation is called on the wrong type.
pub enum Node<'ctx> {
    Ident(Ident<'ctx>),
    IntLit(IntLit<'ctx>),
    OptCons(OptCons<'ctx>),
    Operator(Operator<'ctx>),
    TranslationUnit(TranslationUnit<'ctx>),
    FnDecl(FnDecl<'ctx>),
    VarDecl(VarDecl<'ctx>),
    LogicOp(LogicOp<'ctx>),
    BinOp(BinOp<'ctx>),
    FnCall(FnCall<'ctx>),
    IfExpr(IfExpr<'ctx>),
    LetExpr(LetExpr<'ctx>),
    Tuple(Tuple<'ctx>),
    TuplePattern(TuplePattern<'ctx>),
    Block(Block<'ctx>),
}

/// A trait which must be provided by every kind of `Node`.
pub trait GenNode<'ctx> {
    fn loc(&self) -> source::Loc<'ctx>;
    fn text(&self) -> &'ctx str;
}

impl<'ctx> GenNode<'ctx> for Node<'ctx> {
    fn loc(&self)
    -> source::Loc<'ctx>
    {
        use self::Node::*;

        match self {
            &Ident(ref n)           => n.loc(),
            &IntLit(ref n)          => n.loc(),
            &OptCons(ref n)         => n.loc(),
            &Operator(ref n)        => n.loc(),
            &TranslationUnit(ref n) => n.loc(),
            &FnDecl(ref n)          => n.loc(),
            &VarDecl(ref n)         => n.loc(),
            &LogicOp(ref n)         => n.loc(),
            &BinOp(ref n)           => n.loc(),
            &FnCall(ref n)          => n.loc(),
            &IfExpr(ref n)          => n.loc(),
            &LetExpr(ref n)         => n.loc(),
            &Tuple(ref n)           => n.loc(),
            &TuplePattern(ref n)    => n.loc(),
            &Block(ref n)           => n.loc(),
        }
    }

    fn text(&self)
    -> &'ctx str
    {
        use self::Node::*;

        match self {
            &Ident(ref n)           => n.text(),
            &IntLit(ref n)          => n.text(),
            &OptCons(ref n)         => n.text(),
            &Operator(ref n)        => n.text(),
            &TranslationUnit(ref n) => n.text(),
            &FnDecl(ref n)          => n.text(),
            &VarDecl(ref n)         => n.text(),
            &LogicOp(ref n)         => n.text(),
            &BinOp(ref n)           => n.text(),
            &FnCall(ref n)          => n.text(),
            &IfExpr(ref n)          => n.text(),
            &LetExpr(ref n)         => n.text(),
            &Tuple(ref n)           => n.text(),
            &TuplePattern(ref n)    => n.text(),
            &Block(ref n)           => n.text(),
        }
    }
}

/// The show implementation for `Node` just gives the name of the underlying type. This is intended
/// to be used in debug messages only.
impl<'ctx> cats::Show for Node<'ctx> {
    fn len(&self)
    -> usize
    {
        use self::Node::*;

        match self {
            &Ident(..)           => cat_len!("Ident"),
            &IntLit(..)          => cat_len!("IntLit"),
            &OptCons(..)         => cat_len!("OptCons"),
            &Operator(..)        => cat_len!("Operator"),
            &TranslationUnit(..) => cat_len!("TranslationUnit"),
            &FnDecl(..)          => cat_len!("FnDecl"),
            &VarDecl(..)         => cat_len!("VarDecl"),
            &LogicOp(..)         => cat_len!("LogicOp"),
            &BinOp(..)           => cat_len!("BinOp"),
            &FnCall(..)          => cat_len!("FnCall"),
            &IfExpr(..)          => cat_len!("IfExpr"),
            &LetExpr(..)         => cat_len!("LetExpr"),
            &Tuple(..)           => cat_len!("Tuple"),
            &TuplePattern(..)    => cat_len!("TuplePattern"),
            &Block(..)           => cat_len!("Block"),
        }
    }

    fn write<W: io::Write>(&self, w: &mut W)
    -> io::Result<usize>
    {
        use self::Node::*;

        match self {
            &Ident(..)           => cat_write!(w, "Ident"),
            &IntLit(..)          => cat_write!(w, "IntLit"),
            &OptCons(..)         => cat_write!(w, "OptCons"),
            &Operator(..)        => cat_write!(w, "Operator"),
            &TranslationUnit(..) => cat_write!(w, "TranslationUnit"),
            &FnDecl(..)          => cat_write!(w, "FnDecl"),
            &VarDecl(..)         => cat_write!(w, "VarDecl"),
            &LogicOp(..)         => cat_write!(w, "LogicOp"),
            &BinOp(..)           => cat_write!(w, "BinOp"),
            &FnCall(..)          => cat_write!(w, "FnCall"),
            &IfExpr(..)          => cat_write!(w, "IfExpr"),
            &LetExpr(..)         => cat_write!(w, "LetExpr"),
            &Tuple(..)           => cat_write!(w, "Tuple"),
            &TuplePattern(..)    => cat_write!(w, "TuplePattern"),
            &Block(..)           => cat_write!(w, "Block"),
        }
    }
}

impl<'ctx> util::Tagged<'ctx> for Node<'ctx> {
    fn tag(&'ctx self)
    -> util::Tag<'ctx>
    {
        use self::Node::*;

        match self {
            &Ident(ref n)           => n.tag(),
            &IntLit(ref n)          => n.tag(),
            &OptCons(ref n)         => n.tag(),
            &Operator(ref n)        => n.tag(),
            &TranslationUnit(ref n) => n.tag(),
            &FnDecl(ref n)          => n.tag(),
            &VarDecl(ref n)         => n.tag(),
            &LogicOp(ref n)         => n.tag(),
            &BinOp(ref n)           => n.tag(),
            &FnCall(ref n)          => n.tag(),
            &IfExpr(ref n)          => n.tag(),
            &LetExpr(ref n)         => n.tag(),
            &Tuple(ref n)           => n.tag(),
            &TuplePattern(ref n)    => n.tag(),
            &Block(ref n)           => n.tag(),
        }
    }
}
