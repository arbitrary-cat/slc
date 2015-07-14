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

use util;

use arena::TypedArena;
use cats;

#[derive(PartialEq,Eq,Hash,Clone)]
pub enum Type<'ctx> {
    Unit,
    Int,
    Bool,
    Opt(&'ctx Type<'ctx>),
    Func {
        from: &'ctx Type<'ctx>,
        to:   &'ctx Type<'ctx>,
    },
    Tuple {
        elems: Vec<&'ctx Type<'ctx>>,
    },
}

impl<'ctx> util::Tagged<'ctx> for Type<'ctx> {}

pub struct Context<'ctx> {
    mem:   TypedArena<Type<'ctx>>,
    dedup: RefCell<HashMap<&'ctx Type<'ctx>, &'ctx Type<'ctx>>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new() -> Context<'ctx> {
        Context {
            mem:   TypedArena::new(),
            dedup: RefCell::new(HashMap::new()),
        }
    }

    /// Return a reference to the unit type `()`.
    pub fn unit(&'ctx self) -> &'ctx Type<'ctx> {
        self.mk_type(Type::Unit)
    }

    /// Create a new type in the given context. This function will convert 1-element tuple types
    /// into scalars (since the language does not distinguish, but it's easier to encapsulate the
    /// special-case code here rather everywhere that might produce a 1-element tuple).
    pub fn mk_type(&'ctx self, mut t: Type<'ctx>) -> &'ctx Type<'ctx> {
        let actual = match t {
            Type::Tuple { ref mut elems } if (elems.len() == 1) => elems.pop().unwrap().clone(),
            Type::Tuple { ref mut elems } if (elems.is_empty()) => Type::Unit,
            _ => t,
        };

        let x = self.dedup.borrow().get(&actual).cloned();

        match x {
            Some(ty) => ty,
            None     => {
                let ty = self.mem.alloc(actual);
                self.dedup.borrow_mut().insert(ty, ty);
                ty
            }
        }
    }
}

impl<'ctx> cats::Show for Type<'ctx> {
    fn len(&self) -> usize {
        match self {
            &Type::Int                 => cat_len!("int"),
            &Type::Unit                => cat_len!("()"),
            &Type::Bool                => cat_len!("bool"),
            &Type::Opt(t)              => cat_len!("?", t),
            &Type::Func { from, to }   => cat_len!("func ", from, " -> ", to),
            &Type::Tuple { ref elems } => {
                let mut len = cat_len!("(");
                for (idx, &elem) in elems.iter().enumerate() {
                    len += cat_len!( if idx > 0 { ", " } else { "" }, elem);
                }
                len + cat_len!(")")
            }
        }
    }

    fn write<W: io::Write>(&self, w: &mut W) -> io::Result<usize> {
        match self {
            &Type::Int                 => cat_write!(w, "int"),
            &Type::Unit                => cat_write!(w, "()"),
            &Type::Bool                => cat_write!(w, "bool"),
            &Type::Opt(t)              => cat_write!(w, "?", t),
            &Type::Func { from, to }   => cat_write!(w, "func ", from, " -> ", to),
            &Type::Tuple { ref elems } => {
                try!(cat_write!(w, "("));
                for (idx, &elem) in elems.iter().enumerate() {
                    try!(cat_write!(w,  if idx > 0 { ", " } else { "" }, elem));
                }
                cat_write!(w, ")")
            }
        }
    }
}
