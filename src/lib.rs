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

#![feature(rustc_private)]

extern crate arena;

#[macro_use] extern crate cats;

/// Print `"Hello, World!"` to stdout.
pub fn say_hi() {
    println!("Hello, World!")
}

#[macro_use]
mod macros;

pub mod error;

pub mod expr;

pub mod pattern;

pub mod semantic;

/// Tokenization and location tracking for source files.
pub mod source;

pub mod syntax;

pub mod types;

pub mod util;

pub mod compiler {
    use expr;
    use semantic;
    use source;
    use syntax;
    use types;

    pub struct Context<'ctx> {
        pub file:   &'ctx source::File,
        pub syntax: syntax::Context<'ctx>,
        pub types:  types::Context<'ctx>,

        pub ty_map: expr::TypeMap<'ctx>,
        pub scopes: semantic::ScopeMap<'ctx>,
    }

    impl<'ctx> Context<'ctx> {
        pub fn new(f: &'ctx source::File) -> Context<'ctx> {
            Context {
                file:   f,
                syntax: syntax::Context::new(),
                types:  types::Context::new(),
                ty_map: expr::TypeMap::new(),
                scopes: semantic::ScopeMap::new(),
            }
        }
    }

    /// Convenience type to avoid writing the same typename twice.
    pub type CtxRef<'ctx> = &'ctx Context<'ctx>;
}
