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

extern crate sl;

use std::fs;
use std::env;
use std::path::PathBuf;

use sl::compiler;
use sl::source;
use sl::syntax;
use sl::semantic;

macro_rules! main_try {
    ($x:expr) => {
        match $x {
            Err(e) => {
                e.display();
                return;
            }
            Ok(v)  => v,
        }
    }
}

pub fn main() {
    #![allow(unused_variables)]

    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("\n   usage: slc source_file.sl\n");
        return
    }

    let mut path = PathBuf::from(&args[1][..]);

    let file = source::File::from_path(&path).unwrap();

    let ctx = compiler::Context::new(&file);

    let tu = main_try!(syntax::parse(&ctx));

    let global = semantic::Scope::new();
    
    // main_try!(tu.check(&ctx, &mut global));

    // main_try!(tu.emit(&ctx, &mut cbuf));

    path.set_extension("c");
    let c_file = fs::File::create(&path).unwrap();
}
