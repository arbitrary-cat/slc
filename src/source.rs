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

use cats;

use std::io::{self, Read, Write};
use std::fs;
use std::iter;
use std::path::Path;
use std::str;

pub struct File {
    name: String,
    src:  String,
}

impl File {
    pub fn from_path<P>(path: P) -> io::Result<File>
        where P: AsRef<Path> {

        let mut src = String::new();

        try!(fs::File::open(path.as_ref()).and_then(|mut f| f.read_to_string(&mut src)));

        Ok(File {
            name: format!("{}", path.as_ref().display()),
            src: src,
        })
    }

    pub fn new<N, S>(name: N, src: S) -> File
        where String: From<N> + From<S> {

        File {
            src:  String::from(src),
            name: String::from(name),
        }
    }

    pub fn start_loc<'src>(&'src self) -> Loc<'src> {
        Loc {
            file: self,
            line: 0,
            col:  0,
        }
    }

    pub fn scan<'src>(&'src self) -> Scanner<'src> {
        Scanner {
            src:    &self.src[..],
            loc:    Loc {
                file: self,
                line: 0,
                col:  0,
            },
            ch_ind: self.src.char_indices().peekable(),
        }
    }

    pub fn name(&self) -> &str { &self.name[..] }
}

/// A location in a source file.
#[derive(Clone,Copy)]
pub struct Loc<'src> {
    /// The file containing this location.
    pub file: &'src File,

    /// A zero-indexed line number.
    pub line: usize,

    /// A zero-indexed column number.
    pub col:  usize,
}

impl<'src> Loc<'src> {
    fn advance(&mut self, c: char) {
        match c {
            '\n' => {
                self.line += 1;
                self.col   = 0;
            },
            _    => self.col += 1,
        }
    }

    fn line(&self) -> &'src str {
        self.file.src.lines().nth(self.line).unwrap()
    }

    pub fn in_context(&self) -> String {
        scat!(self.line(), "\n", iter::repeat(' ').take(self.col).collect::<String>(), "^")
    }
}

impl<'src> cats::Show for Loc<'src> {
    fn len(&self) -> usize {
        cat_len!(self.file.name(), ": ln ", self.line + 1, ", col ", self.col + 1)
    }

    fn write<W: io::Write>(&self, w: &mut W) -> io::Result<usize> {
        cat_write!(w, self.file.name(), ": ln ", self.line + 1, ", col ", self.col + 1)
    }
}

/// The semantically relevant portion of a token.
#[derive(Copy,Clone,PartialEq,Eq,Debug)]
pub enum TokenData<'src> {
    /// An integer
    Int(u64),

    /// An identifier
    Id(&'src str),

    /// A reserved keyword.
    Kw(&'src str),

    /// A symbol (i.e. punctuation, delimiters, operators).
    Sym(&'src str),
}

impl<'src> cats::Show for TokenData<'src> {
    fn len(&self) -> usize {
        use self::TokenData::*;

        match *self {
            Int(n)   => cat_len!("integer ", n),
            Id(id)   => cat_len!("identifier `", id, '\''),
            Kw(kw)   => cat_len!("keyword `", kw, '\''),
            Sym(sym) => cat_len!("`", sym, '\''),
        }
    }

    fn write<W: io::Write>(&self, w: &mut W) -> io::Result<usize> {
        use self::TokenData::*;

        match *self {
            Int(n)   => cat_write!(w, "integer ", n),
            Id(id)   => cat_write!(w, "identifier `", id, '\''),
            Kw(kw)   => cat_write!(w, "keyword `", kw, '\''),
            Sym(sym) => cat_write!(w, "`", sym, '\''),
        }
    }
}

impl<'src> From<TokenData<'src>> for String {
    fn from(dat: TokenData<'src>) -> String { scat!(dat) }
}

/// A token of the source code.
#[derive(Copy,Clone)]
pub struct Token<'src> {
    /// The comment immediately preceding this token (if any). This field is currently not set, and
    /// will always be None.
    pub comment: Option<&'src str>,

    /// The semantic component.
    pub data: TokenData<'src>,

    /// The location of this token in the source.
    pub loc: Loc<'src>,
}

/// An iterator to the tokens of a source file.
pub struct Scanner<'src> {
    // Source string.
    src: &'src str,

    // Current location in the source.
    loc: Loc<'src>,

    // Iterator to characters and indices of the source.
    ch_ind: iter::Peekable<str::CharIndices<'src>>,
}

impl<'src> Iterator for Scanner<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Token<'src>> {
        let loc = self.loc;
        if let Some((idx, c)) = self.ch_ind.next() {
            self.scan(loc, idx, c)
        } else {
            None
        }
    }
}

impl<'src> Scanner<'src> {
    fn advance(&mut self, c: char) {
        self.loc.advance(c);
        self.ch_ind.next();
    }

    fn scan(&mut self, loc: Loc<'src>, beg: usize, start: char) -> Option<Token<'src>> {
        self.loc.advance(start);

        match start {
            ' ' | '\n'                  => self.next(),
            '0'...'9'                   => {
                Some(self.scan_int(loc, start))
            },
            'a'...'z' | 'A'...'Z' | '_' | '?' => {
                let x = Some(self.scan_ident(loc, beg, start));
                x
            },

            '(' | ')' | '{' | '}' | '+' | '-' | '*' |
            '/' | '%' | ',' | ':' | '>' | '<' | '=' |
            '!' | '|' | '&' => {
                self.scan_operator(loc, beg, start)
            }
            _                                 => unreachable!(),
        }
    }

    fn scan_int(&mut self, loc: Loc<'src>, start: char) -> Token<'src> {
        let mut n = start.to_digit(10).unwrap() as u64;

        while let Some(&(_, c)) = self.ch_ind.peek() {
            match c {
                '0'...'9' => {
                    self.advance(c);
                    n = n*10 + (c.to_digit(10).unwrap() as u64);
                },
                _ => break,
            }
        }


        Token {
            comment: None,
            data:    TokenData::Int(n),
            loc:     loc,
        }
    }

    fn scan_operator(&mut self, loc: Loc<'src>, beg: usize, start: char) -> Option<Token<'src>> {
        let end = match start {
            '/' => match self.ch_ind.peek() {
                Some(&(_, '/')) => {
                    for (_, c) in self.ch_ind.by_ref() {
                        if c == '\n' { break }
                    }

                    self.advance('\n');

                    // We've iterated through all remaining characters, this comment goes to EOF.
                    return None;
                },
                _                 => beg + start.len_utf8(),
            },
            '-' => match self.ch_ind.peek() {
                Some(&(idx, '>')) => {
                    self.advance('>');
                    idx + '>'.len_utf8()
                },
                _                 => beg + start.len_utf8(),
            },
            '<' | '>' | '!' | '=' => match self.ch_ind.peek() {
                Some(&(idx, '=')) => {
                    self.advance('=');
                    idx + '='.len_utf8()
                },
                _                 => beg + start.len_utf8(),
            },
            '|' => match self.ch_ind.peek() {
                Some(&(idx, '|')) => {
                    self.advance('|');
                    idx + '|'.len_utf8()
                },
                _                 => beg + start.len_utf8(),
            },
            '&' => match self.ch_ind.peek() {
                Some(&(idx, '&')) => {
                    self.advance('&');
                    idx + '&'.len_utf8()
                },
                _                 => beg + start.len_utf8(),
            },
            _  => beg + start.len_utf8(),
        };

        Some(Token {
            comment: None,
            data:    TokenData::Sym(&self.src[beg..end]),
            loc:     loc,
        })
    }

    fn scan_ident(&mut self, loc: Loc<'src>, beg: usize, start: char) -> Token<'src> {
        let mut end  = beg;
        let mut last = start;
        while let Some(&(idx, c)) = self.ch_ind.peek() {
            last = c;
            end  = idx;
            match c {
                'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '?' => self.advance(c),
                 _ => {
                    let txt = &self.src[beg..end];
                    return Token {
                        comment: None,
                        data:    match txt {
                            "fn" | "if" | "else" | "let" | "in" => TokenData::Kw(txt),
                             _   => TokenData::Id(txt),
                        },
                        loc:     loc,
                    }
                }
            }
        }
        end += last.len_utf8();

        let txt = &self.src[beg..end];

        Token {
            comment: None,
            data:    match txt {
                "fn" | "if" | "else" | "let" | "in" => TokenData::Kw(txt),
                 _   => TokenData::Id(txt),
            },
            loc:     loc,
        }
    }
}

#[cfg(test)] mod test {
    use super::*;
    use super::TokenData::*;

    fn exp_dat<'src>(exp: TokenData, tok: Option<Token<'src>>) {

        if let Some(Token { data: got, .. }) = tok {
            assert_eq!(exp, got);
        } else {
            panic!("Expecting {:?}, got None", exp);
        }
    }

    #[test] fn scan_ints() {
        let f = File::new("test-file.sl", "1 02 874 0 89013475");
        let mut it = f.scan();

        exp_dat(Int(1), it.next());
        exp_dat(Int(2), it.next());
        exp_dat(Int(874), it.next());
        exp_dat(Int(0), it.next());
        exp_dat(Int(89013475), it.next());

        assert!(it.next().is_none())
    }

    #[test] fn scan_keywords() {
        let f = File::new("test-file.sl", "fn fn fn");
        let mut it = f.scan();

        exp_dat(Kw("fn"), it.next());
        exp_dat(Kw("fn"), it.next());
        exp_dat(Kw("fn"), it.next());

        assert!(it.next().is_none())
    }

    #[test] fn scan_idents() {
        let f = File::new("test-file.sl", "a ab _123 HELLO _");
        let mut it = f.scan();

        exp_dat(Id("a"), it.next());
        exp_dat(Id("ab"), it.next());
        exp_dat(Id("_123"), it.next());
        exp_dat(Id("HELLO"), it.next());
        exp_dat(Id("_"), it.next());

        assert!(it.next().is_none())
    }

    #[test] fn scan_symbols() {
        let f = File::new("test-file.sl", "{}()+-/%*,");
        let mut it = f.scan();

        exp_dat(Sym("{"), it.next());
        exp_dat(Sym("}"), it.next());
        exp_dat(Sym("("), it.next());
        exp_dat(Sym(")"), it.next());
        exp_dat(Sym("+"), it.next());
        exp_dat(Sym("-"), it.next());
        exp_dat(Sym("/"), it.next());
        exp_dat(Sym("%"), it.next());
        exp_dat(Sym("*"), it.next());
        exp_dat(Sym(","), it.next());

        assert!(it.next().is_none())
    }

    #[test] fn scan_function() {
        let f = File::new("test-file.sl", "fn dot(a, b) { sqrt((a*a) + (b*b)) }");
        let mut it = f.scan();

        exp_dat(Kw("fn"), it.next());

        exp_dat(Id("dot"), it.next());

        exp_dat(Sym("("), it.next());
        exp_dat(Id("a"), it.next());
        exp_dat(Sym(","), it.next());
        exp_dat(Id("b"), it.next());
        exp_dat(Sym(")"), it.next());

        exp_dat(Sym("{"), it.next());
        exp_dat(Id("sqrt"), it.next());
        exp_dat(Sym("("), it.next());

        exp_dat(Sym("("), it.next());
        exp_dat(Id("a"), it.next());
        exp_dat(Sym("*"), it.next());
        exp_dat(Id("a"), it.next());
        exp_dat(Sym(")"), it.next());

        exp_dat(Sym("+"), it.next());

        exp_dat(Sym("("), it.next());
        exp_dat(Id("b"), it.next());
        exp_dat(Sym("*"), it.next());
        exp_dat(Id("b"), it.next());
        exp_dat(Sym(")"), it.next());

        exp_dat(Sym(")"), it.next());

        exp_dat(Sym("}"), it.next());

        assert!(it.next().is_none())
    }
}
