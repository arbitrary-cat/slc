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
use std::cmp;
use std::collections::HashMap;
use std::marker;
use std::mem;

/// A tag which refers to an object by an internal address. Used to uniquely identify items
/// allocated from the same arena.
#[derive(Copy,Clone,Hash,PartialEq,Eq)]
pub struct Tag<'ctx> {
    id:     usize,

    _lifetime: marker::PhantomData<&'ctx ()>
}

impl<'ctx> Tag<'ctx> {
    /// Create a new tag from a pointer.
    ///
    /// Lifetimes let us guarantee that these will be unique. In theory a struct and its first field
    /// could overlap, or other cases like that. *Don't do that!* You'll end up with non-unique IDs.
    pub fn new<T>(t: &'ctx T)
    -> Tag<'ctx>
    {
        Tag {
            id:        unsafe { mem::transmute::<&'ctx T, usize>(t) },
            _lifetime: marker::PhantomData,
        }
    }
}

impl<'ctx> Tagged<'ctx> for Tag<'ctx> {
    fn tag(&'ctx self)
    -> Tag<'ctx>
    where Self: Sized
    {
        self.clone()
    }
}

/// A trait for types whose addresses can be converted into a tag which is unique for that pointer's
/// lifetime.
pub trait Tagged<'ctx> {
    /// The default implementation of `tag` just converts the object's address into a tag directly.
    /// For an example where you'd want to use a different implementation, see `syntax::Node`'s
    /// (`syntax::Node` is the reason that the `Tag` type exists =P).
    fn tag(&'ctx self)
    -> Tag<'ctx>
    where Self: Sized
    {
        Tag {
            id:        unsafe { mem::transmute::<&'ctx Self, usize>(self) },
            _lifetime: marker::PhantomData,
        }
    }
}

/// A table which maps from `Tag`s to arbitrary objects. Used for annotating nodes in the syntax
/// tree.
pub struct TagMap<'ctx, T>
where T: Clone,
{
    map: RefCell<HashMap<Tag<'ctx>, T>>,
}

impl<'ctx, T> TagMap<'ctx, T>
where T: Clone,
{
    pub fn new()
    -> TagMap<'ctx, T>
    {
        TagMap { map: RefCell::new(HashMap::new()) }
    }

    /// Insert an element into the table. If there was already an element stored at that key, it
    /// will be replaced with the new value, and the old value will be returned.
    pub fn insert<K>(&self, key: &'ctx K, val: T)
    -> Option<T>
    where K: Tagged<'ctx>,
    {
        self.map.borrow_mut().insert(key.tag(), val)
    }

    pub fn get<K>(&self, key: &'ctx K)
    -> Option<T>
    where K: Tagged<'ctx>,
    {
        self.map.borrow().get(&key.tag()).cloned()
    }
}

// This number of bytes of space will be allocated at a time by StrArena.
const STR_MAX_CAP: usize = 2048;

/// An arena from which static strings can be allocated.
pub struct StrArena {
    // No String in this buffer can be allowed to reallocate, since we're handing out pointers into
    // them with the same lifetime as `self`.
    buffers: RefCell<Vec<String>>,
}

impl StrArena {
    pub fn new() -> StrArena {
        StrArena { buffers: RefCell::new(Vec::new()) }
    }

    pub fn alloc<'i, 'o>(&'o self, s: &'i str) -> &'o str {
        // This needs to be stored as a temporary so that we don't do nested `.borrow_mut()`s.
        let no_reallocs = self.buffers.borrow_mut().last_mut()
                        .and_then(|buf| {
                            if buf.capacity() - buf.len() < s.len() {
                                // If this string would overflow `buf`, then we need to allocate a new
                                // buffer instead.
                                None
                            } else {
                                let beg = buf.len();
                                buf.push_str(s);

                                Some(unsafe {
                                    // We know for a fact that buf will live as long as self, so we
                                    // can safely swap the lifetimes.
                                    mem::transmute::<&str, &'o str>(&buf[beg..])
                                })
                            }
                        });

        no_reallocs.unwrap_or_else(|| {
            self.buffers.borrow_mut()
                        .push(String::with_capacity(cmp::max(s.len(), STR_MAX_CAP)));

            self.alloc(s)
        })
    }
}
