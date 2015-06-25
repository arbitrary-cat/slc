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
use std::marker;

/// A table which maps from pointers of one type to values of another. The pointers are compared by
/// address, not by the comparison methods of their underlying type.
///
/// This type is intended for use with key-objects allocated from the same `TypedArena`.
pub struct PtrMap<'ctx, K, V>
where V: Clone,
{
    map: RefCell<HashMap<*const K, V>>,

    _lifetime: marker::PhantomData<&'ctx ()>
}

impl<'ctx, K, V> PtrMap<'ctx, K, V>
where V: Clone,
{
    pub fn new()
    -> PtrMap<'ctx, K, V>
    {
        PtrMap {
            map: RefCell::new(HashMap::new()),
            _lifetime: marker::PhantomData,
        }
    }

    /// Insert an element into the table. If there was already an element stored at that key, it
    /// will be replaced with the new value, and the old value will be returned.
    pub fn insert(&self, key: &'ctx K, val: V)
    -> Option<V>
    {
        self.map.borrow_mut().insert(key as *const K, val)
    }

    pub fn get(&self, key: &'ctx K)
    -> Option<V>
    {
        self.map.borrow().get(&(key as *const K)).cloned()
    }
}
