// Function declarations are kind of half-rust/half-go
fn add(x, y: int)
-> int // Return type is on the next line by custom, but the language is whitespace insensitive.
{
    // Function body is just an expression
    x + y
}

// Turn an integer into a 2-tuple of integers
fn twice(x: int)
-> (int, int)
{
    (x, x)
}

// Return double the value of an integer.
fn dbl(x: int)
-> int
{
    // `add` takes a 2-tuple of integers as an argument, and `twice` yields a 2-tuple.
    // This expression could also be written as `add (x, x)`. They are equivalent.
    add (twice x)
}

// Return the maximum of two integers.
fn max(x, y: int)
-> int
{
    // If statements are expressions, like in rust.
    if x > y { x } else { y }
}

// Recursion.
fn fib(n: int)
-> int
{
    if n > 1 {
        // Woo hoo!
        fib (n - 2) + fib (n - 1)
    } else {
        1
    }
}

// This is the upcoming syntax.
//
// This function has type `fn (fn int -> int, (int, int)) -> (int, int)`, if that's not clear.
fn map2(f: fn int -> int, (x, y: int))
-> (int, int)
{
    (f x, f y)
}

fn sqr2(x: int)
-> int
{
    (let xx = x*x in xx + xx) + 2
}

fn snd(t: (int, int, int))
-> int
{
    // `let` expressions can be used to destructure tuples.
    let (_, x, _) = t in x
}

// This function now generates the correct C code!
fn dbl_snd(x, y, z: int)
-> int
{
    let res = snd (x, y, z);
    dbl res
}

fn over_six(x: int)
-> ? int {
    if x > 6 {
        some (x - 6)
    } else {
        nil
    }
}
