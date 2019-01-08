# `tin` [![CircleCI](https://circleci.com/gh/dflemstr/tin.svg?style=svg)](https://circleci.com/gh/dflemstr/tin) [![codecov](https://codecov.io/gh/dflemstr/tin/branch/master/graph/badge.svg)](https://codecov.io/gh/dflemstr/tin) [![docs](https://docs.rs/tin/badge.svg)](https://docs.rs/tin)

`tin` is a new statically structurally typed programming language.  It's main purpose is to be embedded in other
programs (similar to e.g. Lua) but built to easily be compilable (initially JIT).

`tin` is what you need to create the solder for your application.  It's easy to build plugin APIs that are type safe
using `tin`.

Despite tin having the chemical symbol `Sn`, `tin` files conventionally use the `.tn` extension because it's easier to
remember the mnemonic.

Currently, the language is a work in progress.  The MVP will be a Rust library and executable called `tin` that include
a JIT compiler and rudimentary type inference.

[**Documentation**](https://docs.rs/tin)

## Example

`tin` is purely expression oriented; there are no types per se.  In order to define a type, you give an example value
for the type.  This is very similar to prototype-based type systems, except there is no support for inheritance.

```tin
/* Defines the Int type by giving an example value */
Int = 0i64;
/* Similarly for String */
String = "";

/* A Person is anything that has both a name of type String and an age of type Int
 * The definintion is identical to: { name: "", age: 0i64 }
 */
Person = { name: String, age: Int };
```

Everything is either an expression or a variable definition.  There are for example no functions; there are only lambdas
which can be assigned to variables:

```tin
getAge = |person: Person| Int { person.age };
```

`tin` supports structural polymorphic type inference:

```tin
getAge = |person| { person.age };

main = || {
  getAge({age: 3}); /* → returns 3 */
  getAge({age: "Hello"}); /* → returns "Hello" */
  getAge({name: "Hello"}) /* compile time error */
};
```

`tin` has several built-in types:

```tin
/* An unsigned 8-bit integer */
ExampleU8 = 0u8;
/* An unsigned 16-bit integer */
ExampleU16 = 0u16;
/* An unsigned 32-bit integer */
ExampleU32 = 0u32;
/* An unsigned 64-bit integer */
ExampleU64 = 0u64;

/* A signed 8-bit integer */
ExampleI8 = 0i8;
/* A signed 16-bit integer */
ExampleI16 = 0i16;
/* A signed 32-bit integer */
ExampleI32 = 0i32;
/* A signed 64-bit integer */
ExampleI64 = 0i64;

/* A 32 bit floating point number */
ExampleF32 = 0.0f32;
/* A 64 bit floating point number */
ExampleF64 = 0.0f64;

/* An UTF-8 string */
ExampleString = "";

/* A symbol value */
ExampleSymbol = :foo;

/* A tuple */
ExampleTuple = (String, I8, I8);
/* A record */
ExampleRecord = { name: String, x: I8, y: I8 };

/* A lambda */
ExampleLambda = |tuple: ExampleTuple, record: ExampleRecord, int: I8| I8 { int };
```
