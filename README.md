# Static Regular Grammars

[![Build](https://img.shields.io/github/actions/workflow/status/timothee-haudebourg/static-regular-grammar/ci.yml?branch=main&style=flat-square)](https://github.com/timothee-haudebourg/static-regular-grammar/actions)
[![Crate informations](https://img.shields.io/crates/v/static-regular-grammar.svg?style=flat-square)](https://crates.io/crates/static-regular-grammar)
[![License](https://img.shields.io/crates/l/static-regular-grammar.svg?style=flat-square)](https://github.com/timothee-haudebourg/static-regular-grammar#license)
[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/static-regular-grammar)

<!-- cargo-rdme start -->

This library provides the handy `RegularGrammar` derive macro that helps you
create unsized type wrapping byte or char strings validated by a regular
grammar. It works by parsing a grammar specified in the documentation of
your type, statically compiling it into a deterministic, minimal, regular
automaton then translated into a Rust validation function.

For now, only the [ABNF] grammar format is supported.

[ABNF]: <https://datatracker.ietf.org/doc/html/rfc5234>

## Basic Usage

The grammar is specified by code blocks in the type documentation.
The type itself must be a simple tutple struct with a single unnamed field
specifying the grammar "token string type". This token string type can be:
- `[u8]`: the grammar is defined on bytes.
- `str`: the grammar is defined on unicode characters.

### Example

```rust
use static_regular_grammar::RegularGrammar;

/// Example grammar.
///
/// ```abnf
/// foo = "f" 1*("oo") ; the first non-terminal is used as entry point.
/// ```
#[derive(RegularGrammar)]
pub struct Foo([u8]);

let foo = Foo::new(b"foooooo").unwrap();
```

## Sized Type

The `RegularGrammar` macro works on unsized type, but it is often useful
to have an sized equivalent that can own the data while still guaranteeing
the validity of the data. The derive macro can do that for you using the
`sized` attribute.

```rust
/// Example grammar, with sized variant.
///
/// ```abnf
/// foo = "f" 1*("oo")
/// ```
#[derive(RegularGrammar)]
#[sized(FooBuf)] // this will generate a `FooBuf` type.
pub struct Foo([u8]);

let foo = FooBuf::new(b"foooooo".to_vec()).unwrap();
```

The sized type will implement `Deref`, `Borrow` and `AsRef` to the unsized
type. It will also include a method named `as_unsized_type_name` (e.g.
`as_foo` in the example above) returning a reference to the unsized type.

### Common trait implementations

You can specify what common trait to automatically implement for the sized
type using the `derive` sub-attribute.

```rust
#[sized(FooBuf, derive(PartialEq, Eq))]
```

The supported traits are:
- `PartialEq`
- `Eq`
- `PartialOrd`
- `Ord`
- `Hash`

All will rely on an equivalent implementation for the unsized type.

## Caching

When compiled, the input grammar is determinized and minimized. Those are
expensive operation that can take several seconds on large grammars.
To avoid unnecessary work, the resulting automaton is stored on disk until
changes are made to the grammar. By default, the automaton will be stored
in the `target` folder, as `regular-grammar/TypeName.automaton.cbor`. For
instance, in the example above the path will be
`target/regular-grammar/Foo.automaton.cbor`.
You can specify the file path yourself using the `cache` attribute:

```rust
#[cache("path/to/cache.automaton.cbor")]
```

The path must be relative, and must not include `..` segments.
If you have multiple grammar types having the same name, use this attribute
to avoid conflicts, otherwise caching will not work.
For large grammars, it might be a good idea to cache the automaton directly
with the sources, and ship it with your library/application to reduce
compilation time on the user machine.

<!-- cargo-rdme end -->
