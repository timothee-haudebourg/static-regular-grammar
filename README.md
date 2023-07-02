# Static Regular Grammars

[![Build](https://img.shields.io/github/actions/workflow/status/timothee-haudebourg/static-regular-grammar/ci.yml?branch=main&style=flat-square)](https://github.com/timothee-haudebourg/static-regular-grammar/actions)
[![Crate informations](https://img.shields.io/crates/v/static-regular-grammar.svg?style=flat-square)](https://crates.io/crates/static-regular-grammar)
[![License](https://img.shields.io/crates/l/static-regular-grammar.svg?style=flat-square)](https://github.com/timothee-haudebourg/static-regular-grammar#license)
[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/static-regular-grammar)

<!-- cargo-rdme start -->

This library provides the `RegularGrammar` macro that helps you create
unsized type wrapping byte or char strings validated by a regular grammar.
For now, only the ABNF grammar format is supported.

## Example

The grammar is specified by code blocks in the type documentation.

```rust
use static_regular_grammar::RegularGrammar;

/// Example grammar
///
/// ```abnf
/// foo = "f" 1*("oo") ; the first non-terminal is used as entry point.
/// ```
#[derive(RegularGrammar)]
pub struct Foo([u8]);

let foo = Foo::new(b"foooooo").unwrap();
```

<!-- cargo-rdme end -->
