//! This library provides the handy `RegularGrammar` derive macro that helps you
//! create unsized type wrapping byte or char strings validated by a regular
//! grammar. It works by parsing a grammar specified in a file or the
//! documentation of your type, statically compiling it into a deterministic,
//! minimal, regular automaton then translated into a Rust validation function.
//!
//! For now, only the [ABNF] grammar format is supported.
//!
//! [ABNF]: <https://datatracker.ietf.org/doc/html/rfc5234>
//!
//! # Basic Usage
//!
//! The grammar is specified by code blocks in the type documentation.
//! The type itself must be a simple tutple struct with a single unnamed field
//! specifying the grammar "token string type". This token string type can be:
//! - `[u8]`: the grammar is defined on bytes.
//! - `str`: the grammar is defined on unicode characters.
//!
//! ## Example
//!
//! ```
//! use static_regular_grammar::RegularGrammar;
//!
//! /// Example grammar.
//! ///
//! /// ```abnf
//! /// foo = "f" 1*("oo") ; the first non-terminal is used as entry point.
//! /// ```
//! #[derive(RegularGrammar)]
//! pub struct Foo([u8]);
//!
//! let foo = Foo::new(b"foooooo").unwrap();
//! ```
//!
//! The derive macro also provides a `grammar` attribute to configure the
//! grammar and the generated code. With this attribute, instead of using the
//! documentation, you can specify a path to a file containing the grammar:
//!
//! ```
//! # use static_regular_grammar::RegularGrammar;
//! /// Example grammar.
//! #[derive(RegularGrammar)]
//! #[grammar(file = "examples/test.abnf")]
//! pub struct Foo([u8]);
//!
//! let foo = Foo::new(b"foooooo").unwrap();
//! ```
//!
//! # Grammar Entry Point
//!
//! By default the first non-terminal defined in the grammar is used as entry
//! point. You can specify a different entry point using the `entry_point`
//! sub-attribute of the `grammar` attribute:
//!
//! ```
//! # use static_regular_grammar::RegularGrammar;
//! /// Example grammar.
//! #[derive(RegularGrammar)]
//! #[grammar(file = "examples/test.abnf", entry_point = "bar")]
//! pub struct Bar([u8]);
//!
//! let bar = Bar::new(b"baaaar").unwrap();
//! ```
//!
//! # ASCII
//!
//! Using the `[u8]` token string type, it is possible to specify that the
//! value can be interpreted as an ASCII text string. Then the resulting type
//! will implement `Display`, `Deref<Target=str>`, `AsRef<str>`, ect.
//! ```
//! # use static_regular_grammar::RegularGrammar;
//! #[derive(RegularGrammar)]
//! #[grammar(file = "examples/test.abnf", ascii)]
//! pub struct Bar([u8]);
//!
//! let bar = Bar::new(b"baaaar").unwrap();
//! println!("{bar}");
//! ```
//!
//! # Sized Type
//!
//! The `RegularGrammar` macro works on unsized type, but it is often useful
//! to have an sized equivalent that can own the data while still guaranteeing
//! the validity of the data. The derive macro can do that for you using the
//! `sized` sub-attribute of the `grammar` attribute.
//!
//! ```
//! # use static_regular_grammar::RegularGrammar;
//! /// Example grammar, with sized variant.
//! ///
//! /// ```abnf
//! /// foo = "f" 1*("oo")
//! /// ```
//! #[derive(RegularGrammar)]
//! #[grammar(sized(FooBuf))] // this will generate a `FooBuf` type.
//! pub struct Foo([u8]);
//!
//! let foo = FooBuf::new(b"foooooo".to_vec()).unwrap();
//! ```
//!
//! The sized type will implement `Deref`, `Borrow` and `AsRef` to the unsized
//! type. It will also include a method named `as_unsized_type_name` (e.g.
//! `as_foo` in the example above) returning a reference to the unsized type.
//!
//! ## Common trait implementations
//!
//! You can specify what common trait to automatically implement for the sized
//! type using the `derive` sub-attribute.
//!
//! ```ignore
//! #[grammar(sized(FooBuf, derive(PartialEq, Eq)))]
//! ```
//!
//! The supported traits are:
//! - `Debug`
//! - `Display`
//! - `PartialEq`
//! - `Eq`
//! - `PartialOrd`
//! - `Ord`
//! - `Hash`
//!
//! All will rely on an equivalent implementation for the unsized type.
//!
//! # Caching
//!
//! When compiled, the input grammar is determinized and minimized. Those are
//! expensive operation that can take several seconds on large grammars.
//! To avoid unnecessary work, the resulting automaton is stored on disk until
//! changes are made to the grammar. By default, the automaton will be stored
//! in the `target` folder, as `regular-grammar/TypeName.automaton.cbor`. For
//! instance, in the example above the path will be
//! `target/regular-grammar/Foo.automaton.cbor`.
//! You can specify the file path yourself using the `cache` sub-attribute:
//!
//! ```ignore
//! #[grammar(cache = "path/to/cache.automaton.cbor")]
//! ```
//!
//! The path must be relative, and must not include `..` segments.
//! If you have multiple grammar types having the same name, use this attribute
//! to avoid conflicts, otherwise caching will not work.
//! For large grammars, it might be a good idea to cache the automaton directly
//! with the sources, and ship it with your library/application to reduce
//! compilation time on the user machine.
//!
//! # Disable automaton generation
//!
//! When using a linter such as [`rust-analyzer`], it may be too expensive to
//! regenerate the grammar automaton continually, even with caching. On large
//! grammars the generated automaton code can span hundreds or even thousands
//! of lines. In that case it is possible to disable the automaton generation
//! all together using the `disable` option:
//! ```ignore
//! #[grammar(disable)]
//! ```
//!
//! Of course it is best to use this option behind a feature used only by the
//! linter:
//! ```ignore
//! #[cfg_attr(feature = "disable-grammars", grammar(disable))]
//! ```
//!
//! [`rust-analyzer`](https://rust-analyzer.github.io/)
use indoc::formatdoc;
use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};

mod attribute;
mod byteset;
mod charset;
mod grammar;
mod options;
mod token;
mod utils;

use attribute::Attribute;
use byteset::ByteSet;
use charset::CharSet;
use grammar::{extract_grammar, Grammar, GrammarError};
use options::*;
use token::{Token, TokenSet};
use utils::{automaton::DetAutomaton, SnakeCase};

#[proc_macro_derive(RegularGrammar, attributes(grammar))]
#[proc_macro_error]
pub fn derive_regular_grammar(input_tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(input_tokens as DeriveInput);
	match generate_regular_grammar(input) {
		Ok(tokens) => tokens.into(),
		Err((e, span)) => abort!(span, e),
	}
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum Error {
	#[error("unexpected type parameter")]
	UnexpectedTypeParameter,

	#[error("unexpected union type")]
	UnexpectedUnion,

	#[error("unexpected enum type")]
	UnexpectedEnum,

	#[error("unexpected unit structure")]
	UnexpectedUnitStruct,

	#[error("unexpected nammed fields")]
	UnexpectedNamedFields,

	#[error("missing unnamed field")]
	MissingField,

	#[error("unexpected unnamed field")]
	UnexpectedUnnamedField,

	#[error("unsafe visibility")]
	UnsafeVisibility,

	#[error("invalid inner type")]
	InvalidInnerType,

	#[error("`grammar` attribute error: {0}")]
	Attribute(#[from] attribute::Error),

	#[error(transparent)]
	Grammar(GrammarError),

	#[error("target directory not found: {0}")]
	TargetDirNotFound(std::env::VarError),

	#[error("missing sized type identifier")]
	MissingSizedTypeIdent,

	#[error("grammar is not ASCII")]
	NotAscii,
}

enum TokenType {
	Bytes,
	String,
}

fn is_str_path(path: &syn::Path) -> bool {
	path.is_ident("str")
}

fn is_u8_path(path: &syn::Path) -> bool {
	path.is_ident("u8")
}

impl TokenType {
	fn from_type(ty: syn::Type) -> Result<Self, (Error, Span)> {
		match ty {
			syn::Type::Slice(ty) => Self::from_array_element_type(*ty.elem),
			syn::Type::Path(ty) if is_str_path(&ty.path) => Ok(Self::String),
			ty => Err((Error::InvalidInnerType, ty.span())),
		}
	}

	fn from_array_element_type(ty: syn::Type) -> Result<Self, (Error, Span)> {
		match ty {
			syn::Type::Path(ty) if is_u8_path(&ty.path) => Ok(Self::Bytes),
			ty => Err((Error::InvalidInnerType, ty.span())),
		}
	}
}

struct GrammarData {
	vis: syn::Visibility,
	ident: Ident,
	token: TokenType,
	options: Options,
}

fn extract_grammar_data(
	input: DeriveInput,
) -> Result<(GrammarData, Vec<syn::Attribute>), (Error, Span)> {
	match input.data {
		Data::Struct(s) => match s.fields {
			syn::Fields::Unit => Err((Error::UnexpectedUnitStruct, s.fields.span())),
			syn::Fields::Named(_) => Err((Error::UnexpectedNamedFields, s.fields.span())),
			syn::Fields::Unnamed(fields) => {
				let fields_span = fields.span();
				let mut iter = fields.unnamed.into_iter();

				match iter.next() {
					Some(field) => match iter.next() {
						Some(f) => Err((Error::UnexpectedUnnamedField, f.span())),
						None => match field.vis {
							syn::Visibility::Inherited => {
								let mut grammar_attr = Attribute::default();
								let mut attrs_rest = Vec::with_capacity(input.attrs.len());
								for attr in input.attrs {
									if attr.meta.path().is_ident("grammar") {
										grammar_attr
											.append(Attribute::parse(attr).map_err(
												|(e, span)| (Error::Attribute(e), span),
											)?);
									} else if attr.meta.path().is_ident("cfg_attr") {
										// This only happens inside `rustanalyzer`.
										// See: https://github.com/rust-lang/rust-analyzer/issues/13360
										// See: https://github.com/rust-lang/rust-analyzer/issues/13436
										// Just to be sure, we disable the grammar.
										grammar_attr.disable = true
									} else {
										attrs_rest.push(attr);
									}
								}

								let options = Options::from_attribute(&input.ident, grammar_attr)?;

								Ok((
									GrammarData {
										vis: input.vis,
										ident: input.ident,
										token: TokenType::from_type(field.ty)?,
										options,
									},
									attrs_rest,
								))
							}
							vis => Err((Error::UnsafeVisibility, vis.span())),
						},
					},
					None => Err((Error::MissingField, fields_span)),
				}
			}
		},
		Data::Union(u) => Err((Error::UnexpectedUnion, u.union_token.span())),
		Data::Enum(e) => Err((Error::UnexpectedEnum, e.enum_token.span())),
	}
}

fn generate_regular_grammar(input: DeriveInput) -> Result<TokenStream, (Error, Span)> {
	match input.generics.params.iter().next() {
		Some(param) => Err((Error::UnexpectedTypeParameter, param.span())),
		None => {
			let (data, attrs) = extract_grammar_data(input)?;
			match data.token {
				TokenType::Bytes => generate_typed::<u8>(data, attrs),
				TokenType::String => generate_typed::<char>(data, attrs),
			}
		}
	}
}

fn generate_typed<T: Token>(
	data: GrammarData,
	attrs: Vec<syn::Attribute>,
) -> Result<TokenStream, (Error, Span)> {
	let automaton = if data.options.disable {
		let mut aut = DetAutomaton::new(0);
		aut.declare_state(0);
		aut.add_final_state(0);
		aut
	} else {
		let (grammar, hash) = extract_grammar::<T>(
			&data.options.cache_path,
			data.options.file.as_deref(),
			data.options.entry_point.as_deref(),
			attrs,
		)
		.map_err(|(e, span)| (Error::Grammar(e), span))?;
		let cached = grammar.is_cached();
		let automaton = grammar.build_automaton();

		if !cached {
			if let Err(e) = Grammar::<T>::save_to_file(&data.options.cache_path, hash, &automaton) {
				eprintln!("unable to cache regular automaton: {e}")
			}
		}

		automaton
	};

	let contains_empty = automaton.contains_empty();

	let vis = data.vis;
	let ident = data.ident;
	let ascii = data.options.ascii && T::is_ascii(&automaton);
	let name = data
		.options
		.name
		.or(data.options.entry_point)
		.unwrap_or_else(|| ident.to_string());

	if data.options.ascii && !ascii {
		return Err((Error::NotAscii, Span::call_site()));
	}

	let as_ref = format_ident!("as_{}", SnakeCase(&ident.to_string()));

	let token_type = T::rust_type();
	let string_type = T::rust_string_type();
	let iterator_method = T::rust_iterator_method();

	let as_inner = T::rust_as_inner_method();

	let error = format_ident!("Invalid{}", ident);
	let error_msg = format!("Invalid {name} `{{0}}`");

	let new_doc = format!("Creates a new {name} by parsing the `input` value");
	let new_unchecked_doc = formatdoc!(
		r#"
        Creates a new {name} from the `input` value without validation.
        
        # Safety
        
        The input data *must* be a valid {name}."#
	);

	let validate_doc = format!("Checks that the input iterator produces a valid {name}");
	let validate_body = if data.options.disable {
		quote! {
			panic!("automaton not generated")
		}
	} else {
		generate_validation_function::<T>(&automaton)
	};

	let mut tokens = quote! {
		#[derive(Debug)]
		#vis struct #error<T>(pub T);

		impl<T: ::core::fmt::Display> ::core::fmt::Display for #error<T> {
			fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
				write!(f, #error_msg, self.0)
			}
		}

		impl<T: ::core::fmt::Debug + ::core::fmt::Display> ::std::error::Error for #error<T> {}

		impl #ident {
			#[doc = #new_doc]
			pub fn new<T: ?Sized + AsRef<#string_type>>(input: &T) -> Result<&Self, #error<&T>> {
				let input_ref = input.as_ref();
				if Self::validate(input_ref.#iterator_method) {
					Ok(unsafe {
						Self::new_unchecked(input_ref)
					})
				} else {
					Err(#error(input))
				}
			}

			#[doc = #new_unchecked_doc]
			pub const unsafe fn new_unchecked(input: &#string_type) -> &Self {
				::core::mem::transmute(input)
			}

			#[doc = #validate_doc]
			#[allow(unreachable_code)]
			pub fn validate(mut input: impl Iterator<Item = #token_type>) -> bool {
				#validate_body
			}
		}

		impl AsRef<#ident> for #ident {
			fn as_ref(&self) -> &#ident {
				self
			}
		}

		impl<'a> TryFrom<&'a #string_type> for &'a #ident {
			type Error = #error<&'a #string_type>;

			fn try_from(input: &'a #string_type) -> Result<&'a #ident, #error<&'a #string_type>> {
				#ident::new(input)
			}
		}

		impl<'a> From<&'a #ident> for &'a #string_type {
			fn from(value: &'a #ident) -> &'a #string_type {
				&value.0
			}
		}
	};

	if contains_empty {
		let empty_string = T::rust_empty_string();
		tokens.extend(quote! {
			impl #ident {
				pub const EMPTY: &'static Self = unsafe {
					Self::new_unchecked(#empty_string)
				};
			}
		})
	}

	if !data.options.no_borrow {
		let as_bytes = T::rust_inner_as_bytes_method().map(|as_bytes| {
			quote! {
				pub fn as_bytes(&self) -> &[u8] {
					self.0.#as_bytes()
				}
			}
		});

		let borrow_bytes = T::rust_inner_as_bytes_method().map(|as_bytes| {
			quote! {
				impl ::core::convert::AsRef<[u8]> for #ident {
					fn as_ref(&self) -> &[u8] {
						self.0.#as_bytes()
					}
				}
			}
		});

		tokens.extend(quote! {
			impl #ident {
				pub fn #as_inner(&self) -> &#string_type {
					&self.0
				}

				#as_bytes
			}

			impl ::core::borrow::Borrow<#string_type> for #ident {
				fn borrow(&self) -> &#string_type {
					&self.0
				}
			}

			impl ::core::convert::AsRef<#string_type> for #ident {
				fn as_ref(&self) -> &#string_type {
					&self.0
				}
			}

			#borrow_bytes
		});

		if ascii {
			if let Some(as_ascii) = T::rust_inner_as_ascii_method_body() {
				tokens.extend(quote! {
					impl #ident {
						pub fn as_str(&self) -> &str {
							#as_ascii
						}
					}

					impl ::core::convert::AsRef<str> for #ident {
						fn as_ref(&self) -> &str {
							#as_ascii
						}
					}

					impl<'a> TryFrom<&'a str> for &'a #ident {
						type Error = #error<&'a str>;

						fn try_from(input: &'a str) -> Result<&'a #ident, #error<&'a str>> {
							#ident::new(input.as_bytes()).map_err(|_| #error(input))
						}
					}

					impl<'a> From<&'a #ident> for &'a str {
						fn from(value: &'a #ident) -> &'a str {
							value.as_str()
						}
					}
				})
			}
		}
	}

	if !data.options.no_deref {
		match T::rust_inner_as_ascii_method_body() {
			Some(as_ascii) if ascii => tokens.extend(quote! {
				impl ::core::ops::Deref for #ident {
					type Target = str;

					fn deref(&self) -> &str {
						#as_ascii
					}
				}
			}),
			_ => tokens.extend(quote! {
				impl ::core::ops::Deref for #ident {
					type Target = #string_type;

					fn deref(&self) -> &#string_type {
						&self.0
					}
				}
			}),
		}
	}

	if T::UNICODE || ascii {
		tokens.extend(quote! {
			impl ::core::fmt::Display for #ident {
				fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
					::core::fmt::Display::fmt(self.as_str(), f)
				}
			}

			impl ::core::fmt::Debug for #ident {
				fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
					::core::fmt::Debug::fmt(self.as_str(), f)
				}
			}
		});
	}

	if data.options.serde {
		let serialize = if T::UNICODE || ascii {
			quote! {
				serializer.serialize_str(self.as_str())
			}
		} else {
			quote! {
				serializer.serialize_bytes(self.as_bytes())
			}
		};

		let visit_bytes = if T::UNICODE {
			quote! {
				match std::str::from_utf8(v) {
					Ok(s) => #ident::new(s).map_err(|_| ()),
					Err(e) => Err(())
				}
			}
		} else {
			quote! {
				#ident::new(v)
			}
		};

		let expected = format!("some {ident}");

		tokens.extend(quote! {
			impl ::serde::Serialize for #ident {
				fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
				where
					S: ::serde::Serializer
				{
					#serialize
				}
			}

			impl<'a, 'de: 'a> ::serde::Deserialize<'de> for &'a #ident {
				fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
				where
					D: ::serde::Deserializer<'de>
				{
					struct Visitor;

					impl<'de> ::serde::de::Visitor<'de> for Visitor {
						type Value = &'de #ident;

						fn expecting(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
							write!(f, #expected)
						}

						fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
						where
							E: ::serde::de::Error
						{
							#ident::new(v).map_err(|_| E::invalid_value(::serde::de::Unexpected::Str(v), &self))
						}

						fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Self::Value, E>
						where
							E: ::serde::de::Error
						{
							#visit_bytes.map_err(|_| E::invalid_value(::serde::de::Unexpected::Bytes(v), &self))
						}
					}

					deserializer.deserialize_str(Visitor)
				}
			}
		})
	}

	if let Some(buffer) = data.options.sized {
		let buffer_ident = buffer.ident;
		let owned_string_type = T::rust_owned_string_type();

		let owned_doc = format!("Owned {name}.");
		let owned_new_doc = format!("Creates a new owned {name} by parsing the `input` value");
		let owned_new_unchecked_doc = formatdoc!(
			r#"
			Creates a new owned {name} from the `input` value without validation.
			
			# Safety
			
			The input data *must* be a valid {name}."#
		);

		tokens.extend(quote! {
			#[doc = #owned_doc]
			#[derive(Clone)]
			#vis struct #buffer_ident(#owned_string_type);

			impl #buffer_ident {
				#[doc = #owned_new_doc]
				pub fn new(input: #owned_string_type) -> Result<Self, #error<#owned_string_type>> {
					if #ident::validate(input.#iterator_method) {
						Ok(Self(input))
					} else {
						Err(#error(input))
					}
				}

				#[doc = #owned_new_unchecked_doc]
				pub const unsafe fn new_unchecked(input: #owned_string_type) -> Self {
					Self(input)
				}

				pub fn #as_ref(&self) -> &#ident {
					unsafe {
						#ident::new_unchecked(&self.0)
					}
				}
			}

			impl ::core::ops::Deref for #buffer_ident {
				type Target = #ident;

				fn deref(&self) -> &Self::Target {
					self.#as_ref()
				}
			}

			impl ::core::borrow::Borrow<#ident> for #buffer_ident {
				fn borrow(&self) -> &#ident {
					self.#as_ref()
				}
			}

			impl ::core::convert::AsRef<#ident> for #buffer_ident {
				fn as_ref(&self) -> &#ident {
					self.#as_ref()
				}
			}

			impl ::std::borrow::ToOwned for #ident {
				type Owned = #buffer_ident;

				fn to_owned(&self) -> #buffer_ident {
					unsafe {
						#buffer_ident::new_unchecked(
							self.0.to_owned()
						)
					}
				}
			}

			impl TryFrom<#owned_string_type> for #buffer_ident {
				type Error = #error<#owned_string_type>;

				fn try_from(input: #owned_string_type) -> Result<#buffer_ident, #error<#owned_string_type>> {
					#buffer_ident::new(input)
				}
			}

			impl From<#buffer_ident> for #owned_string_type {
				fn from(value: #buffer_ident) -> #owned_string_type {
					value.0
				}
			}
		});

		if !T::UNICODE && ascii {
			tokens.extend(quote! {
				impl #buffer_ident {
					pub fn into_string(self) -> ::std::string::String {
						unsafe {
							::std::string::String::from_utf8_unchecked(self.0)
						}
					}
				}

				impl TryFrom<::std::string::String> for #buffer_ident {
					type Error = #error<::std::string::String>;

					fn try_from(input: ::std::string::String) -> Result<#buffer_ident, #error<::std::string::String>> {
						let bytes = input.into_bytes();
						#buffer_ident::new(bytes).map_err(|#error(bytes)| unsafe {
							#error(::std::string::String::from_utf8_unchecked(bytes))
						})
					}
				}

				impl From<#buffer_ident> for String {
					fn from(value: #buffer_ident) -> String {
						value.into_string()
					}
				}
			})
		}

		if T::UNICODE || ascii {
			tokens.extend(quote! {
				impl ::std::str::FromStr for #buffer_ident {
					type Err = #error<::std::string::String>;

					fn from_str(s: &str) -> Result<Self, #error<::std::string::String>> {
						let buffer = s.to_string();
						buffer.try_into()
					}
				}
			})
		}

		if contains_empty {
			tokens.extend(quote! {
				impl ::core::default::Default for #buffer_ident {
					fn default() -> Self {
						unsafe {
							Self::new_unchecked(::core::default::Default::default())
						}
					}
				}
			})
		}

		if !data.options.no_borrow {
			let as_bytes = T::rust_inner_as_bytes_method().map(|as_bytes| {
				quote! {
					pub fn as_bytes(&self) -> &[u8] {
						self.0.#as_bytes()
					}
				}
			});

			let into_bytes = T::rust_inner_into_bytes_method().map(|into_bytes| {
				quote! {
					pub fn into_bytes(self) -> Vec<u8> {
						self.0.#into_bytes()
					}
				}
			});

			let borrow_bytes = T::rust_inner_as_bytes_method().map(|as_bytes| {
				quote! {
					impl ::core::borrow::Borrow<[u8]> for #buffer_ident {
						fn borrow(&self) -> &[u8] {
							self.0.#as_bytes()
						}
					}

					impl ::core::convert::AsRef<[u8]> for #buffer_ident {
						fn as_ref(&self) -> &[u8] {
							self.0.#as_bytes()
						}
					}
				}
			});

			let borrow_ascii = T::rust_inner_as_ascii_method_body().map(|as_ascii| {
				quote! {
					impl ::core::borrow::Borrow<str> for #buffer_ident {
						fn borrow(&self) -> &str {
							#as_ascii
						}
					}

					impl ::core::convert::AsRef<str> for #buffer_ident {
						fn as_ref(&self) -> &str {
							#as_ascii
						}
					}
				}
			});

			let into_inner = T::rust_into_inner_method();

			tokens.extend(quote! {
				impl #buffer_ident {
					pub fn #as_inner(&self) -> &#string_type {
						&self.0
					}

					pub fn #into_inner(self) -> #owned_string_type {
						self.0
					}

					#as_bytes
					#into_bytes
				}

				impl ::core::borrow::Borrow<#string_type> for #buffer_ident {
					fn borrow(&self) -> &#string_type {
						&self.0
					}
				}

				impl ::core::convert::AsRef<#string_type> for #buffer_ident {
					fn as_ref(&self) -> &#string_type {
						&self.0
					}
				}

				#borrow_bytes
				#borrow_ascii
			});
		}

		if buffer.derives.debug {
			tokens.extend(quote! {
				impl ::core::fmt::Debug for #buffer_ident {
					fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
						<#ident as ::core::fmt::Debug>::fmt(self.#as_ref(), f)
					}
				}
			});
		}

		if buffer.derives.display {
			tokens.extend(quote! {
				impl ::core::fmt::Display for #buffer_ident {
					fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
						<#ident as ::core::fmt::Display>::fmt(self.#as_ref(), f)
					}
				}
			});
		}

		if buffer.derives.partial_eq {
			tokens.extend(quote! {
				impl ::core::cmp::PartialEq for #buffer_ident {
					fn eq(&self, other: &Self) -> bool {
						<#ident as ::core::cmp::PartialEq>::eq(self.#as_ref(), other.#as_ref())
					}
				}

				impl ::core::cmp::PartialEq<#ident> for #buffer_ident {
					fn eq(&self, other: &#ident) -> bool {
						<#ident as ::core::cmp::PartialEq>::eq(self.#as_ref(), other)
					}
				}

				impl<'a> ::core::cmp::PartialEq<&'a #ident> for #buffer_ident {
					fn eq(&self, other: &&'a #ident) -> bool {
						<#ident as ::core::cmp::PartialEq>::eq(self.#as_ref(), *other)
					}
				}
			});
		}

		if buffer.derives.eq {
			tokens.extend(quote! {
				impl ::core::cmp::Eq for #buffer_ident {}
			});
		}

		if buffer.derives.partial_ord {
			if buffer.derives.ord {
				tokens.extend(quote! {
					impl ::core::cmp::Ord for #buffer_ident {
						fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
							<#ident as ::core::cmp::Ord>::cmp(self.#as_ref(), other.#as_ref())
						}
					}

					impl ::core::cmp::PartialOrd for #buffer_ident {
						fn partial_cmp(&self, other: &Self) -> Option<::core::cmp::Ordering> {
							Some(<#buffer_ident as ::core::cmp::Ord>::cmp(self, other))
						}
					}
				});
			} else {
				tokens.extend(quote! {
					impl ::core::cmp::PartialOrd for #buffer_ident {
						fn partial_cmp(&self, other: &Self) -> Option<::core::cmp::Ordering> {
							<#ident as ::core::cmp::PartialOrd>::partial_cmp(self.#as_ref(), other.#as_ref())
						}
					}
				});
			}

			tokens.extend(quote! {
				impl ::core::cmp::PartialOrd<#ident> for #buffer_ident {
					fn partial_cmp(&self, other: &#ident) -> Option<::core::cmp::Ordering> {
						<#ident as ::core::cmp::PartialOrd>::partial_cmp(self.#as_ref(), other)
					}
				}

				impl<'a> ::core::cmp::PartialOrd<&'a #ident> for #buffer_ident {
					fn partial_cmp(&self, other: &&'a #ident) -> Option<::core::cmp::Ordering> {
						<#ident as ::core::cmp::PartialOrd>::partial_cmp(self.#as_ref(), *other)
					}
				}
			});

			if buffer.derives.hash {
				tokens.extend(quote! {
					impl ::core::hash::Hash for #buffer_ident {
						fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
							<#ident as ::core::hash::Hash>::hash(self.#as_ref(), state)
						}
					}
				});
			}
		}

		if data.options.serde {
			let serialize = if T::UNICODE || ascii {
				quote! {
					serializer.serialize_str(self.as_str())
				}
			} else {
				quote! {
					serializer.serialize_bytes(self.as_bytes())
				}
			};

			let (visit_str, visit_bytes) = if T::UNICODE {
				(
					quote! {
						#buffer_ident::new(v)
					},
					quote! {
						match ::std::string::String::from_utf8(v) {
							Ok(s) => #buffer_ident::new(s).map_err(|#error(s)| #error(s.into_bytes())),
							Err(e) => Err(#error(e.into_bytes()))
						}
					},
				)
			} else {
				(
					quote! {
						#buffer_ident::new(v.into_bytes()).map_err(|#error(bytes)| unsafe {
							#error(::std::string::String::from_utf8_unchecked(bytes))
						})
					},
					quote! {
						#buffer_ident::new(v)
					},
				)
			};

			let expected = format!("some {ident}");

			tokens.extend(quote! {
				impl ::serde::Serialize for #buffer_ident {
					fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
					where
						S: ::serde::Serializer
					{
						#serialize
					}
				}

				impl<'de> ::serde::Deserialize<'de> for #buffer_ident {
					fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
					where
						D: ::serde::Deserializer<'de>
					{
						struct Visitor;

						impl<'de> ::serde::de::Visitor<'de> for Visitor {
							type Value = #buffer_ident;

							fn expecting(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
								write!(f, #expected)
							}

							fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
							where
								E: ::serde::de::Error
							{
								self.visit_string(v.to_string())
							}

							fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
							where
								E: ::serde::de::Error
							{
								self.visit_byte_buf(v.to_vec())
							}

							fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
							where
								E: ::serde::de::Error
							{
								#visit_str.map_err(|#error(v)| E::invalid_value(::serde::de::Unexpected::Str(&v), &self))
							}

							fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
							where
								E: ::serde::de::Error
							{
								#visit_bytes.map_err(|#error(v)| E::invalid_value(::serde::de::Unexpected::Bytes(&v), &self))
							}
						}

						deserializer.deserialize_str(Visitor)
					}
				}
			})
		}
	}

	Ok(tokens)
}

fn generate_validation_function<T: Token>(automaton: &DetAutomaton<u32, T::Set>) -> TokenStream {
	let initial_state = *automaton.initial_state();

	let states = automaton.transitions().iter().map(|(q, transitions)| {
		let transitions = transitions.iter().map(|(set, target)| {
			let set = T::Set::rust_set(set);
			quote! {
				Some(#set) => #target
			}
		});

		let is_final = automaton.is_final_state(q);

		quote! {
			#q => match input.next() {
				#(#transitions,)*
				Some(_) => break false,
				None => break #is_final
			}
		}
	});

	quote! {
		let mut state = #initial_state;
		loop {
			state = match state {
				#(#states,)*
				_ => unreachable!()
			}
		}
	}
}
