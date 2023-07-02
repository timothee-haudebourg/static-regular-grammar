//! This library provides the handy `RegularGrammar` derive macro that helps you
//! create unsized type wrapping byte or char strings validated by a regular
//! grammar. For now, only the [ABNF] grammar format is supported.
//!
//! [ABNF]: <https://datatracker.ietf.org/doc/html/rfc5234>
//!
//! # Example
//!
//! The grammar is specified by code blocks in the type documentation.
//!
//! ```
//! use static_regular_grammar::RegularGrammar;
//!
//! /// Example grammar
//! ///
//! /// ```abnf
//! /// foo = "f" 1*("oo") ; the first non-terminal is used as entry point.
//! /// ```
//! #[derive(RegularGrammar)]
//! pub struct Foo([u8]);
//!
//! let foo = Foo::new(b"foooooo").unwrap();
//! ```
use indoc::formatdoc;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote};
use sha2::{Digest, Sha256};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput};

mod byteset;
mod charset;
mod grammar;
mod token;
mod utils;

use byteset::ByteSet;
use charset::CharSet;
use grammar::{Grammar, GrammarError, GrammarType};
use token::{Token, TokenSet};
use utils::{automaton::DetAutomaton, SnakeCase};

#[proc_macro_derive(RegularGrammar, attributes(title, buffer))]
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

	#[error("invalid documentation attribute")]
	InvalidDocumentation,

	#[error("missing grammar")]
	MissingGrammar,

	#[error("unsafe visibility")]
	UnsafeVisibility,

	#[error("invalid inner type")]
	InvalidInnerType,

	#[error("inconsistent grammar type")]
	InconsistentGrammarType,

	#[error("invalid title value")]
	InvalidTitleValue,

	#[error("`buffer` attribute error: {0}")]
	BufferAttribute(#[from] BufferAttributeError),

	#[error(transparent)]
	Grammar(GrammarError),
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
	buffer: Option<BufferOptions>,
	title: String,
	token: TokenType,
}

#[derive(Default)]
struct Derives {
	partial_eq: bool,
	eq: bool,
	partial_ord: bool,
	ord: bool,
	hash: bool,
}

struct BufferOptions {
	ident: Ident,
	derives: Derives,
}

#[derive(Debug, thiserror::Error)]
enum BufferAttributeError {
	#[error("unexpected token {0}")]
	UnexpectedToken(TokenTree),

	#[error("expected parenthesized list")]
	UnexpectedList,

	#[error("missing identifier")]
	MissingIdent,
}

impl BufferOptions {
	pub fn parse(span: Span, tokens: TokenStream) -> Result<Self, (BufferAttributeError, Span)> {
		let mut tokens = tokens.into_iter();

		let mut ident = None;
		let mut derives = Derives::default();

		while let Some(token) = tokens.next() {
			match token {
				TokenTree::Ident(id) if id == "derive" => match tokens.next() {
					Some(TokenTree::Group(group)) => {
						for token in group.stream() {
							match token {
								TokenTree::Punct(_) => (),
								TokenTree::Ident(ident) if ident == "PartialEq" => {
									derives.partial_eq = true
								}
								TokenTree::Ident(ident) if ident == "Eq" => derives.eq = true,
								TokenTree::Ident(ident) if ident == "PartialOrd" => {
									derives.partial_ord = true
								}
								TokenTree::Ident(ident) if ident == "Ord" => derives.ord = true,
								TokenTree::Ident(ident) if ident == "Hash" => derives.hash = true,
								tt => {
									let span = tt.span();
									return Err((BufferAttributeError::UnexpectedToken(tt), span));
								}
							}
						}
					}
					Some(tt) => {
						let span = tt.span();
						return Err((BufferAttributeError::UnexpectedToken(tt), span));
					}
					None => return Err((BufferAttributeError::UnexpectedList, span)),
				},
				TokenTree::Ident(id) => ident = Some(id),
				TokenTree::Punct(_) => (),
				tt => {
					let span = tt.span();
					return Err((BufferAttributeError::UnexpectedToken(tt), span));
				}
			}
		}

		Ok(Self {
			ident: ident.ok_or((BufferAttributeError::MissingIdent, span))?,
			derives,
		})
	}
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
								let mut title = None;
								let mut buffer = None;

								let mut attrs_rest = Vec::with_capacity(input.attrs.len());
								for attr in input.attrs {
									if attr.meta.path().is_ident("title") {
										match attr.meta {
											syn::Meta::NameValue(meta) => {
												if meta.path.is_ident("title") {
													match &meta.value {
														syn::Expr::Lit(syn::ExprLit {
															lit: syn::Lit::Str(value),
															..
														}) => title = Some(value.value()),
														_ => {
															return Err((
																Error::InvalidTitleValue,
																meta.span(),
															))
														}
													}
												}
											}
											_ => {
												return Err((
													Error::InvalidTitleValue,
													attr.meta.span(),
												))
											}
										}
									} else if attr.meta.path().is_ident("buffer") {
										match attr.meta {
											syn::Meta::List(list) => {
												buffer = Some(
													BufferOptions::parse(list.span(), list.tokens)
														.map_err(|(e, span)| (e.into(), span))?,
												);
											}
											_ => todo!(),
										}
									} else {
										attrs_rest.push(attr);
									}
								}

								let title = title.unwrap_or_else(|| input.ident.to_string());

								Ok((
									GrammarData {
										vis: input.vis,
										ident: input.ident,
										buffer,
										title,
										token: TokenType::from_type(field.ty)?,
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

fn extract_grammar<T: Token>(
	title: &str,
	attrs: Vec<syn::Attribute>,
) -> Result<(Grammar<T>, [u8; 32]), (Error, Span)> {
	let mut grammar_ty = None;
	let mut grammar_data = String::new();
	// let mut span = Span::call_site();
	let span = Span::call_site();
	let mut in_block = false;

	for attr in attrs {
		if let syn::Meta::NameValue(meta) = attr.meta {
			let meta_span = meta.span();
			if meta.path.is_ident("doc") {
				match meta.value {
					syn::Expr::Lit(syn::ExprLit {
						lit: syn::Lit::Str(value),
						..
					}) => {
						let value = value.value();

						match grammar_ty {
							Some(grammar_ty) => {
								if in_block {
									if value.trim() == "```" {
										in_block = false
									} else if !value.is_empty() {
										if !value.starts_with(' ') {
											return Err((Error::InvalidDocumentation, meta_span));
										}

										// span = span.join(meta_span).unwrap();
										grammar_data.push_str(&value[1..]);
										grammar_data.push('\n');
									}
								} else {
									let trimmed = value.trim();
									if let Some(suffix) = trimmed.strip_prefix("```") {
										if let Some(new_grammar_ty) =
											GrammarType::new(suffix.trim())
										{
											if new_grammar_ty != grammar_ty {
												return Err((
													Error::InconsistentGrammarType,
													meta_span,
												));
											}
											in_block = true
										}
									}
								}
							}
							None => {
								let trimmed = value.trim();
								if let Some(suffix) = trimmed.strip_prefix("```") {
									grammar_ty = GrammarType::new(suffix.trim());
									in_block = true
								}
							}
						}
					}
					_ => return Err((Error::InvalidDocumentation, meta.span())),
				}
			}
		}
	}

	match grammar_ty {
		Some(grammar_ty) => {
			let hash: [u8; 32] = Sha256::digest(&grammar_data).into();

			let grammar = match Grammar::load_from_file(title, &hash) {
				Ok(Some(grammar)) => Some(Ok(grammar)),
				Ok(None) => None,
				Err(e) => {
					eprintln!("warning: could not load cached automaton: {e}");
					None
				}
			}
			.unwrap_or_else(|| {
				Grammar::new(grammar_ty, grammar_data).map_err(|e| (Error::Grammar(e), span))
			})?;

			// let grammar = Grammar::new(grammar_ty, grammar_data).map_err(|e| (Error::Grammar(e), span))?;

			Ok((grammar, hash))
		}
		None => Err((Error::MissingGrammar, Span::call_site())),
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
	let (grammar, hash) = extract_grammar::<T>(&data.title, attrs)?;
	let cached = grammar.is_cached();
	let automaton = grammar.build_automaton();

	if !cached {
		if let Err(e) = Grammar::<T>::save_to_file(&data.title, hash, &automaton) {
			eprintln!("unable to cache regular automaton: {e}")
		}
	}

	let vis = data.vis;
	let ident = data.ident;

	let as_ref = format_ident!("as_{}", SnakeCase(&ident.to_string()));

	let token_type = T::rust_type();
	let string_type = T::rust_string_type();
	let iterator_method = T::rust_iterator_method();

	let error = format_ident!("Invalid{}", ident);

	let new_doc = format!("Creates a new [`{ident}`] by parsing the `input` value");
	let new_unchecked_doc = formatdoc!(
		r#"
        Creates a new [`{ident}`] from the `input` value without validation.
        
        # Safety
        
        The input data *must* be a valid [`{ident}`]."#
	);

	let validate_doc = format!("Checks that the input iterator produces a valid [`{ident}`]");
	let validate_body = generate_validation_function::<T>(&automaton);

	let mut tokens = quote! {
		#[derive(Debug)]
		#vis struct #error<T>(pub T);

		impl #ident {
			#[doc = #new_doc]
			pub fn new(input: &#string_type) -> Result<&Self, #error<&#string_type>> {
				if Self::validate(input.#iterator_method) {
					Ok(unsafe {
						Self::new_unchecked(input)
					})
				} else {
					Err(#error(input))
				}
			}

			#[doc = #new_unchecked_doc]
			pub unsafe fn new_unchecked(input: &#string_type) -> &Self {
				::core::mem::transmute(input)
			}

			#[doc = #validate_doc]
			pub fn validate(mut input: impl Iterator<Item = #token_type>) -> bool {
				#validate_body
			}
		}
	};

	if let Some(buffer) = data.buffer {
		let buffer_ident = buffer.ident;
		let owned_string_type = T::rust_owned_string_type();

		tokens.extend(quote! {
			#vis struct #buffer_ident(#owned_string_type);

			impl #buffer_ident {
				#[doc = #new_doc]
				pub fn new(input: #owned_string_type) -> Result<Self, #error<#owned_string_type>> {
					if #ident::validate(input.#iterator_method) {
						Ok(Self(input))
					} else {
						Err(#error(input))
					}
				}

				#[doc = #new_unchecked_doc]
				pub unsafe fn new_unchecked(input: #owned_string_type) -> Self {
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
		});

		if buffer.derives.partial_eq {
			tokens.extend(quote! {
				impl ::core::cmp::PartialEq for #buffer_ident {
					fn eq(&self, other: &Self) -> bool {
						self.#as_ref() == other.#as_ref()
					}
				}

				impl ::core::cmp::PartialEq<#ident> for #buffer_ident {
					fn eq(&self, other: &#ident) -> bool {
						self.#as_ref() == other
					}
				}

				impl<'a> ::core::cmp::PartialEq<&'a #ident> for #buffer_ident {
					fn eq(&self, other: &&'a #ident) -> bool {
						self.#as_ref() == *other
					}
				}
			});
		}

		if buffer.derives.eq {
			tokens.extend(quote! {
				impl ::core::cmp::Eq for #buffer_ident where #ident: ::core::cmp::Eq {}
			});
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
