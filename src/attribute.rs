use std::path::PathBuf;

use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use syn::spanned::Spanned;

use crate::Derives;

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error("unexpected token {0}")]
	UnexpectedToken(TokenTree),

	#[error("expected `=`")]
	ExpectedEquals,

	#[error("expected string literal")]
	ExpectedStringLiteral,

	#[error("expected parenthesized group")]
	ExpectedGroup,

	#[error("invalid cache path")]
	InvalidCachePath,
}

#[derive(Default)]
pub struct Attribute {
	pub file: Option<PathBuf>,
	pub entry_point: Option<String>,
	pub sized: Option<SizedTypeAttributes>,
	pub cache_path: Option<PathBuf>,
	pub no_deref: bool,
	pub no_borrow: bool,
	pub ascii: bool,
	pub disable: bool,
}

impl Attribute {
	pub fn append(&mut self, other: Self) {
		self.file = other.file.or(self.file.take());
		self.entry_point = other.entry_point.or(self.entry_point.take());

		if let Some(a) = other.sized {
			self.sized.get_or_insert_with(Default::default).append(a);
		}

		self.cache_path = other.cache_path.or(self.cache_path.take());
		self.no_deref |= other.no_deref;
		self.no_borrow |= other.no_borrow;
		self.ascii |= other.ascii;
		self.disable |= other.disable
	}
}

enum AttributeItem {
	File,
	EntryPoint,
	SizedType,
	CachePath,
	NoDeref,
	NoBorrow,
	Ascii,
	Disable,
	Separator,
}

impl AttributeItem {
	fn parse(token: TokenTree) -> Result<Self, (Error, Span)> {
		match token {
			TokenTree::Ident(id) if id == "file" => Ok(Self::File),
			TokenTree::Ident(id) if id == "entry_point" => Ok(Self::EntryPoint),
			TokenTree::Ident(id) if id == "sized" => Ok(Self::SizedType),
			TokenTree::Ident(id) if id == "cache" => Ok(Self::CachePath),
			TokenTree::Ident(id) if id == "no_deref" => Ok(Self::NoDeref),
			TokenTree::Ident(id) if id == "no_borrow" => Ok(Self::NoBorrow),
			TokenTree::Ident(id) if id == "ascii" => Ok(Self::Ascii),
			TokenTree::Ident(id) if id == "disable" => Ok(Self::Disable),
			TokenTree::Punct(_) => Ok(Self::Separator),
			t => {
				let span = t.span();
				Err((Error::UnexpectedToken(t), span))
			}
		}
	}
}

fn expect_eq(
	tokens: &mut impl Iterator<Item = TokenTree>,
	span: Span,
) -> Result<(), (Error, Span)> {
	match tokens.next() {
		Some(TokenTree::Punct(p)) if p.as_char() == '=' => Ok(()),
		Some(t) => {
			let span = t.span();
			Err((Error::UnexpectedToken(t), span))
		}
		None => Err((Error::ExpectedEquals, span)),
	}
}

fn expect_str_literal(
	tokens: &mut impl Iterator<Item = TokenTree>,
	span: Span,
) -> Result<String, (Error, Span)> {
	match tokens.next() {
		Some(TokenTree::Literal(l)) => {
			let lit = syn::Lit::new(l);
			match lit {
				syn::Lit::Str(s) => Ok(s.value()),
				l => {
					let span = l.span();
					Err((Error::ExpectedStringLiteral, span))
				}
			}
		}
		Some(t) => {
			let span = t.span();
			Err((Error::UnexpectedToken(t), span))
		}
		None => Err((Error::ExpectedStringLiteral, span)),
	}
}

fn expect_group(
	tokens: &mut impl Iterator<Item = TokenTree>,
	span: Span,
) -> Result<TokenStream, (Error, Span)> {
	match tokens.next() {
		Some(TokenTree::Group(g)) => Ok(g.stream()),
		Some(t) => {
			let span = t.span();
			Err((Error::UnexpectedToken(t), span))
		}
		None => Err((Error::ExpectedGroup, span)),
	}
}

impl Attribute {
	pub fn parse(attr: syn::Attribute) -> Result<Self, (Error, Span)> {
		let span = attr.span();
		match attr.meta {
			syn::Meta::List(list) => Self::parse_inner(list.tokens),
			_ => Err((Error::ExpectedGroup, span)),
		}
	}

	fn parse_inner(tokens: TokenStream) -> Result<Self, (Error, Span)> {
		let mut tokens = tokens.into_iter();
		let mut result = Self::default();

		while let Some(token) = tokens.next() {
			let span = token.span();
			match AttributeItem::parse(token)? {
				AttributeItem::Separator => (),
				AttributeItem::File => {
					expect_eq(&mut tokens, span)?;
					result.file = Some(expect_str_literal(&mut tokens, span)?.into())
				}
				AttributeItem::EntryPoint => {
					expect_eq(&mut tokens, span)?;
					result.entry_point = Some(expect_str_literal(&mut tokens, span)?)
				}
				AttributeItem::CachePath => {
					expect_eq(&mut tokens, span)?;
					let path: PathBuf = expect_str_literal(&mut tokens, span)?.into();
					// Check that the path is relative, without `..`.
					if path.is_relative() && path.iter().all(|s| s != "..") {
						result.cache_path = Some(path)
					} else {
						return Err((Error::InvalidCachePath, span));
					}
				}
				AttributeItem::SizedType => {
					let inner = expect_group(&mut tokens, span)?;
					result.sized = Some(SizedTypeAttributes::parse(inner)?)
				}
				AttributeItem::NoDeref => result.no_deref = true,
				AttributeItem::NoBorrow => {
					result.no_borrow = true;
					result.no_deref = true
				}
				AttributeItem::Ascii => result.ascii = true,
				AttributeItem::Disable => result.disable = true,
			}
		}

		Ok(result)
	}
}

#[derive(Default)]
pub struct SizedTypeAttributes {
	pub ident: Option<Ident>,
	pub derives: Derives,
}

impl SizedTypeAttributes {
	pub fn append(&mut self, other: Self) {
		self.ident = other.ident.or(self.ident.take());
		self.derives.append(other.derives)
	}
}

enum SizedTypeAttributeItem {
	Ident(Ident),
	Derive,
	Separator,
}

impl SizedTypeAttributeItem {
	fn parse(token: TokenTree) -> Result<Self, (Error, Span)> {
		match token {
			TokenTree::Ident(id) => {
				if id == "derive" {
					Ok(Self::Derive)
				} else {
					Ok(Self::Ident(id))
				}
			}
			TokenTree::Punct(_) => Ok(Self::Separator),
			t => {
				let span = t.span();
				Err((Error::UnexpectedToken(t), span))
			}
		}
	}
}

impl SizedTypeAttributes {
	pub fn parse(tokens: TokenStream) -> Result<Self, (Error, Span)> {
		let mut tokens = tokens.into_iter();
		let mut result = Self::default();

		while let Some(token) = tokens.next() {
			let span = token.span();
			match SizedTypeAttributeItem::parse(token)? {
				SizedTypeAttributeItem::Separator => (),
				SizedTypeAttributeItem::Ident(id) => result.ident = Some(id),
				SizedTypeAttributeItem::Derive => {
					let inner = expect_group(&mut tokens, span)?;
					let mut derives = Derives::default();

					for token in inner {
						match token {
							TokenTree::Punct(_) => (),
							TokenTree::Ident(ident) if ident == "Debug" => derives.debug = true,
							TokenTree::Ident(ident) if ident == "Display" => derives.display = true,
							TokenTree::Ident(ident) if ident == "PartialEq" => {
								derives.partial_eq = true
							}
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
								return Err((Error::UnexpectedToken(tt), span));
							}
						}
					}

					result.derives.append(derives)
				}
			}
		}

		Ok(result)
	}
}
