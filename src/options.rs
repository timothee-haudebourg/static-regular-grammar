use std::{borrow::Cow, path::PathBuf};

use proc_macro2::{Ident, Span};

use crate::{attribute::SizedTypeAttributes, Attribute, Error};

pub struct Options {
	pub file: Option<PathBuf>,
	pub entry_point: Option<String>,
	pub name: Option<String>,
	pub sized: Option<SizedTypeOptions>,
	pub cache_path: PathBuf,
	pub no_deref: bool,
	pub no_borrow: bool,
	pub ascii: bool,
	pub disable: bool,
	pub serde: bool,
}

impl Options {
	pub(crate) fn from_attribute(ident: &Ident, attr: Attribute) -> Result<Self, (Error, Span)> {
		Ok(Self {
			file: attr.file,
			entry_point: attr.entry_point,
			name: attr.name,
			sized: match attr.sized {
				Some(attr) => Some(SizedTypeOptions::from_attribute(attr, ident.span())?),
				None => None,
			},
			cache_path: build_cache_path(ident, attr.cache_path)?,
			no_deref: attr.no_deref,
			no_borrow: attr.no_borrow,
			ascii: attr.ascii,
			disable: attr.disable,
			serde: attr.serde,
		})
	}
}

fn find_target_dir() -> Result<Cow<'static, str>, std::env::VarError> {
	match std::env::var("OUT_DIR") {
		Ok(dir) => Ok(Cow::Owned(dir)),
		Err(std::env::VarError::NotPresent) => match std::env::var("CARGO_TARGET_DIR") {
			Ok(dir) => Ok(Cow::Owned(dir)),
			Err(std::env::VarError::NotPresent) => Ok(Cow::Borrowed("target")),
			Err(e) => Err(e),
		},
		Err(e) => Err(e),
	}
}

fn build_cache_path(ident: &Ident, path: Option<PathBuf>) -> Result<PathBuf, (Error, Span)> {
	match path {
		Some(path) => Ok(path),
		None => {
			let target =
				find_target_dir().map_err(|e| (Error::TargetDirNotFound(e), ident.span()))?;
			Ok(format!("{target}/regular-grammar/{}.automaton.cbor", ident).into())
		}
	}
}

#[derive(Default)]
pub struct Derives {
	pub debug: bool,
	pub display: bool,
	pub partial_eq: bool,
	pub eq: bool,
	pub partial_ord: bool,
	pub ord: bool,
	pub hash: bool,
}

impl Derives {
	pub fn append(&mut self, other: Self) {
		self.debug |= other.debug;
		self.display |= other.display;
		self.partial_eq |= other.partial_eq;
		self.eq |= other.eq;
		self.partial_ord |= other.partial_ord;
		self.ord |= other.ord;
		self.hash |= other.hash
	}
}

pub struct SizedTypeOptions {
	pub ident: Ident,
	pub derives: Derives,
}

impl SizedTypeOptions {
	pub(crate) fn from_attribute(
		attr: SizedTypeAttributes,
		span: Span,
	) -> Result<Self, (Error, Span)> {
		Ok(Self {
			ident: attr
				.ident
				.ok_or_else(|| (Error::MissingSizedTypeIdent, span))?,
			derives: attr.derives,
		})
	}
}
