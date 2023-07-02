use std::path::Path;

use proc_macro2::Span;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use syn::spanned::Spanned;

use crate::{token::Token, utils::automaton::DetAutomaton};

pub mod abnf;

#[derive(Debug, thiserror::Error)]
pub enum GrammarError {
	#[error(transparent)]
	IO(#[from] std::io::Error),

	#[error("invalid ABNF grammar: {0}")]
	Abnf(::abnf::error::ParseError),

	#[error("undefined non-terminal `{0}`")]
	UndefinedNonTerminal(String),

	#[error("redefined non-terminal `{0}`")]
	RedefinedNonTerminal(String),

	#[error("grammar is not regular")]
	NonRegular,

	#[error("no entry point")]
	NoEntryPoint,

	#[error("unsupported unicode character token `{0}`")]
	UnsupportedCharToken(char),

	#[error("unsupported 32-bit interger token `{0:#x}`")]
	UnsupportedU32Token(u32),

	#[error("missing grammar")]
	MissingGrammar,

	#[error("unknown grammar format")]
	UnknownGrammarFormat,

	#[error("invalid documentation attribute")]
	InvalidDocumentation,

	#[error("inconsistent grammar type")]
	InconsistentGrammarType,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum GrammarType {
	Abnf,
}

impl GrammarType {
	pub fn new(name: &str) -> Option<Self> {
		match name {
			"abnf" => Some(Self::Abnf),
			_ => None,
		}
	}
}

pub enum Grammar<T: Token> {
	Cached(DetAutomaton<u32, T::Set>),
	Abnf(abnf::Grammar<T>),
}

#[derive(Debug, thiserror::Error)]
pub enum FileError {
	#[error(transparent)]
	IO(#[from] std::io::Error),

	#[error(transparent)]
	Var(#[from] std::env::VarError),

	#[error(transparent)]
	Serialization(#[from] ciborium::ser::Error<std::io::Error>),

	#[error(transparent)]
	Deserialization(#[from] ciborium::de::Error<std::io::Error>),
}

#[derive(Deserialize)]
struct CachedAutomaton<T: Token> {
	hash: [u8; 32],
	automaton: DetAutomaton<u32, T::Set>,
}

#[derive(Serialize)]
struct CachedAutomatonRef<'a, T: Token> {
	hash: [u8; 32],
	automaton: &'a DetAutomaton<u32, T::Set>,
}

impl<'a, T: Token> CachedAutomatonRef<'a, T> {
	pub fn new(hash: [u8; 32], automaton: &'a DetAutomaton<u32, T::Set>) -> Self {
		Self { hash, automaton }
	}
}

impl<T: Token> Grammar<T> {
	pub fn new(
		ty: GrammarType,
		data: String,
		entry_point: Option<&str>,
	) -> Result<Self, GrammarError> {
		match ty {
			GrammarType::Abnf => Ok(Self::Abnf(abnf::Grammar::new(data, entry_point)?)),
		}
	}

	pub fn load_from_file(filename: &Path, hash: &[u8; 32]) -> Result<Option<Self>, FileError> {
		match std::fs::File::open(filename) {
			Ok(file) => {
				let input = std::io::BufReader::new(file);
				let cached: CachedAutomaton<T> = ciborium::from_reader(input)?;

				if cached.hash == *hash {
					Ok(Some(Self::Cached(cached.automaton)))
				} else {
					Ok(None)
				}
			}
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
			Err(e) => Err(e.into()),
		}
	}

	pub fn save_to_file(
		filename: &Path,
		hash: [u8; 32],
		automaton: &DetAutomaton<u32, T::Set>,
	) -> Result<(), FileError> {
		if let Some(parent) = filename.parent() {
			std::fs::create_dir_all(parent)?;
		}

		let file = std::fs::File::create(filename)?;
		let output = std::io::BufWriter::new(file);
		Ok(ciborium::into_writer(
			&CachedAutomatonRef::<T>::new(hash, automaton),
			output,
		)?)
	}

	pub fn is_cached(&self) -> bool {
		matches!(self, Self::Cached(_))
	}

	pub fn build_automaton(self) -> DetAutomaton<u32, T::Set> {
		match self {
			Self::Cached(aut) => aut,
			Self::Abnf(g) => g.build_automaton(),
		}
	}
}

pub(crate) fn extract_grammar<T: Token>(
	cache_path: &Path,
	path: Option<&Path>,
	entry_point: Option<&str>,
	attrs: Vec<syn::Attribute>,
) -> Result<(Grammar<T>, [u8; 32]), (GrammarError, Span)> {
	let (ty, data) = match path {
		Some(path) => {
			let ty = match path.extension() {
				Some(ext) if ext == "abnf" => GrammarType::Abnf,
				_ => return Err((GrammarError::UnknownGrammarFormat, Span::call_site())),
			};

			let data = std::fs::read_to_string(path)
				.map_err(|e| (GrammarError::IO(e), Span::call_site()))?;
			(ty, data)
		}
		None => match parse_documentation_grammar(attrs)? {
			Some((ty, data)) => (ty, data),
			None => return Err((GrammarError::MissingGrammar, Span::call_site())),
		},
	};

	let hash: [u8; 32] = Sha256::digest(&data).into();
	let grammar = match Grammar::load_from_file(cache_path, &hash) {
		Ok(Some(grammar)) => Some(Ok(grammar)),
		Ok(None) => None,
		Err(e) => {
			eprintln!("warning: could not load cached automaton: {e}");
			None
		}
	}
	.unwrap_or_else(|| Grammar::new(ty, data, entry_point).map_err(|e| (e, Span::call_site())))?;

	// let grammar = Grammar::new(grammar_ty, grammar_data).map_err(|e| (Error::Grammar(e), span))?;

	Ok((grammar, hash))
}

pub(crate) fn parse_documentation_grammar(
	attrs: Vec<syn::Attribute>,
) -> Result<Option<(GrammarType, String)>, (GrammarError, Span)> {
	let mut grammar_ty = None;
	let mut grammar_data = String::new();
	// let span = Span::call_site();
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
											return Err((
												GrammarError::InvalidDocumentation,
												meta_span,
											));
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
													GrammarError::InconsistentGrammarType,
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
					_ => return Err((GrammarError::InvalidDocumentation, meta.span())),
				}
			}
		}
	}

	Ok(grammar_ty.map(|ty| (ty, grammar_data)))
}
