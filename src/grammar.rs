use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::{token::Token, utils::automaton::DetAutomaton};

pub mod abnf;

#[derive(Debug, thiserror::Error)]
pub enum GrammarError {
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
	pub fn new(ty: GrammarType, data: String) -> Result<Self, GrammarError> {
		match ty {
			GrammarType::Abnf => Ok(Self::Abnf(abnf::Grammar::new(data)?)),
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
