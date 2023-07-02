use core::fmt;
use std::{fmt::Debug, hash::Hash};

use serde::{de::DeserializeOwned, Serialize};

use crate::utils::{automaton, MergeRef};

mod byte;
mod char;

/// Token type.
pub trait Token: Copy {
	/// Range of tokens.
	type Range: TokenRange<Self>;

	/// Set of tokens.
	type Set: TokenSet<Self>;

	/// Token map.
	type Map<V>: TokenMap<Self, V>;

	fn from_u8(b: u8) -> Self;

	fn from_char(c: char) -> Option<Self>;

	fn from_u32(v: u32) -> Option<Self>;

	fn fmt_token(&self, f: &mut fmt::Formatter) -> fmt::Result;

	fn rust_type() -> proc_macro2::TokenStream;

	fn rust_string_type() -> proc_macro2::TokenStream;

	fn rust_owned_string_type() -> proc_macro2::TokenStream;

	fn rust_iterator_method() -> proc_macro2::TokenStream;
}

pub trait TokenRange<T: Token>: Debug + Copy + Ord + Hash {
	/// Creates a new range starting from `a` (inclusive) to `b` (inclusive).
	fn new(a: T, b: T) -> Self;

	fn from_u8(b: u8) -> Self {
		Self::new(T::from_u8(b), T::from_u8(b))
	}

	fn peek(&self) -> Option<T>;
}

pub trait TokenSet<T: Token>:
	Debug
	+ Default
	+ Clone
	+ Ord
	+ Hash
	+ automaton::DeterminizeLabel<Range = T::Range>
	+ MergeRef
	+ Serialize
	+ DeserializeOwned
{
	fn singleton(token: T, case_sensitive: bool) -> Self;

	fn from_range(range: T::Range) -> Self {
		let mut result = Self::default();
		result.insert_range(range);
		result
	}

	fn is_empty(&self) -> bool;

	fn len(&self) -> usize;

	fn peek(&self) -> Option<T>;

	fn intersects_range(&self, range: T::Range) -> bool;

	fn merge_with(&mut self, other: Self);

	fn rust_set(&self) -> proc_macro2::TokenStream;
}

pub trait TokenMap<K: Token, V>: Default + IntoIterator<Item = (K::Range, V)> {
	type Iter<'a>: Iterator<Item = (&'a K::Range, &'a V)>
	where
		K::Range: 'a,
		V: 'a,
		Self: 'a;

	fn is_empty(&self) -> bool;

	fn len(&self) -> usize;

	fn iter(&self) -> Self::Iter<'_>;

	fn insert_range(&mut self, range: K::Range, value: V)
	where
		V: PartialEq + Clone;

	fn insert(&mut self, set: K::Set, value: V)
	where
		V: PartialEq + Clone;

	fn update_range(&mut self, range: K::Range, f: impl Fn(Option<&V>) -> Option<V>)
	where
		V: PartialEq + Clone;

	fn update(&mut self, set: &K::Set, f: impl Fn(Option<&V>) -> Option<V>)
	where
		V: PartialEq + Clone;
}
