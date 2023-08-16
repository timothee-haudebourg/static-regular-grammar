use std::fmt::Display;

use btree_range_map::{AnyRange, RangeMap};
use quote::quote;

use crate::{
	charset,
	utils::{automaton, MergeRef, Sanitized},
	CharSet,
};

use super::{Token, TokenMap, TokenRange, TokenSet};

/// Unicode character token.
impl Token for char {
	type Range = AnyRange<char>;

	type Set = CharSet;

	type Map<V> = RangeMap<char, V>;

	const UNICODE: bool = true;

	fn from_u8(b: u8) -> Self {
		b as char
	}

	fn from_char(c: char) -> Option<Self> {
		Some(c)
	}

	fn from_u32(v: u32) -> Option<Self> {
		char::from_u32(v)
	}

	fn fmt_token(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		Sanitized(*self).fmt(f)
	}

	fn is_ascii(automaton: &automaton::DetAutomaton<u32, Self::Set>) -> bool {
		for transitions in automaton.transitions().values() {
			if transitions.keys().any(|set| !set.is_ascii()) {
				return false;
			}
		}

		true
	}

	fn rust_type() -> proc_macro2::TokenStream {
		quote!(char)
	}

	fn rust_string_type() -> proc_macro2::TokenStream {
		quote!(str)
	}

	fn rust_owned_string_type() -> proc_macro2::TokenStream {
		quote!(String)
	}

	fn rust_iterator_method() -> proc_macro2::TokenStream {
		quote!(chars())
	}

	fn rust_as_inner_method() -> proc_macro2::TokenStream {
		quote!(as_str)
	}

	fn rust_into_inner_method() -> proc_macro2::TokenStream {
		quote!(into_string)
	}

	fn rust_inner_as_bytes_method() -> Option<proc_macro2::TokenStream> {
		Some(quote!(as_bytes))
	}

	fn rust_inner_into_bytes_method() -> Option<proc_macro2::TokenStream> {
		Some(quote!(into_bytes))
	}

	fn rust_empty_string() -> proc_macro2::TokenStream {
		quote! {
			""
		}
	}
}

impl TokenRange<char> for AnyRange<char> {
	fn new(a: char, b: char) -> Self {
		(a..=b).into()
	}

	fn peek(&self) -> Option<char> {
		self.first()
	}
}

impl TokenSet<char> for CharSet {
	fn singleton(token: char, case_sensitive: bool) -> Self {
		CharSet::from_char(token, case_sensitive)
	}

	fn is_empty(&self) -> bool {
		self.is_empty()
	}

	fn len(&self) -> usize {
		self.len() as usize
	}

	fn peek(&self) -> Option<char> {
		self.first()
	}

	fn intersects_range(&self, range: <char as Token>::Range) -> bool {
		self.intersects(range)
	}

	fn merge_with(&mut self, other: Self) {
		self.extend(other.ranges())
	}

	fn rust_set(&self) -> proc_macro2::TokenStream {
		let ranges = self.ranges().map(|range| match range.len() {
			0 => panic!("empty range"),
			1 => {
				let a = range.first().unwrap();
				quote! {
					#a
				}
			}
			_ => {
				let a = range.first().unwrap();
				let b = range.last().unwrap();
				quote! {
					#a..=#b
				}
			}
		});

		quote! { #(#ranges)|* }
	}
}

impl MergeRef for CharSet {
	fn merge_with_ref(&mut self, other: &Self) {
		self.extend(other.ranges())
	}
}

impl automaton::DeterminizeLabel for CharSet {
	type Range = AnyRange<char>;

	type Ranges<'a> = charset::Ranges<'a>;

	type RangeMap<V: Clone + PartialEq + std::fmt::Debug> = RangeMap<char, V>;

	fn ranges(&self) -> Self::Ranges<'_> {
		CharSet::ranges(self)
	}

	fn insert_range(&mut self, range: <char as Token>::Range) {
		self.insert(range)
	}
}

impl<V> TokenMap<char, V> for RangeMap<char, V> {
	type Iter<'a> = btree_range_map::generic::map::Iter<'a, char, V, btree_range_map::DefaultMapContainer<char, V>> where V: 'a, Self: 'a;

	fn is_empty(&self) -> bool {
		self.is_empty()
	}

	fn len(&self) -> usize {
		self.len() as usize
	}

	fn iter(&self) -> Self::Iter<'_> {
		self.iter()
	}

	fn insert_range(&mut self, range: <char as Token>::Range, value: V)
	where
		V: PartialEq + Clone,
	{
		self.insert(range, value)
	}

	fn insert(&mut self, set: <char as Token>::Set, value: V)
	where
		V: PartialEq + Clone,
	{
		let mut ranges = set.ranges();

		if let Some(first) = ranges.next() {
			for range in ranges {
				self.insert_range(range, value.clone())
			}

			self.insert_range(first, value)
		}
	}

	fn update_range(&mut self, range: <char as Token>::Range, f: impl Fn(Option<&V>) -> Option<V>)
	where
		V: PartialEq + Clone,
	{
		self.update(range, f)
	}

	fn update(&mut self, set: &<char as Token>::Set, f: impl Fn(Option<&V>) -> Option<V>)
	where
		V: PartialEq + Clone,
	{
		for range in set.ranges() {
			self.update(range, &f)
		}
	}
}

impl<V: Clone + PartialEq> automaton::RangeMap<AnyRange<char>, V> for RangeMap<char, V> {
	fn update(&mut self, key: AnyRange<char>, f: impl Fn(Option<&V>) -> Option<V>) {
		RangeMap::update(self, key, f)
	}
}
