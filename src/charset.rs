use btree_range_map::{AnyRange, RangeSet};
use serde::{Deserialize, Serialize};
use std::{
	fmt,
	ops::{Deref, DerefMut, RangeInclusive},
};

pub struct DisplayChar(pub char);

impl fmt::Display for DisplayChar {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let c = self.0;
		match c {
			'\\' => write!(f, "\\\\"),
			'\r' => write!(f, "\\r"),
			'\n' => write!(f, "\\n"),
			' ' => write!(f, "\\s"),
			'\t' => write!(f, "\\t"),
			_ if c.is_control() => {
				let d = c as u32;
				if d <= 0xff {
					write!(f, "\\x{:02x}", d)
				} else if d <= 0xffff {
					write!(f, "\\u{:04x}", d)
				} else {
					write!(f, "\\U{:08x}", d)
				}
			}
			_ => c.fmt(f),
		}
	}
}

pub struct DisplayString<'a>(pub &'a str);

impl<'a> fmt::Display for DisplayString<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for c in self.0.chars() {
			DisplayChar(c).fmt(f)?;
		}

		Ok(())
	}
}

pub struct DisplayCharRange<'a>(pub &'a AnyRange<char>);

impl<'a> fmt::Display for DisplayCharRange<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(first) = self.0.first() {
			let last = self.0.last().unwrap();

			if first == last {
				fmt::Display::fmt(&DisplayChar(first), f)
			} else if first as u32 + 1 == last as u32 {
				// Note: no risk of overflowing here with `char`.
				write!(f, "{}{}", DisplayChar(first), DisplayChar(last))
			} else {
				write!(f, "{}-{}", DisplayChar(first), DisplayChar(last))
			}
		} else {
			Ok(())
		}
	}
}

#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct CharSet(RangeSet<char>);

impl CharSet {
	pub fn new() -> CharSet {
		CharSet(RangeSet::new())
	}

	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	pub fn len(&self) -> u64 {
		self.0.len()
	}

	pub fn is_ascii(&self) -> bool {
		self.0
			.iter()
			.all(|range| range.first().unwrap().is_ascii() && range.last().unwrap().is_ascii())
	}

	pub fn from_char(c: char, case_sensitive: bool) -> CharSet {
		let mut set = CharSet::new();

		if case_sensitive {
			set.insert(c)
		} else {
			for c in c.to_uppercase() {
				set.insert(c)
			}
			for c in c.to_lowercase() {
				set.insert(c)
			}
		}

		set
	}

	pub fn from_ranges(ranges: impl IntoIterator<Item = RangeInclusive<char>>) -> Self {
		let mut set = RangeSet::new();

		for range in ranges {
			set.insert(range)
		}

		CharSet(set)
	}

	pub fn from_chars(chars: impl IntoIterator<Item = char>) -> Self {
		let mut set = RangeSet::new();

		for c in chars {
			set.insert(c)
		}

		CharSet(set)
	}

	pub fn ranges(&self) -> Ranges {
		Ranges(self.0.iter())
	}

	pub fn first(&self) -> Option<char> {
		self.0.iter().next().and_then(|range| range.first())
	}
}

pub struct Ranges<'a>(
	btree_range_map::generic::set::Iter<'a, char, btree_range_map::DefaultSetContainer<char>>,
);

impl<'a> Iterator for Ranges<'a> {
	type Item = AnyRange<char>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0.next().copied()
	}
}

impl From<AnyRange<char>> for CharSet {
	fn from(value: AnyRange<char>) -> Self {
		let mut result = CharSet::new();
		result.insert(value);
		result
	}
}

impl From<char> for CharSet {
	fn from(value: char) -> Self {
		let mut result = CharSet::new();
		result.insert(value);
		result
	}
}

impl<const N: usize> From<[char; N]> for CharSet {
	fn from(value: [char; N]) -> Self {
		Self::from_chars(value)
	}
}

impl<const N: usize> From<[RangeInclusive<char>; N]> for CharSet {
	fn from(value: [RangeInclusive<char>; N]) -> Self {
		Self::from_ranges(value)
	}
}

impl fmt::Display for CharSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for range in &self.0 {
			DisplayCharRange(range).fmt(f)?;
		}

		Ok(())
	}
}

impl fmt::Debug for CharSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "CharSet({})", self)
	}
}

impl Deref for CharSet {
	type Target = RangeSet<char>;

	fn deref(&self) -> &RangeSet<char> {
		&self.0
	}
}

impl DerefMut for CharSet {
	fn deref_mut(&mut self) -> &mut RangeSet<char> {
		&mut self.0
	}
}
