use btree_range_map::{AnyRange, RangeSet};
use serde::{Deserialize, Serialize};
use std::{
	fmt,
	ops::{Deref, DerefMut, RangeInclusive},
};

use crate::utils::Sanitized;

pub struct DisplayByte(pub u8);

impl fmt::Display for DisplayByte {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		Sanitized(self.0).fmt(f)
	}
}

pub struct DisplayBytes<'a>(pub &'a str);

impl<'a> fmt::Display for DisplayBytes<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		Sanitized(self.0).fmt(f)
	}
}

pub struct DisplayByteRange<'a>(pub &'a AnyRange<u8>);

impl<'a> fmt::Display for DisplayByteRange<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(first) = self.0.first() {
			let last = self.0.last().unwrap();

			if first == last {
				fmt::Display::fmt(&DisplayByte(first), f)
			} else if first as u32 + 1 == last as u32 {
				// Note: no risk of overflowing here with `u8`.
				write!(f, "{}{}", DisplayByte(first), DisplayByte(last))
			} else {
				write!(f, "{}-{}", DisplayByte(first), DisplayByte(last))
			}
		} else {
			Ok(())
		}
	}
}

#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ByteSet(RangeSet<u8>);

impl ByteSet {
	pub fn new() -> ByteSet {
		ByteSet(RangeSet::new())
	}

	// pub fn is_empty(&self) -> bool {
	// 	self.0.is_empty()
	// }

	// pub fn len(&self) -> u16 {
	// 	self.0.len()
	// }

	pub fn from_u8(c: u8, case_sensitive: bool) -> ByteSet {
		let mut set = ByteSet::new();

		if case_sensitive {
			set.insert(c)
		} else {
			set.insert(c.to_ascii_uppercase());
			set.insert(c.to_ascii_lowercase())
		}

		set
	}

	pub fn from_ranges(ranges: impl IntoIterator<Item = RangeInclusive<u8>>) -> Self {
		let mut set = RangeSet::new();

		for range in ranges {
			set.insert(range)
		}

		ByteSet(set)
	}

	pub fn from_u8s(u8s: impl IntoIterator<Item = u8>) -> Self {
		let mut set = RangeSet::new();

		for c in u8s {
			set.insert(c)
		}

		ByteSet(set)
	}

	pub fn ranges(&self) -> Ranges {
		Ranges(self.0.iter())
	}

	// pub fn first(&self) -> Option<u8> {
	// 	self.0.iter().next().and_then(|range| range.first())
	// }

	pub fn is_ascii(&self) -> bool {
		for range in self.0.iter() {
			if let Some(b) = range.first() {
				if !b.is_ascii() {
					return false;
				}
			}

			if let Some(b) = range.first() {
				if !b.is_ascii() {
					return false;
				}
			}
		}

		true
	}
}

pub struct Ranges<'a>(
	btree_range_map::generic::set::Iter<'a, u8, btree_range_map::DefaultSetContainer<u8>>,
);

impl<'a> Iterator for Ranges<'a> {
	type Item = AnyRange<u8>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0.next().copied()
	}
}

impl From<AnyRange<u8>> for ByteSet {
	fn from(value: AnyRange<u8>) -> Self {
		let mut result = ByteSet::new();
		result.insert(value);
		result
	}
}

impl From<u8> for ByteSet {
	fn from(value: u8) -> Self {
		let mut result = ByteSet::new();
		result.insert(value);
		result
	}
}

impl<const N: usize> From<[u8; N]> for ByteSet {
	fn from(value: [u8; N]) -> Self {
		Self::from_u8s(value)
	}
}

impl<const N: usize> From<[RangeInclusive<u8>; N]> for ByteSet {
	fn from(value: [RangeInclusive<u8>; N]) -> Self {
		Self::from_ranges(value)
	}
}

impl fmt::Display for ByteSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for range in &self.0 {
			DisplayByteRange(range).fmt(f)?;
		}

		Ok(())
	}
}

impl fmt::Debug for ByteSet {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "CharSet({})", self)
	}
}

impl Deref for ByteSet {
	type Target = RangeSet<u8>;

	fn deref(&self) -> &RangeSet<u8> {
		&self.0
	}
}

impl DerefMut for ByteSet {
	fn deref_mut(&mut self) -> &mut RangeSet<u8> {
		&mut self.0
	}
}
