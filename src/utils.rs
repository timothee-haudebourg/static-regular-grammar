use std::fmt;

pub mod automaton;
mod case;
pub mod scc;

pub use automaton::Automaton;
pub use case::*;
pub use scc::SccGraph;

pub fn is_graphic_char(c: char) -> bool {
	c.is_ascii_graphic() || !c.is_control()
}

pub fn is_graphic_byte(c: u8) -> bool {
	c.is_ascii_graphic() || !c.is_ascii_control()
}

pub struct Sanitized<T>(pub T);

impl fmt::Display for Sanitized<char> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt_char_sanitized(self.0, f)
	}
}

impl fmt::Display for Sanitized<u8> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt_u8_sanitized(self.0, f)
	}
}

impl<'a> fmt::Display for Sanitized<&'a str> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for c in self.0.chars() {
			fmt_char_sanitized(c, f)?;
		}

		Ok(())
	}
}

impl<'a> fmt::Display for Sanitized<&'a [u8]> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for &c in self.0 {
			fmt_u8_sanitized(c, f)?;
		}

		Ok(())
	}
}

impl<'a> fmt::Display for Sanitized<&'a String> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for c in self.0.chars() {
			fmt_char_sanitized(c, f)?;
		}

		Ok(())
	}
}

impl<'a> fmt::Display for Sanitized<&'a Vec<u8>> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for &c in self.0 {
			fmt_u8_sanitized(c, f)?;
		}

		Ok(())
	}
}

pub fn fmt_char_sanitized(c: char, f: &mut fmt::Formatter) -> fmt::Result {
	use fmt::Display;

	match c {
		'\u{00}' => "\\0".fmt(f),
		'\u{07}' => "\\a".fmt(f),
		'\u{08}' => "\\b".fmt(f),
		'\t' => "\\t".fmt(f),
		'\n' => "\\n".fmt(f),
		'\u{0b}' => "\\v".fmt(f),
		'\u{0c}' => "\\f".fmt(f),
		'\r' => "\\r".fmt(f),
		'\u{1b}' => "\\e".fmt(f),
		c if is_graphic_char(c) => c.fmt(f),
		c => {
			let value = c as u32;
			if value <= 0xff {
				write!(f, "\\x{{{:02x}}}", value)
			} else if value <= 0xffff {
				write!(f, "\\x{{{:04x}}}", value)
			} else {
				write!(f, "\\x{{{:08x}}}", value)
			}
		}
	}
}

pub fn fmt_u8_sanitized(c: u8, f: &mut fmt::Formatter) -> fmt::Result {
	use fmt::Display;

	match c {
		0x00 => "\\0".fmt(f),
		0x07 => "\\a".fmt(f),
		0x08 => "\\b".fmt(f),
		b'\t' => "\\t".fmt(f),
		b'\n' => "\\n".fmt(f),
		0x0b => "\\v".fmt(f),
		0x0c => "\\f".fmt(f),
		b'\r' => "\\r".fmt(f),
		0x1b => "\\e".fmt(f),
		c if is_graphic_byte(c) => (c as char).fmt(f),
		c => {
			write!(f, "\\x{{{:02x}}}", c)
		}
	}
}

// pub trait Get2Mut {
// 	type Item;

// 	fn get2_mut(&mut self, a: usize, b: usize) -> (&mut Self::Item, &mut Self::Item);
// }

// impl<T> Get2Mut for [T] {
// 	type Item = T;

// 	fn get2_mut(&mut self, a: usize, b: usize) -> (&mut Self::Item, &mut Self::Item) {
// 		match a.cmp(&b) {
// 			Ordering::Less => {
// 				let (left, right) = self.split_at_mut(b);
// 				(&mut left[a], &mut right[0])
// 			}
// 			Ordering::Greater => {
// 				let (left, right) = self.split_at_mut(a);
// 				(&mut right[0], &mut left[b])
// 			}
// 			Ordering::Equal => {
// 				panic!("a == b")
// 			}
// 		}
// 	}
// }

pub trait MergeRef {
	fn merge_with_ref(&mut self, other: &Self);
}
