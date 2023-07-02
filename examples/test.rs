use static_regular_grammar::RegularGrammar;
use std::fmt;

/// Test
///
/// # Grammar
///
/// ```abnf
/// test = "%" / "$"
/// ```
#[derive(RegularGrammar, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[sized(TestBuf, derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Test(str);

impl fmt::Display for Test {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.0.fmt(f)
	}
}

fn main() {
	Test::new("$").unwrap();
}
