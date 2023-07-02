use static_regular_grammar::RegularGrammar;

/// Test
///
/// # Grammar
///
/// ```abnf
/// test = "%" / "$"
/// ```
#[derive(RegularGrammar, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[sized(TestBuf, derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Test([u8]);

fn main() {
	Test::new(b"$").unwrap();
}
