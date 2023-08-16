use static_regular_grammar::RegularGrammar;

/// Test
///
/// # Grammar
///
/// ```abnf
/// test = "%" / "$"
/// ```
#[derive(RegularGrammar, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(sized(TestBuf, derive(Debug, Display, PartialEq, Eq, PartialOrd, Ord, Hash)))]
pub struct Test(str);

fn main() {
	Test::new("$").unwrap();
}
