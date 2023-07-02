use static_regular_grammar::RegularGrammar;

/// Test
///
/// # Grammar
///
/// ```abnf
/// test = "%" / "$"
/// ```
#[derive(RegularGrammar)]
pub struct Test([u8]);

fn main() {
	Test::new(b"$").unwrap();
}
