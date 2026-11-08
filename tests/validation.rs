use static_regular_grammar::RegularGrammar;

/// ```abnf
/// word = 3*5%x61-7A
/// ```
#[derive(RegularGrammar)]
#[grammar(ascii, sized(WordBuf))]
pub struct Word([u8]);

/// ```abnf
/// moon = %x1F319
/// ```
#[derive(RegularGrammar)]
pub struct Moon(str);

#[test]
fn grammar_bounds_and_owned_values_are_preserved() {
	for (input, accepted) in [
		(b"aa".as_slice(), false),
		(b"aaa".as_slice(), true),
		(b"aaaa".as_slice(), true),
		(b"aaaaa".as_slice(), true),
		(b"aaaaaa".as_slice(), false),
		(b"a1a".as_slice(), false),
	] {
		assert_eq!(Word::new(input).is_ok(), accepted);
		assert_eq!(WordBuf::new(input.to_vec()).is_ok(), accepted);
	}
	assert_eq!(Word::new(b"abc").unwrap().as_str(), "abc");
}

#[test]
fn unicode_token_grammar_is_preserved() {
	assert!(Moon::new("").is_err());
	assert!(Moon::new("🌙").is_ok());
	assert!(Moon::new("🌙🌙").is_err());
	assert!(Moon::new("x").is_err());
}
