use static_regular_grammar::RegularGrammar;

/// ```abnf
/// broken = (
/// ```
#[derive(RegularGrammar)]
pub struct Broken([u8]);
