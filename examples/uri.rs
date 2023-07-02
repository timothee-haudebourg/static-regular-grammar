use static_regular_grammar::RegularGrammar;

/// URI Authority.
///
/// # Grammar
///
/// ```abnf
/// authority   = [ userinfo "@" ] host [ ":" port ]
///
/// userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
///
/// host        = IP-literal / IPv4address / reg-name
///
/// port        = *DIGIT
/// ```
///
/// ## Host rules
///
/// ```abnf
/// IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
///
/// IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
///
/// IPv6address =                            6( h16 ":" ) ls32
///             /                       "::" 5( h16 ":" ) ls32
///             / [               h16 ] "::" 4( h16 ":" ) ls32
///             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
///             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
///             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
///             / [ *4( h16 ":" ) h16 ] "::"              ls32
///             / [ *5( h16 ":" ) h16 ] "::"              h16
///             / [ *6( h16 ":" ) h16 ] "::"
///
/// ls32        = ( h16 ":" h16 ) / IPv4address
/// ; least-significant 32 bits of address
///
/// h16         = 1*4HEXDIG
/// ; 16 bits of address represented in hexadecimal
///
/// IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
///
/// dec-octet   = DIGIT                 ; 0-9
///             / %x31-39 DIGIT         ; 10-99
///             / "1" 2DIGIT            ; 100-199
///             / "2" %x30-34 DIGIT     ; 200-249
///             / "25" %x30-35          ; 250-255
///
/// reg-name    = *( unreserved / pct-encoded / sub-delims )
/// ```
///
/// ## Misc rules
///
/// ```abnf
/// reserved    = gen-delims / sub-delims
///
/// gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
///
/// sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
///             / "*" / "+" / "," / ";" / "="
///
/// unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
///
/// pct-encoded = "%" HEXDIG HEXDIG
/// ```
#[derive(RegularGrammar, PartialEq, Eq)]
#[cache("target/examples/uri.automaton.cbor")]
#[buffer(AuthorityBuf, derive(PartialEq, Eq))]
pub struct Authority([u8]);

fn main() {
	Authority::new(b"timothee@example.org:12").unwrap();
	AuthorityBuf::new(b"timothee@example.org:12".to_vec()).unwrap();
}
