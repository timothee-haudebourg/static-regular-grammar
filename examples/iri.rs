use static_regular_grammar::RegularGrammar;

/// URI Authority.
///
/// # Grammar
///
/// ```abnf
/// iauthority  = [ iuserinfo "@" ] ihost [ ":" port ]
///
/// iuserinfo   = *( iunreserved / pct-encoded / sub-delims / ":" )
///
/// ihost       = IP-literal / IPv4address / ireg-name
///
/// port        = *DIGIT
/// ```
///
/// ## Host rules
///
/// ```abnf
/// IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
///
/// IPvFuture   = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
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
/// ireg-name   = *( iunreserved / pct-encoded / sub-delims )
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
/// iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
///
/// ucschar     = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
///             / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
///             / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
///             / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
///             / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
///             / %xD0000-DFFFD / %xE1000-EFFFD
///
/// pct-encoded = "%" HEXDIG HEXDIG
/// ```
#[derive(RegularGrammar, PartialEq, Eq)]
#[cache("target/examples/iri.automaton.cbor")]
#[sized(AuthorityBuf, derive(PartialEq, Eq))]
pub struct Authority(str);

fn main() {
	Authority::new("timothee@example.org:12").unwrap();
	AuthorityBuf::new("timothee@example.org:12".to_string()).unwrap();
}
