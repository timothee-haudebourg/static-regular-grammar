use abnf::types::{Kind, Node, Repeat, TerminalValues};
use std::collections::HashMap;

use crate::{
	token::{TokenRange, TokenSet},
	utils::{automaton::DetAutomaton, scc::Components, Automaton, SccGraph},
	Token,
};

use super::GrammarError;

struct NonTerminal<T: Token> {
	constructors: Vec<BuiltNode<T>>,
}

impl<T: Token> Default for NonTerminal<T> {
	fn default() -> Self {
		Self {
			constructors: Vec::new(),
		}
	}
}

impl<T: Token> NonTerminal<T> {
	fn build_dependencies(&self) -> Vec<usize> {
		let mut result = Vec::new();

		for c in &self.constructors {
			c.collect_dependencies(&mut result)
		}

		result.sort_unstable();
		result.dedup();
		result
	}

	fn is_left_regular(&self, components: &Components<usize>, c: usize) -> bool {
		self.constructors
			.iter()
			.all(|node| node.is_left_regular(components, c))
	}

	fn is_right_regular(&self, components: &Components<usize>, c: usize) -> bool {
		self.constructors
			.iter()
			.all(|node| node.is_right_regular(components, c))
	}
}

/// Terminal created by numerical values.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BuiltTerminalValues<T: Token> {
	// FIXME: Keep base (bin, dec, hex)
	// Relaxed for Unicode support, see <https://github.com/duesee/abnf/pull/3>.
	/// A single value within a range (e.g. `%x01-ff`).
	Range(T::Range),
	/// A terminal defined by a concatenation of values (e.g. `%x0f.f1.ce`).
	Concatenation(Vec<T>),
}

/// A `BuiltNode` enumerates all building blocks in ABNF.
/// Any (built) rule is a composition of `BuiltNode`s.
pub enum BuiltNode<T: Token> {
	/// An alternation, e.g. `A / B / C`.
	Alternatives(Vec<Self>),
	/// A concatenation, e.g. `A B C`.
	Concatenation(Vec<Self>),
	/// A repetition, e.g. `*A`.
	Repetition {
		/// How often ...
		repeat: Repeat,
		/// ... is which node repeated?
		node: Box<Self>,
	},
	/// A non-terminal.
	NonTerminal(usize),
	/// A group, e.g. `(A B)`.
	Group(Box<Self>),
	/// An option, e.g. `[A]`.
	Optional(Box<Self>),
	/// A literal text string/terminal, e.g. `"http"`, `%i"hTtP"` or `%s"http"`.
	String(Vec<T::Set>),
	/// A single value within a range (e.g. `%x01-ff`)
	/// or a terminal defined by a series of values (e.g. `%x0f.f1.ce`).
	TerminalValues(BuiltTerminalValues<T>),
	/// A prose string, i.e. `<good luck implementing this>`.
	Prose(Vec<T>),
}

impl<T: Token> BuiltNode<T> {
	fn from_node(
		name_map: &HashMap<&str, usize>,
		node: &Node,
	) -> Result<BuiltNode<T>, GrammarError> {
		match node {
			Node::Alternatives(nodes) => {
				let mut result = Vec::with_capacity(nodes.len());
				for n in nodes {
					result.push(Self::from_node(name_map, n)?)
				}
				Ok(Self::Alternatives(result))
			}
			Node::Concatenation(nodes) => {
				let mut result = Vec::with_capacity(nodes.len());
				for n in nodes {
					result.push(Self::from_node(name_map, n)?)
				}
				Ok(Self::Concatenation(result))
			}
			Node::Group(n) => Ok(Self::Group(Box::new(Self::from_node(name_map, n)?))),
			Node::Optional(n) => Ok(Self::Optional(Box::new(Self::from_node(name_map, n)?))),
			Node::Prose(p) => {
				let mut string = Vec::with_capacity(p.len());
				for c in p.chars() {
					string.push(T::from_char(c).ok_or(GrammarError::UnsupportedCharToken(c))?)
				}

				Ok(Self::Prose(string))
			}
			Node::Repetition { repeat, node } => Ok(Self::Repetition {
				repeat: repeat.clone(),
				node: Box::new(Self::from_node(name_map, node)?),
			}),
			Node::Rulename(name) => {
				let i = *name_map
					.get(name.as_str())
					.ok_or_else(|| GrammarError::UndefinedNonTerminal(name.clone()))?;
				Ok(Self::NonTerminal(i))
			}
			Node::String(s) => {
				let mut string = Vec::with_capacity(s.value().len());
				for c in s.value().chars() {
					let token = T::from_char(c).ok_or(GrammarError::UnsupportedCharToken(c))?;
					string.push(T::Set::singleton(token, s.is_case_sensitive()))
				}

				Ok(Self::String(string))
			}
			Node::TerminalValues(values) => {
				let result = match values {
					TerminalValues::Range(a, b) => {
						let a = T::from_u32(*a).ok_or(GrammarError::UnsupportedU32Token(*a))?;
						let b = T::from_u32(*b).ok_or(GrammarError::UnsupportedU32Token(*b))?;
						BuiltTerminalValues::Range(T::Range::new(a, b))
					}
					TerminalValues::Concatenation(list) => {
						let mut tokens = Vec::with_capacity(list.len());
						for &i in list {
							let token =
								T::from_u32(i).ok_or(GrammarError::UnsupportedU32Token(i))?;
							tokens.push(token)
						}
						BuiltTerminalValues::Concatenation(tokens)
					}
				};

				Ok(Self::TerminalValues(result))
			}
		}
	}

	fn collect_dependencies(&self, result: &mut Vec<usize>) {
		match self {
			Self::Alternatives(nodes) => {
				for n in nodes {
					n.collect_dependencies(result)
				}
			}
			Self::Concatenation(nodes) => {
				for n in nodes {
					n.collect_dependencies(result)
				}
			}
			Self::Group(n) => n.collect_dependencies(result),
			Self::NonTerminal(i) => result.push(*i),
			Self::Optional(n) => n.collect_dependencies(result),
			Self::Repetition { node, .. } => node.collect_dependencies(result),
			_ => (),
		}
	}

	fn is_non_recursive(&self, components: &Components<usize>, c: usize) -> bool {
		match self {
			Self::Alternatives(nodes) => nodes.iter().all(|n| n.is_non_recursive(components, c)),
			Self::Concatenation(nodes) => nodes.iter().all(|n| n.is_non_recursive(components, c)),
			Self::Group(g) => g.is_non_recursive(components, c),
			Self::NonTerminal(i) => components.vertex_component(i).unwrap() != c,
			Self::Optional(n) => n.is_non_recursive(components, c),
			Self::Repetition { node, .. } => node.is_non_recursive(components, c),
			_ => true,
		}
	}

	fn is_left_regular(&self, components: &Components<usize>, c: usize) -> bool {
		match self {
			Self::Alternatives(nodes) => nodes.iter().all(|n| n.is_left_regular(components, c)),
			Self::Concatenation(nodes) => match nodes.split_first() {
				Some((n, rest)) => {
					n.is_left_regular(components, c)
						&& rest.iter().all(|n| n.is_non_recursive(components, c))
				}
				None => true,
			},
			Self::Group(n) => n.is_left_regular(components, c),
			Self::Optional(n) => n.is_left_regular(components, c),
			Self::Repetition { node, .. } => node.is_left_regular(components, c),
			_ => true,
		}
	}

	fn is_right_regular(&self, components: &Components<usize>, c: usize) -> bool {
		match self {
			Self::Alternatives(nodes) => nodes.iter().all(|n| n.is_left_regular(components, c)),
			Self::Concatenation(nodes) => match nodes.split_last() {
				Some((n, rest)) => {
					n.is_right_regular(components, c)
						&& rest.iter().all(|n| n.is_non_recursive(components, c))
				}
				None => true,
			},
			Self::Group(n) => n.is_left_regular(components, c),
			Self::Optional(n) => n.is_left_regular(components, c),
			Self::Repetition { node, .. } => node.is_left_regular(components, c),
			_ => true,
		}
	}
}

struct DependencyGraph {
	nodes: Vec<Vec<usize>>,
}

impl DependencyGraph {
	fn new<T: Token>(non_terminals: &[NonTerminal<T>]) -> Self {
		Self {
			nodes: non_terminals
				.iter()
				.map(|nt| nt.build_dependencies())
				.collect(),
		}
	}
}

impl SccGraph for DependencyGraph {
	type Vertex = usize;

	type Vertices<'a> = std::ops::Range<usize>;

	type Successors<'a> = std::iter::Copied<std::slice::Iter<'a, usize>>;

	fn vertices(&self) -> Self::Vertices<'_> {
		0..self.nodes.len()
	}

	fn successors(&self, v: Self::Vertex) -> Self::Successors<'_> {
		self.nodes[v].iter().copied()
	}
}

struct StateBuilder(u32);

impl StateBuilder {
	fn next<L>(&mut self, aut: &mut Automaton<u32, L>) -> u32 {
		let q = self.0;
		self.0 += 1;
		aut.declare_state(q);
		q
	}

	fn next_pair<L>(&mut self, aut: &mut Automaton<u32, L>) -> (u32, u32) {
		let a = self.next(aut);
		let b = self.next(aut);
		(a, b)
	}
}

pub struct Grammar<T: Token> {
	non_terminals: Vec<NonTerminal<T>>,
	entry_point: usize,
	components: Components<usize>,
}

// fn no_core_rules<'a, T: Token>() -> (Vec<NonTerminal<T>>, HashMap<&'a str, usize>) {
// 	(Vec::new(), HashMap::new())
// }

/// Defines the core ABNF rules.
///
/// ```abnf
/// ALPHA  =  %x41-5A / %x61-7A
///               ; A-Z / a-z
///
/// BIT    =  "0" / "1"
///
/// CHAR   =  %x01-7F
///               ; any 7-bit US-ASCII character,
///               ;  excluding NUL
///
/// CR     = %x0D ; carriage return
///
/// CRLF   = CR LF
///               ; Internet standard newline
///
/// CTL    = %x00-1F / %x7F
///               ; controls
///
/// DIGIT  = %x30-39
///               ; 0-9
///
/// DQUOTE = %x22 ; " (Double Quote)
///
/// HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
///
/// HTAB   = %x09 ; horizontal tab
///
/// LF     = %x0A ; linefeed
///
/// LWSP   = *(WSP / CRLF WSP)
///               ; Use of this linear-white-space rule
///               ;  permits lines containing only white
///               ;  space that are no longer legal in
///               ;  mail headers and have caused
///               ;  interoperability problems in other
///               ;  contexts.
///               ; Do not use when defining mail
///               ;  headers and use with caution in
///               ;  other contexts.
///
/// OCTET  = %x00-FF
///               ; 8 bits of data
///
/// SP     =  %x20
///
/// VCHAR  =  %x21-7E
///               ; visible (printing) characters
///
/// WSP    =  SP / HTAB
///               ; white space
/// ```
fn define_core_rules<'a, T: Token>() -> (Vec<NonTerminal<T>>, HashMap<&'a str, usize>) {
	let mut non_terminals = vec![
		NonTerminal::default(), //  0: ALPHA
		NonTerminal::default(), //  1: BIT
		NonTerminal::default(), //  2: CHAR
		NonTerminal::default(), //  3: CR
		NonTerminal::default(), //  4: CRLF
		NonTerminal::default(), //  5: CTL
		NonTerminal::default(), //  6: DIGIT
		NonTerminal::default(), //  7: DQUOTE
		NonTerminal::default(), //  8: HEXDIG
		NonTerminal::default(), //  9: HTAB
		NonTerminal::default(), // 10: LF
		NonTerminal::default(), // 11: LWSP
		NonTerminal::default(), // 12: OCTET
		NonTerminal::default(), // 13: SP
		NonTerminal::default(), // 14: VCHAR
		NonTerminal::default(), // 15: WSP
	];

	// ALPHA
	non_terminals[0]
		.constructors
		.push(BuiltNode::Alternatives(vec![
			BuiltNode::TerminalValues(BuiltTerminalValues::Range(T::Range::new(
				T::from_u8(b'A'),
				T::from_u8(b'Z'),
			))),
			BuiltNode::TerminalValues(BuiltTerminalValues::Range(T::Range::new(
				T::from_u8(b'a'),
				T::from_u8(b'z'),
			))),
		]));

	// BIT
	non_terminals[1]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::new(T::from_u8(b'0'), T::from_u8(b'1')),
		)));

	// CHAR
	non_terminals[2]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::new(T::from_u8(0x01), T::from_u8(0x7f)),
		)));

	// CR
	non_terminals[3]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::from_u8(0x0d),
		)));

	// CRLF
	non_terminals[4]
		.constructors
		.push(BuiltNode::Concatenation(vec![
			BuiltNode::NonTerminal(3),  // CR
			BuiltNode::NonTerminal(10), // LF
		]));

	// CTL
	non_terminals[5]
		.constructors
		.push(BuiltNode::Alternatives(vec![
			BuiltNode::TerminalValues(BuiltTerminalValues::Range(T::Range::new(
				T::from_u8(0x00),
				T::from_u8(0x1f),
			))),
			BuiltNode::TerminalValues(BuiltTerminalValues::Range(T::Range::from_u8(0x7f))),
		]));

	// DIGIT
	non_terminals[6]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::new(T::from_u8(b'0'), T::from_u8(b'9')),
		)));

	// DQUOTE
	non_terminals[7]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::from_u8(b'"'),
		)));

	// HEXDIG
	non_terminals[8]
		.constructors
		.push(BuiltNode::Alternatives(vec![
			BuiltNode::NonTerminal(6), // DIGIT
			BuiltNode::TerminalValues(BuiltTerminalValues::Range(T::Range::new(
				T::from_u8(b'A'),
				T::from_u8(b'F'),
			))),
			BuiltNode::TerminalValues(BuiltTerminalValues::Range(T::Range::new(
				T::from_u8(b'a'),
				T::from_u8(b'f'),
			))),
		]));

	// HTAB
	non_terminals[9]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::from_u8(0x09),
		)));

	// LF
	non_terminals[10]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::from_u8(0x0a),
		)));

	// LWSP
	non_terminals[11].constructors.push(BuiltNode::Repetition {
		repeat: Repeat::Variable {
			min: None,
			max: None,
		},
		node: Box::new(BuiltNode::Alternatives(vec![
			BuiltNode::NonTerminal(15), // WSP
			BuiltNode::Concatenation(vec![
				BuiltNode::NonTerminal(4),  // CRLF
				BuiltNode::NonTerminal(15), // WSP
			]),
		])),
	});

	// OCTET
	non_terminals[12]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::new(T::from_u8(0x00), T::from_u8(0xff)),
		)));

	// SP
	non_terminals[13]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::from_u8(0x20),
		)));

	// VCHAR
	non_terminals[14]
		.constructors
		.push(BuiltNode::TerminalValues(BuiltTerminalValues::Range(
			T::Range::new(T::from_u8(0x21), T::from_u8(0x7e)),
		)));

	// WSP
	non_terminals[15]
		.constructors
		.push(BuiltNode::Alternatives(vec![
			BuiltNode::NonTerminal(13), // SP
			BuiltNode::NonTerminal(9),  // HTAB
		]));

	let mut by_name = HashMap::new();
	by_name.insert("ALPHA", 0);
	by_name.insert("BIT", 1);
	by_name.insert("CHAR", 2);
	by_name.insert("CR", 3);
	by_name.insert("CRLF", 4);
	by_name.insert("CTL", 5);
	by_name.insert("DIGIT", 6);
	by_name.insert("DQUOTE", 7);
	by_name.insert("HEXDIG", 8);
	by_name.insert("HTAB", 9);
	by_name.insert("LF", 10);
	by_name.insert("LWSP", 11);
	by_name.insert("OCTET", 12);
	by_name.insert("SP", 13);
	by_name.insert("VCHAR", 14);
	by_name.insert("WSP", 15);

	(non_terminals, by_name)
}

impl<T: Token> Grammar<T> {
	pub fn new(data: String, entry_point: Option<&str>) -> Result<Self, GrammarError> {
		let rules = abnf::rulelist(&data).map_err(GrammarError::Abnf)?;

		let (mut non_terminals, mut by_name) = define_core_rules::<T>();
		// let (mut non_terminals, mut by_name) = no_core_rules::<T>();
		let mut entry_point_index = None;

		for rule in &rules {
			if rule.kind() == Kind::Basic {
				let i = non_terminals.len();

				if entry_point_index.is_none() {
					entry_point_index = Some(i)
				}

				non_terminals.push(NonTerminal::default());
				if by_name.insert(rule.name(), i).is_some() {
					return Err(GrammarError::RedefinedNonTerminal(rule.name().to_string()));
				}
			}
		}

		for rule in &rules {
			let i = *by_name
				.get(rule.name())
				.ok_or_else(|| GrammarError::UndefinedNonTerminal(rule.name().to_string()))?;
			let node = BuiltNode::from_node(&by_name, rule.node())?;
			non_terminals[i].constructors.push(node);
		}

		if let Some(name) = entry_point {
			let i = *by_name
				.get(name)
				.ok_or_else(|| GrammarError::UndefinedNonTerminal(name.to_string()))?;
			entry_point_index = Some(i)
		}

		Self::from_non_terminals(
			non_terminals,
			entry_point_index.ok_or(GrammarError::NoEntryPoint)?,
		)
	}

	fn from_non_terminals(
		non_terminals: Vec<NonTerminal<T>>,
		entry_point: usize,
	) -> Result<Self, GrammarError> {
		let deps = DependencyGraph::new(&non_terminals);
		let components = deps.strongly_connected_components();

		fn is_left_regular<T: Token>(
			non_terminals: &[NonTerminal<T>],
			components: &Components<usize>,
			c: usize,
		) -> bool {
			let component = components.get(c).unwrap();
			component
				.iter()
				.all(|&i| non_terminals[i].is_left_regular(components, c))
		}

		fn is_right_regular<T: Token>(
			non_terminals: &[NonTerminal<T>],
			components: &Components<usize>,
			c: usize,
		) -> bool {
			let component = components.get(c).unwrap();
			component
				.iter()
				.all(|&i| non_terminals[i].is_right_regular(components, c))
		}

		// check regularity.
		for c in 0..components.len() {
			// only recursive components needs to be checked.
			if components.successors(c).unwrap().any(|s| s == c) {
				// check component regularity.
				if !is_left_regular(&non_terminals, &components, c)
					&& !is_right_regular(&non_terminals, &components, c)
				{
					return Err(GrammarError::NonRegular);
				}
			}
		}

		Ok(Self {
			non_terminals,
			entry_point,
			components,
		})
	}

	fn build_from(
		&self,
		aut: &mut Automaton<u32, T::Set>,
		states: &mut StateBuilder,
		i: usize,
	) -> (u32, u32) {
		let c = self.components.vertex_component(&i).unwrap();
		self.build_component_from(aut, states, c, i)
	}

	fn build_component_from(
		&self,
		aut: &mut Automaton<u32, T::Set>,
		states: &mut StateBuilder,
		c: usize,
		i: usize,
	) -> (u32, u32) {
		let component = self.components.get(c).unwrap();
		let component_states: HashMap<usize, (u32, u32)> = component
			.iter()
			.map(|&i| (i, (states.next(aut), states.next(aut))))
			.collect();

		for &i in component {
			let (a, b) = component_states[&i];
			let nt = &self.non_terminals[i];
			for node in &nt.constructors {
				let (n_a, n_b) = self.build_node(aut, states, &component_states, node);
				aut.add(a, None, n_a);
				aut.add(n_b, None, b);
			}
		}

		component_states[&i]
	}

	fn build_node(
		&self,
		aut: &mut Automaton<u32, T::Set>,
		states: &mut StateBuilder,
		component_states: &HashMap<usize, (u32, u32)>,
		node: &BuiltNode<T>,
	) -> (u32, u32) {
		match node {
			BuiltNode::Alternatives(nodes) => {
				let (a, b) = states.next_pair(aut);
				for n in nodes {
					let (n_a, n_b) = self.build_node(aut, states, component_states, n);
					aut.add(a, None, n_a);
					aut.add(n_b, None, b);
				}
				(a, b)
			}
			BuiltNode::Concatenation(nodes) => {
				let a = states.next(aut);
				let mut b = a;
				for n in nodes {
					let (n_a, n_b) = self.build_node(aut, states, component_states, n);
					aut.add(b, None, n_a);
					b = n_b
				}
				(a, b)
			}
			BuiltNode::Group(n) => self.build_node(aut, states, component_states, n),
			BuiltNode::NonTerminal(i) => match component_states.get(i).copied() {
				Some(p) => p,
				None => self.build_from(aut, states, *i),
			},
			BuiltNode::Optional(n) => {
				let (a, b) = states.next_pair(aut);
				aut.add(a, None, b);
				let (n_a, n_b) = self.build_node(aut, states, component_states, n);
				aut.add(a, None, n_a);
				aut.add(n_b, None, b);
				(a, b)
			}
			BuiltNode::Prose(prose) => {
				let a = states.next(aut);
				let mut b = a;
				for token in prose {
					let c = states.next(aut);
					aut.add(b, Some(T::Set::singleton(*token, true)), c);
					b = c;
				}
				(a, b)
			}
			BuiltNode::Repetition { repeat, node: n } => {
				let (min, max) = match repeat {
					Repeat::Specific(n) => (*n, *n),
					Repeat::Variable { min, max } => (min.unwrap_or(0), max.unwrap_or(usize::MAX)),
				};

				self.build_repeat(aut, states, component_states, n, min, max)
			}
			BuiltNode::String(string) => {
				let a = states.next(aut);
				let mut b = a;
				for tokens in string {
					let c = states.next(aut);
					aut.add(b, Some(tokens.clone()), c);
					b = c;
				}
				(a, b)
			}
			BuiltNode::TerminalValues(values) => match values {
				BuiltTerminalValues::Range(range) => {
					let (a, b) = states.next_pair(aut);
					aut.add(a, Some(T::Set::from_range(*range)), b);
					(a, b)
				}
				BuiltTerminalValues::Concatenation(tokens) => {
					let a = states.next(aut);
					let mut b = a;
					for token in tokens {
						let c = states.next(aut);
						aut.add(b, Some(T::Set::singleton(*token, true)), c);
						b = c;
					}
					(a, b)
				}
			},
		}
	}

	fn build_repeat(
		&self,
		aut: &mut Automaton<u32, T::Set>,
		states: &mut StateBuilder,
		component_states: &HashMap<usize, (u32, u32)>,
		node: &BuiltNode<T>,
		min: usize,
		max: usize,
	) -> (u32, u32) {
		if max < min {
			states.next_pair(aut)
		} else if max == 0 {
			let a = states.next(aut);
			(a, a)
		} else if min > 0 {
			let (a, c0) = self.build_node(aut, states, component_states, node);
			let (c1, b) = self.build_repeat(
				aut,
				states,
				component_states,
				node,
				min - 1,
				if max == usize::MAX {
					usize::MAX
				} else {
					max - 1
				},
			);
			aut.add(c0, None, c1);
			(a, b)
		} else if max < usize::MAX {
			let (a, c0) = self.build_node(aut, states, component_states, node);
			let (c1, b) = self.build_repeat(aut, states, component_states, node, 0, max - 1);
			aut.add(c0, None, c1);
			aut.add(a, None, b);
			(a, b)
		} else {
			let (a, b) = self.build_node(aut, states, component_states, node);
			aut.add(a, None, b);
			aut.add(b, None, a);
			(a, b)
		}
	}

	/// Builds a non-deterministic automaton from a regular grammar.
	fn build_nd_automaton(&self) -> Automaton<u32, T::Set> {
		let mut aut = Automaton::new();
		let mut states = StateBuilder(0);
		let (a, b) = self.build_from(&mut aut, &mut states, self.entry_point);
		aut.add_initial_state(a);
		aut.add_final_state(b);
		aut
	}

	pub fn build_automaton(&self) -> DetAutomaton<u32, T::Set> {
		let mut count = 0;
		let nd_aut = self.build_nd_automaton();

		let det_aut = nd_aut.determinize();

		let simple_aut = DetAutomaton::map(
			&det_aut,
			|_| {
				let i = count;
				count += 1;
				i
			},
			|label: &T::Set| label.clone(),
		);

		let partition = DetAutomaton::partition(&simple_aut, |q| simple_aut.is_final_state(q));
		let minimized = simple_aut.minimize(partition.into_values());

		let mut count = 0;
		DetAutomaton::map(
			&minimized,
			|_| {
				let i = count;
				count += 1;
				i
			},
			|label: &T::Set| label.clone(),
		)
	}
}
