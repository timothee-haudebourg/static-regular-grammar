use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
	collections::{BTreeMap, BTreeSet, HashMap, HashSet},
	hash::Hash,
};

use btree_range_map::AnyRange;

use crate::charset::CharSet;

pub trait DeterminizeLabel {
	type Range: Clone + Ord;

	type Ranges<'a>: 'a + Iterator<Item = Self::Range>
	where
		Self: 'a;

	type RangeMap<V: Clone + PartialEq>: RangeMap<Self::Range, V>;

	fn ranges(&self) -> Self::Ranges<'_>;
}

pub trait RangeMap<R, V>: Default + IntoIterator<Item = (R, V)> {
	fn update(&mut self, key: R, f: impl Fn(Option<&V>) -> Option<V>);
}

/// Non deterministic state transitions.
pub type Transitions<Q, L = CharSet> = BTreeMap<Option<L>, BTreeSet<Q>>;

/// Non deterministic lexing automaton.
#[derive(Debug, Serialize, Deserialize)]
#[serde(bound = "Q: Ord + Serialize + DeserializeOwned, L: Ord + Serialize + DeserializeOwned")]
pub struct Automaton<Q, L = CharSet> {
	transitions: BTreeMap<Q, Transitions<Q, L>>,
	initial_states: BTreeSet<Q>,
	final_states: BTreeSet<Q>,
}

impl<Q, L> Automaton<Q, L> {
	/// Create a new empty non deterministic automaton.
	pub fn new() -> Self {
		Self {
			transitions: BTreeMap::new(),
			initial_states: BTreeSet::new(),
			final_states: BTreeSet::new(),
		}
	}
}

impl<Q: Ord, L> Automaton<Q, L> {
	pub fn declare_state(&mut self, q: Q) {
		self.transitions.entry(q).or_default();
	}

	pub fn add_initial_state(&mut self, q: Q) -> bool {
		self.initial_states.insert(q)
	}

	pub fn add_final_state(&mut self, q: Q) -> bool {
		self.final_states.insert(q)
	}
}

impl<Q: Ord, L: Ord> Automaton<Q, L> {
	pub fn add(&mut self, source: Q, label: Option<L>, target: Q)
	where
		Q: Clone,
	{
		self.declare_state(target.clone());
		self.transitions
			.entry(source)
			.or_default()
			.entry(label)
			.or_default()
			.insert(target);
	}

	fn modulo_epsilon_state<'a>(&'a self, qs: impl IntoIterator<Item = &'a Q>) -> BTreeSet<&'a Q> {
		let mut states = BTreeSet::new();
		let mut stack: Vec<_> = qs.into_iter().collect();

		while let Some(q) = stack.pop() {
			if states.insert(q) {
				// add states reachable trough epsilon-transitions.
				if let Some(transitions) = self.transitions.get(q) {
					if let Some(epsilon_qs) = transitions.get(&None) {
						for t in epsilon_qs {
							stack.push(t)
						}
					}
				}
			}
		}

		states
	}

	fn determinize_transitions_for(&self, states: &BTreeSet<&Q>) -> BTreeMap<L::Range, BTreeSet<&Q>>
	where
		L: DeterminizeLabel,
	{
		let mut map = L::RangeMap::default();

		for q in states {
			if let Some(transitions) = self.transitions.get(q) {
				for (label, targets) in transitions {
					if let Some(label) = label {
						for range in label.ranges() {
							map.update(
								range.clone(),
								|current_target_states_opt: Option<&BTreeSet<&Q>>| {
									let mut current_target_states = match current_target_states_opt
									{
										Some(current_target_states) => {
											current_target_states.clone()
										}
										None => BTreeSet::new(),
									};

									for q in targets {
										current_target_states
											.extend(self.modulo_epsilon_state(Some(q)));
									}

									Some(current_target_states)
								},
							);
						}
					}
				}
			}
		}

		let mut simplified_map = BTreeMap::new();

		for (range, set) in map {
			simplified_map.insert(range, set);
		}

		simplified_map
	}

	pub fn determinize(&self) -> DetAutomaton<BTreeSet<&Q>, L::Range>
	where
		Q: Hash,
		L: DeterminizeLabel,
	{
		let mut transitions = BTreeMap::new();

		// create the initial deterministic state.
		let initial_state = self.modulo_epsilon_state(&self.initial_states);
		let mut final_states = BTreeSet::new();

		let mut visited_states = HashSet::new();
		let mut stack = vec![initial_state.clone()];
		while let Some(det_q) = stack.pop() {
			if visited_states.insert(det_q.clone()) {
				if det_q.iter().any(|q| self.final_states.contains(q)) {
					final_states.insert(det_q.clone());
				}

				let map = self.determinize_transitions_for(&det_q);

				for next_det_q in map.values() {
					stack.push(next_det_q.clone())
				}

				transitions.insert(det_q, map);
			}
		}

		DetAutomaton {
			initial_state,
			final_states,
			transitions,
		}
	}
}

/// Deterministic epsilon-free automaton.
#[derive(Debug, Serialize, Deserialize)]
#[serde(bound = "Q: Ord + Serialize + DeserializeOwned, L: Ord + Serialize + DeserializeOwned")]
pub struct DetAutomaton<Q, L = AnyRange<char>> {
	initial_state: Q,
	final_states: BTreeSet<Q>,
	transitions: BTreeMap<Q, BTreeMap<L, Q>>,
}

impl<Q, L> DetAutomaton<Q, L> {
	pub fn new(initial_state: Q) -> Self {
		Self {
			initial_state,
			final_states: BTreeSet::new(),
			transitions: BTreeMap::new(),
		}
	}

	pub fn initial_state(&self) -> &Q {
		&self.initial_state
	}

	pub fn transitions(&self) -> &BTreeMap<Q, BTreeMap<L, Q>> {
		&self.transitions
	}
}

impl<Q: Ord, L: Ord> DetAutomaton<Q, L> {
	pub fn is_final_state(&self, q: &Q) -> bool {
		self.final_states.contains(q)
	}

	pub fn add_final_state(&mut self, q: Q) -> bool {
		self.final_states.insert(q)
	}

	pub fn declare_state(&mut self, q: Q) {
		self.transitions.entry(q).or_default();
	}

	pub fn successors(&self, q: &Q) -> DetSuccessors<Q, L> {
		DetSuccessors::new(self.transitions.get(q))
	}

	pub fn add(&mut self, source: Q, label: L, target: Q) {
		self.transitions
			.entry(source)
			.or_default()
			.insert(label, target);
	}

	pub fn partition<P, F>(&self, f: F) -> HashMap<P, BTreeSet<&Q>>
	where
		Q: Ord + Hash + Eq,
		P: Hash + Eq,
		F: Fn(&Q) -> P,
	{
		unsafe {
			self.try_partition::<P, _, std::convert::Infallible>(|q| Ok(f(q)))
				.unwrap_unchecked() // safe because infallible.
		}
	}

	pub fn try_partition<P, F, E>(&self, f: F) -> Result<HashMap<P, BTreeSet<&Q>>, E>
	where
		Q: Ord + Hash + Eq,
		P: Hash + Eq,
		F: Fn(&Q) -> Result<P, E>,
	{
		let mut partition = HashMap::new();
		let mut visited = HashSet::new();
		self.try_partition_from(&self.initial_state, &f, &mut visited, &mut partition)?;
		Ok(partition)
	}

	fn try_partition_from<'a, P, F, E>(
		&'a self,
		q: &'a Q,
		f: &F,
		visited: &mut HashSet<&'a Q>,
		partition: &mut HashMap<P, BTreeSet<&'a Q>>,
	) -> Result<(), E>
	where
		Q: Ord + Hash + Eq,
		P: Hash + Eq,
		F: Fn(&Q) -> Result<P, E>,
	{
		if visited.insert(q) {
			let p = f(q)?;

			partition.entry(p).or_default().insert(q);

			for (_, r) in self.successors(q) {
				self.try_partition_from(r, f, visited, partition)?;
			}
		}

		Ok(())
	}

	/// Minimizes the automaton.
	// Hopcroft's algorithm.
	// https://en.wikipedia.org/wiki/DFA_minimization
	pub fn minimize<'a, P>(&'a self, partition: P) -> DetAutomaton<BTreeSet<&Q>, &L>
	where
		Q: Hash,
		L: Hash,
		P: Iterator<Item = BTreeSet<&'a Q>>,
	{
		let mut partition: BTreeSet<_> = partition.collect();

		let mut working = partition.clone();

		while let Some(a) = working.pop_first() {
			let mut sources_by_label: HashMap<&L, BTreeSet<&Q>> = HashMap::new();

			for (source, targets) in &self.transitions {
				for (label, target) in targets {
					if a.contains(target) {
						if sources_by_label.contains_key(label) {
							let sources = sources_by_label.get_mut(label).unwrap();
							sources.insert(source);
						} else {
							let mut sources = BTreeSet::new();
							sources.insert(source);
							sources_by_label.insert(label, sources);
						}
					}
				}
			}

			for sources in sources_by_label.values() {
				for y in partition.clone() {
					if y.intersection(sources).next().is_some()
						&& y.difference(sources).next().is_some()
					{
						let intersection: BTreeSet<&Q> = y.intersection(sources).cloned().collect();
						let difference: BTreeSet<&Q> = y.difference(sources).cloned().collect();

						if working.contains(&y) {
							working.remove(&y);
							working.insert(intersection.clone());
							working.insert(difference.clone());
						} else if intersection.len() <= difference.len() {
							working.insert(intersection.clone());
						} else {
							working.insert(difference.clone());
						}

						partition.remove(&y);
						partition.insert(intersection);
						partition.insert(difference);
					}
				}
			}
		}

		let mut map = HashMap::new();
		for member in partition {
			for q in &member {
				map.insert(*q, member.clone());
			}
		}

		let mut result = DetAutomaton::new(map[&self.initial_state].clone());
		for q in map.values() {
			result.declare_state(q.clone());
		}

		for (source, transitions) in &self.transitions {
			for (range, target) in transitions {
				result.add(map[source].clone(), range, map[target].clone());
			}
		}

		for q in &self.final_states {
			result.add_final_state(map[q].clone());
		}

		result
	}

	pub fn map<P, M>(
		&self,
		mut f: impl FnMut(&Q) -> P,
		mut g: impl FnMut(&L) -> M,
	) -> DetAutomaton<P, M>
	where
		Q: Hash,
		L: Hash,
		P: Clone + Ord + Hash,
		M: Clone + Ord + Hash,
	{
		let mut map = HashMap::new();
		let mapped_initial_state = f(&self.initial_state);
		map.insert(&self.initial_state, mapped_initial_state.clone());

		let mut label_map = HashMap::new();

		let mut result = DetAutomaton::new(mapped_initial_state);
		for (source, transitions) in &self.transitions {
			let source = map.entry(source).or_insert_with(|| f(source)).clone();

			for (range, target) in transitions {
				let target = map.entry(target).or_insert_with(|| f(target)).clone();
				let range = label_map.entry(range).or_insert_with(|| g(range)).clone();
				result.add(source.clone(), range, target);
			}

			result.declare_state(source)
		}

		for q in &self.final_states {
			result.add_final_state(map.entry(q).or_insert_with(|| f(q)).clone());
		}

		result
	}
}

pub struct DetSuccessors<'a, Q, L> {
	inner: Option<std::collections::btree_map::Iter<'a, L, Q>>,
}

impl<'a, Q, L> DetSuccessors<'a, Q, L> {
	pub fn new(map: Option<&'a BTreeMap<L, Q>>) -> Self {
		Self {
			inner: map.map(|map| map.iter()),
		}
	}
}

impl<'a, Q, L> Iterator for DetSuccessors<'a, Q, L> {
	type Item = (&'a L, &'a Q);

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.as_mut().and_then(|inner| inner.next())
	}
}
