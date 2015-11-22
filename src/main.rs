
use std::collections::HashSet;

#[derive(Debug,Clone)]
pub enum Regex {
    Empty,
    Single(char),
    Or(Box<Regex>, Box<Regex>),
    Then(Box<Regex>, Box<Regex>),
    Star(Box<Regex>),
}

impl Regex {

    pub fn or(&self, s: &Regex) -> Regex {
        Regex::Or(Box::new(self.clone()), Box::new(s.clone()))
    }

    pub fn then(&self, s: &Regex) -> Regex {
        Regex::Then(Box::new(self.clone()), Box::new(s.clone()))
    }

    pub fn star(&self) -> Regex {
        Regex::Star(Box::new(self.clone()))
    }
}

#[derive(Debug,Clone)]
struct Node {
    /// Transitions with first entry None are e-steps
    transitions: Vec<(Option<char>, usize)>,
}

impl Node {
    fn neighbours(&self, a: Option<char>) -> Vec<usize> {
        self.transitions
            .iter()
            .filter(|t| t.0 == a)
            .map(|x| x.1)
            .collect::<Vec<usize>>()
    }

    fn new(ts: Vec<(Option<char>, usize)>) -> Node {
        Node { transitions: ts }
    }
}

#[derive(Debug,Clone)]
pub struct NFA {
    nodes: Vec<Node>,
    start_idx: usize,
    final_idx: usize,
}

impl NFA {

    pub fn empty() -> NFA {
        NFA {
            nodes: vec![Node::new(vec![(None, 1)]), Node::new(vec![])],
            start_idx: 0,
            final_idx: 1,
        }
    }

    pub fn single(a: char) -> NFA {
        NFA {
            nodes: vec![Node::new(vec![(Some(a), 1)]), Node::new(vec![])],
            start_idx: 0,
            final_idx: 1,
        }
    }

    pub fn from_regex(reg: &Regex) -> NFA {
        return match *reg {
            Regex::Empty => Self::empty(),
            Regex::Single(c) => Self::single(c),
            Regex::Or(ref r, ref s) => {
                let nr = Self::from_regex(&*r);
                let ns = Self::from_regex(&*s);
                Self::or(nr, ns)
            },
            Regex::Then(ref r, ref s) => {
                let nr = Self::from_regex(&*r);
                let ns = Self::from_regex(&*s);
                Self::then(nr, ns)
            },
            Regex::Star(ref r) => Self::star(Self::from_regex(&*r))
        };
    }

    fn then(a: NFA, b: NFA) -> NFA {
        let mut nodes = vec![Node::new(vec![]); a.nodes.len() + b.nodes.len() + 2];
        let start_idx = 0;
        let final_idx = nodes.len() - 1;
        nodes[start_idx] = Node::new(vec![
           (None, 1), // e-step to start of embedded copy of a
        ]);
        nodes[final_idx] = Node::new(vec![]);

        Self::embed(&mut nodes, &a, 1, &[a.nodes.len() + 1]);
        Self::embed(&mut nodes, &b, 1 + a.nodes.len(), &[final_idx]);

        NFA {
            nodes: nodes,
            start_idx: start_idx,
            final_idx: final_idx,
        }
    }

    fn or(a: NFA, b: NFA) -> NFA {
        let mut nodes = vec![Node::new(vec![]); a.nodes.len() + b.nodes.len() + 2];
        let start_idx = 0;
        let final_idx = nodes.len() - 1;
        nodes[start_idx] = Node::new(vec![
            (None, 1),                // e-step to start of embedded copy of a
            (None, 1 + a.nodes.len()) // e-step to start of embedded copy of b
        ]);
        nodes[final_idx] = Node::new(vec![]);

        Self::embed(&mut nodes, &a, 1, &[final_idx]);
        Self::embed(&mut nodes, &b, 1 + a.nodes.len(), &[final_idx]);

        NFA {
            nodes: nodes,
            start_idx: start_idx,
            final_idx: final_idx,
        }
    }

    fn star(a: NFA) -> NFA {
        let mut nodes = vec![Node::new(vec![]); a.nodes.len() + 2];
        let start_idx = 0;
        let final_idx = nodes.len() - 1;
        nodes[start_idx] = Node::new(vec![
            (None, 1),          // e-step to start of embedded copy of a
            (None, final_idx)   // e-step to accepting state
        ]);
        nodes[final_idx] = Node::new(vec![]);
    
        Self::embed(&mut nodes, &a, 1, &[final_idx, 0]);
    
        NFA {
            nodes: nodes,
            start_idx: start_idx,
            final_idx: final_idx,
        }
    }

    fn embed(nodes: &mut [Node], sub: &NFA, offset: usize, final_trans: &[usize]) {
        for (i, n) in sub.nodes.iter().enumerate() {
            let mut m = n.clone();
            for p in m.transitions.iter_mut() {
                // update internal pointers
                p.1 += offset;
            }
            if i == sub.final_idx {
                // e-steps from end of embedded NFA
                for &t in final_trans {
                    m.transitions.push((None, t));    
                }
            }
            nodes[i + offset] = m;
        }
    }

    pub fn accepts(&self, xs: &[char]) -> bool {
        let mut states = HashSet::new();
        states.insert(self.start_idx);
        self.epsilon_closure(&mut states);

        for &c in xs.iter() {
            states = self.step(&states, Some(c));
            if states.is_empty() {
                return false;
            }
            self.epsilon_closure(&mut states);
        }

        states.contains(&self.final_idx)
    }

    fn epsilon_closure(&self, states: &mut HashSet<usize>) {
        let mut size = states.len();
        loop {
            let mut new_nodes = self.step(states, None);
            if new_nodes.len() == 0 {
                break;
            }
            for n in new_nodes.into_iter() {
                states.insert(n);
            }
            if states.len() == size {
                break;
            }
            size = states.len();
        }
    }

    fn step(&self, states: &HashSet<usize>, a: Option<char>) -> HashSet<usize> {
        let mut nodes = HashSet::new();
        for s in states.iter() {
            for n in self.nodes[*s].neighbours(a) {
                nodes.insert(n);
            }
        }
        nodes
    }
}

fn main() {

    let r = Regex::Empty;
    let s = r.or(&r).then(&r);
    let t = NFA::single('a');

    println!("{:?}\n{:?}", s, t);
}

mod test {

    use super::{NFA, Regex};

    #[test]
    fn test_nfa_single() {
        let n = NFA::from_regex(&Regex::Single('a'));

        assert!(n.accepts(&['a']));
        assert!(!n.accepts(&['b']));
        assert!(!n.accepts(&['a', 'b']));
    }

    #[test]
    fn test_nfa_or() {
        let a = Regex::Single('a');
        let b = Regex::Single('b');
        let c = Regex::Single('c');
        let r = a.or(&b).or(&c);
        let n = NFA::from_regex(&r);

        assert!(n.accepts(&['a']));
        assert!(n.accepts(&['b']));
        assert!(n.accepts(&['c']));
        assert!(!n.accepts(&['a', 'b']));
    }
    
    #[test]
    fn test_nfa_then() {
        let a = Regex::Single('a');
        let b = Regex::Single('b');
        let r = a.then(&b);
        let n = NFA::from_regex(&r);

        assert!(!n.accepts(&['a']));
        assert!(!n.accepts(&['b']));
        assert!(n.accepts(&['a', 'b']));
    }
    
    #[test]
    fn test_nfa_star() {
        let a = Regex::Single('a');
        let b = Regex::Single('b');
        let r = a.or(&b).star();
        let n = NFA::from_regex(&r);

        assert!(n.accepts(&['a']));
        assert!(n.accepts(&['b']));
        assert!(n.accepts(&['a', 'b', 'a', 'a']));
        assert!(!n.accepts(&['c']));
        assert!(!n.accepts(&['a', 'c']));
    }
}
