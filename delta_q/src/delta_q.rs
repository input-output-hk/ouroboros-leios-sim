use crate::{CDFError, CDF};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Display},
    str::FromStr,
};

#[derive(Debug, PartialEq)]
pub enum DeltaQError {
    CDFError(CDFError),
    NameError(String),
    BlackBox,
}

impl std::error::Error for DeltaQError {}

impl Display for DeltaQError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeltaQError::CDFError(e) => write!(f, "CDF error: {}", e),
            DeltaQError::NameError(name) => write!(f, "Name error: {}", name),
            DeltaQError::BlackBox => write!(f, "Black box encountered"),
        }
    }
}

impl From<CDFError> for DeltaQError {
    fn from(e: CDFError) -> DeltaQError {
        DeltaQError::CDFError(e)
    }
}

#[derive(Debug, Clone, PartialEq, Default, serde::Serialize, serde::Deserialize)]
#[serde(from = "BTreeMap<String, DeltaQ>", into = "BTreeMap<String, DeltaQ>")]
pub struct EvaluationContext {
    ctx: BTreeMap<String, (DeltaQ, Option<CDF>)>,
    deps: BTreeMap<String, BTreeSet<String>>,
}

impl EvaluationContext {
    pub fn put(&mut self, name: String, delta_q: DeltaQ) {
        // first remove all computed values that depend on this name
        let mut to_remove = vec![name.clone()];
        while let Some(name) = to_remove.pop() {
            if self.ctx.get_mut(&name).and_then(|x| x.1.take()).is_some() {
                tracing::info!("Removing computed value for {}", name);
                for (k, v) in self.deps.iter() {
                    if v.contains(&name) {
                        to_remove.push(k.clone());
                    }
                }
            }
        }
        self.deps.insert(name.clone(), delta_q.deps());
        self.ctx.insert(name, (delta_q, None));
    }

    pub fn remove(&mut self, name: &str) -> Option<DeltaQ> {
        // first remove all computed values that depend on this name
        let mut to_remove = vec![name.to_owned()];
        while let Some(name) = to_remove.pop() {
            if self.ctx.get_mut(&name).and_then(|x| x.1.take()).is_some() {
                tracing::info!("Removing computed value for {}", name);
                for (k, v) in self.deps.iter() {
                    if v.contains(&name) {
                        to_remove.push(k.clone());
                    }
                }
            }
        }
        self.deps.remove(name);
        self.ctx.remove(name).map(|(dq, _)| dq)
    }

    pub fn get(&self, name: &str) -> Option<&DeltaQ> {
        self.ctx.get(name).map(|(dq, _)| dq)
    }

    pub fn eval(&mut self, name: &str) -> Result<CDF, DeltaQError> {
        DeltaQ::name(name).eval(self)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &DeltaQ)> {
        self.ctx.iter().map(|(k, (v, _))| (k, v))
    }
}

impl From<BTreeMap<String, DeltaQ>> for EvaluationContext {
    fn from(value: BTreeMap<String, DeltaQ>) -> Self {
        let deps = value.iter().map(|(k, v)| (k.clone(), v.deps())).collect();
        Self {
            ctx: value.into_iter().map(|(k, v)| (k, (v, None))).collect(),
            deps,
        }
    }
}

impl Into<BTreeMap<String, DeltaQ>> for EvaluationContext {
    fn into(self) -> BTreeMap<String, DeltaQ> {
        self.ctx.into_iter().map(|(k, (v, _))| (k, v)).collect()
    }
}

/// A DeltaQ is a representation of a probability distribution that can be
/// manipulated in various ways.
///
/// The Display implementation prints out the expression using the syntax from the paper:
/// - Names are printed as-is.
/// - CDFs are printed as-is.
/// - Sequences are printed as `A ->- B`.
/// - Choices are printed as `A a<>b B`.
/// - Universal quantifications are printed as `all(A|B)`.
/// - Existential quantifications are printed as `some(A|B)`.
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum DeltaQ {
    /// Un unelaborated and unknown DeltaQ.
    BlackBox,
    /// A named DeltaQ that can be referenced elsewhere.
    Name(String),
    /// A CDF that is used as a DeltaQ.
    CDF(CDF),
    /// The convolution of two DeltaQs, describing the sequential execution of two outcomes.
    Seq(Box<DeltaQ>, Box<DeltaQ>),
    /// A choice between two DeltaQs (i.e. their outcomes), with a given weight of each.
    Choice(Box<DeltaQ>, f32, Box<DeltaQ>, f32),
    /// A DeltaQ that is the result of a universal quantification over two DeltaQs,
    /// meaning that both outcomes must occur.
    ForAll(Box<DeltaQ>, Box<DeltaQ>),
    /// A DeltaQ that is the result of an existential quantification over two DeltaQs,
    /// meaning that at least one of the outcomes must occur.
    ForSome(Box<DeltaQ>, Box<DeltaQ>),
}

impl Display for DeltaQ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(f, false)
    }
}

impl FromStr for DeltaQ {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        crate::parser::parse(s)
    }
}

impl DeltaQ {
    /// Create a new DeltaQ from a name, referencing a variable.
    pub fn name(name: &str) -> DeltaQ {
        DeltaQ::Name(name.to_string())
    }

    /// Create a new DeltaQ from a CDF.
    pub fn cdf(cdf: CDF) -> DeltaQ {
        DeltaQ::CDF(cdf)
    }

    /// Create a new DeltaQ from the convolution of two DeltaQs.
    pub fn seq(first: DeltaQ, second: DeltaQ) -> DeltaQ {
        DeltaQ::Seq(Box::new(first), Box::new(second))
    }

    /// Create a new DeltaQ from a choice between two DeltaQs.
    pub fn choice(first: DeltaQ, first_weight: f32, second: DeltaQ, second_weight: f32) -> DeltaQ {
        DeltaQ::Choice(
            Box::new(first),
            first_weight,
            Box::new(second),
            second_weight,
        )
    }

    /// Create a new DeltaQ from a universal quantification over two DeltaQs.
    pub fn for_all(first: DeltaQ, second: DeltaQ) -> DeltaQ {
        DeltaQ::ForAll(Box::new(first), Box::new(second))
    }

    /// Create a new DeltaQ from an existential quantification over two DeltaQs.
    pub fn for_some(first: DeltaQ, second: DeltaQ) -> DeltaQ {
        DeltaQ::ForSome(Box::new(first), Box::new(second))
    }

    pub fn deps(&self) -> BTreeSet<String> {
        match self {
            DeltaQ::BlackBox => BTreeSet::new(),
            DeltaQ::Name(name) => {
                let mut deps = BTreeSet::new();
                deps.insert(name.clone());
                deps
            }
            DeltaQ::CDF(_) => BTreeSet::new(),
            DeltaQ::Seq(first, second) => {
                let mut deps = first.deps();
                deps.extend(second.deps());
                deps
            }
            DeltaQ::Choice(first, _, second, _) => {
                let mut deps = first.deps();
                deps.extend(second.deps());
                deps
            }
            DeltaQ::ForAll(first, second) => {
                let mut deps = first.deps();
                deps.extend(second.deps());
                deps
            }
            DeltaQ::ForSome(first, second) => {
                let mut deps = first.deps();
                deps.extend(second.deps());
                deps
            }
        }
    }

    fn display(&self, f: &mut fmt::Formatter<'_>, parens: bool) -> fmt::Result {
        match self {
            DeltaQ::BlackBox => {
                write!(f, "BB")
            }
            DeltaQ::Name(name) => {
                write!(f, "{}", name)
            }
            DeltaQ::CDF(cdf) => {
                write!(f, "{}", cdf)
            }
            DeltaQ::Seq(first, second) => {
                if parens {
                    write!(f, "(")?;
                }
                first.display(f, true)?;
                write!(f, " ->- ")?;
                second.display(f, true)?;
                if parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
            DeltaQ::Choice(first, first_weight, second, second_weight) => {
                if parens {
                    write!(f, "(")?;
                }
                first.display(f, true)?;
                write!(f, " {}<>{} ", first_weight, second_weight)?;
                second.display(f, true)?;
                if parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
            DeltaQ::ForAll(first, second) => {
                write!(f, "all({} | {})", first, second)
            }
            DeltaQ::ForSome(first, second) => {
                write!(f, "some({} | {})", first, second)
            }
        }
    }

    pub fn eval(&self, ctx: &mut EvaluationContext) -> Result<CDF, DeltaQError> {
        match self {
            DeltaQ::BlackBox => Err(DeltaQError::BlackBox),
            DeltaQ::Name(n) => {
                if let Some((_, Some(cdf))) = ctx.ctx.get(n) {
                    Ok(cdf.clone())
                } else if let Some((dq, _)) = ctx.ctx.remove(n) {
                    match dq.eval(ctx) {
                        Ok(cdf) => {
                            ctx.ctx.insert(n.to_owned(), (dq, Some(cdf.clone())));
                            Ok(cdf)
                        }
                        Err(e) => {
                            ctx.ctx.insert(n.to_owned(), (dq, None));
                            Err(e)
                        }
                    }
                } else {
                    Err(DeltaQError::NameError(n.to_owned()))
                }
            }
            DeltaQ::CDF(cdf) => Ok(cdf.clone()),
            DeltaQ::Seq(first, second) => {
                let first_cdf = first.eval(ctx)?;
                let second_cdf = second.eval(ctx)?;
                first_cdf
                    .convolve(&second_cdf)
                    .map_err(DeltaQError::CDFError)
            }
            DeltaQ::Choice(first, first_fraction, second, second_fraction) => {
                let first_cdf = first.eval(ctx)?;
                let second_cdf = second.eval(ctx)?;
                first_cdf
                    .choice(
                        *first_fraction / (*first_fraction + *second_fraction),
                        &second_cdf,
                    )
                    .map_err(DeltaQError::CDFError)
            }
            DeltaQ::ForAll(first, second) => {
                let first_cdf = first.eval(ctx)?;
                let second_cdf = second.eval(ctx)?;
                first_cdf
                    .for_all(&second_cdf)
                    .map_err(DeltaQError::CDFError)
            }
            DeltaQ::ForSome(first, second) => {
                let first_cdf = first.eval(ctx)?;
                let second_cdf = second.eval(ctx)?;
                first_cdf
                    .for_some(&second_cdf)
                    .map_err(DeltaQError::CDFError)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use maplit::btreemap;

    #[test]
    fn test_display_name() {
        let dq = DeltaQ::name("A");
        assert_eq!(dq.to_string(), "A");
        assert_eq!(dq, "A".parse().unwrap());
    }

    #[test]
    fn test_display_cdf() {
        let cdf = CDF::new(&[0.0, 0.2, 0.9], 1.0).unwrap();
        let dq = DeltaQ::cdf(cdf.clone());
        assert_eq!(dq.to_string(), "CDF[(1, 0.2), (2, 0.9)]");
        assert_eq!(dq, "CDF[(1, 0.2), (2, 0.9)]".parse().unwrap());
    }

    #[test]
    fn test_display_seq() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let seq = DeltaQ::seq(dq1, dq2);
        assert_eq!(seq.to_string(), "A ->- B");
        assert_eq!(seq, "A ->- B".parse().unwrap());
    }

    #[test]
    fn test_display_choice() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let choice = DeltaQ::choice(dq1, 0.3, dq2, 0.7);
        assert_eq!(choice.to_string(), "A 0.3<>0.7 B");
        assert_eq!(choice, "A 0.3<>0.7 B".parse().unwrap());
    }

    #[test]
    fn test_display_for_all() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let for_all = DeltaQ::for_all(dq1, dq2);
        assert_eq!(for_all.to_string(), "all(A | B)");
        assert_eq!(for_all, "all(A | B)".parse().unwrap());
    }

    #[test]
    fn test_display_for_some() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let for_some = DeltaQ::for_some(dq1, dq2);
        assert_eq!(for_some.to_string(), "some(A | B)");
        assert_eq!(for_some, "some(A | B)".parse().unwrap());
    }

    #[test]
    fn test_display_nested_seq() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let dq3 = DeltaQ::name("C");
        let nested_seq = DeltaQ::seq(DeltaQ::seq(dq1, dq2), dq3);
        assert_eq!(nested_seq.to_string(), "(A ->- B) ->- C");
        assert_eq!(nested_seq, "(A ->- B) ->- C".parse().unwrap());
        assert_eq!(nested_seq, "A ->- B ->- C".parse().unwrap());
    }

    #[test]
    fn test_display_nested_choice() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let dq3 = DeltaQ::name("C");
        let nested_choice = DeltaQ::choice(DeltaQ::choice(dq1, 0.3, dq2, 0.7), 0.5, dq3, 0.5);
        assert_eq!(nested_choice.to_string(), "(A 0.3<>0.7 B) 0.5<>0.5 C");
        assert_eq!(nested_choice, "(A 0.3<>0.7 B) 0.5<>0.5 C".parse().unwrap());
        assert_eq!(nested_choice, "A 0.3<>0.7 B 0.5<>0.5 C".parse().unwrap());
    }

    #[test]
    fn test_display_nested_for_all() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let dq3 = DeltaQ::name("C");
        let dq4 = DeltaQ::name("D");
        let nested_for_all = DeltaQ::for_all(DeltaQ::for_all(dq1, dq2), DeltaQ::seq(dq3, dq4));
        assert_eq!(nested_for_all.to_string(), "all(all(A | B) | C ->- D)");
        assert_eq!(nested_for_all, "all(all(A | B) | C ->- D)".parse().unwrap());
    }

    #[test]
    fn test_display_nested_for_some() {
        let dq1 = DeltaQ::name("A");
        let dq2 = DeltaQ::name("B");
        let dq3 = DeltaQ::name("C");
        let dq4 = DeltaQ::name("D");
        let nested_for_some = DeltaQ::for_some(
            DeltaQ::for_some(dq1, dq2),
            DeltaQ::choice(dq3, 1.0, dq4, 2.0),
        );
        assert_eq!(nested_for_some.to_string(), "some(some(A | B) | C 1<>2 D)");
        assert_eq!(
            nested_for_some,
            "some(some(A | B) | C 1<>2 D)".parse().unwrap()
        );
    }

    #[test]
    fn test_scenario_from_paper_64k() {
        let ctx = btreemap! {
            "single".to_owned() =>
                DeltaQ::cdf(CDF::step(
                    &[(0.024, 1.0 / 3.0), (0.143, 2.0 / 3.0), (0.531, 1.0)],
                )
                .unwrap()),
            "model2".to_owned() =>
                DeltaQ::choice(
                    DeltaQ::name("single"),
                    1.0,
                    DeltaQ::seq(DeltaQ::name("single"), DeltaQ::name("single")),
                    100.0,
                ),
            "model3".to_owned() =>
                DeltaQ::choice(
                    DeltaQ::name("single"),
                    1.0,
                    DeltaQ::seq(DeltaQ::name("single"), DeltaQ::name("model2")),
                    100.0,
                ),
            "model4".to_owned() =>
                DeltaQ::choice(
                    DeltaQ::name("single"),
                    1.0,
                    DeltaQ::seq(DeltaQ::name("single"), DeltaQ::name("model3")),
                    100.0,
                ),
            "model5".to_owned() =>
                DeltaQ::choice(
                    DeltaQ::name("single"),
                    1.0,
                    DeltaQ::seq(DeltaQ::name("single"), DeltaQ::name("model4")),
                    100.0,
                ),
        };
        let result = DeltaQ::name("model5").eval(&mut ctx.into()).unwrap();
        assert_eq!(result.to_string(), "CDF[(0.024, 0.0033), (0.048, 0.00439), (0.072, 0.00475), (0.096, 0.00487), (0.12, 0.00882), (0.143, 0.01212), (0.167, 0.0143), (0.191, 0.01538), (0.215, 0.0155), (0.215, 0.01585), (0.239, 0.02376), (0.239, 0.03563), (0.286, 0.03672), (0.31, 0.03779), (0.334, 0.03815), (0.334, 0.03851), (0.358, 0.05037), (0.358, 0.06619), (0.358, 0.07805), (0.429, 0.07841), (0.453, 0.07889), (0.477, 0.10657), (0.477, 0.11843), (0.531, 0.12173), (0.555, 0.12391), (0.572, 0.12403), (0.579, 0.12511), (0.596, 0.14488), (0.603, 0.14524), (0.603, 0.14536), (0.627, 0.15722), (0.627, 0.16513), (0.674, 0.16731), (0.698, 0.16947), (0.715, 0.17342), (0.722, 0.17484), (0.746, 0.23416), (0.746, 0.24207), (0.746, 0.25394), (0.817, 0.25502), (0.841, 0.25644), (0.865, 0.36322), (0.865, 0.37508), (0.96, 0.37555), (0.984, 0.45465), (1.062, 0.45574), (1.086, 0.45645), (1.086, 0.45681), (1.103, 0.47659), (1.11, 0.47718), (1.11, 0.4773), (1.134, 0.50894), (1.134, 0.51289), (1.134, 0.51685), (1.205, 0.51792), (1.229, 0.51816), (1.229, 0.51935), (1.253, 0.59449), (1.253, 0.63799), (1.348, 0.6387), (1.372, 0.69406), (1.372, 0.75734), (1.491, 0.79689), (1.593, 0.79725), (1.617, 0.79748), (1.617, 0.79772), (1.641, 0.8254), (1.641, 0.83727), (1.736, 0.83774), (1.76, 0.85356), (1.76, 0.91683), (1.879, 0.95638), (2.124, 0.9565), (2.148, 0.96836), (2.148, 0.97627), (2.267, 0.99605), (2.655, 1)]");
    }

    #[test]
    fn test_recursive_deltaq() {
        let ctx = btreemap! {
            "recursive".to_owned() =>
                DeltaQ::choice(
                    DeltaQ::name("base"),
                    1.0,
                    DeltaQ::seq(DeltaQ::name("base"), DeltaQ::name("recursive")),
                    1.0,
                ),
            "base".to_owned() => DeltaQ::cdf(CDF::new(&[0.0, 0.5, 1.0], 1.0).unwrap()),
        };
        let result = DeltaQ::name("recursive").eval(&mut ctx.into()).unwrap_err();
        assert_eq!(result, DeltaQError::NameError("recursive".to_owned()));
    }

    #[test]
    fn parse_cdf() {
        let res = "CDF[(2, 0.2), (2, 0.9)]".parse::<DeltaQ>().unwrap_err();
        assert!(res.contains("must contain monotonic"), "{}", res);

        let res = "CDF[(2a, 0.2), (2, 0.9)]".parse::<DeltaQ>().unwrap_err();
        assert!(res.contains("expected CDF[("), "{}", res);

        let res = "+a".parse::<DeltaQ>().unwrap_err();
        assert!(res.contains("expected 'BB', name, CDF, 'all(', 'some(', or a parentheses"), "{}", res);
    }
}
