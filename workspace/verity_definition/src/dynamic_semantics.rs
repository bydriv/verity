use crate::common::*;
use crate::syntax;

pub type Environment = std::collections::BTreeMap<String, Constituent<Value>>;

pub enum Error {
    Unsound { location: (usize, usize) },
}

pub enum Value {
    Bool(bool),
    Int(num_bigint::BigInt),
    Rational(num_rational::BigRational),
    String(String),
    Binary(Vec<u8>),
    Array(Vec<Constituent<Value>>),
    Reference(std::rc::Rc<std::cell::RefCell<Constituent<Value>>>),
    Pair(Constituent<Value>, Constituent<Value>),
    Abstraction(
        Environment,
        Constituent<syntax::AtomicValueParameter>,
        Constituent<syntax::ValueExpression>,
    ),
    Quasi(
        Vec<Constituent<syntax::ValueParameters>>,
        Constituent<Value>,
    ),
    Quasienv(Environment, Constituent<Value>),
    Lazy(Environment, Constituent<syntax::ValueExpression>),
    Record(std::collections::BTreeMap<String, Constituent<Value>>),
    Variant(String, Option<Constituent<Value>>),
    RecordProj(String),
    RecordProjDefault(String),
    VariantInj(String),
    Family(std::collections::BTreeMap<String, Constituent<Value>>),
    ForeignFn(fn(&Environment, &Constituent<Value>) -> Result<Constituent<Value>, Error>),
}
