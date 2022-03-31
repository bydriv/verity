use crate::common::*;

pub enum KindValue {
    Type,
    PositiveType,
    NegativeType,
    Product(Constituent<KindValue>, Constituent<KindValue>),
    Arrow(Constituent<KindValue>, Constituent<KindValue>),
    Intersection(Constituent<KindValue>, Constituent<KindValue>),
}

pub enum TypeParameter<P> {
    Single(P),
    Pair(Constituent<TypeParameter<P>>, Constituent<TypeParameter<P>>),
}

pub enum TypeMember<P, V> {
    Required(Lexeme, Constituent<TypeValue<P, V>>),
    Optional(Lexeme, Constituent<TypeValue<P, V>>),
}

pub enum TypeVariant<P, V> {
    Variant(Lexeme),
    VariantOf(Lexeme, Constituent<TypeValue<P, V>>),
}

pub enum TypeValue<P, V> {
    TypeVariable(V),
    Bool,
    Int,
    Rational,
    String,
    Binary,
    Array,
    Reference(Constituent<TypeValue<P, V>>),
    Effect(Constituent<TypeValue<P, V>>),
    Product(Constituent<TypeValue<P, V>>, Constituent<TypeValue<P, V>>),
    Application(Constituent<TypeValue<P, V>>, Constituent<TypeValue<P, V>>),
    Abstraction(Constituent<TypeParameter<P>>, Constituent<TypeValue<P, V>>),
    Arrow(Constituent<TypeValue<P, V>>, Constituent<TypeValue<P, V>>),
    Forall(Constituent<TypeParameter<P>>, Constituent<TypeValue<P, V>>),
    Exists(Constituent<TypeParameter<P>>, Constituent<TypeValue<P, V>>),
    Pair(Constituent<TypeValue<P, V>>, Constituent<TypeValue<P, V>>),
    NominalRecord(Vec<Lexeme>),
    NominalVariant(Vec<Lexeme>),
    NominalFamily(Vec<Lexeme>),
}

pub type TypeEnvironment<P, V> =
    std::collections::BTreeMap<String, (Constituent<TypeValue<P, V>>, Constituent<KindValue>)>;

pub type ValueEnvironment<P, V> = std::collections::BTreeMap<String, Constituent<TypeValue<P, V>>>;

pub type FamilyEnvironment<P, V> =
    std::collections::BTreeMap<String, Constituent<FamilyValue<P, V>>>;

pub type RecordEnvironment<P, V> =
    std::collections::BTreeMap<String, Vec<Constituent<TypeMember<P, V>>>>;

pub type VariantEnvironment<P, V> =
    std::collections::BTreeMap<String, Vec<Constituent<TypeVariant<P, V>>>>;

pub struct FamilyValue<P, V> {
    pub family_path: Vec<Lexeme>,
    pub family_parameter: Option<Constituent<TypeParameter<P>>>,
    pub family_parameter_kind: Option<Constituent<KindValue>>,
    pub family_parameter_type: Option<Constituent<TypeValue<P, V>>>,
    pub family_type: Option<Constituent<TypeValue<P, V>>>,
    pub type_env: TypeEnvironment<P, V>,
    pub value_env: ValueEnvironment<P, V>,
    pub family_env: FamilyEnvironment<P, V>,
    pub record_env: RecordEnvironment<P, V>,
    pub variant_env: VariantEnvironment<P, V>,
}

pub struct Environment<P, V> {
    pub family_path: Vec<Lexeme>,
    pub family_parameter: Option<Constituent<TypeParameter<P>>>,
    pub family_parameter_kind: Option<Constituent<KindValue>>,
    pub family_parameter_type: Option<Constituent<TypeValue<P, V>>>,
    pub family_type: Option<Constituent<TypeValue<P, V>>>,
    pub type_env: TypeEnvironment<P, V>,
    pub value_env: ValueEnvironment<P, V>,
    pub family_env: FamilyEnvironment<P, V>,
    pub record_env: RecordEnvironment<P, V>,
    pub variant_env: VariantEnvironment<P, V>,
}
