extern crate verity_definition;

use verity_definition::common::*;
use verity_definition::static_semantics;
use verity_definition::syntax;

pub type KindValue = static_semantics::KindValue;
pub type TypeParameter = static_semantics::TypeParameter<TypeVariableValue>;
pub type TypeMember = static_semantics::TypeMember<TypeVariableValue, TypeVariableRef>;
pub type TypeVariant = static_semantics::TypeVariant<TypeVariableValue, TypeVariableRef>;
pub type TypeValue = static_semantics::TypeValue<TypeVariableValue, TypeVariableRef>;
pub type FamilyValue = static_semantics::FamilyValue<TypeVariableValue, TypeVariableRef>;
pub type Environment = static_semantics::Environment<TypeVariableValue, TypeVariableRef>;
pub type TypeEnvironment = static_semantics::TypeEnvironment<TypeVariableValue, TypeVariableRef>;
pub type ValueEnvironment = static_semantics::ValueEnvironment<TypeVariableValue, TypeVariableRef>;
pub type FamilyEnvironment =
    static_semantics::FamilyEnvironment<TypeVariableValue, TypeVariableRef>;
pub type RecordEnvironment =
    static_semantics::RecordEnvironment<TypeVariableValue, TypeVariableRef>;
pub type VariantEnvironment =
    static_semantics::VariantEnvironment<TypeVariableValue, TypeVariableRef>;

type TypeVariableLevel = i64;
type TypeVariableValue = i64;
type TypeRank = i64;

pub enum TypeQuantifier {
    Forall,
    Exists,
}

pub enum TypeVariable {
    Bound(TypeQuantifier, TypeVariableLevel, TypeVariableValue),
    Free(TypeQuantifier, TypeVariableLevel, TypeVariableValue),
    Link(Constituent<TypeValue>),
}

pub struct TypeVariableRef(std::rc::Rc<std::cell::RefCell<TypeVariable>>);

impl TypeVariableRef {
    pub fn new(type_variable: TypeVariable) -> Self {
        Self(std::rc::Rc::new(std::cell::RefCell::new(type_variable)))
    }
}

impl Clone for TypeVariableRef {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

pub enum Error {
    TypeNotInScope { location: (usize, usize) },
    TypeNotInFamily { location: (usize, usize) },
    ValueNotInScope { location: (usize, usize) },
    ValueNotInFamily { location: (usize, usize) },
    FamilyNotInScope { location: (usize, usize) },
    FamilyNotInFamily { location: (usize, usize) },
    RecordNotInScope { location: (usize, usize) },
    RecordNotInFamily { location: (usize, usize) },
    VariantNotInScope { location: (usize, usize) },
    VariantNotInFamily { location: (usize, usize) },
    KindMismatch { location: (usize, usize) },
    TypeMismatch { location: (usize, usize) },
    ValueAlreadyDefined { location: (usize, usize) },
    TypeAlreadyDefined { location: (usize, usize) },
    RecordAlreadyDefined { location: (usize, usize) },
    VariantAlreadyDefined { location: (usize, usize) },
    FamilyAlreadyDefined { location: (usize, usize) },
    Cyclic { location: (usize, usize) },
    InvalidPattern { location: (usize, usize) },
    InvalidRecord { location: (usize, usize) },
    ToplevelEffect { location: (usize, usize) },
    EffectInPattern { location: (usize, usize) },
}

pub struct Verifier {
    rng: rand::rngs::ThreadRng,
    type_variable_level: TypeVariableLevel,
}

pub mod pp {
    use super::*;

    pub fn show_kind(kind_value: &Constituent<KindValue>) -> String {
        match &**kind_value {
            KindValue::Type => String::from("type"),
            KindValue::PositiveType => String::from("+type"),
            KindValue::NegativeType => String::from("-type"),
            KindValue::Product(kind_value1, kind_value2) => {
                format!("({} * {})", show_kind(kind_value1), show_kind(kind_value2))
            }
            KindValue::Arrow(kind_value1, kind_value2) => {
                format!("({} -> {})", show_kind(kind_value1), show_kind(kind_value2))
            }
            KindValue::Intersection(kind_value1, kind_value2) => {
                format!(
                    "({} /\\ {})",
                    show_kind(kind_value1),
                    show_kind(kind_value2)
                )
            }
        }
    }

    pub fn show_type_parameter(type_parameter: &Constituent<TypeParameter>) -> String {
        match &**type_parameter {
            TypeParameter::Single(type_variable_value) => {
                format!("_{:016X}", type_variable_value)
            }
            TypeParameter::Pair(type_parameter1, type_parameter2) => format!(
                "({}, {})",
                show_type_parameter(type_parameter1),
                show_type_parameter(type_parameter2)
            ),
        }
    }

    pub fn show_type(type_value: &Constituent<TypeValue>) -> String {
        match &**type_value {
            TypeValue::TypeVariable(type_variable_ref) => {
                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Bound(_, _, type_variable_value) => {
                        format!("_{:016X}", type_variable_value)
                    }
                    TypeVariable::Free(_, _, type_variable_value) => {
                        format!("'{:016X}", type_variable_value)
                    }
                    TypeVariable::Link(type_value) => show_type(&type_value),
                }
            }
            TypeValue::Bool => String::from("bool"),
            TypeValue::Int => String::from("int"),
            TypeValue::Rational => String::from("rational"),
            TypeValue::String => String::from("string"),
            TypeValue::Binary => String::from("binary"),
            TypeValue::Array => String::from("array"),
            TypeValue::Reference(type_value) => {
                format!("(&{})", show_type(type_value))
            }
            TypeValue::Effect(type_value) => {
                format!("({}!)", show_type(type_value))
            }
            TypeValue::Product(type_value1, type_value2) => {
                format!("({} * {})", show_type(type_value1), show_type(type_value2))
            }
            TypeValue::Application(type_value1, type_value2) => {
                format!("({} {})", show_type(type_value1), show_type(type_value2))
            }
            TypeValue::Abstraction(type_parameter, type_value) => {
                format!(
                    "(^{}. {})",
                    show_type_parameter(type_parameter),
                    show_type(type_value)
                )
            }
            TypeValue::Arrow(type_value1, type_value2) => {
                format!("({} -> {})", show_type(type_value1), show_type(type_value2))
            }
            TypeValue::Forall(type_parameter, type_value) => {
                format!(
                    "(forall {}. {})",
                    show_type_parameter(type_parameter),
                    show_type(type_value)
                )
            }
            TypeValue::Exists(type_parameter, type_value) => {
                format!(
                    "(exists {}. {})",
                    show_type_parameter(type_parameter),
                    show_type(type_value)
                )
            }
            TypeValue::Pair(type_value1, type_value2) => {
                format!("({}, {})", show_type(type_value1), show_type(type_value2))
            }
            TypeValue::NominalRecord(record_path) => {
                format!(
                    "{}",
                    record_path
                        .iter()
                        .map(|id| String::from(&**id))
                        .collect::<Vec<String>>()
                        .join(".")
                )
            }
            TypeValue::NominalVariant(variant_path) => {
                format!(
                    "{}",
                    variant_path
                        .iter()
                        .map(|id| String::from(&**id))
                        .collect::<Vec<String>>()
                        .join(".")
                )
            }
            TypeValue::NominalFamily(family_path) => {
                format!(
                    "{}",
                    family_path
                        .iter()
                        .map(|id| String::from(&**id))
                        .collect::<Vec<String>>()
                        .join(".")
                )
            }
        }
    }
}

pub mod kind {
    use super::*;

    /// ```text
    ///                |-   type <: type
    ///                |-   type <: +type
    ///                |-   type <: -type
    ///
    ///                |-  +type <: +type
    ///
    ///                |-  -type <: -type
    ///
    /// a <: c, b <: d |-  a * b <: c * d
    ///
    /// c <: a, b <: d |- a -> b <: c -> d
    ///
    /// a <: b, a <: c |-      a <: b /\ c
    ///
    ///         a <: c |- a /\ b <: c
    ///         b <: c |- a /\ b <: c
    /// ```
    pub fn subtype(
        kind_value1: &Constituent<KindValue>,
        kind_value2: &Constituent<KindValue>,
    ) -> bool {
        match (&**kind_value1, &**kind_value2) {
            (KindValue::Type, KindValue::Type) => true,
            (KindValue::Type, KindValue::PositiveType) => true,
            (KindValue::Type, KindValue::NegativeType) => true,
            (KindValue::PositiveType, KindValue::PositiveType) => true,
            (KindValue::NegativeType, KindValue::NegativeType) => true,
            (KindValue::Product(a, b), KindValue::Product(c, d)) => subtype(a, c) && subtype(b, d),
            (KindValue::Arrow(a, b), KindValue::Arrow(c, d)) => subtype(c, a) && subtype(b, d),
            (_, KindValue::Intersection(b, c)) => {
                subtype(kind_value1, b) && subtype(kind_value1, c)
            }
            (KindValue::Intersection(a, b), _) => {
                subtype(a, kind_value2) || subtype(b, kind_value2)
            }
            (_, _) => false,
        }
    }

    /// ```text
    /// inverse(type)   = type
    /// inverse(+type)  = -type
    /// inverse(-type)  = +type
    /// inverse(a * b)  = inverse(a) * inverse(b)
    /// inverse(a -> b) = inverse(a) -> inverse(b)
    /// inverse(a /\ b) = inverse(a) /\ inverse(b)
    /// ```
    pub fn inverse(kind_value: &Constituent<KindValue>) -> Constituent<KindValue> {
        let location = kind_value.location();

        match &**kind_value {
            KindValue::Type => Constituent::new(location, KindValue::Type),
            KindValue::PositiveType => Constituent::new(location, KindValue::NegativeType),
            KindValue::NegativeType => Constituent::new(location, KindValue::PositiveType),
            KindValue::Product(a, b) => {
                Constituent::new(location, KindValue::Product(inverse(a), inverse(b)))
            }
            KindValue::Arrow(a, b) => {
                Constituent::new(location, KindValue::Arrow(inverse(a), inverse(b)))
            }
            KindValue::Intersection(a, b) => {
                Constituent::new(location, KindValue::Intersection(inverse(a), inverse(b)))
            }
        }
    }

    pub fn include_positive_or_negative(kind_value: &Constituent<KindValue>) -> bool {
        match &**kind_value {
            KindValue::Type => false,
            KindValue::PositiveType => true,
            KindValue::NegativeType => true,
            KindValue::Product(a, b) => {
                include_positive_or_negative(a) || include_positive_or_negative(b)
            }
            KindValue::Arrow(a, b) => {
                include_positive_or_negative(a) || include_positive_or_negative(b)
            }
            KindValue::Intersection(a, b) => {
                include_positive_or_negative(a) || include_positive_or_negative(b)
            }
        }
    }

    pub fn positive_and_negative(location: (usize, usize)) -> Constituent<KindValue> {
        Constituent::new(location, KindValue::Type)
    }

    pub fn positive_or_negative(
        location: (usize, usize),
        positive: bool,
    ) -> Constituent<KindValue> {
        Constituent::new(
            location,
            if positive {
                KindValue::PositiveType
            } else {
                KindValue::NegativeType
            },
        )
    }

    pub fn construct_intersection(
        location: (usize, usize),
        kind_values: &[Constituent<KindValue>],
    ) -> Result<Constituent<KindValue>, Error> {
        let mut result = Err(Error::KindMismatch { location });

        for kind_value1 in kind_values {
            match result {
                Err(_) => {
                    result = Ok(kind_value1.clone());
                }
                Ok(kind_value2) => {
                    match (&**kind_value1, &*kind_value2) {
                        (KindValue::Type, KindValue::Type)
                        | (KindValue::Type, KindValue::PositiveType)
                        | (KindValue::Type, KindValue::NegativeType)
                        | (KindValue::PositiveType, KindValue::Type)
                        | (KindValue::NegativeType, KindValue::Type)
                        | (KindValue::PositiveType, KindValue::NegativeType)
                        | (KindValue::NegativeType, KindValue::PositiveType) => {
                            result = Ok(Constituent::new(location, KindValue::Type));
                            continue;
                        }
                        _ => (),
                    }

                    if subtype(&kind_value2, kind_value1) {
                        result = Ok(kind_value2);
                        continue;
                    }

                    result = Ok(Constituent::new(
                        location,
                        KindValue::Intersection(kind_value2, kind_value1.clone()),
                    ));
                }
            }
        }

        result
    }

    pub fn destruct_intersection(
        kind_value: &Constituent<KindValue>,
    ) -> Vec<Constituent<KindValue>> {
        match &**kind_value {
            KindValue::Intersection(a, b) => {
                vec![destruct_intersection(&a), destruct_intersection(&b)].concat()
            }
            _ => {
                vec![kind_value.clone()]
            }
        }
    }
}

pub mod substitution {
    use super::*;

    pub type Substitution = std::collections::BTreeMap<TypeVariableValue, Constituent<TypeValue>>;

    pub fn from_type_parameter(
        type_parameter: &Constituent<TypeParameter>,
        type_value: &Constituent<TypeValue>,
    ) -> Result<Substitution, Error> {
        match (&**type_parameter, &**type_value) {
            (TypeParameter::Single(type_variable_value), _) => {
                let substitution = vec![(*type_variable_value, type_value.clone())]
                    .into_iter()
                    .collect();

                Ok(substitution)
            }
            (
                TypeParameter::Pair(type_parameter1, type_parameter2),
                TypeValue::Pair(type_value1, type_value2),
            ) => {
                let substitution1 = from_type_parameter(type_parameter1, type_value1)?;
                let substitution2 = from_type_parameter(type_parameter2, type_value2)?;

                Ok(substitution1.into_iter().chain(substitution2).collect())
            }
            (_, _) => Err(Error::KindMismatch {
                location: type_value.location(),
            }),
        }
    }

    pub fn substitute(
        substitution: &Substitution,
        type_value: &Constituent<TypeValue>,
    ) -> Constituent<TypeValue> {
        let location = type_value.location();

        match &**type_value {
            TypeValue::TypeVariable(type_variable_ref) => {
                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Bound(_, _type_variable_level, type_variable_value) => {
                        if let Some(type_value) = substitution.get(&type_variable_value) {
                            type_value.clone()
                        } else {
                            type_value.clone()
                        }
                    }
                    TypeVariable::Free(_, _type_variable_level, _type_variable_value) => {
                        type_value.clone()
                    }
                    TypeVariable::Link(type_value) => substitute(substitution, type_value),
                }
            }
            TypeValue::Bool => type_value.clone(),
            TypeValue::Int => type_value.clone(),
            TypeValue::Rational => type_value.clone(),
            TypeValue::String => type_value.clone(),
            TypeValue::Binary => type_value.clone(),
            TypeValue::Array => type_value.clone(),
            TypeValue::Reference(type_value) => {
                let type_value = substitute(substitution, type_value);

                Constituent::new(location, TypeValue::Reference(type_value))
            }
            TypeValue::Effect(type_value) => {
                let type_value = substitute(substitution, type_value);

                Constituent::new(location, TypeValue::Effect(type_value))
            }
            TypeValue::Product(type_value1, type_value2) => {
                let type_value1 = substitute(substitution, type_value1);
                let type_value2 = substitute(substitution, type_value2);

                Constituent::new(location, TypeValue::Product(type_value1, type_value2))
            }
            TypeValue::Application(type_value1, type_value2) => {
                let type_value1 = substitute(substitution, type_value1);
                let type_value2 = substitute(substitution, type_value2);

                Constituent::new(location, TypeValue::Application(type_value1, type_value2))
            }
            TypeValue::Abstraction(type_parameter, type_value) => {
                let type_value = substitute(substitution, type_value);

                Constituent::new(
                    location,
                    TypeValue::Abstraction(type_parameter.clone(), type_value),
                )
            }
            TypeValue::Arrow(type_value1, type_value2) => {
                let type_value1 = substitute(substitution, type_value1);
                let type_value2 = substitute(substitution, type_value2);

                Constituent::new(location, TypeValue::Arrow(type_value1, type_value2))
            }
            TypeValue::Forall(type_parameter, type_value) => {
                let type_value = substitute(substitution, type_value);

                Constituent::new(
                    location,
                    TypeValue::Forall(type_parameter.clone(), type_value),
                )
            }
            TypeValue::Exists(type_parameter, type_value) => {
                let type_value = substitute(substitution, type_value);

                Constituent::new(
                    location,
                    TypeValue::Exists(type_parameter.clone(), type_value),
                )
            }
            TypeValue::Pair(type_value1, type_value2) => {
                let type_value1 = substitute(substitution, type_value1);
                let type_value2 = substitute(substitution, type_value2);

                Constituent::new(location, TypeValue::Pair(type_value1, type_value2))
            }
            TypeValue::NominalRecord(record_path) => {
                Constituent::new(location, TypeValue::NominalRecord(record_path.clone()))
            }
            TypeValue::NominalVariant(variant_path) => {
                Constituent::new(location, TypeValue::NominalVariant(variant_path.clone()))
            }
            TypeValue::NominalFamily(family_path) => {
                Constituent::new(location, TypeValue::NominalFamily(family_path.clone()))
            }
        }
    }
}

pub fn equal_qualified_identifier(
    qualified_identifier1: &Constituent<syntax::QualifiedIdentifier>,
    qualified_identifier2: &Constituent<syntax::QualifiedIdentifier>,
) -> bool {
    match (&**qualified_identifier1, &**qualified_identifier2) {
        (
            syntax::QualifiedIdentifier::Identifier {
                identifier: identifier1,
            },
            syntax::QualifiedIdentifier::Identifier {
                identifier: identifier2,
            },
        ) => &**identifier1 == &**identifier2,
        (
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier: qualified_identifier3,
                dot: _dot1,
                identifier: identifier1,
            },
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier: qualified_identifier4,
                dot: _dot2,
                identifier: identifier2,
            },
        ) => {
            equal_qualified_identifier(qualified_identifier3, qualified_identifier4)
                && &**identifier1 == &**identifier2
        }
        (_, _) => false,
    }
}

pub fn fold_effect(n: TypeRank, type_value: &Constituent<TypeValue>) -> Constituent<TypeValue> {
    if n <= 0 {
        type_value.clone()
    } else {
        Constituent::new(
            type_value.location(),
            TypeValue::Effect(fold_effect(n - 1, type_value)),
        )
    }
}

pub fn unfold_effect(type_value: &Constituent<TypeValue>) -> Constituent<TypeValue> {
    match &**type_value {
        TypeValue::Effect(type_value) => unfold_effect(type_value),
        TypeValue::TypeVariable(type_variable_ref) => {
            match &*type_variable_ref.0.as_ref().borrow() {
                TypeVariable::Link(type_value) => unfold_effect(type_value),
                _ => type_value.clone(),
            }
        }
        _ => type_value.clone(),
    }
}

pub fn rank(type_value: &Constituent<TypeValue>) -> TypeRank {
    match &**type_value {
        TypeValue::TypeVariable(type_variable_ref) => {
            match &*type_variable_ref.0.as_ref().borrow() {
                TypeVariable::Bound(_, _, _) => 0,
                TypeVariable::Free(_, _, _) => 0,
                TypeVariable::Link(type_value) => rank(type_value),
            }
        }
        TypeValue::Bool => 0,
        TypeValue::Int => 0,
        TypeValue::Rational => 0,
        TypeValue::String => 0,
        TypeValue::Binary => 0,
        TypeValue::Array => 0,
        TypeValue::Reference(t1) => rank(t1),
        TypeValue::Effect(t1) => rank(t1) + 1,
        TypeValue::Product(t1, t2) => std::cmp::max(rank(t1), rank(t2)),
        TypeValue::Application(t1, t2) => std::cmp::max(rank(t1), rank(t2)),
        TypeValue::Arrow(t1, t2) => std::cmp::max(rank(t1), rank(t2)),
        TypeValue::NominalRecord(_)
        | TypeValue::NominalVariant(_)
        | TypeValue::NominalFamily(_) => 0,
        TypeValue::Abstraction(_, type_value) => rank(type_value),
        TypeValue::Forall(_, type_value) => rank(type_value),
        TypeValue::Exists(_, type_value) => rank(type_value),
        TypeValue::Pair(t1, t2) => std::cmp::max(rank(t1), rank(t2)),
    }
}

pub fn check_cycle(
    type_variable_value: TypeVariableValue,
    type_value: &Constituent<TypeValue>,
) -> Result<(), Error> {
    match &**type_value {
        TypeValue::TypeVariable(type_variable_ref) => {
            match &*type_variable_ref.0.as_ref().borrow() {
                TypeVariable::Bound(_, _, _) => Ok(()),
                TypeVariable::Free(_, _, type_variable_value1) => {
                    if type_variable_value == *type_variable_value1 {
                        Err(Error::Cyclic {
                            location: type_value.location(),
                        })
                    } else {
                        Ok(())
                    }
                }
                TypeVariable::Link(type_value1) => check_cycle(type_variable_value, type_value1),
            }
        }
        TypeValue::Bool => Ok(()),
        TypeValue::Int => Ok(()),
        TypeValue::Rational => Ok(()),
        TypeValue::String => Ok(()),
        TypeValue::Binary => Ok(()),
        TypeValue::Array => Ok(()),
        TypeValue::Reference(t1) => check_cycle(type_variable_value, t1),
        TypeValue::Effect(t1) => check_cycle(type_variable_value, t1),
        TypeValue::Product(t1, t2) => {
            check_cycle(type_variable_value, t1)?;
            check_cycle(type_variable_value, t2)
        }
        TypeValue::Arrow(t1, t2) => {
            check_cycle(type_variable_value, t1)?;
            check_cycle(type_variable_value, t2)
        }
        TypeValue::NominalRecord(_)
        | TypeValue::NominalVariant(_)
        | TypeValue::NominalFamily(_) => Ok(()),
        TypeValue::Application(t1, t2) => {
            check_cycle(type_variable_value, t1)?;
            check_cycle(type_variable_value, t2)
        }
        TypeValue::Abstraction(_, type_value) => check_cycle(type_variable_value, type_value),
        TypeValue::Forall(_, type_value) => check_cycle(type_variable_value, type_value),
        TypeValue::Exists(_, type_value) => check_cycle(type_variable_value, type_value),
        TypeValue::Pair(t1, t2) => {
            check_cycle(type_variable_value, t1)?;
            check_cycle(type_variable_value, t2)
        }
    }
}

pub fn unify(
    location: (usize, usize),
    type_value1: &Constituent<TypeValue>,
    type_value2: &Constituent<TypeValue>,
) -> Result<(), Error> {
    match (&**type_value1, &**type_value2) {
        (TypeValue::Bool, TypeValue::Bool)
        | (TypeValue::Int, TypeValue::Int)
        | (TypeValue::Rational, TypeValue::Rational)
        | (TypeValue::String, TypeValue::String)
        | (TypeValue::Binary, TypeValue::Binary) => Ok(()),
        (TypeValue::Array, TypeValue::Array) => Ok(()),
        (TypeValue::Reference(t1), TypeValue::Reference(t2)) => unify(location, t1, t2),
        (TypeValue::Effect(t1), TypeValue::Effect(t2)) => unify(location, t1, t2),
        (TypeValue::Product(t1, t2), TypeValue::Product(t3, t4)) => {
            unify(location, t1, t3)?;
            unify(location, t2, t4)
        }
        (TypeValue::Arrow(t1, t2), TypeValue::Arrow(t3, t4)) => {
            unify(location, t1, t3)?;
            unify(location, t2, t4)
        }
        (TypeValue::Application(t1, t2), TypeValue::Application(t3, t4)) => {
            unify(location, t1, t3)?;
            unify(location, t2, t4)
        }
        (TypeValue::NominalRecord(record_path1), TypeValue::NominalRecord(record_path2)) => {
            if record_path1.len() != record_path2.len() {
                return Err(Error::TypeMismatch { location });
            }

            for (id1, id2) in std::iter::zip(record_path1, record_path2) {
                if &**id1 != &**id2 {
                    return Err(Error::TypeMismatch { location });
                }
            }

            Ok(())
        }
        (TypeValue::NominalVariant(variant_path1), TypeValue::NominalVariant(variant_path2)) => {
            if variant_path1.len() != variant_path2.len() {
                return Err(Error::TypeMismatch { location });
            }

            for (id1, id2) in std::iter::zip(variant_path1, variant_path2) {
                if &**id1 != &**id2 {
                    return Err(Error::TypeMismatch { location });
                }
            }

            Ok(())
        }
        (TypeValue::NominalFamily(family_path1), TypeValue::NominalFamily(family_path2)) => {
            if family_path1.len() != family_path2.len() {
                return Err(Error::TypeMismatch { location });
            }

            for (id1, id2) in std::iter::zip(family_path1, family_path2) {
                if &**id1 != &**id2 {
                    return Err(Error::TypeMismatch { location });
                }
            }

            Ok(())
        }
        (
            TypeValue::TypeVariable(type_variable_ref1),
            TypeValue::TypeVariable(type_variable_ref2),
        ) => {
            let mut update1 = false;
            let mut update2 = false;
            let mut unif1 = None;
            let mut unif2 = None;

            match (
                &*type_variable_ref1.0.as_ref().borrow(),
                &*type_variable_ref2.0.as_ref().borrow(),
            ) {
                (
                    TypeVariable::Bound(_, _, type_variable_value1),
                    TypeVariable::Bound(_, _, type_variable_value2),
                ) => {
                    if type_variable_value1 != type_variable_value2 {
                        return Err(Error::TypeMismatch { location });
                    }
                }
                (
                    TypeVariable::Free(_, type_variable_level1, type_variable_value1),
                    TypeVariable::Free(_, type_variable_level2, type_variable_value2),
                ) => {
                    if type_variable_value1 != type_variable_value2 {
                        if type_variable_level1 <= type_variable_level2 {
                            update2 = true;
                        } else {
                            update1 = true;
                        }
                    }
                }
                (TypeVariable::Link(type_value3), _) => {
                    unif1 = Some(type_value3.clone());
                }
                (_, TypeVariable::Link(type_value3)) => {
                    unif2 = Some(type_value3.clone());
                }
                (TypeVariable::Free(_, _, type_variable_value), _) => {
                    check_cycle(*type_variable_value, type_value2)?;
                    update1 = true;
                }
                (_, TypeVariable::Free(_, _, type_variable_value)) => {
                    check_cycle(*type_variable_value, type_value1)?;
                    update2 = true;
                }
            }

            if let Some(type_value3) = unif1 {
                unify(location, &type_value3, type_value2)?;
            }

            if let Some(type_value3) = unif2 {
                unify(location, type_value1, &type_value3)?;
            }

            if update1 {
                *type_variable_ref1.0.as_ref().borrow_mut() =
                    TypeVariable::Link(type_value2.clone());
            }

            if update2 {
                *type_variable_ref2.0.as_ref().borrow_mut() =
                    TypeVariable::Link(type_value1.clone());
            }

            Ok(())
        }
        (TypeValue::TypeVariable(type_variable_ref), _) => {
            let mut update = false;
            let mut unif = None;

            match &*type_variable_ref.0.as_ref().borrow() {
                TypeVariable::Bound(_, _, _) => {
                    return Err(Error::TypeMismatch { location });
                }
                TypeVariable::Free(_, _, type_variable_value) => {
                    check_cycle(*type_variable_value, type_value2)?;
                    update = true;
                }
                TypeVariable::Link(type_value3) => {
                    unif = Some(type_value3.clone());
                }
            }

            if let Some(type_value3) = unif {
                unify(location, &type_value3, type_value2)?;
            }

            if update {
                *type_variable_ref.0.as_ref().borrow_mut() =
                    TypeVariable::Link(type_value2.clone());
            }

            Ok(())
        }
        (_, TypeValue::TypeVariable(type_variable_ref)) => {
            let mut update = false;
            let mut unif = None;

            match &*type_variable_ref.0.as_ref().borrow() {
                TypeVariable::Bound(_, _, _) => {
                    return Err(Error::TypeMismatch { location });
                }
                TypeVariable::Free(_, _, type_variable_value) => {
                    check_cycle(*type_variable_value, type_value1)?;
                    update = true;
                }
                TypeVariable::Link(type_value3) => {
                    unif = Some(type_value3.clone());
                }
            }

            if let Some(type_value3) = unif {
                unify(location, type_value1, &type_value3)?;
            }

            if update {
                *type_variable_ref.0.as_ref().borrow_mut() =
                    TypeVariable::Link(type_value1.clone());
            }

            Ok(())
        }
        (_, _) => Err(Error::TypeMismatch { location }),
    }
}

fn type_parameter_occur(
    type_parameter: &Constituent<TypeParameter>,
) -> std::collections::BTreeSet<TypeVariableValue> {
    match &**type_parameter {
        TypeParameter::Single(type_variable_value) => {
            let mut occur = std::collections::BTreeSet::new();
            occur.insert(*type_variable_value);
            occur
        }
        TypeParameter::Pair(type_parameter1, type_parameter2) => {
            let mut occur1 = type_parameter_occur(type_parameter1);
            let mut occur2 = type_parameter_occur(type_parameter2);
            occur1.append(&mut occur2);
            occur1
        }
    }
}

impl Verifier {
    pub fn new() -> Self {
        Verifier {
            rng: rand::thread_rng(),
            type_variable_level: 0,
        }
    }

    pub fn type_parameter_to_type_argument(
        &mut self,
        type_parameter: &Constituent<TypeParameter>,
    ) -> Constituent<TypeValue> {
        match &**type_parameter {
            TypeParameter::Single(type_variable_value) => Constituent::new(
                type_parameter.location(),
                TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Bound(
                    TypeQuantifier::Forall,
                    self.type_variable_level,
                    *type_variable_value,
                ))),
            ),
            TypeParameter::Pair(type_parameter1, type_parameter2) => Constituent::new(
                type_parameter.location(),
                TypeValue::Pair(
                    self.type_parameter_to_type_argument(type_parameter1),
                    self.type_parameter_to_type_argument(type_parameter2),
                ),
            ),
        }
    }

    pub fn bound_substitution(
        &mut self,
        type_quantifier: &TypeQuantifier,
        type_parameter: &Constituent<TypeParameter>,
    ) -> substitution::Substitution {
        match &**type_parameter {
            TypeParameter::Single(type_variable_value) => {
                let type_variable_value_instance =
                    rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                let q = match type_quantifier {
                    TypeQuantifier::Forall => TypeQuantifier::Forall,
                    TypeQuantifier::Exists => TypeQuantifier::Exists,
                };

                let type_value = Constituent::new(
                    type_parameter.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Bound(
                        q,
                        self.type_variable_level,
                        type_variable_value_instance,
                    ))),
                );

                let substitution = vec![(*type_variable_value, type_value)]
                    .into_iter()
                    .collect();

                substitution
            }
            TypeParameter::Pair(type_parameter1, type_parameter2) => {
                let substitution1 = self.bound_substitution(type_quantifier, type_parameter1);
                let substitution2 = self.bound_substitution(type_quantifier, type_parameter2);
                substitution1.into_iter().chain(substitution2).collect()
            }
        }
    }

    pub fn free_substitution(
        &mut self,
        type_quantifier: &TypeQuantifier,
        type_parameter: &Constituent<TypeParameter>,
    ) -> substitution::Substitution {
        match &**type_parameter {
            TypeParameter::Single(type_variable_value) => {
                let type_variable_value_instance =
                    rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                let q = match type_quantifier {
                    TypeQuantifier::Forall => TypeQuantifier::Forall,
                    TypeQuantifier::Exists => TypeQuantifier::Exists,
                };

                let type_value = Constituent::new(
                    type_parameter.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Free(
                        q,
                        self.type_variable_level,
                        type_variable_value_instance,
                    ))),
                );

                let substitution = vec![(*type_variable_value, type_value)]
                    .into_iter()
                    .collect();

                substitution
            }
            TypeParameter::Pair(type_parameter1, type_parameter2) => {
                let substitution1 = self.free_substitution(type_quantifier, type_parameter1);
                let substitution2 = self.free_substitution(type_quantifier, type_parameter2);
                substitution1.into_iter().chain(substitution2).collect()
            }
        }
    }

    pub fn instantiate_type(
        &mut self,
        type_value: &Constituent<TypeValue>,
    ) -> Constituent<TypeValue> {
        match &**type_value {
            TypeValue::Forall(type_parameter, type_value) => {
                let substitution = self.free_substitution(&TypeQuantifier::Forall, type_parameter);
                substitution::substitute(&substitution, &self.instantiate_type(type_value))
            }
            TypeValue::Exists(type_parameter, type_value) => {
                let substitution = self.bound_substitution(&TypeQuantifier::Exists, type_parameter);
                substitution::substitute(&substitution, &self.instantiate_type(type_value))
            }
            TypeValue::Arrow(type_value1, type_value2) => {
                let type_value1 = self.instantiate_type_parameter(&type_value1);
                let type_value2 = self.instantiate_type(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Arrow(type_value1, type_value2),
                )
            }
            TypeValue::Product(type_value1, type_value2) => {
                let type_value1 = self.instantiate_type(&type_value1);
                let type_value2 = self.instantiate_type(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Product(type_value1, type_value2),
                )
            }
            TypeValue::TypeVariable(type_variable_ref) => {
                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Link(type_value) => self.instantiate_type(type_value),
                    _ => type_value.clone(),
                }
            }
            _ => type_value.clone(),
        }
    }

    pub fn instantiate_type_parameter(
        &mut self,
        type_value: &Constituent<TypeValue>,
    ) -> Constituent<TypeValue> {
        match &**type_value {
            TypeValue::Forall(type_parameter, type_value) => {
                let substitution = self.free_substitution(&TypeQuantifier::Exists, type_parameter);
                substitution::substitute(&substitution, &self.instantiate_type(type_value))
            }
            TypeValue::Exists(type_parameter, type_value) => {
                let substitution = self.bound_substitution(&TypeQuantifier::Forall, type_parameter);
                substitution::substitute(&substitution, &self.instantiate_type(type_value))
            }
            TypeValue::Arrow(type_value1, type_value2) => {
                let type_value1 = self.instantiate_type(&type_value1);
                let type_value2 = self.instantiate_type_parameter(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Arrow(type_value1, type_value2),
                )
            }
            TypeValue::Product(type_value1, type_value2) => {
                let type_value1 = self.instantiate_type_parameter(&type_value1);
                let type_value2 = self.instantiate_type_parameter(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Product(type_value1, type_value2),
                )
            }
            TypeValue::TypeVariable(type_variable_ref) => {
                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Link(type_value) => self.instantiate_type_parameter(type_value),
                    _ => type_value.clone(),
                }
            }
            _ => type_value.clone(),
        }
    }

    pub fn ascribe_type(&mut self, type_value: &Constituent<TypeValue>) -> Constituent<TypeValue> {
        match &**type_value {
            TypeValue::Forall(type_parameter, type_value) => {
                let substitution = self.bound_substitution(&TypeQuantifier::Forall, type_parameter);
                substitution::substitute(&substitution, &self.ascribe_type(type_value))
            }
            TypeValue::Exists(type_parameter, type_value) => {
                let substitution = self.free_substitution(&TypeQuantifier::Exists, type_parameter);
                substitution::substitute(&substitution, &self.ascribe_type(type_value))
            }
            TypeValue::Arrow(type_value1, type_value2) => {
                let type_value1 = self.ascribe_type_parameter(&type_value1);
                let type_value2 = self.ascribe_type(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Arrow(type_value1, type_value2),
                )
            }
            TypeValue::Product(type_value1, type_value2) => {
                let type_value1 = self.ascribe_type(&type_value1);
                let type_value2 = self.ascribe_type(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Product(type_value1, type_value2),
                )
            }
            TypeValue::TypeVariable(type_variable_ref) => {
                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Link(type_value) => self.ascribe_type(type_value),
                    _ => type_value.clone(),
                }
            }
            _ => type_value.clone(),
        }
    }

    pub fn ascribe_type_parameter(
        &mut self,
        type_value: &Constituent<TypeValue>,
    ) -> Constituent<TypeValue> {
        match &**type_value {
            TypeValue::Forall(type_parameter, type_value) => {
                let substitution = self.bound_substitution(&TypeQuantifier::Forall, type_parameter);
                substitution::substitute(&substitution, &self.ascribe_type_parameter(type_value))
            }
            TypeValue::Exists(type_parameter, type_value) => {
                let substitution = self.free_substitution(&TypeQuantifier::Exists, type_parameter);
                substitution::substitute(&substitution, &self.ascribe_type_parameter(type_value))
            }
            TypeValue::Arrow(type_value1, type_value2) => {
                let type_value1 = self.ascribe_type(&type_value1);
                let type_value2 = self.ascribe_type_parameter(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Arrow(type_value1, type_value2),
                )
            }
            TypeValue::Product(type_value1, type_value2) => {
                let type_value1 = self.ascribe_type_parameter(&type_value1);
                let type_value2 = self.ascribe_type_parameter(&type_value2);
                Constituent::new(
                    type_value.location(),
                    TypeValue::Product(type_value1, type_value2),
                )
            }
            TypeValue::TypeVariable(type_variable_ref) => {
                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Link(type_value) => self.ascribe_type_parameter(type_value),
                    _ => type_value.clone(),
                }
            }
            _ => type_value.clone(),
        }
    }

    pub fn generalize_type(
        &mut self,
        cache: &mut std::collections::BTreeMap<TypeVariableValue, Constituent<TypeValue>>,
        occur: &std::collections::BTreeSet<TypeVariableValue>,
        type_value: &Constituent<TypeValue>,
    ) -> (
        std::collections::BTreeSet<TypeVariableValue>,
        std::collections::BTreeSet<TypeVariableValue>,
        Constituent<TypeValue>,
    ) {
        let location = type_value.location();

        match &**type_value {
            TypeValue::TypeVariable(type_variable_ref) => {
                let mut forall_params = std::collections::BTreeSet::new();
                let mut exists_params = std::collections::BTreeSet::new();

                match &*type_variable_ref.0.as_ref().borrow() {
                    TypeVariable::Bound(
                        type_quantifier,
                        type_variable_level,
                        type_variable_value,
                    ) => {
                        if occur.get(&type_variable_value).is_none()
                            && self.type_variable_level < *type_variable_level
                        {
                            if let Some(t) = cache.get(&type_variable_value) {
                                (forall_params, exists_params, t.clone())
                            } else {
                                let type_variable_value_instance =
                                    rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                                match type_quantifier {
                                    TypeQuantifier::Forall => {
                                        forall_params.insert(type_variable_value_instance);
                                    }
                                    TypeQuantifier::Exists => {
                                        exists_params.insert(type_variable_value_instance);
                                    }
                                }

                                let q = match type_quantifier {
                                    TypeQuantifier::Forall => TypeQuantifier::Forall,
                                    TypeQuantifier::Exists => TypeQuantifier::Exists,
                                };

                                let t = Constituent::new(
                                    location,
                                    TypeValue::TypeVariable(TypeVariableRef::new(
                                        TypeVariable::Bound(
                                            q,
                                            self.type_variable_level,
                                            type_variable_value_instance,
                                        ),
                                    )),
                                );

                                cache.insert(*type_variable_value, t.clone());

                                (forall_params, exists_params, t)
                            }
                        } else {
                            (
                                forall_params,
                                exists_params,
                                Constituent::new(
                                    location,
                                    TypeValue::TypeVariable(type_variable_ref.clone()),
                                ),
                            )
                        }
                    }
                    TypeVariable::Free(
                        type_quantifier,
                        type_variable_level,
                        type_variable_value,
                    ) => {
                        if occur.get(&type_variable_value).is_none()
                            && self.type_variable_level < *type_variable_level
                        {
                            if let Some(t) = cache.get(&type_variable_value) {
                                (forall_params, exists_params, t.clone())
                            } else {
                                let type_variable_value_instance =
                                    rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                                match type_quantifier {
                                    TypeQuantifier::Forall => {
                                        forall_params.insert(type_variable_value_instance);
                                    }
                                    TypeQuantifier::Exists => {
                                        exists_params.insert(type_variable_value_instance);
                                    }
                                }

                                let q = match type_quantifier {
                                    TypeQuantifier::Forall => TypeQuantifier::Forall,
                                    TypeQuantifier::Exists => TypeQuantifier::Exists,
                                };

                                let t = Constituent::new(
                                    location,
                                    TypeValue::TypeVariable(TypeVariableRef::new(
                                        TypeVariable::Bound(
                                            q,
                                            self.type_variable_level,
                                            type_variable_value_instance,
                                        ),
                                    )),
                                );

                                cache.insert(*type_variable_value, t.clone());

                                (forall_params, exists_params, t)
                            }
                        } else {
                            (
                                forall_params,
                                exists_params,
                                Constituent::new(
                                    location,
                                    TypeValue::TypeVariable(type_variable_ref.clone()),
                                ),
                            )
                        }
                    }
                    TypeVariable::Link(type_value) => {
                        let (mut p, mut q, type_value) =
                            self.generalize_type(cache, occur, type_value);
                        forall_params.append(&mut p);
                        exists_params.append(&mut q);

                        (forall_params, exists_params, type_value)
                    }
                }
            }
            TypeValue::Bool => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::Int => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::Rational => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::String => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::Binary => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::Array => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::Reference(type_value) => {
                let (forall_params, exists_params, type_value) =
                    self.generalize_type(cache, occur, type_value);
                (
                    forall_params,
                    exists_params,
                    Constituent::new(location, TypeValue::Reference(type_value)),
                )
            }
            TypeValue::Effect(type_value) => {
                let (forall_params, exists_params, type_value) =
                    self.generalize_type(cache, occur, type_value);
                (
                    forall_params,
                    exists_params,
                    Constituent::new(location, TypeValue::Effect(type_value)),
                )
            }
            TypeValue::Product(t1, t2) => {
                let (mut p1, mut q1, t1) = self.generalize_type(cache, occur, t1);
                let (mut p2, mut q2, t2) = self.generalize_type(cache, occur, t2);
                p1.append(&mut p2);
                q1.append(&mut q2);
                (
                    p1,
                    q1,
                    Constituent::new(location, TypeValue::Product(t1, t2)),
                )
            }
            TypeValue::Application(t1, t2) => {
                let (mut p1, mut q1, t1) = self.generalize_type(cache, occur, t1);
                let (mut p2, mut q2, t2) = self.generalize_type(cache, occur, t2);
                p1.append(&mut p2);
                q1.append(&mut q2);
                (
                    p1,
                    q1,
                    Constituent::new(location, TypeValue::Application(t1, t2)),
                )
            }
            TypeValue::Abstraction(type_parameter, type_value) => {
                let mut occur = occur.clone();
                occur.append(&mut type_parameter_occur(type_parameter));

                let (forall_params, exists_params, type_value) =
                    self.generalize_type(cache, &occur, type_value);

                (
                    forall_params,
                    exists_params,
                    Constituent::new(
                        location,
                        TypeValue::Abstraction(type_parameter.clone(), type_value),
                    ),
                )
            }
            TypeValue::Arrow(t1, t2) => {
                let (mut p1, mut q1, t1) = self.generalize_type(cache, occur, t1);
                let (mut p2, mut q2, t2) = self.generalize_type(cache, occur, t2);
                p1.append(&mut p2);
                q1.append(&mut q2);
                (p1, q1, Constituent::new(location, TypeValue::Arrow(t1, t2)))
            }
            TypeValue::Forall(type_parameter, type_value) => {
                let mut occur = occur.clone();
                occur.append(&mut type_parameter_occur(type_parameter));

                let (forall_params, exists_params, type_value) =
                    self.generalize_type(cache, &occur, type_value);

                (
                    forall_params,
                    exists_params,
                    Constituent::new(
                        location,
                        TypeValue::Forall(type_parameter.clone(), type_value),
                    ),
                )
            }
            TypeValue::Exists(type_parameter, type_value) => {
                let mut occur = occur.clone();
                occur.append(&mut type_parameter_occur(type_parameter));

                let (forall_params, exists_params, type_value) =
                    self.generalize_type(cache, &occur, type_value);

                (
                    forall_params,
                    exists_params,
                    Constituent::new(
                        location,
                        TypeValue::Exists(type_parameter.clone(), type_value),
                    ),
                )
            }
            TypeValue::Pair(t1, t2) => {
                let (mut p1, mut q1, t1) = self.generalize_type(cache, occur, t1);
                let (mut p2, mut q2, t2) = self.generalize_type(cache, occur, t2);
                p1.append(&mut p2);
                q1.append(&mut q2);
                (p1, q1, Constituent::new(location, TypeValue::Pair(t1, t2)))
            }
            TypeValue::NominalRecord(_) => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::NominalVariant(_) => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
            TypeValue::NominalFamily(_) => (
                std::collections::BTreeSet::new(),
                std::collections::BTreeSet::new(),
                type_value.clone(),
            ),
        }
    }

    pub fn gen(&mut self, type_value: &Constituent<TypeValue>) -> Constituent<TypeValue> {
        let (forall_type_variable_values, exists_type_variable_values, type_value) = self
            .generalize_type(
                &mut std::collections::BTreeMap::new(),
                &std::collections::BTreeSet::new(),
                type_value,
            );

        let mut forall_type_parameter = None;
        let mut exists_type_parameter = None;

        for type_variable_value in forall_type_variable_values.into_iter() {
            match forall_type_parameter {
                None => {
                    forall_type_parameter = Some(Constituent::new(
                        type_value.location(),
                        TypeParameter::Single(type_variable_value),
                    ))
                }
                Some(p) => {
                    forall_type_parameter = Some(Constituent::new(
                        type_value.location(),
                        TypeParameter::Pair(
                            Constituent::new(
                                type_value.location(),
                                TypeParameter::Single(type_variable_value),
                            ),
                            p,
                        ),
                    ));
                }
            }
        }

        for type_variable_value in exists_type_variable_values.into_iter() {
            match exists_type_parameter {
                None => {
                    exists_type_parameter = Some(Constituent::new(
                        type_value.location(),
                        TypeParameter::Single(type_variable_value),
                    ))
                }
                Some(p) => {
                    exists_type_parameter = Some(Constituent::new(
                        type_value.location(),
                        TypeParameter::Pair(
                            Constituent::new(
                                type_value.location(),
                                TypeParameter::Single(type_variable_value),
                            ),
                            p,
                        ),
                    ));
                }
            }
        }

        let type_value = match exists_type_parameter {
            None => type_value,
            Some(type_parameter) => Constituent::new(
                type_value.location(),
                TypeValue::Exists(type_parameter, type_value),
            ),
        };

        let type_value = match forall_type_parameter {
            None => type_value,
            Some(type_parameter) => Constituent::new(
                type_value.location(),
                TypeValue::Forall(type_parameter, type_value),
            ),
        };

        type_value
    }

    pub fn refresh_type(&mut self, type_value: &Constituent<TypeValue>) -> Constituent<TypeValue> {
        self.type_variable_level += 1;
        let type_value = self.instantiate_type(&type_value);
        self.type_variable_level -= 1;

        self.gen(&type_value)
    }

    pub fn eval_type_identifier(
        &self,
        environment: &Environment,
        qualified_identifier: &Constituent<syntax::QualifiedIdentifier>,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = qualified_identifier.location();

        match &**qualified_identifier {
            syntax::QualifiedIdentifier::Identifier { identifier } => {
                match environment.type_env.get(&**identifier) {
                    Some((type_value, kind_value)) => Ok((
                        type_value.clone().relocate(location),
                        kind_value.clone().relocate(location),
                    )),
                    None => Err(Error::TypeNotInScope {
                        location: identifier.location(),
                    }),
                }
            }
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier,
                dot: _dot,
                identifier,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                match family_value.type_env.get(&**identifier) {
                    Some((type_value, kind_value)) => Ok((
                        type_value.clone().relocate(location),
                        kind_value.clone().relocate(location),
                    )),
                    None => Err(Error::TypeNotInFamily {
                        location: identifier.location(),
                    }),
                }
            }
        }
    }

    pub fn eval_value_identifier(
        &self,
        environment: &Environment,
        qualified_identifier: &Constituent<syntax::QualifiedIdentifier>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = qualified_identifier.location();

        match &**qualified_identifier {
            syntax::QualifiedIdentifier::Identifier { identifier } => {
                match environment.value_env.get(&**identifier) {
                    Some(type_value) => Ok(type_value.clone().relocate(location)),
                    None => Err(Error::ValueNotInScope {
                        location: identifier.location(),
                    }),
                }
            }
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier,
                dot: _dot,
                identifier,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                match family_value.value_env.get(&**identifier) {
                    Some(type_value) => Ok(type_value.clone().relocate(location)),
                    None => Err(Error::ValueNotInFamily {
                        location: identifier.location(),
                    }),
                }
            }
        }
    }

    pub fn eval_family_identifier(
        &self,
        environment: &Environment,
        qualified_identifier: &Constituent<syntax::QualifiedIdentifier>,
    ) -> Result<Constituent<FamilyValue>, Error> {
        let location = qualified_identifier.location();

        match &**qualified_identifier {
            syntax::QualifiedIdentifier::Identifier { identifier } => {
                match environment.family_env.get(&**identifier) {
                    Some(family_value) => Ok(family_value.clone().relocate(location)),
                    None => Err(Error::FamilyNotInScope {
                        location: identifier.location(),
                    }),
                }
            }
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier,
                dot: _dot,
                identifier,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                match family_value.family_env.get(&**identifier) {
                    Some(family_value) => Ok(family_value.clone().relocate(location)),
                    None => Err(Error::FamilyNotInFamily {
                        location: identifier.location(),
                    }),
                }
            }
        }
    }

    pub fn eval_record_identifier(
        &self,
        environment: &Environment,
        qualified_identifier: &Constituent<syntax::QualifiedIdentifier>,
    ) -> Result<Vec<Constituent<TypeMember>>, Error> {
        let location = qualified_identifier.location();

        match &**qualified_identifier {
            syntax::QualifiedIdentifier::Identifier { identifier } => {
                match environment.record_env.get(&**identifier) {
                    Some(type_members) => Ok(type_members
                        .iter()
                        .map(|type_member| type_member.clone().relocate(location))
                        .collect()),
                    None => Err(Error::RecordNotInScope {
                        location: identifier.location(),
                    }),
                }
            }
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier,
                dot: _dot,
                identifier,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                match family_value.record_env.get(&**identifier) {
                    Some(type_members) => Ok(type_members
                        .iter()
                        .map(|type_member| type_member.clone().relocate(location))
                        .collect()),
                    None => Err(Error::RecordNotInFamily {
                        location: identifier.location(),
                    }),
                }
            }
        }
    }

    pub fn eval_variant_identifier(
        &self,
        environment: &Environment,
        qualified_identifier: &Constituent<syntax::QualifiedIdentifier>,
    ) -> Result<Vec<Constituent<TypeVariant>>, Error> {
        let location = qualified_identifier.location();

        match &**qualified_identifier {
            syntax::QualifiedIdentifier::Identifier { identifier } => {
                match environment.variant_env.get(&**identifier) {
                    Some(type_variants) => Ok(type_variants
                        .iter()
                        .map(|type_variant| type_variant.clone().relocate(location))
                        .collect()),
                    None => Err(Error::VariantNotInScope {
                        location: identifier.location(),
                    }),
                }
            }
            syntax::QualifiedIdentifier::Dot {
                qualified_identifier,
                dot: _dot,
                identifier,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                match family_value.variant_env.get(&**identifier) {
                    Some(type_variants) => Ok(type_variants
                        .iter()
                        .map(|type_variant| type_variant.clone().relocate(location))
                        .collect()),
                    None => Err(Error::VariantNotInFamily {
                        location: identifier.location(),
                    }),
                }
            }
        }
    }

    pub fn eval_atomic_kind_expression(
        &self,
        environment: &Environment,
        atomic_kind_expression: &Constituent<syntax::AtomicKindExpression>,
    ) -> Result<Constituent<KindValue>, Error> {
        let location = atomic_kind_expression.location();

        match &**atomic_kind_expression {
            syntax::AtomicKindExpression::Type { type_: _type_ } => {
                Ok(Constituent::new(location, KindValue::Type))
            }
            syntax::AtomicKindExpression::PositiveType {
                plus: _plus,
                type_: _type_,
            } => Ok(Constituent::new(location, KindValue::PositiveType)),
            syntax::AtomicKindExpression::NegativeType {
                minus: _minus,
                type_: _type_,
            } => Ok(Constituent::new(location, KindValue::NegativeType)),
            syntax::AtomicKindExpression::Prec {
                lparen: _lparen,
                kind_expression,
                rparen: _rparen,
            } => self.eval_kind_expression(environment, kind_expression),
        }
    }

    pub fn eval_multiplicative_kind_expression(
        &self,
        environment: &Environment,
        multiplicative_kind_expression: &Constituent<syntax::MultiplicativeKindExpression>,
    ) -> Result<Constituent<KindValue>, Error> {
        let location = multiplicative_kind_expression.location();

        match &**multiplicative_kind_expression {
            syntax::MultiplicativeKindExpression::AtomicKindExpression {
                atomic_kind_expression,
            } => self.eval_atomic_kind_expression(environment, atomic_kind_expression),
            syntax::MultiplicativeKindExpression::Asterisk {
                atomic_kind_expression,
                asterisk: _asterisk,
                multiplicative_kind_expression,
            } => {
                let kind_value1 =
                    self.eval_atomic_kind_expression(environment, atomic_kind_expression)?;

                let kind_value2 = self.eval_multiplicative_kind_expression(
                    environment,
                    multiplicative_kind_expression,
                )?;

                Ok(Constituent::new(
                    location,
                    KindValue::Product(kind_value1, kind_value2),
                ))
            }
        }
    }

    pub fn eval_kind_expression(
        &self,
        environment: &Environment,
        kind_expression: &Constituent<syntax::KindExpression>,
    ) -> Result<Constituent<KindValue>, Error> {
        let location = kind_expression.location();

        match &**kind_expression {
            syntax::KindExpression::MultiplicativeKindExpression {
                multiplicative_kind_expression,
            } => self
                .eval_multiplicative_kind_expression(environment, multiplicative_kind_expression),
            syntax::KindExpression::Arrow {
                multiplicative_kind_expression,
                arrow: _arrow,
                kind_expression,
            } => {
                let kind_value1 = self.eval_multiplicative_kind_expression(
                    environment,
                    multiplicative_kind_expression,
                )?;

                let kind_value2 = self.eval_kind_expression(environment, kind_expression)?;

                Ok(Constituent::new(
                    location,
                    KindValue::Arrow(kind_value1, kind_value2),
                ))
            }
        }
    }

    pub fn eval_atomic_type_parameter(
        &mut self,
        environment: &Environment,
        atomic_type_parameter: &Constituent<syntax::AtomicTypeParameter>,
        positive: bool,
        type_quantifier: &TypeQuantifier,
    ) -> Result<
        (
            Constituent<TypeParameter>,
            Constituent<KindValue>,
            TypeEnvironment,
        ),
        Error,
    > {
        let location = atomic_type_parameter.location();

        match &**atomic_type_parameter {
            syntax::AtomicTypeParameter::Identifier { identifier } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                let kind_value = Constituent::new(identifier.location(), KindValue::Type);

                let type_param =
                    Constituent::new(location, TypeParameter::Single(type_variable_value));

                let type_env = TypeEnvironment::from([(
                    String::from(&**identifier.clone()),
                    (
                        Constituent::new(
                            location,
                            TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Bound(
                                match type_quantifier {
                                    TypeQuantifier::Forall => TypeQuantifier::Forall,
                                    TypeQuantifier::Exists => TypeQuantifier::Exists,
                                },
                                self.type_variable_level,
                                type_variable_value,
                            ))),
                        ),
                        kind_value.clone(),
                    ),
                )]);

                Ok((type_param, kind_value, type_env))
            }
            syntax::AtomicTypeParameter::Tuple {
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
            } => self.eval_type_parameters(environment, type_parameters, positive, type_quantifier),
        }
    }

    pub fn eval_type_parameter(
        &mut self,
        environment: &Environment,
        type_parameter: &Constituent<syntax::TypeParameter>,
        positive: bool,
        type_quantifier: &TypeQuantifier,
    ) -> Result<
        (
            Constituent<TypeParameter>,
            Constituent<KindValue>,
            TypeEnvironment,
        ),
        Error,
    > {
        let location = type_parameter.location();

        match &**type_parameter {
            syntax::TypeParameter::AtomicTypeParameter {
                atomic_type_parameter,
            } => self.eval_atomic_type_parameter(
                environment,
                atomic_type_parameter,
                positive,
                type_quantifier,
            ),
            syntax::TypeParameter::Colon {
                identifier,
                colon: _colon,
                kind_expression,
            } => {
                let kind_value = self.eval_kind_expression(environment, kind_expression)?;
                let type_variable_value = rand::Rng::gen::<TypeVariableLevel>(&mut self.rng);

                let type_value = Constituent::new(
                    kind_value.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Bound(
                        match type_quantifier {
                            TypeQuantifier::Forall => TypeQuantifier::Forall,
                            TypeQuantifier::Exists => TypeQuantifier::Exists,
                        },
                        self.type_variable_level,
                        type_variable_value,
                    ))),
                );

                let type_param =
                    Constituent::new(location, TypeParameter::Single(type_variable_value));

                let type_env = TypeEnvironment::from([(
                    String::from(&**identifier.clone()),
                    (type_value, kind_value.clone()),
                )]);

                Ok((type_param, kind_value, type_env))
            }
        }
    }

    pub fn eval_type_parameters(
        &mut self,
        environment: &Environment,
        type_parameters: &Constituent<syntax::TypeParameters>,
        positive: bool,
        type_quantifier: &TypeQuantifier,
    ) -> Result<
        (
            Constituent<TypeParameter>,
            Constituent<KindValue>,
            TypeEnvironment,
        ),
        Error,
    > {
        let location = type_parameters.location();

        match &**type_parameters {
            syntax::TypeParameters::One { type_parameter } => {
                self.eval_type_parameter(environment, type_parameter, positive, type_quantifier)
            }
            syntax::TypeParameters::More {
                type_parameter,
                comma: _comma,
                type_parameters,
            } => {
                let (type_param, kind_value1, type_env1) = self.eval_type_parameter(
                    environment,
                    type_parameter,
                    positive,
                    type_quantifier,
                )?;
                let (type_params, kind_value2, type_env2) = self.eval_type_parameters(
                    environment,
                    type_parameters,
                    positive,
                    type_quantifier,
                )?;

                let type_params =
                    Constituent::new(location, TypeParameter::Pair(type_param, type_params));
                let kind_value =
                    Constituent::new(location, KindValue::Product(kind_value1, kind_value2));

                let type_env = type_env1.into_iter().chain(type_env2).collect();
                Ok((type_params, kind_value, type_env))
            }
        }
    }

    pub fn eval_type_member(
        &mut self,
        environment: &Environment,
        type_member: &Constituent<syntax::TypeMember>,
        positive: bool,
    ) -> Result<(Constituent<TypeMember>, Constituent<KindValue>), Error> {
        let location = type_member.location();

        match &**type_member {
            syntax::TypeMember::Required {
                identifier,
                colon: _colon,
                type_expression,
            } => {
                let (type_value, kind_value) =
                    self.eval_type_expression(environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(
                        location,
                        TypeMember::Required(identifier.clone(), type_value),
                    ),
                    kind_value,
                ))
            }
            syntax::TypeMember::Optional {
                identifier,
                question: _question,
                colon: _colon,
                type_expression,
            } => {
                let (type_value, kind_value) =
                    self.eval_type_expression(environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(
                        location,
                        TypeMember::Optional(identifier.clone(), type_value),
                    ),
                    kind_value,
                ))
            }
        }
    }

    pub fn eval_type_members(
        &mut self,
        environment: &Environment,
        type_members: &Constituent<syntax::TypeMembers>,
        positive: bool,
    ) -> Result<Vec<(Constituent<TypeMember>, Constituent<KindValue>)>, Error> {
        match &**type_members {
            syntax::TypeMembers::One { type_member } => {
                let type_member = self.eval_type_member(environment, type_member, positive)?;
                Ok(vec![type_member])
            }
            syntax::TypeMembers::More {
                type_member,
                comma: _comma,
                type_members,
            } => {
                let type_member = self.eval_type_member(environment, type_member, positive)?;
                let type_members = self.eval_type_members(environment, type_members, positive)?;
                Ok(vec![vec![type_member], type_members].concat())
            }
        }
    }

    pub fn eval_type_variant(
        &mut self,
        environment: &Environment,
        type_variant: &Constituent<syntax::TypeVariant>,
        positive: bool,
    ) -> Result<(Constituent<TypeVariant>, Constituent<KindValue>), Error> {
        let location = type_variant.location();

        match &**type_variant {
            syntax::TypeVariant::Identifier { identifier } => Ok((
                Constituent::new(location, TypeVariant::Variant(identifier.clone())),
                kind::positive_and_negative(type_variant.location()),
            )),
            syntax::TypeVariant::Of {
                identifier,
                of: _of,
                type_expression,
            } => {
                let (type_value, kind_value) =
                    self.eval_type_expression(environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(
                        location,
                        TypeVariant::VariantOf(identifier.clone(), type_value),
                    ),
                    kind_value,
                ))
            }
        }
    }

    pub fn eval_type_variants(
        &mut self,
        environment: &Environment,
        type_variants: &Constituent<syntax::TypeVariants>,
        positive: bool,
    ) -> Result<Vec<(Constituent<TypeVariant>, Constituent<KindValue>)>, Error> {
        match &**type_variants {
            syntax::TypeVariants::One { type_variant } => {
                let type_variant = self.eval_type_variant(environment, type_variant, positive)?;
                Ok(vec![type_variant])
            }
            syntax::TypeVariants::More {
                type_variant,
                pipe: _pipe,
                type_variants,
            } => {
                let type_variant = self.eval_type_variant(environment, type_variant, positive)?;
                let type_variants =
                    self.eval_type_variants(environment, type_variants, positive)?;
                Ok(vec![vec![type_variant], type_variants].concat())
            }
        }
    }

    pub fn eval_atomic_type_expression(
        &mut self,
        environment: &Environment,
        atomic_type_expression: &Constituent<syntax::AtomicTypeExpression>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        match &**atomic_type_expression {
            syntax::AtomicTypeExpression::QualifiedIdentifier {
                qualified_identifier,
            } => self.eval_type_identifier(environment, qualified_identifier),
            syntax::AtomicTypeExpression::Tuple {
                lparen: _lparen,
                type_expressions,
                rparen: _rparen,
            } => self.eval_type_expressions(environment, type_expressions, positive),
        }
    }

    pub fn eval_applicative_type_expression(
        &mut self,
        environment: &Environment,
        applicative_type_expression: &Constituent<syntax::ApplicativeTypeExpression>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = applicative_type_expression.location();

        match &**applicative_type_expression {
            syntax::ApplicativeTypeExpression::AtomicTypeExpression {
                atomic_type_expression,
            } => self.eval_atomic_type_expression(environment, atomic_type_expression, positive),
            syntax::ApplicativeTypeExpression::Apply {
                applicative_type_expression,
                atomic_type_expression,
            } => {
                let (type_value1, kind_value1) = self.eval_applicative_type_expression(
                    environment,
                    applicative_type_expression,
                    positive,
                )?;

                let (type_value2, kind_value2) = self.eval_atomic_type_expression(
                    environment,
                    atomic_type_expression,
                    positive,
                )?;

                let mut kind_values = vec![];

                for kind_value1 in kind::destruct_intersection(&kind_value1) {
                    if let KindValue::Arrow(kind_value3, kind_value4) = &*kind_value1 {
                        if kind::subtype(&kind_value2, &kind_value3) {
                            kind_values.push(kind_value4.clone());
                        }
                    }
                }

                let kind_value4 = kind::construct_intersection(location, &kind_values)?;

                if let TypeValue::Abstraction(type_parameter, type_value3) = &*type_value1 {
                    let substitution =
                        substitution::from_type_parameter(type_parameter, &type_value2)?;
                    let type_value4 = substitution::substitute(&substitution, type_value3);

                    return Ok((type_value4, kind_value4.clone()));
                } else {
                    return Ok((
                        Constituent::new(
                            location,
                            TypeValue::Application(type_value1, type_value2),
                        ),
                        kind_value4.clone(),
                    ));
                }
            }
        }
    }

    pub fn eval_reference_type_expression(
        &mut self,
        environment: &Environment,
        reference_type_expression: &Constituent<syntax::ReferenceTypeExpression>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = reference_type_expression.location();

        match &**reference_type_expression {
            syntax::ReferenceTypeExpression::ApplicativeTypeExpression {
                applicative_type_expression,
            } => self.eval_applicative_type_expression(
                environment,
                applicative_type_expression,
                positive,
            ),
            syntax::ReferenceTypeExpression::Amp {
                amp: _amp,
                reference_type_expression,
            } => {
                let (type_value, kind_value) = self.eval_reference_type_expression(
                    environment,
                    reference_type_expression,
                    positive,
                )?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(reference_type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: reference_type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(location, TypeValue::Reference(type_value)),
                    if kind::include_positive_or_negative(&kind_value) {
                        kind::positive_or_negative(location, positive)
                    } else {
                        kind::positive_and_negative(location)
                    },
                ))
            }
        }
    }

    pub fn eval_effective_type_expression(
        &mut self,
        environment: &Environment,
        effective_type_expression: &Constituent<syntax::EffectiveTypeExpression>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = effective_type_expression.location();

        match &**effective_type_expression {
            syntax::EffectiveTypeExpression::ReferenceTypeExpression {
                reference_type_expression,
            } => self.eval_reference_type_expression(
                environment,
                reference_type_expression,
                positive,
            ),
            syntax::EffectiveTypeExpression::Exclamation {
                effective_type_expression,
                exclamation: _exclamation,
            } => {
                let (type_value, kind_value) = self.eval_effective_type_expression(
                    environment,
                    effective_type_expression,
                    positive,
                )?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(effective_type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: effective_type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(location, TypeValue::Effect(type_value)),
                    if kind::include_positive_or_negative(&kind_value) {
                        kind::positive_or_negative(location, positive)
                    } else {
                        kind::positive_and_negative(location)
                    },
                ))
            }
        }
    }

    pub fn eval_multiplicative_type_expression(
        &mut self,
        environment: &Environment,
        multiplicative_type_expression: &Constituent<syntax::MultiplicativeTypeExpression>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = multiplicative_type_expression.location();

        match &**multiplicative_type_expression {
            syntax::MultiplicativeTypeExpression::EffectiveTypeExpression {
                effective_type_expression,
            } => self.eval_effective_type_expression(
                environment,
                effective_type_expression,
                positive,
            ),
            syntax::MultiplicativeTypeExpression::Asterisk {
                effective_type_expression,
                asterisk: _asterisk,
                multiplicative_type_expression,
            } => {
                let (type_value1, kind_value1) = self.eval_effective_type_expression(
                    environment,
                    effective_type_expression,
                    positive,
                )?;

                let (type_value2, kind_value2) = self.eval_multiplicative_type_expression(
                    environment,
                    multiplicative_type_expression,
                    positive,
                )?;

                if !kind::subtype(
                    &kind_value1,
                    &kind::positive_or_negative(
                        multiplicative_type_expression.location(),
                        positive,
                    ),
                ) {
                    return Err(Error::KindMismatch {
                        location: multiplicative_type_expression.location(),
                    });
                }

                if !kind::subtype(
                    &kind_value2,
                    &kind::positive_or_negative(effective_type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: effective_type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(location, TypeValue::Product(type_value1, type_value2)),
                    if kind::include_positive_or_negative(&kind_value1)
                        && kind::include_positive_or_negative(&kind_value2)
                    {
                        kind::positive_or_negative(location, positive)
                    } else {
                        kind::positive_and_negative(location)
                    },
                ))
            }
        }
    }

    pub fn eval_type_expression(
        &mut self,
        environment: &Environment,
        type_expression: &Constituent<syntax::TypeExpression>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = type_expression.location();

        match &**type_expression {
            syntax::TypeExpression::MultiplicativeTypeExpression {
                multiplicative_type_expression,
            } => self.eval_multiplicative_type_expression(
                environment,
                multiplicative_type_expression,
                positive,
            ),
            syntax::TypeExpression::Arrow {
                multiplicative_type_expression,
                arrow: _arrow,
                type_expression,
            } => {
                let (type_value1, kind_value1) = self.eval_multiplicative_type_expression(
                    environment,
                    multiplicative_type_expression,
                    !positive,
                )?;

                let (type_value2, kind_value2) =
                    self.eval_type_expression(environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value1,
                    &kind::positive_or_negative(
                        multiplicative_type_expression.location(),
                        !positive,
                    ),
                ) {
                    return Err(Error::KindMismatch {
                        location: multiplicative_type_expression.location(),
                    });
                }

                if !kind::subtype(
                    &kind_value2,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(location, TypeValue::Arrow(type_value1, type_value2)),
                    if kind::include_positive_or_negative(&kind_value1)
                        && kind::include_positive_or_negative(&kind_value2)
                    {
                        kind::positive_or_negative(location, positive)
                    } else {
                        kind::positive_and_negative(location)
                    },
                ))
            }
            syntax::TypeExpression::Abstraction {
                hat: _hat,
                atomic_type_parameter,
                dot: _dot,
                type_expression,
            } => {
                let (type_param, kind_value1, mut type_env) = self.eval_atomic_type_parameter(
                    environment,
                    atomic_type_parameter,
                    positive,
                    &TypeQuantifier::Forall,
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.type_env.append(&mut type_env);

                let (type_value, kind_value2) =
                    self.eval_type_expression(&local_environment, type_expression, positive)?;

                Ok((
                    Constituent::new(location, TypeValue::Abstraction(type_param, type_value)),
                    Constituent::new(location, KindValue::Arrow(kind_value1, kind_value2)),
                ))
            }
            syntax::TypeExpression::Forall {
                forall: _forall,
                atomic_type_parameter,
                dot: _dot,
                type_expression,
            } => {
                let (type_param, _kind_value1, mut type_env) = self.eval_atomic_type_parameter(
                    environment,
                    atomic_type_parameter,
                    positive,
                    &TypeQuantifier::Forall,
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.type_env.append(&mut type_env);

                let (type_value, kind_value2) =
                    self.eval_type_expression(&local_environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value2,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(location, TypeValue::Forall(type_param, type_value)),
                    if kind::include_positive_or_negative(&kind_value2) {
                        kind::positive_or_negative(location, positive)
                    } else {
                        kind::positive_and_negative(location)
                    },
                ))
            }
            syntax::TypeExpression::Exists {
                exists: _exists,
                atomic_type_parameter,
                dot: _dot,
                type_expression,
            } => {
                let (type_param, _kind_value1, mut type_env) = self.eval_atomic_type_parameter(
                    environment,
                    atomic_type_parameter,
                    positive,
                    &TypeQuantifier::Exists,
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.type_env.append(&mut type_env);

                let (type_value, kind_value2) =
                    self.eval_type_expression(&local_environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value2,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                Ok((
                    Constituent::new(location, TypeValue::Exists(type_param, type_value)),
                    if kind::include_positive_or_negative(&kind_value2) {
                        kind::positive_or_negative(location, positive)
                    } else {
                        kind::positive_and_negative(location)
                    },
                ))
            }
        }
    }

    pub fn eval_type_expressions(
        &mut self,
        environment: &Environment,
        type_expressions: &Constituent<syntax::TypeExpressions>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, Constituent<KindValue>), Error> {
        let location = type_expressions.location();

        match &**type_expressions {
            syntax::TypeExpressions::One { type_expression } => {
                self.eval_type_expression(environment, type_expression, positive)
            }
            syntax::TypeExpressions::More {
                type_expression,
                comma: _comma,
                type_expressions,
            } => {
                let (type_value1, kind_value1) =
                    self.eval_type_expression(environment, type_expression, positive)?;
                let (type_value2, kind_value2) =
                    self.eval_type_expressions(environment, type_expressions, positive)?;

                let type_value =
                    Constituent::new(location, TypeValue::Pair(type_value1, type_value2));
                let kind_value =
                    Constituent::new(location, KindValue::Product(kind_value1, kind_value2));

                Ok((type_value, kind_value))
            }
        }
    }

    fn eval_type_definition_type(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        identifier: &Lexeme,
        type_parameters: Option<&Constituent<syntax::TypeParameters>>,
        kind_expression: Option<&Constituent<syntax::KindExpression>>,
        type_expression: &Constituent<syntax::TypeExpression>,
    ) -> Result<(), Error> {
        if export_environment.record_env.get(&**identifier).is_some() {
            return Err(Error::RecordAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.variant_env.get(&**identifier).is_some() {
            return Err(Error::VariantAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.family_env.get(&**identifier).is_some() {
            return Err(Error::FamilyAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.type_env.get(&**identifier).is_some() {
            return Err(Error::TypeAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.value_env.get(&**identifier).is_some() {
            return Err(Error::ValueAlreadyDefined {
                location: identifier.location(),
            });
        }

        let mut local_environment = Environment {
            family_path: environment.family_path.clone(),
            family_parameter: environment.family_parameter.clone(),
            family_parameter_kind: environment.family_parameter_kind.clone(),
            family_parameter_type: environment.family_parameter_type.clone(),
            family_type: environment.family_type.clone(),
            type_env: environment.type_env.clone(),
            value_env: environment.value_env.clone(),
            family_env: environment.family_env.clone(),
            record_env: environment.record_env.clone(),
            variant_env: environment.variant_env.clone(),
        };

        local_environment
            .type_env
            .append(&mut local_type_env.clone());

        let (type_param, kind_value1) = if let Some(type_parameters) = type_parameters {
            let (type_param, kind_value1, mut type_env) = self.eval_type_parameters(
                &local_environment,
                type_parameters,
                true,
                &TypeQuantifier::Forall,
            )?;

            local_environment.type_env.append(&mut type_env);

            (Some(type_param), Some(kind_value1))
        } else {
            (None, None)
        };

        let kind_value2 = if let Some(kind_expression) = kind_expression {
            let kind_value2 = self.eval_kind_expression(&local_environment, kind_expression)?;
            Some(kind_value2)
        } else {
            None
        };

        let (type_value, kind_value3) =
            self.eval_type_expression(&local_environment, type_expression, true)?;

        if let Some(ref kind_value2) = kind_value2 {
            if !kind::subtype(&kind_value3, &kind_value2) {
                return Err(Error::KindMismatch {
                    location: type_expression.location(),
                });
            }
        }

        let kind_value = if let Some(kind_value1) = kind_value1 {
            if let Some(kind_value2) = kind_value2 {
                Constituent::new(
                    identifier.location(),
                    KindValue::Arrow(kind_value1, kind_value2),
                )
            } else {
                Constituent::new(
                    identifier.location(),
                    KindValue::Arrow(kind_value1, kind_value3),
                )
            }
        } else {
            if let Some(kind_value2) = kind_value2 {
                kind_value2
            } else {
                kind_value3
            }
        };

        let kind_value = if kind::include_positive_or_negative(&kind_value) {
            let inverse_kind_value = kind::inverse(&kind_value);

            kind::construct_intersection(
                type_expression.location(),
                &vec![kind_value, inverse_kind_value],
            )?
        } else {
            kind_value
        };

        let type_value = if let Some(type_param) = type_param {
            Constituent::new(
                identifier.location(),
                TypeValue::Abstraction(type_param, type_value),
            )
        } else {
            type_value
        };

        export_environment.type_env.insert(
            String::from(&**identifier),
            (type_value.clone(), kind_value.clone()),
        );

        Ok(())
    }

    fn eval_type_definition_record(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        identifier: &Lexeme,
        type_parameters: Option<&Constituent<syntax::TypeParameters>>,
        kind_expression: Option<&Constituent<syntax::KindExpression>>,
        type_members: Option<&Constituent<syntax::TypeMembers>>,
    ) -> Result<(), Error> {
        if export_environment.record_env.get(&**identifier).is_some() {
            return Err(Error::RecordAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.variant_env.get(&**identifier).is_some() {
            return Err(Error::VariantAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.family_env.get(&**identifier).is_some() {
            return Err(Error::FamilyAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.type_env.get(&**identifier).is_some() {
            return Err(Error::TypeAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.value_env.get(&**identifier).is_some() {
            return Err(Error::ValueAlreadyDefined {
                location: identifier.location(),
            });
        }

        let mut local_environment = Environment {
            family_path: environment.family_path.clone(),
            family_parameter: environment.family_parameter.clone(),
            family_parameter_kind: environment.family_parameter_kind.clone(),
            family_parameter_type: environment.family_parameter_type.clone(),
            family_type: environment.family_type.clone(),
            type_env: environment.type_env.clone(),
            value_env: environment.value_env.clone(),
            family_env: environment.family_env.clone(),
            record_env: environment.record_env.clone(),
            variant_env: environment.variant_env.clone(),
        };

        local_environment
            .type_env
            .append(&mut local_type_env.clone());

        let (type_param, kind_value1) = if let Some(type_parameters) = type_parameters {
            let (type_param, kind_value1, mut type_env) = self.eval_type_parameters(
                &local_environment,
                type_parameters,
                true,
                &TypeQuantifier::Forall,
            )?;

            local_environment.type_env.append(&mut type_env);

            (Some(type_param), Some(kind_value1))
        } else {
            (None, None)
        };

        let kind_value2 = if let Some(kind_expression) = kind_expression {
            let kind_value2 = self.eval_kind_expression(&local_environment, kind_expression)?;
            Some(kind_value2)
        } else {
            None
        };

        let mut accessors = ValueEnvironment::new();
        let mut kind_include_positive_or_negative = false;
        let mut members = vec![];

        if let Some(type_members) = type_members {
            let type_members = self.eval_type_members(&local_environment, type_members, true)?;

            for (type_member, kind_value3) in type_members.iter() {
                members.push(type_member.clone());

                match &**type_member {
                    TypeMember::Required(id, type_value) => {
                        if accessors.get(&**id).is_some() {
                            return Err(Error::ValueAlreadyDefined {
                                location: id.location(),
                            });
                        }

                        kind_include_positive_or_negative = kind_include_positive_or_negative
                            || kind::include_positive_or_negative(&kind_value3);

                        if let Some(ref type_param) = type_param {
                            let type_arg = self.type_parameter_to_type_argument(type_param);

                            accessors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Forall(
                                        type_param.clone(),
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::Arrow(
                                                Constituent::new(
                                                    id.location(),
                                                    TypeValue::Application(
                                                        Constituent::new(
                                                            id.location(),
                                                            TypeValue::NominalRecord(
                                                                vec![
                                                                    environment.family_path.clone(),
                                                                    vec![identifier.clone()],
                                                                ]
                                                                .concat(),
                                                            ),
                                                        ),
                                                        type_arg,
                                                    ),
                                                ),
                                                type_value.clone(),
                                            ),
                                        ),
                                    ),
                                ),
                            );
                        } else {
                            accessors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Arrow(
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::NominalRecord(
                                                vec![
                                                    environment.family_path.clone(),
                                                    vec![identifier.clone()],
                                                ]
                                                .concat(),
                                            ),
                                        ),
                                        type_value.clone(),
                                    ),
                                ),
                            );
                        }
                    }
                    TypeMember::Optional(id, type_value) => {
                        if accessors.get(&**id).is_some() {
                            return Err(Error::ValueAlreadyDefined {
                                location: id.location(),
                            });
                        }

                        kind_include_positive_or_negative = kind_include_positive_or_negative
                            || kind::include_positive_or_negative(&kind_value3);

                        if let Some(ref type_param) = type_param {
                            let type_arg = self.type_parameter_to_type_argument(type_param);

                            accessors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Forall(
                                        type_param.clone(),
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::Arrow(
                                                Constituent::new(
                                                    id.location(),
                                                    TypeValue::Product(
                                                Constituent::new(
                                                    id.location(),
                                                    TypeValue::Application(
                                                        Constituent::new(
                                                            id.location(),
                                                            TypeValue::NominalRecord(
                                                                vec![
                                                                    environment.family_path.clone(),
                                                                    vec![identifier.clone()],
                                                                ]
                                                                .concat(),
                                                            ),
                                                        ), type_arg
                                                    )),
                                                        type_value.clone(),
                                                    ),
                                                ),
                                                type_value.clone(),
                                            ),
                                        ),
                                    ),
                                ),
                            );
                        } else {
                            accessors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Arrow(
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::Product(
                                                Constituent::new(
                                                    id.location(),
                                                    TypeValue::NominalRecord(
                                                        vec![
                                                            environment.family_path.clone(),
                                                            vec![identifier.clone()],
                                                        ]
                                                        .concat(),
                                                    ),
                                                ),
                                                type_value.clone(),
                                            ),
                                        ),
                                        type_value.clone(),
                                    ),
                                ),
                            );
                        }
                    }
                }
            }
        }

        let kind_value3 = if kind_include_positive_or_negative {
            kind::positive_or_negative(identifier.location(), true)
        } else {
            kind::positive_and_negative(identifier.location())
        };

        if let Some(ref kind_value2) = kind_value2 {
            if !kind::subtype(&kind_value3, kind_value2) {
                return Err(Error::KindMismatch {
                    location: kind_value2.location(),
                });
            }
        }

        let kind_value = if let Some(kind_value1) = kind_value1 {
            if let Some(kind_value2) = kind_value2 {
                Constituent::new(
                    identifier.location(),
                    KindValue::Arrow(kind_value1, kind_value2),
                )
            } else {
                Constituent::new(
                    identifier.location(),
                    KindValue::Arrow(kind_value1, kind_value3),
                )
            }
        } else {
            if let Some(kind_value2) = kind_value2 {
                kind_value2
            } else {
                kind_value3
            }
        };

        let kind_value = if kind::include_positive_or_negative(&kind_value) {
            let inverse_kind_value = kind::inverse(&kind_value);

            kind::construct_intersection(
                kind_value.location(),
                &vec![kind_value, inverse_kind_value],
            )?
        } else {
            kind_value
        };

        let type_value = if let Some(ref type_param) = type_param {
            let type_arg = self.type_parameter_to_type_argument(type_param);

            Constituent::new(
                identifier.location(),
                TypeValue::Abstraction(
                    type_param.clone(),
                    Constituent::new(
                        identifier.location(),
                        TypeValue::Application(
                            Constituent::new(
                                identifier.location(),
                                TypeValue::NominalRecord(
                                    vec![environment.family_path.clone(), vec![identifier.clone()]]
                                        .concat(),
                                ),
                            ),
                            type_arg,
                        ),
                    ),
                ),
            )
        } else {
            Constituent::new(
                identifier.location(),
                TypeValue::NominalRecord(
                    vec![environment.family_path.clone(), vec![identifier.clone()]].concat(),
                ),
            )
        };

        export_environment.type_env.insert(
            String::from(&**identifier),
            (type_value.clone(), kind_value.clone()),
        );

        export_environment.family_env.insert(
            String::from(&**identifier),
            Constituent::new(
                identifier.location(),
                FamilyValue {
                    family_path: vec![],
                    family_parameter: None,
                    family_parameter_kind: None,
                    family_parameter_type: None,
                    family_type: None,
                    type_env: TypeEnvironment::new(),
                    value_env: accessors,
                    family_env: FamilyEnvironment::new(),
                    record_env: RecordEnvironment::new(),
                    variant_env: VariantEnvironment::new(),
                },
            ),
        );

        export_environment
            .record_env
            .insert(String::from(&**identifier), members);

        Ok(())
    }

    fn eval_type_definition_variant(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        identifier: &Lexeme,
        type_parameters: Option<&Constituent<syntax::TypeParameters>>,
        kind_expression: Option<&Constituent<syntax::KindExpression>>,
        type_variants: Option<&Constituent<syntax::TypeVariants>>,
    ) -> Result<(), Error> {
        if export_environment.record_env.get(&**identifier).is_some() {
            return Err(Error::RecordAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.variant_env.get(&**identifier).is_some() {
            return Err(Error::VariantAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.family_env.get(&**identifier).is_some() {
            return Err(Error::FamilyAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.type_env.get(&**identifier).is_some() {
            return Err(Error::TypeAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.value_env.get(&**identifier).is_some() {
            return Err(Error::ValueAlreadyDefined {
                location: identifier.location(),
            });
        }

        let mut local_environment = Environment {
            family_path: environment.family_path.clone(),
            family_parameter: environment.family_parameter.clone(),
            family_parameter_kind: environment.family_parameter_kind.clone(),
            family_parameter_type: environment.family_parameter_type.clone(),
            family_type: environment.family_type.clone(),
            type_env: environment.type_env.clone(),
            value_env: environment.value_env.clone(),
            family_env: environment.family_env.clone(),
            record_env: environment.record_env.clone(),
            variant_env: environment.variant_env.clone(),
        };

        local_environment
            .type_env
            .append(&mut local_type_env.clone());

        let (type_param, kind_value1) = if let Some(type_parameters) = type_parameters {
            let (type_param, kind_value1, mut type_env) = self.eval_type_parameters(
                &local_environment,
                type_parameters,
                true,
                &TypeQuantifier::Forall,
            )?;

            local_environment.type_env.append(&mut type_env);

            (Some(type_param), Some(kind_value1))
        } else {
            (None, None)
        };

        let kind_value2 = if let Some(kind_expression) = kind_expression {
            let kind_value2 = self.eval_kind_expression(&local_environment, kind_expression)?;
            Some(kind_value2)
        } else {
            None
        };

        let mut variants = vec![];
        let mut constructors = ValueEnvironment::new();
        let mut kind_include_positive_or_negative = false;

        if let Some(type_variants) = type_variants {
            let type_variants = self.eval_type_variants(&local_environment, type_variants, true)?;

            for (type_variant, kind_value3) in type_variants.iter() {
                variants.push(type_variant.clone());

                match &**type_variant {
                    TypeVariant::Variant(id) => {
                        if constructors.get(&**id).is_some() {
                            return Err(Error::ValueAlreadyDefined {
                                location: id.location(),
                            });
                        }

                        if let Some(ref type_param) = type_param {
                            let type_arg = self.type_parameter_to_type_argument(type_param);

                            constructors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Forall(
                                        type_param.clone(),
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::Application(
                                                Constituent::new(
                                                    id.location(),
                                                    TypeValue::NominalVariant(
                                                        vec![
                                                            environment.family_path.clone(),
                                                            vec![identifier.clone()],
                                                        ]
                                                        .concat(),
                                                    ),
                                                ),
                                                type_arg,
                                            ),
                                        ),
                                    ),
                                ),
                            );
                        } else {
                            constructors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::NominalVariant(
                                        vec![
                                            environment.family_path.clone(),
                                            vec![identifier.clone()],
                                        ]
                                        .concat(),
                                    ),
                                ),
                            );
                        }
                    }
                    TypeVariant::VariantOf(id, type_value) => {
                        if constructors.get(&**id).is_some() {
                            return Err(Error::ValueAlreadyDefined {
                                location: id.location(),
                            });
                        }

                        kind_include_positive_or_negative = kind_include_positive_or_negative
                            || kind::include_positive_or_negative(&kind_value3);

                        if let Some(ref type_param) = type_param {
                            let type_arg = self.type_parameter_to_type_argument(type_param);

                            constructors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Forall(
                                        type_param.clone(),
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::Arrow(
                                                type_value.clone(),
                                                Constituent::new(
                                                    id.location(),
                                                    TypeValue::Application(
                                                        Constituent::new(
                                                            id.location(),
                                                            TypeValue::NominalVariant(
                                                                vec![
                                                                    environment.family_path.clone(),
                                                                    vec![identifier.clone()],
                                                                ]
                                                                .concat(),
                                                            ),
                                                        ),
                                                        type_arg,
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            );
                        } else {
                            constructors.insert(
                                (&**id).clone(),
                                Constituent::new(
                                    id.location(),
                                    TypeValue::Arrow(
                                        type_value.clone(),
                                        Constituent::new(
                                            id.location(),
                                            TypeValue::NominalVariant(
                                                vec![
                                                    environment.family_path.clone(),
                                                    vec![identifier.clone()],
                                                ]
                                                .concat(),
                                            ),
                                        ),
                                    ),
                                ),
                            );
                        }
                    }
                }
            }
        }

        let kind_value3 = if kind_include_positive_or_negative {
            kind::positive_or_negative(identifier.location(), true)
        } else {
            kind::positive_and_negative(identifier.location())
        };

        if let Some(ref kind_value2) = kind_value2 {
            if !kind::subtype(&kind_value3, kind_value2) {
                return Err(Error::KindMismatch {
                    location: kind_value2.location(),
                });
            }
        }

        let kind_value = if let Some(kind_value1) = kind_value1 {
            if let Some(kind_value2) = kind_value2 {
                Constituent::new(
                    identifier.location(),
                    KindValue::Arrow(kind_value1, kind_value2),
                )
            } else {
                Constituent::new(
                    identifier.location(),
                    KindValue::Arrow(kind_value1, kind_value3),
                )
            }
        } else {
            if let Some(kind_value2) = kind_value2 {
                kind_value2
            } else {
                kind_value3
            }
        };

        let kind_value = if kind::include_positive_or_negative(&kind_value) {
            let inverse_kind_value = kind::inverse(&kind_value);

            kind::construct_intersection(
                kind_value.location(),
                &vec![kind_value, inverse_kind_value],
            )?
        } else {
            kind_value
        };

        let type_value = if let Some(ref type_param) = type_param {
            let type_arg = self.type_parameter_to_type_argument(type_param);

            Constituent::new(
                identifier.location(),
                TypeValue::Abstraction(
                    type_param.clone(),
                    Constituent::new(
                        identifier.location(),
                        TypeValue::Application(
                            Constituent::new(
                                identifier.location(),
                                TypeValue::NominalVariant(
                                    vec![environment.family_path.clone(), vec![identifier.clone()]]
                                        .concat(),
                                ),
                            ),
                            type_arg,
                        ),
                    ),
                ),
            )
        } else {
            Constituent::new(
                identifier.location(),
                TypeValue::NominalVariant(
                    vec![environment.family_path.clone(), vec![identifier.clone()]].concat(),
                ),
            )
        };

        export_environment.type_env.insert(
            String::from(&**identifier),
            (type_value.clone(), kind_value.clone()),
        );

        export_environment.family_env.insert(
            String::from(&**identifier),
            Constituent::new(
                identifier.location(),
                FamilyValue {
                    family_path: vec![],
                    family_parameter: None,
                    family_parameter_kind: None,
                    family_parameter_type: None,
                    family_type: None,
                    type_env: TypeEnvironment::new(),
                    value_env: constructors,
                    family_env: FamilyEnvironment::new(),
                    record_env: RecordEnvironment::new(),
                    variant_env: VariantEnvironment::new(),
                },
            ),
        );

        export_environment
            .variant_env
            .insert(String::from(&**identifier), variants);

        Ok(())
    }

    fn eval_type_definition_structure(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        identifier: &Lexeme,
        type_parameters: Option<&Constituent<syntax::TypeParameters>>,
        kind_expression: Option<&Constituent<syntax::KindExpression>>,
        structure_expression: &Constituent<syntax::StructureExpression>,
    ) -> Result<(), Error> {
        match &**structure_expression {
            syntax::StructureExpression::RecordExpression { record_expression } => {
                if let syntax::RecordExpression::NonEmpty {
                    lbrace: _lbrace,
                    type_members,
                    rbrace: _rbrace,
                } = &**record_expression
                {
                    self.eval_type_definition_record(
                        environment,
                        export_environment,
                        local_type_env,
                        identifier,
                        type_parameters,
                        kind_expression,
                        Some(type_members),
                    )
                } else {
                    self.eval_type_definition_record(
                        environment,
                        export_environment,
                        local_type_env,
                        identifier,
                        type_parameters,
                        kind_expression,
                        None,
                    )
                }
            }
            syntax::StructureExpression::VariantExpression { variant_expression } => {
                if let syntax::VariantExpression::NonEmpty {
                    lbracket: _lbracket,
                    type_variants,
                    rbracket: _rbracket,
                } = &**variant_expression
                {
                    self.eval_type_definition_variant(
                        environment,
                        export_environment,
                        local_type_env,
                        identifier,
                        type_parameters,
                        kind_expression,
                        Some(type_variants),
                    )
                } else {
                    self.eval_type_definition_variant(
                        environment,
                        export_environment,
                        local_type_env,
                        identifier,
                        type_parameters,
                        kind_expression,
                        None,
                    )
                }
            }
        }
    }

    pub fn eval_type_definition(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        type_definition: &Constituent<syntax::TypeDefinition>,
    ) -> Result<(), Error> {
        match &**type_definition {
            syntax::TypeDefinition::Type {
                type_: _type_,
                identifier,
                coloneq: _coloneq,
                type_expression,
            } => self.eval_type_definition_type(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                type_expression,
            ),
            syntax::TypeDefinition::TypeColon {
                type_: _type_,
                identifier,
                colon: _colon,
                kind_expression,
                coloneq: _coloneq,
                type_expression,
            } => self.eval_type_definition_type(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(kind_expression),
                type_expression,
            ),
            syntax::TypeDefinition::TypeFn {
                type_: _type_,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                type_expression,
            } => self.eval_type_definition_type(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                type_expression,
            ),
            syntax::TypeDefinition::TypeFnColon {
                type_: _type_,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                colon: _colon,
                kind_expression,
                coloneq: _coloneq,
                type_expression,
            } => self.eval_type_definition_type(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(kind_expression),
                type_expression,
            ),
            syntax::TypeDefinition::Struct {
                struct_: _struct_,
                identifier,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                structure_expression,
            ),
            syntax::TypeDefinition::StructColon {
                struct_: _struct_,
                identifier,
                colon: _colon,
                kind_expression,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(kind_expression),
                structure_expression,
            ),
            syntax::TypeDefinition::StructFn {
                struct_: _struct_,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                structure_expression,
            ),
            syntax::TypeDefinition::StructFnColon {
                struct_: _struct_,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                colon: _colon,
                kind_expression,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(kind_expression),
                structure_expression,
            ),
            syntax::TypeDefinition::Inductive {
                inductive: _inductive,
                identifier,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                structure_expression,
            ),
            syntax::TypeDefinition::InductiveColon {
                inductive: _inductive,
                identifier,
                colon: _colon,
                kind_expression,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(kind_expression),
                structure_expression,
            ),
            syntax::TypeDefinition::InductiveFn {
                inductive: _inductive,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                structure_expression,
            ),
            syntax::TypeDefinition::InductiveFnColon {
                inductive: _inductive,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                colon: _colon,
                kind_expression,
                coloneq: _coloneq,
                structure_expression,
            } => self.eval_type_definition_structure(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(kind_expression),
                structure_expression,
            ),
        }
    }

    pub fn eval_value_parameter_member(
        &mut self,
        environment: &Environment,
        value_parameter_member: &Constituent<syntax::ValueParameterMember>,
    ) -> Result<(Lexeme, bool, Constituent<TypeValue>, ValueEnvironment), Error> {
        match &**value_parameter_member {
            syntax::ValueParameterMember::Identifier { identifier } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                let type_value = Constituent::new(
                    identifier.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Free(
                        TypeQuantifier::Forall,
                        self.type_variable_level,
                        type_variable_value,
                    ))),
                );

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value.clone());

                Ok((identifier.clone(), true, type_value, value_env))
            }
            syntax::ValueParameterMember::Default {
                identifier,
                question: _question,
                value_expression,
            } => {
                let type_value = self.type_value_expression(environment, value_expression)?;

                if rank(&type_value) != rank(&unfold_effect(&type_value)) {
                    return Err(Error::EffectInPattern {
                        location: value_expression.location(),
                    });
                }

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value.clone());

                Ok((identifier.clone(), false, type_value, value_env))
            }
            syntax::ValueParameterMember::DefaultMap {
                identifier,
                question: _question,
                value_expression1,
                colon: _colon,
                value_expression2,
            } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                let type_value = Constituent::new(
                    identifier.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Free(
                        TypeQuantifier::Forall,
                        self.type_variable_level,
                        type_variable_value,
                    ))),
                );

                let type_value1 = self.type_value_expression(environment, value_expression1)?;
                let type_value2 = self.type_value_expression(environment, value_expression2)?;

                if rank(&type_value1) != rank(&unfold_effect(&type_value1)) {
                    return Err(Error::EffectInPattern {
                        location: value_expression1.location(),
                    });
                }

                if rank(&type_value2) != rank(&unfold_effect(&type_value2)) {
                    return Err(Error::EffectInPattern {
                        location: value_expression2.location(),
                    });
                }

                unify(
                    value_expression2.location(),
                    &Constituent::new(
                        value_expression2.location(),
                        TypeValue::Arrow(type_value.clone(), type_value1.clone()),
                    ),
                    &type_value2,
                )?;

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value1.clone());

                Ok((identifier.clone(), false, type_value, value_env))
            }
            syntax::ValueParameterMember::Colon {
                identifier,
                colon: _colon,
                type_expression,
            } => {
                let (type_value, kind_value) =
                    self.eval_type_expression(environment, type_expression, true)?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), true),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value.clone());

                Ok((identifier.clone(), true, type_value, value_env))
            }
            syntax::ValueParameterMember::ColonDefault {
                identifier,
                colon: _colon,
                type_expression,
                question: _question,
                value_expression,
            } => {
                let (type_value1, kind_value) =
                    self.eval_type_expression(environment, type_expression, true)?;
                let type_value2 = self.type_value_expression(environment, value_expression)?;

                if rank(&type_value2) != rank(&unfold_effect(&type_value2)) {
                    return Err(Error::EffectInPattern {
                        location: value_expression.location(),
                    });
                }

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), true),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                let type_value3 = self.ascribe_type(&type_value1);
                unify(value_expression.location(), &type_value2, &type_value3)?;

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value1.clone());

                Ok((identifier.clone(), false, type_value1, value_env))
            }
            syntax::ValueParameterMember::ColonDefaultMap {
                identifier,
                colon1: _colon1,
                type_expression,
                question: _question,
                value_expression1,
                colon2: _colon2,
                value_expression2,
            } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                let type_value = Constituent::new(
                    identifier.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Free(
                        TypeQuantifier::Forall,
                        self.type_variable_level,
                        type_variable_value,
                    ))),
                );

                let (type_value1, kind_value) =
                    self.eval_type_expression(environment, type_expression, true)?;
                let type_value2 = self.type_value_expression(environment, value_expression1)?;
                let type_value3 = self.type_value_expression(environment, value_expression2)?;

                if rank(&type_value2) != rank(&unfold_effect(&type_value2)) {
                    return Err(Error::EffectInPattern {
                        location: value_expression1.location(),
                    });
                }

                if rank(&type_value3) != rank(&unfold_effect(&type_value3)) {
                    return Err(Error::EffectInPattern {
                        location: value_expression2.location(),
                    });
                }

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), true),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                let type_value4 = self.ascribe_type(&type_value1);
                unify(value_expression1.location(), &type_value2, &type_value)?;
                unify(
                    value_expression2.location(),
                    &type_value3,
                    &Constituent::new(
                        value_expression2.location(),
                        TypeValue::Arrow(type_value4.clone(), type_value.clone()),
                    ),
                )?;

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value.clone());

                Ok((identifier.clone(), false, type_value1, value_env))
            }
        }
    }

    pub fn eval_value_parameter_members(
        &mut self,
        environment: &Environment,
        value_parameter_members: &Constituent<syntax::ValueParameterMembers>,
    ) -> Result<
        (
            Vec<(Lexeme, bool, Constituent<TypeValue>)>,
            ValueEnvironment,
        ),
        Error,
    > {
        match &**value_parameter_members {
            syntax::ValueParameterMembers::One {
                value_parameter_member,
            } => {
                let (id, required, type_value, value_env) =
                    self.eval_value_parameter_member(environment, value_parameter_member)?;

                Ok((vec![(id, required, type_value)], value_env))
            }
            syntax::ValueParameterMembers::More {
                value_parameter_member,
                comma: _comma,
                value_parameter_members,
            } => {
                let (id, required, type_value, mut value_env1) =
                    self.eval_value_parameter_member(environment, value_parameter_member)?;
                let (mut type_values2, mut value_env2) =
                    self.eval_value_parameter_members(environment, value_parameter_members)?;

                let mut type_values1 = vec![(id, required, type_value)];

                type_values1.append(&mut type_values2);
                value_env1.append(&mut value_env2);

                Ok((type_values1, value_env1))
            }
        }
    }

    pub fn eval_atomic_value_parameter(
        &mut self,
        environment: &Environment,
        atomic_value_parameter: &Constituent<syntax::AtomicValueParameter>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, ValueEnvironment), Error> {
        match &**atomic_value_parameter {
            syntax::AtomicValueParameter::Identifier { identifier } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);

                let type_value = Constituent::new(
                    identifier.location(),
                    TypeValue::TypeVariable(TypeVariableRef::new(TypeVariable::Free(
                        TypeQuantifier::Forall,
                        self.type_variable_level,
                        type_variable_value,
                    ))),
                );

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value.clone());

                Ok((type_value, value_env))
            }
            syntax::AtomicValueParameter::Tuple {
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
            } => self.eval_value_parameters(environment, value_parameters, positive),
        }
    }

    pub fn eval_value_parameter(
        &mut self,
        environment: &Environment,
        value_parameter: &Constituent<syntax::ValueParameter>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, ValueEnvironment), Error> {
        let location = value_parameter.location();

        match &**value_parameter {
            syntax::ValueParameter::AtomicValueParameter {
                atomic_value_parameter,
            } => self.eval_atomic_value_parameter(environment, atomic_value_parameter, positive),
            syntax::ValueParameter::Colon {
                identifier,
                colon: _colon,
                type_expression,
            } => {
                let (type_value, kind_value) =
                    self.eval_type_expression(environment, type_expression, positive)?;

                if !kind::subtype(
                    &kind_value,
                    &kind::positive_or_negative(type_expression.location(), positive),
                ) {
                    return Err(Error::KindMismatch {
                        location: type_expression.location(),
                    });
                }

                let mut value_env = ValueEnvironment::new();
                value_env.insert(String::from(&**identifier), type_value.clone());

                Ok((type_value, value_env))
            }
            syntax::ValueParameter::EmptyRecord {
                qualified_identifier,
                lbrace: _lbrace,
                rbrace: _rbrace,
            } => {
                let type_members =
                    self.eval_record_identifier(environment, qualified_identifier)?;

                let (type_value, _kind_value) =
                    self.eval_type_identifier(environment, qualified_identifier)?;

                for member in type_members {
                    if let TypeMember::Required(_, _) = &*member {
                        return Err(Error::InvalidPattern { location });
                    }
                }

                let type_value = if let TypeValue::Abstraction(type_parameter, t) = &*type_value {
                    self.instantiate_type(&Constituent::new(
                        type_value.location(),
                        TypeValue::Forall(type_parameter.clone(), t.clone()),
                    ))
                } else {
                    type_value
                };

                let value_env = ValueEnvironment::new();

                Ok((type_value, value_env))
            }
            syntax::ValueParameter::NonEmptyRecord {
                qualified_identifier,
                lbrace: _lbrace,
                value_parameter_members,
                rbrace: _rbrace,
            } => {
                let type_members =
                    self.eval_record_identifier(environment, qualified_identifier)?;

                let (type_value, _kind_value) =
                    self.eval_type_identifier(environment, qualified_identifier)?;

                let (members, value_env) =
                    self.eval_value_parameter_members(environment, value_parameter_members)?;

                let mut required_ids1 = std::collections::BTreeSet::new();
                let mut optional_ids1 = std::collections::BTreeSet::new();
                let mut required_ids2 = std::collections::BTreeSet::new();
                let mut optional_ids2 = std::collections::BTreeSet::new();

                for type_member in type_members.iter() {
                    match &**type_member {
                        TypeMember::Required(id, _) => {
                            required_ids1.insert(String::from(&**id));
                        }
                        TypeMember::Optional(id, _) => {
                            optional_ids1.insert(String::from(&**id));
                        }
                    }
                }

                for (id, required, _) in members.iter() {
                    if *required {
                        required_ids2.insert(String::from(&**id));
                    } else {
                        optional_ids2.insert(String::from(&**id));
                    }
                }

                if type_members.len() != members.len() {
                    return Err(Error::InvalidPattern { location });
                }

                for type_member in type_members.iter() {
                    match &**type_member {
                        TypeMember::Required(id, _) => {
                            if required_ids2.get(&String::from(&**id)).is_none() {
                                return Err(Error::InvalidPattern { location });
                            }
                        }
                        TypeMember::Optional(id, _) => {
                            if optional_ids2.get(&String::from(&**id)).is_none() {
                                return Err(Error::InvalidPattern { location });
                            }
                        }
                    }
                }

                for (id, required, _) in members.iter() {
                    if *required {
                        if required_ids1.get(&String::from(&**id)).is_none() {
                            return Err(Error::InvalidPattern { location });
                        }
                    } else {
                        if optional_ids1.get(&String::from(&**id)).is_none() {
                            return Err(Error::InvalidPattern { location });
                        }
                    }
                }

                let (subst, type_value1) = if let TypeValue::Abstraction(type_parameter, t) =
                    &*type_value
                {
                    let subst = self.free_substitution(&TypeQuantifier::Forall, &type_parameter);
                    let t = substitution::substitute(&subst, &t);
                    (Some(subst), t)
                } else {
                    (None, type_value.clone())
                };

                for type_member in type_members.iter() {
                    for (id1, _required, t) in members.iter() {
                        match &**type_member {
                            TypeMember::Required(id2, u) => {
                                if String::from(&**id1) == String::from(&**id2) {
                                    let type_value = if let Some(subst) = &subst {
                                        self.ascribe_type(&substitution::substitute(subst, &u))
                                    } else {
                                        self.ascribe_type(u)
                                    };

                                    unify(location, &t, &type_value)?;
                                }
                            }
                            TypeMember::Optional(id2, u) => {
                                if String::from(&**id1) == String::from(&**id2) {
                                    let type_value = if let Some(subst) = &subst {
                                        self.ascribe_type(&substitution::substitute(subst, &u))
                                    } else {
                                        self.ascribe_type(u)
                                    };

                                    unify(location, &t, &type_value)?;
                                }
                            }
                        }
                    }
                }

                Ok((type_value1, value_env))
            }
            syntax::ValueParameter::Family {
                family: _family,
                qualified_identifier,
                atomic_value_parameter,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                if family_value.family_path.len() == 0 {
                    return Err(Error::FamilyNotInScope { location });
                }

                let (type_value, value_env) = self.eval_atomic_value_parameter(
                    environment,
                    atomic_value_parameter,
                    positive,
                )?;

                let subst = if let Some(family_parameter) = &family_value.family_parameter {
                    Some(self.free_substitution(&TypeQuantifier::Forall, &family_parameter))
                } else {
                    None
                };

                if let Some(family_type) = &family_value.family_type {
                    if let Some(subst) = &subst {
                        let family_type_value =
                            substitution::substitute(subst, &self.ascribe_type(&family_type));
                        unify(location, &type_value, &family_type_value)?;
                    } else {
                        let family_type_value = self.ascribe_type(family_type);
                        unify(location, &type_value, &family_type_value)?;
                    };
                } else {
                    let x = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                    let type_param = Constituent::new(location, TypeParameter::Single(x));
                    let type_variable_ref = TypeVariableRef::new(TypeVariable::Bound(
                        TypeQuantifier::Forall,
                        self.type_variable_level,
                        x,
                    ));
                    let family_type = Constituent::new(
                        location,
                        TypeValue::Exists(
                            type_param,
                            Constituent::new(location, TypeValue::TypeVariable(type_variable_ref)),
                        ),
                    );

                    if let Some(subst) = &subst {
                        let family_type_value =
                            substitution::substitute(subst, &self.instantiate_type(&family_type));
                        unify(location, &type_value, &family_type_value)?;
                    } else {
                        let family_type_value = self.instantiate_type(&family_type);
                        unify(location, &type_value, &family_type_value)?;
                    }
                }

                if let Some(family_parameter) = &family_value.family_parameter {
                    let type_arg = self.type_parameter_to_type_argument(family_parameter);

                    let nominal_family_type_value = if let Some(subst) = &subst {
                        substitution::substitute(
                            subst,
                            &Constituent::new(
                                location,
                                TypeValue::Application(
                                    Constituent::new(
                                        location,
                                        TypeValue::NominalFamily(family_value.family_path.clone()),
                                    ),
                                    type_arg,
                                ),
                            ),
                        )
                    } else {
                        Constituent::new(
                            location,
                            TypeValue::NominalFamily(family_value.family_path.clone()),
                        )
                    };

                    Ok((nominal_family_type_value, value_env))
                } else {
                    let nominal_family_type_value = Constituent::new(
                        location,
                        TypeValue::NominalFamily(family_value.family_path.clone()),
                    );

                    Ok((nominal_family_type_value, value_env))
                }
            }
        }
    }

    pub fn eval_value_parameters(
        &mut self,
        environment: &Environment,
        value_parameters: &Constituent<syntax::ValueParameters>,
        positive: bool,
    ) -> Result<(Constituent<TypeValue>, ValueEnvironment), Error> {
        let location = value_parameters.location();

        match &**value_parameters {
            syntax::ValueParameters::One { value_parameter } => {
                self.eval_value_parameter(environment, value_parameter, positive)
            }
            syntax::ValueParameters::More {
                value_parameter,
                comma: _comma,
                value_parameters,
            } => {
                let (type_value1, mut type_env1) =
                    self.eval_value_parameter(environment, value_parameter, positive)?;
                let (type_value2, mut type_env2) =
                    self.eval_value_parameters(environment, value_parameters, positive)?;

                let type_value =
                    Constituent::new(location, TypeValue::Product(type_value1, type_value2));
                type_env1.append(&mut type_env2);

                Ok((type_value, type_env1))
            }
        }
    }

    pub fn eval_value_member(
        &mut self,
        environment: &Environment,
        value_member: &Constituent<syntax::ValueMember>,
    ) -> Result<(Lexeme, Constituent<TypeValue>), Error> {
        match &**value_member {
            syntax::ValueMember::Identifier { identifier } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        identifier.location(),
                        syntax::QualifiedIdentifier::Identifier {
                            identifier: identifier.clone(),
                        },
                    ),
                )?;

                Ok((identifier.clone(), type_value))
            }
            syntax::ValueMember::Coloneq {
                identifier,
                coloneq: _coloneq,
                value_expression,
            } => {
                self.type_variable_level += 1;
                let type_value = self.type_value_expression(environment, value_expression)?;
                self.type_variable_level -= 1;

                let type_value = if rank(&type_value) == rank(&unfold_effect(&type_value)) {
                    self.refresh_type(&type_value)
                } else {
                    type_value
                };

                Ok((identifier.clone(), type_value))
            }
        }
    }

    pub fn eval_value_members(
        &mut self,
        environment: &Environment,
        value_members: &Constituent<syntax::ValueMembers>,
    ) -> Result<Vec<(Lexeme, Constituent<TypeValue>)>, Error> {
        match &**value_members {
            syntax::ValueMembers::One { value_member } => {
                let value_member = self.eval_value_member(environment, &value_member)?;
                Ok(vec![value_member])
            }
            syntax::ValueMembers::More {
                value_member,
                comma: _comma,
                value_members,
            } => {
                let value_member = self.eval_value_member(environment, &value_member)?;
                let value_members = self.eval_value_members(environment, &value_members)?;
                Ok(vec![vec![value_member], value_members].concat())
            }
        }
    }

    pub fn eval_value_implication(
        &mut self,
        environment: &Environment,
        value_implication: &Constituent<syntax::ValueImplication>,
        positive: bool,
    ) -> Result<
        (
            Lexeme,
            Option<Constituent<TypeValue>>,
            Constituent<TypeValue>,
        ),
        Error,
    > {
        match &**value_implication {
            syntax::ValueImplication::Variant {
                identifier,
                darrow: _darrow,
                value_expression,
            } => {
                let type_value = self.type_value_expression(environment, value_expression)?;
                Ok((identifier.clone(), None, type_value))
            }
            syntax::ValueImplication::VariantOf {
                identifier,
                atomic_value_parameter,
                darrow: _darrow,
                value_expression,
            } => {
                let (type_value1, mut value_env) = self.eval_atomic_value_parameter(
                    environment,
                    atomic_value_parameter,
                    positive,
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let type_value2 =
                    self.type_value_expression(&local_environment, value_expression)?;

                Ok((identifier.clone(), Some(type_value1), type_value2))
            }
        }
    }

    pub fn eval_value_implications(
        &mut self,
        environment: &Environment,
        value_implications: &Constituent<syntax::ValueImplications>,
        positive: bool,
    ) -> Result<
        Vec<(
            Lexeme,
            Option<Constituent<TypeValue>>,
            Constituent<TypeValue>,
        )>,
        Error,
    > {
        match &**value_implications {
            syntax::ValueImplications::One { value_implication } => {
                let value_implication =
                    self.eval_value_implication(environment, &value_implication, positive)?;
                Ok(vec![value_implication])
            }
            syntax::ValueImplications::More {
                value_implication,
                pipe: _pipe,
                value_implications,
            } => {
                let value_implication =
                    self.eval_value_implication(environment, &value_implication, positive)?;
                let value_implications =
                    self.eval_value_implications(environment, &value_implications, positive)?;
                Ok(vec![vec![value_implication], value_implications].concat())
            }
        }
    }

    pub fn type_atomic_value_expression(
        &mut self,
        environment: &Environment,
        atomic_value_expression: &Constituent<syntax::AtomicValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = atomic_value_expression.location();

        match &**atomic_value_expression {
            syntax::AtomicValueExpression::False { false_: _false } => {
                Ok(Constituent::new(location, TypeValue::Bool))
            }
            syntax::AtomicValueExpression::True { true_: _true } => {
                Ok(Constituent::new(location, TypeValue::Bool))
            }
            syntax::AtomicValueExpression::Integer { integer: _integer } => {
                Ok(Constituent::new(location, TypeValue::Int))
            }
            syntax::AtomicValueExpression::Rational {
                rational: _rational,
            } => Ok(Constituent::new(location, TypeValue::Rational)),
            syntax::AtomicValueExpression::String { string: _string } => {
                Ok(Constituent::new(location, TypeValue::String))
            }
            syntax::AtomicValueExpression::Binary { binary: _binary } => {
                Ok(Constituent::new(location, TypeValue::Binary))
            }
            syntax::AtomicValueExpression::QualifiedIdentifier {
                qualified_identifier,
            } => {
                let type_value = self.eval_value_identifier(environment, qualified_identifier)?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotEq {
                qualified_identifier,
                dot,
                eq,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: eq.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotNoteq {
                qualified_identifier,
                dot,
                noteq,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: noteq.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotLte {
                qualified_identifier,
                dot,
                lte,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: lte.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotLt {
                qualified_identifier,
                dot,
                lt,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: lt.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotGte {
                qualified_identifier,
                dot,
                gte,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: gte.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotGt {
                qualified_identifier,
                dot,
                gt,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: gt.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotPlus {
                qualified_identifier,
                dot,
                plus,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: plus.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotMinus {
                qualified_identifier,
                dot,
                minus,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: minus.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotAsterisk {
                qualified_identifier,
                dot,
                asterisk,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: asterisk.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotSlash {
                qualified_identifier,
                dot,
                slash,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: slash.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierDotPercent {
                qualified_identifier,
                dot,
                percent,
            } => {
                let type_value = self.eval_value_identifier(
                    environment,
                    &Constituent::new(
                        location,
                        syntax::QualifiedIdentifier::Dot {
                            qualified_identifier: qualified_identifier.clone(),
                            dot: dot.clone(),
                            identifier: percent.clone(),
                        },
                    ),
                )?;
                Ok(self.instantiate_type(&type_value))
            }
            syntax::AtomicValueExpression::QualifiedIdentifierColoncolonIdentifier {
                qualified_identifier,
                coloncolon: _coloncolon,
                identifier,
            } => {
                let family_value =
                    self.eval_family_identifier(environment, qualified_identifier)?;

                if family_value.family_path.len() == 0 {
                    return Err(Error::FamilyNotInScope { location });
                }

                if family_value.value_env.get(&**identifier).is_none() {
                    return Err(Error::ValueNotInFamily {
                        location: identifier.location(),
                    });
                }

                let type_value = if let Some(family_parameter) = &family_value.family_parameter {
                    if let Some(family_parameter_type) = &family_value.family_parameter_type {
                        let type_arg = self.type_parameter_to_type_argument(family_parameter);

                        self.instantiate_type(&Constituent::new(
                            location,
                            TypeValue::Forall(
                                family_parameter.clone(),
                                Constituent::new(
                                    location,
                                    TypeValue::Arrow(
                                        family_parameter_type.clone(),
                                        Constituent::new(
                                            location,
                                            TypeValue::Application(
                                                Constituent::new(
                                                    location,
                                                    TypeValue::NominalFamily(
                                                        family_value.family_path.clone(),
                                                    ),
                                                ),
                                                type_arg,
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ))
                    } else {
                        let type_arg = self.type_parameter_to_type_argument(family_parameter);

                        self.instantiate_type(&Constituent::new(
                            location,
                            TypeValue::Forall(
                                family_parameter.clone(),
                                Constituent::new(
                                    location,
                                    TypeValue::Application(
                                        Constituent::new(
                                            location,
                                            TypeValue::NominalFamily(
                                                family_value.family_path.clone(),
                                            ),
                                        ),
                                        type_arg,
                                    ),
                                ),
                            ),
                        ))
                    }
                } else {
                    if let Some(family_parameter_type) = &family_value.family_parameter_type {
                        Constituent::new(
                            location,
                            TypeValue::Arrow(
                                family_parameter_type.clone(),
                                Constituent::new(
                                    location,
                                    TypeValue::NominalFamily(family_value.family_path.clone()),
                                ),
                            ),
                        )
                    } else {
                        Constituent::new(
                            location,
                            TypeValue::NominalFamily(family_value.family_path.clone()),
                        )
                    }
                };

                Ok(type_value)
            }
            syntax::AtomicValueExpression::EmptyArray {
                lbracket: _lbracket,
                rbracket: _rbracket,
            } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                let type_variable = TypeVariable::Free(
                    TypeQuantifier::Forall,
                    self.type_variable_level,
                    type_variable_value,
                );
                let type_variable_ref = TypeVariableRef::new(type_variable);
                let type_value =
                    Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

                Ok(Constituent::new(
                    location,
                    TypeValue::Application(
                        Constituent::new(location, TypeValue::Array),
                        type_value,
                    ),
                ))
            }
            syntax::AtomicValueExpression::NonEmptyArray {
                lbracket: _lbracket,
                value_expressions,
                rbracket: _rbracket,
            } => {
                let type_value =
                    self.type_value_expressions_array(environment, value_expressions)?;

                let n = rank(&type_value);
                let type_value = unfold_effect(&type_value);
                let m = rank(&type_value);

                let type_value = Constituent::new(
                    location,
                    TypeValue::Application(
                        Constituent::new(location, TypeValue::Array),
                        type_value,
                    ),
                );

                let type_value = fold_effect(n - m, &type_value);

                Ok(type_value)
            }
            syntax::AtomicValueExpression::MapArray {
                lbracket: _lbracket,
                value_expression1,
                pipe: _pipe,
                value_parameter,
                in_: _in_,
                value_expression2,
                rbracket: _rbracket,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_value_parameter(environment, value_parameter, true)?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let type_value2 =
                    self.type_value_expression(&local_environment, value_expression1)?;

                let type_value3 = self.type_value_expression(environment, value_expression2)?;

                let n = rank(&type_value2);
                let m = rank(&type_value3);

                let type_value2 = unfold_effect(&type_value2);
                let type_value3 = unfold_effect(&type_value3);

                let n1 = rank(&type_value2);
                let m1 = rank(&type_value3);

                let type_value4 = Constituent::new(
                    location,
                    TypeValue::Application(
                        Constituent::new(location, TypeValue::Array),
                        type_value1,
                    ),
                );

                unify(value_expression2.location(), &type_value3, &type_value4)?;

                let type_value6 = Constituent::new(
                    location,
                    TypeValue::Application(
                        Constituent::new(location, TypeValue::Array),
                        type_value2.clone(),
                    ),
                );

                let type_value6 = fold_effect(std::cmp::max(n - n1, m - m1), &type_value6);

                Ok(type_value6)
            }
            syntax::AtomicValueExpression::MapRange {
                lbracket: _lbracket,
                value_expression1,
                pipe: _pipe,
                value_parameter,
                in_: _in_,
                value_expression2,
                dotdot: _dotdot,
                value_expression3,
                rbracket: _rbracket,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_value_parameter(environment, value_parameter, true)?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let type_value2 =
                    self.type_value_expression(&local_environment, value_expression1)?;

                let type_value3 = self.type_value_expression(environment, value_expression2)?;

                let type_value4 = self.type_value_expression(environment, value_expression3)?;

                let n = rank(&type_value2);
                let m = rank(&type_value3);
                let r = rank(&type_value4);

                let type_value2 = unfold_effect(&type_value2);
                let type_value3 = unfold_effect(&type_value3);
                let type_value4 = unfold_effect(&type_value4);

                let n1 = rank(&type_value2);
                let m1 = rank(&type_value3);
                let r1 = rank(&type_value4);

                unify(
                    value_parameter.location(),
                    &type_value1,
                    &Constituent::new(location, TypeValue::Int),
                )?;

                unify(
                    value_expression2.location(),
                    &type_value3,
                    &Constituent::new(location, TypeValue::Int),
                )?;

                unify(
                    value_expression3.location(),
                    &type_value4,
                    &Constituent::new(location, TypeValue::Int),
                )?;

                let type_value5 = Constituent::new(
                    location,
                    TypeValue::Application(
                        Constituent::new(location, TypeValue::Array),
                        type_value2.clone(),
                    ),
                );

                let type_value5 = fold_effect(
                    std::cmp::max(n - n1, std::cmp::max(m - m1, r - r1)),
                    &type_value5,
                );

                Ok(type_value5)
            }
            syntax::AtomicValueExpression::Tuple {
                lparen: _lparen,
                value_expressions,
                rparen: _rparen,
            } => self.type_value_expressions(environment, value_expressions),
        }
    }

    pub fn type_applicative_value_expression(
        &mut self,
        environment: &Environment,
        applicative_value_expression: &Constituent<syntax::ApplicativeValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = applicative_value_expression.location();

        match &**applicative_value_expression {
            syntax::ApplicativeValueExpression::AtomicValueExpression {
                atomic_value_expression,
            } => self.type_atomic_value_expression(environment, atomic_value_expression),
            syntax::ApplicativeValueExpression::Apply {
                applicative_value_expression,
                atomic_value_expression,
            } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                let type_variable = TypeVariable::Free(
                    TypeQuantifier::Forall,
                    self.type_variable_level,
                    type_variable_value,
                );
                let type_variable_ref = TypeVariableRef::new(type_variable);
                let type_value =
                    Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

                let type_value1 = self
                    .type_applicative_value_expression(environment, applicative_value_expression)?;

                let type_value2 =
                    self.type_atomic_value_expression(environment, atomic_value_expression)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                unify(
                    applicative_value_expression.location(),
                    &type_value1,
                    &Constituent::new(
                        applicative_value_expression.location(),
                        TypeValue::Arrow(type_value2, type_value.clone()),
                    ),
                )?;

                let r = rank(&type_value);
                let type_value = unfold_effect(&type_value);
                let r1 = rank(&type_value);

                let type_value = fold_effect(
                    std::cmp::max(n - n1, std::cmp::max(m - m1, r - r1 + (n - n1))),
                    &type_value,
                );

                Ok(type_value)
            }
        }
    }

    pub fn type_record_value_expression(
        &mut self,
        environment: &Environment,
        record_value_expression: &Constituent<syntax::RecordValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = record_value_expression.location();

        match &**record_value_expression {
            syntax::RecordValueExpression::ApplicativeValueExpression {
                applicative_value_expression,
            } => self.type_applicative_value_expression(environment, applicative_value_expression),
            syntax::RecordValueExpression::EmptyRecord {
                qualified_identifier,
                lbrace: _lbrace,
                rbrace: _rbrace,
            } => {
                let type_members =
                    self.eval_record_identifier(environment, qualified_identifier)?;

                let (type_value, _kind_value) =
                    self.eval_type_identifier(environment, qualified_identifier)?;

                for member in type_members {
                    if let TypeMember::Required(_, _) = &*member {
                        return Err(Error::TypeMismatch {
                            location: qualified_identifier.location(),
                        });
                    }
                }

                let type_value = if let TypeValue::Abstraction(type_parameter, t) = &*type_value {
                    self.instantiate_type(&Constituent::new(
                        type_value.location(),
                        TypeValue::Forall(type_parameter.clone(), t.clone()),
                    ))
                } else {
                    type_value
                };

                return Ok(type_value);
            }
            syntax::RecordValueExpression::NonEmptyRecord {
                qualified_identifier,
                lbrace: _lbrace,
                value_members,
                rbrace: _rbrace,
            } => {
                let type_members =
                    self.eval_record_identifier(environment, qualified_identifier)?;

                let (type_value, _kind_value) =
                    self.eval_type_identifier(environment, qualified_identifier)?;

                let members = self.eval_value_members(environment, value_members)?;

                let mut required_ids1 = std::collections::BTreeSet::new();
                let mut optional_ids1 = std::collections::BTreeSet::new();
                let mut value_member_ids = std::collections::BTreeSet::new();

                for type_member in type_members.iter() {
                    match &**type_member {
                        TypeMember::Required(id, _) => {
                            required_ids1.insert(String::from(&**id));
                        }
                        TypeMember::Optional(id, _) => {
                            optional_ids1.insert(String::from(&**id));
                        }
                    }
                }

                for (id, _) in members.iter() {
                    value_member_ids.insert(String::from(&**id));
                }

                for type_member in type_members.iter() {
                    match &**type_member {
                        TypeMember::Required(id, _) => {
                            if value_member_ids.get(&String::from(&**id)).is_none() {
                                return Err(Error::InvalidRecord { location });
                            }
                        }
                        TypeMember::Optional(_, _) => (),
                    }
                }

                for (id, _) in members.iter() {
                    if required_ids1.get(&String::from(&**id)).is_none()
                        && optional_ids1.get(&String::from(&**id)).is_none()
                    {
                        return Err(Error::InvalidRecord { location });
                    }
                }

                let (subst, type_value1) = if let TypeValue::Abstraction(type_parameter, t) =
                    &*type_value
                {
                    let subst = self.free_substitution(&TypeQuantifier::Forall, &type_parameter);
                    let t = substitution::substitute(&subst, &t);
                    (Some(subst), t)
                } else {
                    (None, type_value.clone())
                };

                let mut r = 0;

                for type_member in type_members.iter() {
                    for (id1, t) in members.iter() {
                        match &**type_member {
                            TypeMember::Required(id2, u) => {
                                if String::from(&**id1) == String::from(&**id2) {
                                    let type_value = if let Some(subst) = &subst {
                                        self.ascribe_type(&substitution::substitute(subst, &u))
                                    } else {
                                        self.ascribe_type(u)
                                    };

                                    let t = self.instantiate_type(&t);

                                    let n = rank(&t);
                                    let t = unfold_effect(&t);
                                    let n1 = rank(&t);

                                    r = std::cmp::max(r, n - n1);

                                    unify(location, &t, &type_value)?;
                                }
                            }
                            TypeMember::Optional(id2, u) => {
                                if String::from(&**id1) == String::from(&**id2) {
                                    let type_value = if let Some(subst) = &subst {
                                        self.ascribe_type(&substitution::substitute(subst, &u))
                                    } else {
                                        self.ascribe_type(u)
                                    };

                                    let t = self.instantiate_type(&t);

                                    let n = rank(&t);
                                    let t = unfold_effect(&t);
                                    let n1 = rank(&t);

                                    r = std::cmp::max(r, n - n1);

                                    unify(location, &t, &type_value)?;
                                }
                            }
                        }
                    }
                }

                Ok(fold_effect(r, &type_value1))
            }
        }
    }

    pub fn type_complementary_value_expression(
        &mut self,
        environment: &Environment,
        complementary_value_expression: &Constituent<syntax::ComplementaryValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = complementary_value_expression.location();

        match &**complementary_value_expression {
            syntax::ComplementaryValueExpression::RecordValueExpression {
                record_value_expression,
            } => self.type_record_value_expression(environment, record_value_expression),
            syntax::ComplementaryValueExpression::Exclamation {
                exclamation: _exclamation,
                complementary_value_expression,
            } => {
                let type_value = self.type_complementary_value_expression(
                    environment,
                    complementary_value_expression,
                )?;

                let n = rank(&type_value);
                let type_value = unfold_effect(&type_value);
                let m = rank(&type_value);

                unify(
                    location,
                    &type_value,
                    &Constituent::new(location, TypeValue::Bool),
                )?;

                Ok(fold_effect(
                    n - m,
                    &Constituent::new(location, TypeValue::Bool),
                ))
            }
            syntax::ComplementaryValueExpression::Amp {
                amp: _amp,
                complementary_value_expression,
            } => {
                let type_value = self.type_complementary_value_expression(
                    environment,
                    complementary_value_expression,
                )?;

                let n = rank(&type_value);
                let type_value = unfold_effect(&type_value);
                let m = rank(&type_value);

                Ok(fold_effect(
                    std::cmp::max(n - m, 1),
                    &Constituent::new(location, TypeValue::Reference(type_value)),
                ))
            }
            syntax::ComplementaryValueExpression::Asterisk {
                asterisk: _asterisk,
                complementary_value_expression,
            } => {
                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                let type_variable = TypeVariable::Free(
                    TypeQuantifier::Forall,
                    self.type_variable_level,
                    type_variable_value,
                );
                let type_variable_ref = TypeVariableRef::new(type_variable);
                let type_value1 =
                    Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

                let type_value = self.type_complementary_value_expression(
                    environment,
                    complementary_value_expression,
                )?;

                let n = rank(&type_value);
                let type_value = unfold_effect(&type_value);
                let m = rank(&type_value);

                unify(
                    location,
                    &type_value,
                    &Constituent::new(location, TypeValue::Reference(type_value1.clone())),
                )?;

                Ok(fold_effect(std::cmp::max(n - m, 1), &type_value1))
            }
        }
    }

    fn type_multiplicative_value_expression_op(
        &mut self,
        environment: &Environment,
        location: (usize, usize),
        multiplicative_value_expression: &Constituent<syntax::MultiplicativeValueExpression>,
        op: &Lexeme,
        complementary_value_expression: &Constituent<syntax::ComplementaryValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let type_value1 = self.eval_value_identifier(
            environment,
            &Constituent::new(
                op.location(),
                syntax::QualifiedIdentifier::Identifier {
                    identifier: op.clone(),
                },
            ),
        )?;
        let type_value1 = self.instantiate_type(&type_value1);

        let type_value2 = self
            .type_multiplicative_value_expression(environment, multiplicative_value_expression)?;
        let type_value3 =
            self.type_complementary_value_expression(environment, complementary_value_expression)?;

        let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
        let type_variable = TypeVariable::Free(
            TypeQuantifier::Forall,
            self.type_variable_level,
            type_variable_value,
        );
        let type_variable_ref = TypeVariableRef::new(type_variable);
        let type_value = Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

        let n = rank(&type_value1);
        let m = rank(&type_value2);
        let r = rank(&type_value3);

        let type_value1 = unfold_effect(&type_value1);
        let type_value2 = unfold_effect(&type_value2);
        let type_value3 = unfold_effect(&type_value3);

        let n1 = rank(&type_value1);
        let m1 = rank(&type_value2);
        let r1 = rank(&type_value3);

        unify(
            location,
            &type_value1,
            &Constituent::new(
                location,
                TypeValue::Arrow(
                    Constituent::new(location, TypeValue::Product(type_value2, type_value3)),
                    type_value.clone(),
                ),
            ),
        )?;

        let k = rank(&type_value);
        let type_value = unfold_effect(&type_value);
        let k1 = rank(&type_value);

        let type_value = fold_effect(
            std::cmp::max(
                n - n1,
                std::cmp::max(m - m1, std::cmp::max(r - r1, k - k1 + (n - n1))),
            ),
            &type_value,
        );

        Ok(type_value)
    }

    pub fn type_multiplicative_value_expression(
        &mut self,
        environment: &Environment,
        multiplicative_value_expression: &Constituent<syntax::MultiplicativeValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = multiplicative_value_expression.location();

        match &**multiplicative_value_expression {
            syntax::MultiplicativeValueExpression::ComplementaryValueExpression {
                complementary_value_expression,
            } => self
                .type_complementary_value_expression(environment, complementary_value_expression),
            syntax::MultiplicativeValueExpression::Asterisk {
                multiplicative_value_expression,
                asterisk,
                complementary_value_expression,
            } => self.type_multiplicative_value_expression_op(
                environment,
                location,
                multiplicative_value_expression,
                asterisk,
                complementary_value_expression,
            ),
            syntax::MultiplicativeValueExpression::Slash {
                multiplicative_value_expression,
                slash,
                complementary_value_expression,
            } => self.type_multiplicative_value_expression_op(
                environment,
                location,
                multiplicative_value_expression,
                slash,
                complementary_value_expression,
            ),
            syntax::MultiplicativeValueExpression::Percent {
                multiplicative_value_expression,
                percent,
                complementary_value_expression,
            } => self.type_multiplicative_value_expression_op(
                environment,
                location,
                multiplicative_value_expression,
                percent,
                complementary_value_expression,
            ),
        }
    }

    fn type_additive_value_expression_op(
        &mut self,
        environment: &Environment,
        location: (usize, usize),
        additive_value_expression: &Constituent<syntax::AdditiveValueExpression>,
        op: &Lexeme,
        multiplicative_value_expression: &Constituent<syntax::MultiplicativeValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let type_value1 = self.eval_value_identifier(
            environment,
            &Constituent::new(
                op.location(),
                syntax::QualifiedIdentifier::Identifier {
                    identifier: op.clone(),
                },
            ),
        )?;
        let type_value1 = self.instantiate_type(&type_value1);

        let type_value2 =
            self.type_additive_value_expression(environment, additive_value_expression)?;
        let type_value3 = self
            .type_multiplicative_value_expression(environment, multiplicative_value_expression)?;

        let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
        let type_variable = TypeVariable::Free(
            TypeQuantifier::Forall,
            self.type_variable_level,
            type_variable_value,
        );
        let type_variable_ref = TypeVariableRef::new(type_variable);
        let type_value = Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

        let n = rank(&type_value1);
        let m = rank(&type_value2);
        let r = rank(&type_value3);

        let type_value1 = unfold_effect(&type_value1);
        let type_value2 = unfold_effect(&type_value2);
        let type_value3 = unfold_effect(&type_value3);

        let n1 = rank(&type_value1);
        let m1 = rank(&type_value2);
        let r1 = rank(&type_value3);

        unify(
            location,
            &type_value1,
            &Constituent::new(
                location,
                TypeValue::Arrow(
                    Constituent::new(location, TypeValue::Product(type_value2, type_value3)),
                    type_value.clone(),
                ),
            ),
        )?;

        let k = rank(&type_value);
        let type_value = unfold_effect(&type_value);
        let k1 = rank(&type_value);

        let type_value = fold_effect(
            std::cmp::max(
                n - n1,
                std::cmp::max(m - m1, std::cmp::max(r - r1, k - k1 + (n - n1))),
            ),
            &type_value,
        );

        Ok(type_value)
    }

    pub fn type_additive_value_expression(
        &mut self,
        environment: &Environment,
        additive_value_expression: &Constituent<syntax::AdditiveValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = additive_value_expression.location();

        match &**additive_value_expression {
            syntax::AdditiveValueExpression::MultiplicativeValueExpression {
                multiplicative_value_expression,
            } => self
                .type_multiplicative_value_expression(environment, multiplicative_value_expression),
            syntax::AdditiveValueExpression::Plus {
                additive_value_expression,
                plus,
                multiplicative_value_expression,
            } => self.type_additive_value_expression_op(
                environment,
                location,
                additive_value_expression,
                plus,
                multiplicative_value_expression,
            ),
            syntax::AdditiveValueExpression::Minus {
                additive_value_expression,
                minus,
                multiplicative_value_expression,
            } => self.type_additive_value_expression_op(
                environment,
                location,
                additive_value_expression,
                minus,
                multiplicative_value_expression,
            ),
        }
    }

    fn type_comparative_value_expression_op(
        &mut self,
        environment: &Environment,
        location: (usize, usize),
        comparative_value_expression: &Constituent<syntax::ComparativeValueExpression>,
        op: &Lexeme,
        additive_value_expression: &Constituent<syntax::AdditiveValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let type_value1 = self.eval_value_identifier(
            environment,
            &Constituent::new(
                op.location(),
                syntax::QualifiedIdentifier::Identifier {
                    identifier: op.clone(),
                },
            ),
        )?;
        let type_value1 = self.instantiate_type(&type_value1);

        let type_value2 =
            self.type_comparative_value_expression(environment, comparative_value_expression)?;
        let type_value3 =
            self.type_additive_value_expression(environment, additive_value_expression)?;

        let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
        let type_variable = TypeVariable::Free(
            TypeQuantifier::Forall,
            self.type_variable_level,
            type_variable_value,
        );
        let type_variable_ref = TypeVariableRef::new(type_variable);
        let type_value = Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

        let n = rank(&type_value1);
        let m = rank(&type_value2);
        let r = rank(&type_value3);

        let type_value1 = unfold_effect(&type_value1);
        let type_value2 = unfold_effect(&type_value2);
        let type_value3 = unfold_effect(&type_value3);

        let n1 = rank(&type_value1);
        let m1 = rank(&type_value2);
        let r1 = rank(&type_value3);

        unify(
            location,
            &type_value1,
            &Constituent::new(
                location,
                TypeValue::Arrow(
                    Constituent::new(location, TypeValue::Product(type_value2, type_value3)),
                    type_value.clone(),
                ),
            ),
        )?;

        let k = rank(&type_value);
        let type_value = unfold_effect(&type_value);
        let k1 = rank(&type_value);

        let type_value = fold_effect(
            std::cmp::max(
                n - n1,
                std::cmp::max(m - m1, std::cmp::max(r - r1, k - k1 + (n - n1))),
            ),
            &type_value,
        );

        Ok(type_value)
    }

    pub fn type_comparative_value_expression(
        &mut self,
        environment: &Environment,
        comparative_value_expression: &Constituent<syntax::ComparativeValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = comparative_value_expression.location();

        match &**comparative_value_expression {
            syntax::ComparativeValueExpression::AdditiveValueExpression {
                additive_value_expression,
            } => self.type_additive_value_expression(environment, additive_value_expression),
            syntax::ComparativeValueExpression::Eq {
                comparative_value_expression,
                eq,
                additive_value_expression,
            } => self.type_comparative_value_expression_op(
                environment,
                location,
                comparative_value_expression,
                eq,
                additive_value_expression,
            ),
            syntax::ComparativeValueExpression::Noteq {
                comparative_value_expression,
                noteq,
                additive_value_expression,
            } => self.type_comparative_value_expression_op(
                environment,
                location,
                comparative_value_expression,
                noteq,
                additive_value_expression,
            ),
            syntax::ComparativeValueExpression::Lte {
                comparative_value_expression,
                lte,
                additive_value_expression,
            } => self.type_comparative_value_expression_op(
                environment,
                location,
                comparative_value_expression,
                lte,
                additive_value_expression,
            ),
            syntax::ComparativeValueExpression::Lt {
                comparative_value_expression,
                lt,
                additive_value_expression,
            } => self.type_comparative_value_expression_op(
                environment,
                location,
                comparative_value_expression,
                lt,
                additive_value_expression,
            ),
            syntax::ComparativeValueExpression::Gte {
                comparative_value_expression,
                gte,
                additive_value_expression,
            } => self.type_comparative_value_expression_op(
                environment,
                location,
                comparative_value_expression,
                gte,
                additive_value_expression,
            ),
            syntax::ComparativeValueExpression::Gt {
                comparative_value_expression,
                gt,
                additive_value_expression,
            } => self.type_comparative_value_expression_op(
                environment,
                location,
                comparative_value_expression,
                gt,
                additive_value_expression,
            ),
        }
    }

    pub fn type_conjunctive_value_expression(
        &mut self,
        environment: &Environment,
        conjunctive_value_expression: &Constituent<syntax::ConjunctiveValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = conjunctive_value_expression.location();

        match &**conjunctive_value_expression {
            syntax::ConjunctiveValueExpression::ComparativeValueExpression {
                comparative_value_expression,
            } => self.type_comparative_value_expression(environment, comparative_value_expression),
            syntax::ConjunctiveValueExpression::And {
                conjunctive_value_expression,
                and: _and,
                comparative_value_expression,
            } => {
                let type_value1 = self
                    .type_conjunctive_value_expression(environment, conjunctive_value_expression)?;
                let type_value2 = self
                    .type_comparative_value_expression(environment, comparative_value_expression)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                unify(
                    location,
                    &type_value1,
                    &Constituent::new(location, TypeValue::Bool),
                )?;

                unify(
                    location,
                    &type_value2,
                    &Constituent::new(location, TypeValue::Bool),
                )?;

                let type_value = fold_effect(
                    std::cmp::max(n - n1, m - m1),
                    &Constituent::new(location, TypeValue::Bool),
                );

                Ok(type_value)
            }
        }
    }

    pub fn type_disjunctive_value_expression(
        &mut self,
        environment: &Environment,
        disjunctive_value_expression: &Constituent<syntax::DisjunctiveValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = disjunctive_value_expression.location();

        match &**disjunctive_value_expression {
            syntax::DisjunctiveValueExpression::ConjunctiveValueExpression {
                conjunctive_value_expression,
            } => self.type_conjunctive_value_expression(environment, conjunctive_value_expression),
            syntax::DisjunctiveValueExpression::Or {
                disjunctive_value_expression,
                or: _or,
                conjunctive_value_expression,
            } => {
                let type_value1 = self
                    .type_disjunctive_value_expression(environment, disjunctive_value_expression)?;
                let type_value2 = self
                    .type_conjunctive_value_expression(environment, conjunctive_value_expression)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                unify(
                    location,
                    &type_value1,
                    &Constituent::new(location, TypeValue::Bool),
                )?;

                unify(
                    location,
                    &type_value2,
                    &Constituent::new(location, TypeValue::Bool),
                )?;

                let type_value = fold_effect(
                    std::cmp::max(n - n1, m - m1),
                    &Constituent::new(location, TypeValue::Bool),
                );

                Ok(type_value)
            }
        }
    }

    pub fn type_assignment_value_expression(
        &mut self,
        environment: &Environment,
        assignment_value_expression: &Constituent<syntax::AssignmentValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = assignment_value_expression.location();

        match &**assignment_value_expression {
            syntax::AssignmentValueExpression::DisjunctiveValueExpression {
                disjunctive_value_expression,
            } => self.type_disjunctive_value_expression(environment, disjunctive_value_expression),
            syntax::AssignmentValueExpression::Coloneq {
                disjunctive_value_expression,
                coloneq: _coloneq,
                assignment_value_expression,
            } => {
                let type_value1 = self
                    .type_disjunctive_value_expression(environment, disjunctive_value_expression)?;
                let type_value2 = self
                    .type_assignment_value_expression(environment, assignment_value_expression)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                unify(
                    location,
                    &type_value1,
                    &Constituent::new(location, TypeValue::Reference(type_value2)),
                )?;

                let type_value = Constituent::new(location, TypeValue::Bool);

                let type_value =
                    fold_effect(std::cmp::max(n - n1, std::cmp::max(m - m1, 1)), &type_value);

                Ok(type_value)
            }
        }
    }

    pub fn type_control_value_expression(
        &mut self,
        environment: &Environment,
        control_value_expression: &Constituent<syntax::ControlValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = control_value_expression.location();

        match &**control_value_expression {
            syntax::ControlValueExpression::AssignmentValueExpression {
                assignment_value_expression,
            } => self.type_assignment_value_expression(environment, assignment_value_expression),
            syntax::ControlValueExpression::EmptyMatch {
                qualified_identifier,
                dot: _dot,
                match_: _match_,
                value_expression,
                with: _with,
                end: _end,
            } => {
                let type_variants =
                    self.eval_variant_identifier(environment, qualified_identifier)?;

                if type_variants.len() != 0 {
                    return Err(Error::InvalidPattern { location });
                }

                let (type_value, _kind_value) =
                    self.eval_type_identifier(environment, qualified_identifier)?;

                let type_value = if let TypeValue::Abstraction(type_parameter, t) = &*type_value {
                    self.instantiate_type(&Constituent::new(
                        type_value.location(),
                        TypeValue::Forall(type_parameter.clone(), t.clone()),
                    ))
                } else {
                    type_value
                };

                let type_value1 = self.type_value_expression(environment, value_expression)?;

                let n = rank(&type_value1);
                let type_value1 = unfold_effect(&type_value1);
                let n1 = rank(&type_value1);

                unify(value_expression.location(), &type_value1, &type_value)?;

                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                let type_variable = TypeVariable::Free(
                    TypeQuantifier::Forall,
                    self.type_variable_level,
                    type_variable_value,
                );
                let type_variable_ref = TypeVariableRef::new(type_variable);
                let type_value =
                    Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

                Ok(fold_effect(n - n1, &type_value))
            }
            syntax::ControlValueExpression::NonEmptyMatch {
                qualified_identifier,
                dot: _dot,
                match_: _match_,
                value_expression,
                with: _with,
                value_implications,
                end: _end,
            } => {
                let type_variants =
                    self.eval_variant_identifier(environment, qualified_identifier)?;

                let (type_value, _kind_value) =
                    self.eval_type_identifier(environment, qualified_identifier)?;

                let (subst, type_value) = if let TypeValue::Abstraction(type_parameter, t) =
                    &*type_value
                {
                    let subst = self.free_substitution(&TypeQuantifier::Forall, &type_parameter);
                    let t = substitution::substitute(&subst, &t);
                    (Some(subst), t)
                } else {
                    (None, type_value.clone())
                };

                let type_value1 = self.type_value_expression(environment, value_expression)?;
                let value_implications =
                    self.eval_value_implications(environment, value_implications, true)?;

                if type_variants.len() != value_implications.len() {
                    return Err(Error::InvalidPattern { location });
                }

                let type_variable_value = rand::Rng::gen::<TypeVariableValue>(&mut self.rng);
                let type_variable = TypeVariable::Free(
                    TypeQuantifier::Forall,
                    self.type_variable_level,
                    type_variable_value,
                );
                let type_variable_ref = TypeVariableRef::new(type_variable);
                let result_type_value =
                    Constituent::new(location, TypeValue::TypeVariable(type_variable_ref));

                let n = rank(&type_value1);
                let type_value1 = unfold_effect(&type_value1);
                let n1 = rank(&type_value1);

                let mut r = n - n1;

                'l1: for (id, param, result) in value_implications.iter() {
                    let n = rank(&result);
                    let result = unfold_effect(&result);
                    let n1 = rank(&result);

                    r = std::cmp::max(r, n - n1);

                    for type_variant in type_variants.iter() {
                        match &**type_variant {
                            TypeVariant::Variant(id1) => {
                                if String::from(&**id) == String::from(&**id1) {
                                    if param.is_some() {
                                        return Err(Error::InvalidPattern { location });
                                    }

                                    unify(location, &result, &result_type_value)?;

                                    continue 'l1;
                                }
                            }
                            TypeVariant::VariantOf(id1, ty1) => {
                                if String::from(&**id) == String::from(&**id1) {
                                    match param {
                                        None => return Err(Error::InvalidPattern { location }),
                                        Some(ty) => {
                                            if let Some(subst) = &subst {
                                                unify(
                                                    location,
                                                    &ty,
                                                    &substitution::substitute(subst, &ty1),
                                                )?;
                                            } else {
                                                unify(location, &ty, &ty1)?;
                                            }

                                            unify(location, &result, &result_type_value)?;
                                        }
                                    }

                                    continue 'l1;
                                }
                            }
                        }
                    }

                    return Err(Error::InvalidPattern { location });
                }

                unify(value_expression.location(), &type_value1, &type_value)?;

                Ok(fold_effect(r, &result_type_value))
            }
            syntax::ControlValueExpression::ForArray {
                for_: _for_,
                value_parameter,
                in_: _in_,
                value_expression1,
                do_: _do_,
                value_expression2,
                end: _end,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_value_parameter(environment, value_parameter, true)?;

                let type_value2 = self.type_value_expression(environment, value_expression1)?;

                let n = rank(&type_value2);
                let type_value2 = unfold_effect(&type_value2);
                let n1 = rank(&type_value2);

                unify(
                    value_parameter.location(),
                    &type_value2,
                    &Constituent::new(
                        value_expression1.location(),
                        TypeValue::Application(
                            Constituent::new(value_expression1.location(), TypeValue::Array),
                            type_value1,
                        ),
                    ),
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let type_value3 =
                    self.type_value_expression(&local_environment, value_expression2)?;

                let m = rank(&type_value3);
                let type_value3 = unfold_effect(&type_value3);
                let m1 = rank(&type_value3);

                unify(
                    value_expression2.location(),
                    &type_value3,
                    &Constituent::new(value_expression2.location(), TypeValue::Bool),
                )?;

                Ok(fold_effect(std::cmp::max(n - n1, m - m1), &type_value3))
            }
            syntax::ControlValueExpression::ForArrayWhere {
                for_: _for_,
                value_parameter1,
                in_: _in_,
                value_expression1,
                where_: _where,
                value_parameter2,
                coloneq: _coloneq,
                value_expression2,
                do_: _do_,
                value_expression3,
                end: _end,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_value_parameter(environment, value_parameter1, true)?;

                let type_value2 = self.type_value_expression(environment, value_expression1)?;

                let n = rank(&type_value2);
                let type_value2 = unfold_effect(&type_value2);
                let n1 = rank(&type_value2);

                unify(
                    value_parameter1.location(),
                    &type_value2,
                    &Constituent::new(
                        value_expression1.location(),
                        TypeValue::Application(
                            Constituent::new(value_expression1.location(), TypeValue::Array),
                            type_value1,
                        ),
                    ),
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let (type_value5, mut value_env2) =
                    self.eval_value_parameter(environment, value_parameter2, true)?;

                local_environment.value_env.append(&mut value_env2);

                let type_value3 = self.type_value_expression(&environment, value_expression2)?;

                let type_value4 =
                    self.type_value_expression(&local_environment, value_expression3)?;

                let m = rank(&type_value3);
                let type_value3 = unfold_effect(&type_value3);
                let m1 = rank(&type_value3);

                let r = rank(&type_value4);
                let type_value4 = unfold_effect(&type_value4);
                let r1 = rank(&type_value4);

                unify(value_expression2.location(), &type_value3, &type_value5)?;
                unify(value_expression2.location(), &type_value3, &type_value4)?;

                Ok(fold_effect(
                    std::cmp::max(n - n1, std::cmp::max(m - m1, r - r1)),
                    &type_value4,
                ))
            }
            syntax::ControlValueExpression::ForRange {
                for_: _for_,
                value_parameter,
                in_: _in_,
                value_expression1,
                dotdot: _dotdot,
                value_expression2,
                do_: _do_,
                value_expression3,
                end: _end,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_value_parameter(environment, value_parameter, true)?;

                let type_value2 = self.type_value_expression(environment, value_expression1)?;
                let type_value3 = self.type_value_expression(environment, value_expression2)?;

                let n = rank(&type_value2);
                let type_value2 = unfold_effect(&type_value2);
                let n1 = rank(&type_value2);

                let m = rank(&type_value3);
                let type_value3 = unfold_effect(&type_value3);
                let m1 = rank(&type_value3);

                unify(
                    value_parameter.location(),
                    &type_value1,
                    &Constituent::new(value_parameter.location(), TypeValue::Int),
                )?;

                unify(
                    value_parameter.location(),
                    &type_value2,
                    &Constituent::new(value_expression1.location(), TypeValue::Int),
                )?;

                unify(
                    value_parameter.location(),
                    &type_value3,
                    &Constituent::new(value_expression2.location(), TypeValue::Int),
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let type_value4 =
                    self.type_value_expression(&local_environment, value_expression3)?;

                let r = rank(&type_value4);
                let type_value4 = unfold_effect(&type_value4);
                let r1 = rank(&type_value4);

                unify(
                    value_expression3.location(),
                    &type_value4,
                    &Constituent::new(value_expression3.location(), TypeValue::Bool),
                )?;

                Ok(fold_effect(
                    std::cmp::max(n - n1, std::cmp::max(m - m1, r - r1)),
                    &type_value4,
                ))
            }
            syntax::ControlValueExpression::ForRangeWhere {
                for_: _for_,
                value_parameter1,
                in_: _in_,
                value_expression1,
                dotdot: _dotdot,
                value_expression2,
                where_: _where,
                value_parameter2,
                coloneq: _coloneq,
                value_expression3,
                do_: _do_,
                value_expression4,
                end: _end,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_value_parameter(environment, value_parameter1, true)?;

                let type_value2 = self.type_value_expression(environment, value_expression1)?;
                let type_value3 = self.type_value_expression(environment, value_expression2)?;

                let n = rank(&type_value2);
                let type_value2 = unfold_effect(&type_value2);
                let n1 = rank(&type_value2);

                let m = rank(&type_value3);
                let type_value3 = unfold_effect(&type_value3);
                let m1 = rank(&type_value3);

                unify(
                    value_parameter1.location(),
                    &type_value1,
                    &Constituent::new(value_parameter1.location(), TypeValue::Int),
                )?;

                unify(
                    value_parameter1.location(),
                    &type_value2,
                    &Constituent::new(value_expression1.location(), TypeValue::Int),
                )?;

                unify(
                    value_parameter1.location(),
                    &type_value3,
                    &Constituent::new(value_expression2.location(), TypeValue::Int),
                )?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let (type_value6, mut value_env2) =
                    self.eval_value_parameter(environment, value_parameter2, true)?;

                local_environment.value_env.append(&mut value_env2);

                let type_value4 = self.type_value_expression(&environment, value_expression3)?;

                let type_value5 =
                    self.type_value_expression(&local_environment, value_expression4)?;

                let r = rank(&type_value4);
                let type_value4 = unfold_effect(&type_value4);
                let r1 = rank(&type_value4);

                let k = rank(&type_value5);
                let type_value5 = unfold_effect(&type_value5);
                let k1 = rank(&type_value5);

                unify(value_expression3.location(), &type_value4, &type_value6)?;
                unify(value_expression3.location(), &type_value4, &type_value5)?;

                Ok(fold_effect(
                    std::cmp::max(n - n1, std::cmp::max(m - m1, std::cmp::max(r - r1, k - k1))),
                    &type_value4,
                ))
            }
        }
    }

    pub fn type_value_expression(
        &mut self,
        environment: &Environment,
        value_expression: &Constituent<syntax::ValueExpression>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = value_expression.location();

        match &**value_expression {
            syntax::ValueExpression::ControlValueExpression {
                control_value_expression,
            } => self.type_control_value_expression(environment, control_value_expression),
            syntax::ValueExpression::Semicolon {
                control_value_expression,
                semicolon: _semicolon,
                value_expression,
            } => {
                let type_value1 =
                    self.type_control_value_expression(environment, control_value_expression)?;
                let type_value2 = self.type_value_expression(environment, value_expression)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                unify(
                    location,
                    &type_value1,
                    &Constituent::new(location, TypeValue::Bool),
                )?;

                let type_value = fold_effect(std::cmp::max(n - n1, m - m1), &type_value2);

                Ok(type_value)
            }
            syntax::ValueExpression::Abstraction {
                hat: _hat,
                atomic_value_parameter,
                dot: _dot,
                value_expression,
            } => {
                let (type_value1, mut value_env) =
                    self.eval_atomic_value_parameter(environment, atomic_value_parameter, false)?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment.value_env.append(&mut value_env);

                let type_value2 =
                    self.type_value_expression(&local_environment, value_expression)?;

                Ok(Constituent::new(
                    value_expression.location(),
                    TypeValue::Arrow(type_value1, type_value2),
                ))
            }
            syntax::ValueExpression::Let {
                let_: _let_,
                identifier,
                coloneq: _coloneq,
                value_expression1,
                in_: _in_,
                value_expression2,
            } => {
                self.type_variable_level += 1;
                let type_value2 = self.type_value_expression(environment, value_expression1)?;
                self.type_variable_level -= 1;

                let type_value2 = if rank(&type_value2) == rank(&unfold_effect(&type_value2)) {
                    self.gen(&type_value2)
                } else {
                    type_value2
                };

                let n = rank(&type_value2);
                let type_value2 = unfold_effect(&type_value2);
                let n1 = rank(&type_value2);

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                local_environment
                    .value_env
                    .insert(String::from(&**identifier), type_value2);

                let type_value3 =
                    self.type_value_expression(&local_environment, value_expression2)?;

                let m = rank(&type_value3);
                let type_value3 = unfold_effect(&type_value3);
                let m1 = rank(&type_value3);

                Ok(fold_effect(std::cmp::max(n - n1, m - m1), &type_value3))
            }
            syntax::ValueExpression::If {
                if_: _if_,
                value_expression1,
                then: _then,
                value_expression2,
                else_: _else_,
                value_expression3,
            } => {
                let type_value1 = self.type_value_expression(environment, value_expression1)?;

                let n = rank(&type_value1);
                let type_value1 = unfold_effect(&type_value1);
                let n1 = rank(&type_value1);

                let type_value2 = self.type_value_expression(environment, value_expression2)?;

                let m = rank(&type_value2);
                let type_value2 = unfold_effect(&type_value2);
                let m1 = rank(&type_value2);

                let type_value3 = self.type_value_expression(environment, value_expression3)?;

                let r = rank(&type_value3);
                let type_value3 = unfold_effect(&type_value3);
                let r1 = rank(&type_value3);

                unify(
                    value_expression3.location(),
                    &type_value1,
                    &Constituent::new(value_expression1.location(), TypeValue::Bool),
                )?;

                unify(value_expression3.location(), &type_value2, &type_value3)?;

                Ok(fold_effect(
                    std::cmp::max(n - n1, std::cmp::max(m - m1, r - r1)),
                    &type_value3,
                ))
            }
        }
    }

    pub fn type_value_expressions(
        &mut self,
        environment: &Environment,
        value_expressions: &Constituent<syntax::ValueExpressions>,
    ) -> Result<Constituent<TypeValue>, Error> {
        let location = value_expressions.location();

        match &**value_expressions {
            syntax::ValueExpressions::One { value_expression } => {
                self.type_value_expression(environment, value_expression)
            }
            syntax::ValueExpressions::More {
                value_expression,
                comma: _comma,
                value_expressions,
            } => {
                let type_value1 = self.type_value_expression(environment, value_expression)?;
                let type_value2 = self.type_value_expressions(environment, value_expressions)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                let type_value =
                    Constituent::new(location, TypeValue::Product(type_value1, type_value2));

                let type_value = fold_effect(std::cmp::max(n - n1, m - m1), &type_value);

                Ok(type_value)
            }
        }
    }

    pub fn type_value_expressions_array(
        &mut self,
        environment: &Environment,
        value_expressions: &Constituent<syntax::ValueExpressions>,
    ) -> Result<Constituent<TypeValue>, Error> {
        match &**value_expressions {
            syntax::ValueExpressions::One { value_expression } => {
                self.type_value_expression(environment, value_expression)
            }
            syntax::ValueExpressions::More {
                value_expression,
                comma: _comma,
                value_expressions,
            } => {
                let type_value1 = self.type_value_expression(environment, value_expression)?;
                let type_value2 =
                    self.type_value_expressions_array(environment, value_expressions)?;

                let n = rank(&type_value1);
                let m = rank(&type_value2);

                let type_value1 = unfold_effect(&type_value1);
                let type_value2 = unfold_effect(&type_value2);

                let n1 = rank(&type_value1);
                let m1 = rank(&type_value2);

                unify(value_expression.location(), &type_value1, &type_value2)?;

                let type_value = fold_effect(std::cmp::max(n - n1, m - m1), &type_value1);

                Ok(type_value)
            }
        }
    }

    fn eval_value_definition_def(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        identifier: &Lexeme,
        type_parameters: Option<&Constituent<syntax::TypeParameters>>,
        value_parameters: Option<&Constituent<syntax::ValueParameters>>,
        type_expression: Option<&Constituent<syntax::TypeExpression>>,
        value_expression: &Constituent<syntax::ValueExpression>,
    ) -> Result<(), Error> {
        if export_environment.record_env.get(&**identifier).is_some() {
            return Err(Error::RecordAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.variant_env.get(&**identifier).is_some() {
            return Err(Error::VariantAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.family_env.get(&**identifier).is_some() {
            return Err(Error::FamilyAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.type_env.get(&**identifier).is_some() {
            return Err(Error::TypeAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.value_env.get(&**identifier).is_some() {
            return Err(Error::ValueAlreadyDefined {
                location: identifier.location(),
            });
        }

        let mut local_environment = Environment {
            family_path: environment.family_path.clone(),
            family_parameter: environment.family_parameter.clone(),
            family_parameter_kind: environment.family_parameter_kind.clone(),
            family_parameter_type: environment.family_parameter_type.clone(),
            family_type: environment.family_type.clone(),
            type_env: environment.type_env.clone(),
            value_env: environment.value_env.clone(),
            family_env: environment.family_env.clone(),
            record_env: environment.record_env.clone(),
            variant_env: environment.variant_env.clone(),
        };

        local_environment
            .type_env
            .append(&mut local_type_env.clone());

        self.type_variable_level += 1;
        let (type_param, _kind_value1) = if let Some(type_parameters) = type_parameters {
            let (type_param, kind_value1, mut type_env) = self.eval_type_parameters(
                &local_environment,
                type_parameters,
                true,
                &TypeQuantifier::Forall,
            )?;

            local_environment.type_env.append(&mut type_env);

            (Some(type_param), Some(kind_value1))
        } else {
            (None, None)
        };
        self.type_variable_level -= 1;

        self.type_variable_level += 1;
        let param_type = if let Some(value_parameters) = value_parameters {
            Some(self.eval_value_parameters(&local_environment, value_parameters, false)?)
        } else {
            None
        };
        self.type_variable_level -= 1;

        let parameter_type = if let Some((param_type, _value_env)) = &param_type {
            Some(param_type.clone())
        } else {
            None
        };

        if let Some((_, value_env)) = &param_type {
            local_environment.value_env.append(&mut value_env.clone());
        }

        self.type_variable_level += 1;
        let type_value = self.type_value_expression(&local_environment, value_expression)?;
        self.type_variable_level -= 1;

        let type_value = if let Some(type_expression) = type_expression {
            self.type_variable_level += 1;
            let (type_value1, kind_value) =
                self.eval_type_expression(&local_environment, type_expression, true)?;
            self.type_variable_level -= 1;

            if !kind::subtype(
                &kind_value,
                &kind::positive_or_negative(type_expression.location(), true),
            ) {
                return Err(Error::KindMismatch {
                    location: type_expression.location(),
                });
            }

            let type_value3 = self.ascribe_type(&type_value1);

            unify(type_expression.location(), &type_value, &type_value3)?;

            type_value1
        } else {
            type_value
        };

        let type_value = if let Some(param_type) = parameter_type {
            Constituent::new(
                identifier.location(),
                TypeValue::Arrow(param_type, type_value),
            )
        } else {
            type_value
        };

        if rank(&type_value) != rank(&unfold_effect(&type_value)) {
            return Err(Error::ToplevelEffect {
                location: identifier.location(),
            });
        }

        let type_value = if let Some(type_param) = &type_param {
            self.refresh_type(&Constituent::new(
                type_value.location(),
                TypeValue::Forall(type_param.clone(), type_value),
            ))
        } else {
            self.refresh_type(&type_value)
        };

        if let Some(family_type) = &environment.family_type {
            let type_value2 = self.instantiate_type(&type_value);
            let type_value3 = self.ascribe_type(&family_type);

            unify(identifier.location(), &type_value2, &type_value3)?;
        }

        export_environment
            .value_env
            .insert(String::from(&**identifier), type_value.clone());

        Ok(())
    }

    pub fn eval_value_definition(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        value_definition: &Constituent<syntax::ValueDefinition>,
    ) -> Result<(), Error> {
        match &**value_definition {
            syntax::ValueDefinition::Def {
                def: _def,
                identifier,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefColon {
                def: _def,
                identifier,
                colon: _colon,
                type_expression,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                Some(type_expression),
                value_expression,
            ),
            syntax::ValueDefinition::DefFn {
                def: _def,
                identifier,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(value_parameters),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefFnColon {
                def: _def,
                identifier,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                colon: _colon,
                type_expression,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(value_parameters),
                Some(type_expression),
                value_expression,
            ),
            syntax::ValueDefinition::DefPoly {
                def: _def,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefPolyColon {
                def: _def,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                colon: _colon,
                type_expression,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                Some(type_expression),
                value_expression,
            ),
            syntax::ValueDefinition::DefPolyFn {
                def: _def,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(value_parameters),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefPolyFnColon {
                def: _def,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                colon: _colon,
                type_expression,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(value_parameters),
                Some(type_expression),
                value_expression,
            ),
            syntax::ValueDefinition::DefEq {
                def: _def,
                identifier1,
                eq,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                eq,
                None,
                Some(&Constituent::new(
                    eq.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            eq.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    eq.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: eq.clone(),
                        value_parameters: Constituent::new(
                            eq.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    eq.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            eq.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefNoteq {
                def: _def,
                identifier1,
                noteq,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                noteq,
                None,
                Some(&Constituent::new(
                    noteq.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            noteq.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    noteq.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: noteq.clone(),
                        value_parameters: Constituent::new(
                            noteq.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    noteq.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            noteq.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefLte {
                def: _def,
                identifier1,
                lte,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                lte,
                None,
                Some(&Constituent::new(
                    lte.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            lte.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    lte.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: lte.clone(),
                        value_parameters: Constituent::new(
                            lte.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    lte.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            lte.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefLt {
                def: _def,
                identifier1,
                lt,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                lt,
                None,
                Some(&Constituent::new(
                    lt.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            lt.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    lt.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: lt.clone(),
                        value_parameters: Constituent::new(
                            lt.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    lt.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            lt.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefGte {
                def: _def,
                identifier1,
                gte,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                gte,
                None,
                Some(&Constituent::new(
                    gte.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            gte.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    gte.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: gte.clone(),
                        value_parameters: Constituent::new(
                            gte.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    gte.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            gte.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefGt {
                def: _def,
                identifier1,
                gt,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                gt,
                None,
                Some(&Constituent::new(
                    gt.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            gt.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    gt.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: gt.clone(),
                        value_parameters: Constituent::new(
                            gt.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    gt.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            gt.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefPlus {
                def: _def,
                identifier1,
                plus,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                plus,
                None,
                Some(&Constituent::new(
                    plus.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            plus.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    plus.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: plus.clone(),
                        value_parameters: Constituent::new(
                            plus.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    plus.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            plus.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefMinus {
                def: _def,
                identifier1,
                minus,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                minus,
                None,
                Some(&Constituent::new(
                    minus.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            minus.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    minus.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: minus.clone(),
                        value_parameters: Constituent::new(
                            minus.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    minus.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            minus.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefAsterisk {
                def: _def,
                identifier1,
                asterisk,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                asterisk,
                None,
                Some(&Constituent::new(
                    asterisk.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            asterisk.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    asterisk.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: asterisk.clone(),
                        value_parameters: Constituent::new(
                            asterisk.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    asterisk.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            asterisk.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefSlash {
                def: _def,
                identifier1,
                slash,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                slash,
                None,
                Some(&Constituent::new(
                    slash.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            slash.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    slash.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: slash.clone(),
                        value_parameters: Constituent::new(
                            slash.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    slash.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            slash.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
            syntax::ValueDefinition::DefPercent {
                def: _def,
                identifier1,
                percent,
                identifier2,
                coloneq: _coloneq,
                value_expression,
            } => self.eval_value_definition_def(
                environment,
                export_environment,
                local_type_env,
                percent,
                None,
                Some(&Constituent::new(
                    percent.location(),
                    syntax::ValueParameters::More {
                        value_parameter: Constituent::new(
                            percent.location(),
                            syntax::ValueParameter::AtomicValueParameter {
                                atomic_value_parameter: Constituent::new(
                                    percent.location(),
                                    syntax::AtomicValueParameter::Identifier {
                                        identifier: identifier1.clone(),
                                    },
                                ),
                            },
                        ),
                        comma: percent.clone(),
                        value_parameters: Constituent::new(
                            percent.location(),
                            syntax::ValueParameters::One {
                                value_parameter: Constituent::new(
                                    percent.location(),
                                    syntax::ValueParameter::AtomicValueParameter {
                                        atomic_value_parameter: Constituent::new(
                                            percent.location(),
                                            syntax::AtomicValueParameter::Identifier {
                                                identifier: identifier2.clone(),
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                    },
                )),
                None,
                value_expression,
            ),
        }
    }

    fn eval_family_definition_family(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        identifier: &Lexeme,
        type_parameters: Option<&Constituent<syntax::TypeParameters>>,
        value_parameters: Option<&Constituent<syntax::ValueParameters>>,
        type_expression: Option<&Constituent<syntax::TypeExpression>>,
        family_expression: &Constituent<syntax::FamilyExpression>,
    ) -> Result<(), Error> {
        if export_environment.record_env.get(&**identifier).is_some() {
            return Err(Error::RecordAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.variant_env.get(&**identifier).is_some() {
            return Err(Error::VariantAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.family_env.get(&**identifier).is_some() {
            return Err(Error::FamilyAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.type_env.get(&**identifier).is_some() {
            return Err(Error::TypeAlreadyDefined {
                location: identifier.location(),
            });
        }

        if export_environment.value_env.get(&**identifier).is_some() {
            return Err(Error::ValueAlreadyDefined {
                location: identifier.location(),
            });
        }

        let family_path = vec![environment.family_path.clone(), vec![identifier.clone()]].concat();

        self.type_variable_level += 1;
        let family_param = if let Some(type_parameters) = type_parameters {
            Some(self.eval_type_parameters(
                environment,
                type_parameters,
                true,
                &TypeQuantifier::Exists,
            )?)
        } else {
            None
        };
        self.type_variable_level -= 1;

        let family_parameter =
            if let Some((family_parameter, _family_parameter_kind, _family_parameter_type_env)) =
                &family_param
            {
                if let Some(parent_family_parameter) = &environment.family_parameter {
                    Some(Constituent::new(
                        identifier.location(),
                        TypeParameter::Pair(
                            parent_family_parameter.clone(),
                            family_parameter.clone(),
                        ),
                    ))
                } else {
                    Some(family_parameter.clone())
                }
            } else {
                environment.family_parameter.clone()
            };

        let family_parameter_kind =
            if let Some((_family_parameter, family_parameter_kind, _family_parameter_type_env)) =
                &family_param
            {
                if let Some(parent_family_parameter_kind) = &environment.family_parameter_kind {
                    Some(Constituent::new(
                        identifier.location(),
                        KindValue::Product(
                            parent_family_parameter_kind.clone(),
                            family_parameter_kind.clone(),
                        ),
                    ))
                } else {
                    Some(family_parameter_kind.clone())
                }
            } else {
                environment.family_parameter_kind.clone()
            };

        let mut local_environment = Environment {
            family_path: environment.family_path.clone(),
            family_parameter: environment.family_parameter.clone(),
            family_parameter_kind: environment.family_parameter_kind.clone(),
            family_parameter_type: environment.family_parameter_type.clone(),
            family_type: environment.family_type.clone(),
            type_env: environment.type_env.clone(),
            value_env: environment.value_env.clone(),
            family_env: environment.family_env.clone(),
            record_env: environment.record_env.clone(),
            variant_env: environment.variant_env.clone(),
        };

        local_environment
            .type_env
            .append(&mut local_type_env.clone());

        if let Some((_family_parameter, _family_parameter_kind, family_parameter_type_env)) =
            &family_param
        {
            local_environment
                .type_env
                .append(&mut family_parameter_type_env.clone());
        }

        self.type_variable_level += 1;
        let param_type = if let Some(value_parameters) = value_parameters {
            Some(self.eval_value_parameters(&local_environment, value_parameters, false)?)
        } else {
            None
        };
        self.type_variable_level -= 1;

        let family_parameter_type = if let Some((family_param_type, _value_env)) = &param_type {
            if let Some(parent_family_parameter_type) = &environment.family_parameter_type {
                Some(Constituent::new(
                    identifier.location(),
                    TypeValue::Product(
                        parent_family_parameter_type.clone(),
                        family_param_type.clone(),
                    ),
                ))
            } else {
                Some(family_param_type.clone())
            }
        } else {
            None
        };

        if let Some((_, value_env)) = &param_type {
            local_environment.value_env.append(&mut value_env.clone());
        }

        self.type_variable_level += 1;
        let family_type = if let Some(type_expression) = type_expression {
            let (family_type, family_kind) =
                self.eval_type_expression(&local_environment, type_expression, true)?;

            if !kind::subtype(
                &family_kind,
                &kind::positive_or_negative(type_expression.location(), true),
            ) {
                return Err(Error::KindMismatch {
                    location: type_expression.location(),
                });
            }

            Some(family_type)
        } else {
            None
        };
        self.type_variable_level -= 1;

        local_environment.family_path = family_path.clone();
        local_environment.family_parameter = family_parameter.clone();
        local_environment.family_parameter_kind = family_parameter_kind.clone();
        local_environment.family_parameter_type = family_parameter_type.clone();
        local_environment.family_type = family_type.clone();

        let mut family_environment = Environment {
            family_path: family_path.clone(),
            family_parameter: family_parameter.clone(),
            family_parameter_kind: family_parameter_kind.clone(),
            family_parameter_type: family_parameter_type.clone(),
            family_type: family_type.clone(),
            type_env: TypeEnvironment::new(),
            value_env: ValueEnvironment::new(),
            family_env: FamilyEnvironment::new(),
            record_env: RecordEnvironment::new(),
            variant_env: VariantEnvironment::new(),
        };

        let syntax::FamilyExpression::Family {
            family: _family,
            definitions,
            end: _end,
        } = &**family_expression;

        self.type_variable_level += 1;
        self.eval_definitions(&mut local_environment, &mut family_environment, definitions)?;
        self.type_variable_level -= 1;

        let mut new_type_env = TypeEnvironment::new();
        let mut new_value_env = ValueEnvironment::new();

        for (id, (type_value, k)) in family_environment.type_env {
            let family_parameter = family_parameter.clone();
            let family_parameter_kind = family_parameter_kind.clone();

            if let Some(family_parameter) = family_parameter {
                if let Some(family_parameter_kind) = family_parameter_kind {
                    if family_environment.record_env.get(&id).is_none()
                        && family_environment.variant_env.get(&id).is_none()
                        && family_environment.family_env.get(&id).is_none()
                    {
                        let t = Constituent::new(
                            identifier.location(),
                            TypeValue::Abstraction(family_parameter.clone(), type_value.clone()),
                        );
                        let k = Constituent::new(
                            identifier.location(),
                            KindValue::Arrow(family_parameter_kind.clone(), k.clone()),
                        );

                        new_type_env.insert(id, (t, k));

                        continue;
                    }
                }
            }

            new_type_env.insert(id, (type_value, k));
        }

        for (id, type_value) in family_environment.value_env {
            let family_parameter_type = family_parameter_type.clone();

            self.type_variable_level += 1;
            let t = self.instantiate_type(&type_value);
            self.type_variable_level -= 1;

            let type_value = if let Some(family_parameter) = &family_parameter {
                if let Some(family_parameter_type) = &family_parameter_type {
                    Constituent::new(
                        identifier.location(),
                        TypeValue::Forall(
                            family_parameter.clone(),
                            Constituent::new(
                                identifier.location(),
                                TypeValue::Arrow(family_parameter_type.clone(), t.clone()),
                            ),
                        ),
                    )
                } else {
                    Constituent::new(
                        identifier.location(),
                        TypeValue::Forall(family_parameter.clone(), t.clone()),
                    )
                }
            } else {
                if let Some(family_parameter_type) = &family_parameter_type {
                    Constituent::new(
                        identifier.location(),
                        TypeValue::Arrow(family_parameter_type.clone(), t.clone()),
                    )
                } else {
                    t.clone()
                }
            };

            let type_value = self.refresh_type(&type_value);

            new_value_env.insert(id, type_value);
        }

        family_environment.type_env = new_type_env;
        family_environment.value_env = new_value_env;

        let family_value = Constituent::new(
            family_expression.location(),
            FamilyValue {
                family_path: family_path.clone(),
                family_parameter: family_parameter.clone(),
                family_parameter_kind: family_parameter_kind.clone(),
                family_parameter_type: family_parameter_type.clone(),
                family_type: family_type.clone(),
                type_env: family_environment.type_env.clone(),
                value_env: family_environment.value_env.clone(),
                family_env: family_environment.family_env.clone(),
                record_env: family_environment.record_env.clone(),
                variant_env: family_environment.variant_env.clone(),
            },
        );

        if let Some((family_parameter, family_parameter_kind, _family_parameter_type_env)) =
            &family_param
        {
            let family_arg = self.type_parameter_to_type_argument(&family_parameter);

            export_environment.type_env.insert(
                String::from(&**identifier),
                (
                    Constituent::new(
                        identifier.location(),
                        TypeValue::Abstraction(
                            family_parameter.clone(),
                            Constituent::new(
                                identifier.location(),
                                TypeValue::Application(
                                    Constituent::new(
                                        identifier.location(),
                                        TypeValue::NominalFamily(family_path.clone()),
                                    ),
                                    family_arg,
                                ),
                            ),
                        ),
                    ),
                    if kind::include_positive_or_negative(&family_parameter_kind) {
                        let k = Constituent::new(
                            identifier.location(),
                            KindValue::Arrow(
                                family_parameter_kind.clone(),
                                kind::positive_or_negative(identifier.location(), true),
                            ),
                        );

                        kind::construct_intersection(
                            identifier.location(),
                            &vec![k.clone(), kind::inverse(&k)],
                        )?
                    } else {
                        Constituent::new(
                            identifier.location(),
                            KindValue::Arrow(
                                family_parameter_kind.clone(),
                                kind::positive_and_negative(identifier.location()),
                            ),
                        )
                    },
                ),
            );
        } else {
            export_environment.type_env.insert(
                String::from(&**identifier),
                (
                    Constituent::new(
                        identifier.location(),
                        TypeValue::NominalFamily(family_path.clone()),
                    ),
                    kind::positive_and_negative(identifier.location()),
                ),
            );
        }

        export_environment
            .family_env
            .insert(String::from(&**identifier), family_value);

        Ok(())
    }

    pub fn eval_family_definition(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        family_definition: &Constituent<syntax::FamilyDefinition>,
    ) -> Result<(), Error> {
        match &**family_definition {
            syntax::FamilyDefinition::Family {
                family: _family,
                identifier,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                None,
                family_expression,
            ),
            syntax::FamilyDefinition::FamilySubtype {
                family: _family,
                identifier,
                subtype: _subtype,
                type_expression,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                None,
                Some(type_expression),
                family_expression,
            ),
            syntax::FamilyDefinition::FamilyFn {
                family: _family,
                identifier,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(value_parameters),
                None,
                family_expression,
            ),
            syntax::FamilyDefinition::FamilyFnSubtype {
                family: _family,
                identifier,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                subtype: _subtype,
                type_expression,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                None,
                Some(value_parameters),
                Some(type_expression),
                family_expression,
            ),
            syntax::FamilyDefinition::PolyFamily {
                family: _family,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                None,
                family_expression,
            ),
            syntax::FamilyDefinition::PolyFamilySubtype {
                family: _family,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                subtype: _subtype,
                type_expression,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                None,
                Some(type_expression),
                family_expression,
            ),
            syntax::FamilyDefinition::PolyFamilyFn {
                family: _family,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(value_parameters),
                None,
                family_expression,
            ),
            syntax::FamilyDefinition::PolyFamilyFnSubtype {
                family: _family,
                identifier,
                lbracket: _lbracket,
                type_parameters,
                rbracket: _rbracket,
                lparen: _lparen,
                value_parameters,
                rparen: _rparen,
                subtype: _subtype,
                type_expression,
                coloneq: _coloneq,
                family_expression,
            } => self.eval_family_definition_family(
                environment,
                export_environment,
                local_type_env,
                identifier,
                Some(type_parameters),
                Some(value_parameters),
                Some(type_expression),
                family_expression,
            ),
        }
    }

    fn get_type_definition_inductive_type_environment(
        &mut self,
        environment: &Environment,
        type_definition: &Constituent<syntax::TypeDefinition>,
    ) -> Result<TypeEnvironment, Error> {
        match &**type_definition {
            syntax::TypeDefinition::Type {
                type_: _type_,
                identifier: _identifier,
                coloneq: _coloneq,
                type_expression: _type_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::TypeColon {
                type_: _type_,
                identifier: _identifier,
                colon: _colon,
                kind_expression: _kind_expression,
                coloneq: _coloneq,
                type_expression: _type_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::TypeFn {
                type_: _type_,
                identifier: _identifier,
                lparen: _lparen,
                type_parameters: _type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                type_expression: _type_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::TypeFnColon {
                type_: _type_,
                identifier: _identifier,
                lparen: _lparen,
                type_parameters: _type_parameters,
                rparen: _rparen,
                colon: _colon,
                kind_expression: _kind_expression,
                coloneq: _coloneq,
                type_expression: _type_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::Struct {
                struct_: _struct_,
                identifier: _identifier,
                coloneq: _coloneq,
                structure_expression: _structure_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::StructColon {
                struct_: _struct_,
                identifier: _identifier,
                colon: _colon,
                kind_expression: _kind_expression,
                coloneq: _coloneq,
                structure_expression: _structure_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::StructFn {
                struct_: _struct_,
                identifier: _identifier,
                lparen: _lparen,
                type_parameters: _type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                structure_expression: _structure_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::StructFnColon {
                struct_: _struct_,
                identifier: _identifier,
                lparen: _lparen,
                type_parameters: _type_parameters,
                rparen: _rparen,
                colon: _colon,
                kind_expression: _kind_expression,
                coloneq: _coloneq,
                structure_expression: _structure_expression,
            } => Ok(TypeEnvironment::new()),
            syntax::TypeDefinition::Inductive {
                inductive: _inductive,
                identifier,
                coloneq: _coloneq,
                structure_expression,
            } => match &**structure_expression {
                syntax::StructureExpression::RecordExpression {
                    record_expression: _record_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::NominalRecord(
                            vec![environment.family_path.clone(), vec![identifier.clone()]]
                                .concat(),
                        ),
                    );
                    let kind_value = kind::positive_or_negative(identifier.location(), true);
                    type_env.insert(String::from(&**identifier), (type_value, kind_value));
                    Ok(type_env)
                }
                syntax::StructureExpression::VariantExpression {
                    variant_expression: _variant_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::NominalVariant(
                            vec![environment.family_path.clone(), vec![identifier.clone()]]
                                .concat(),
                        ),
                    );
                    let kind_value = kind::positive_or_negative(identifier.location(), true);
                    type_env.insert(String::from(&**identifier), (type_value, kind_value));
                    Ok(type_env)
                }
            },
            syntax::TypeDefinition::InductiveColon {
                inductive: _inductive,
                identifier,
                coloneq: _coloneq,
                colon: _colon,
                kind_expression,
                structure_expression,
            } => match &**structure_expression {
                syntax::StructureExpression::RecordExpression {
                    record_expression: _record_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::NominalRecord(
                            vec![environment.family_path.clone(), vec![identifier.clone()]]
                                .concat(),
                        ),
                    );
                    let kind_value = kind::positive_or_negative(identifier.location(), true);

                    let kind_value1 = self.eval_kind_expression(environment, kind_expression)?;
                    if kind::subtype(&kind_value, &kind_value1) {
                        return Err(Error::KindMismatch {
                            location: kind_value1.location(),
                        });
                    }

                    type_env.insert(String::from(&**identifier), (type_value, kind_value1));
                    Ok(type_env)
                }
                syntax::StructureExpression::VariantExpression {
                    variant_expression: _variant_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::NominalVariant(
                            vec![environment.family_path.clone(), vec![identifier.clone()]]
                                .concat(),
                        ),
                    );
                    let kind_value = kind::positive_or_negative(identifier.location(), true);

                    let kind_value1 = self.eval_kind_expression(environment, kind_expression)?;
                    if kind::subtype(&kind_value, &kind_value1) {
                        return Err(Error::KindMismatch {
                            location: kind_value1.location(),
                        });
                    }

                    type_env.insert(String::from(&**identifier), (type_value, kind_value1));
                    Ok(type_env)
                }
            },
            syntax::TypeDefinition::InductiveFn {
                inductive: _inductive,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                structure_expression,
            } => match &**structure_expression {
                syntax::StructureExpression::RecordExpression {
                    record_expression: _record_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let kind_value = kind::positive_or_negative(identifier.location(), true);

                    let (type_param, kind_value1, _type_env1) = self.eval_type_parameters(
                        environment,
                        type_parameters,
                        true,
                        &TypeQuantifier::Forall,
                    )?;

                    let type_arg = self.type_parameter_to_type_argument(&type_param);

                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::Abstraction(
                            type_param,
                            Constituent::new(
                                identifier.location(),
                                TypeValue::Application(
                                    Constituent::new(
                                        identifier.location(),
                                        TypeValue::NominalRecord(
                                            vec![
                                                environment.family_path.clone(),
                                                vec![identifier.clone()],
                                            ]
                                            .concat(),
                                        ),
                                    ),
                                    type_arg,
                                ),
                            ),
                        ),
                    );

                    type_env.insert(
                        String::from(&**identifier),
                        (
                            type_value,
                            Constituent::new(
                                identifier.location(),
                                KindValue::Arrow(kind_value1, kind_value),
                            ),
                        ),
                    );
                    Ok(type_env)
                }
                syntax::StructureExpression::VariantExpression {
                    variant_expression: _variant_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let kind_value = kind::positive_or_negative(identifier.location(), true);

                    let (type_param, kind_value1, _type_env1) = self.eval_type_parameters(
                        environment,
                        type_parameters,
                        true,
                        &TypeQuantifier::Forall,
                    )?;

                    let type_arg = self.type_parameter_to_type_argument(&type_param);

                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::Abstraction(
                            type_param,
                            Constituent::new(
                                identifier.location(),
                                TypeValue::Application(
                                    Constituent::new(
                                        identifier.location(),
                                        TypeValue::NominalVariant(
                                            vec![
                                                environment.family_path.clone(),
                                                vec![identifier.clone()],
                                            ]
                                            .concat(),
                                        ),
                                    ),
                                    type_arg,
                                ),
                            ),
                        ),
                    );

                    type_env.insert(
                        String::from(&**identifier),
                        (
                            type_value,
                            Constituent::new(
                                identifier.location(),
                                KindValue::Arrow(kind_value1, kind_value),
                            ),
                        ),
                    );
                    Ok(type_env)
                }
            },
            syntax::TypeDefinition::InductiveFnColon {
                inductive: _inductive,
                identifier,
                lparen: _lparen,
                type_parameters,
                rparen: _rparen,
                coloneq: _coloneq,
                colon: _colon,
                kind_expression,
                structure_expression,
            } => match &**structure_expression {
                syntax::StructureExpression::RecordExpression {
                    record_expression: _record_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let kind_value = kind::positive_or_negative(identifier.location(), true);

                    let kind_value1 = self.eval_kind_expression(environment, kind_expression)?;
                    if kind::subtype(&kind_value, &kind_value1) {
                        return Err(Error::KindMismatch {
                            location: kind_value1.location(),
                        });
                    }

                    let (type_param, kind_value2, _type_env1) = self.eval_type_parameters(
                        environment,
                        type_parameters,
                        true,
                        &TypeQuantifier::Forall,
                    )?;

                    let type_arg = self.type_parameter_to_type_argument(&type_param);

                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::Abstraction(
                            type_param,
                            Constituent::new(
                                identifier.location(),
                                TypeValue::Application(
                                    Constituent::new(
                                        identifier.location(),
                                        TypeValue::NominalRecord(
                                            vec![
                                                environment.family_path.clone(),
                                                vec![identifier.clone()],
                                            ]
                                            .concat(),
                                        ),
                                    ),
                                    type_arg,
                                ),
                            ),
                        ),
                    );

                    type_env.insert(
                        String::from(&**identifier),
                        (
                            type_value,
                            Constituent::new(
                                identifier.location(),
                                KindValue::Arrow(kind_value2, kind_value1),
                            ),
                        ),
                    );

                    Ok(type_env)
                }
                syntax::StructureExpression::VariantExpression {
                    variant_expression: _variant_expression,
                } => {
                    let mut type_env = TypeEnvironment::new();
                    let kind_value = kind::positive_or_negative(identifier.location(), true);

                    let kind_value1 = self.eval_kind_expression(environment, kind_expression)?;
                    if kind::subtype(&kind_value, &kind_value1) {
                        return Err(Error::KindMismatch {
                            location: kind_value1.location(),
                        });
                    }

                    let (type_param, kind_value2, _type_env1) = self.eval_type_parameters(
                        environment,
                        type_parameters,
                        true,
                        &TypeQuantifier::Forall,
                    )?;

                    let type_arg = self.type_parameter_to_type_argument(&type_param);

                    let type_value = Constituent::new(
                        identifier.location(),
                        TypeValue::Abstraction(
                            type_param,
                            Constituent::new(
                                identifier.location(),
                                TypeValue::Application(
                                    Constituent::new(
                                        identifier.location(),
                                        TypeValue::NominalVariant(
                                            vec![
                                                environment.family_path.clone(),
                                                vec![identifier.clone()],
                                            ]
                                            .concat(),
                                        ),
                                    ),
                                    type_arg,
                                ),
                            ),
                        ),
                    );

                    type_env.insert(
                        String::from(&**identifier),
                        (
                            type_value,
                            Constituent::new(
                                identifier.location(),
                                KindValue::Arrow(kind_value2, kind_value1),
                            ),
                        ),
                    );

                    Ok(type_env)
                }
            },
        }
    }

    fn get_definition_inductive_type_environment(
        &mut self,
        environment: &Environment,
        definition: &Constituent<syntax::Definition>,
    ) -> Result<TypeEnvironment, Error> {
        match &**definition {
            syntax::Definition::TypeDefinition { type_definition } => {
                self.get_type_definition_inductive_type_environment(environment, type_definition)
            }
            syntax::Definition::TypeDefinitionWhere {
                type_definition,
                where_: _where_,
                definition,
            } => {
                let mut local_type_env1 = self
                    .get_type_definition_inductive_type_environment(environment, type_definition)?;
                let mut local_type_env2 =
                    self.get_definition_inductive_type_environment(environment, definition)?;
                local_type_env1.append(&mut local_type_env2);

                Ok(local_type_env1)
            }
            syntax::Definition::ValueDefinition {
                value_definition: _value_definition,
            } => Ok(TypeEnvironment::new()),
            syntax::Definition::ValueDefinitionWhere {
                value_definition: _value_definition,
                where_: _where_,
                definition,
            } => self.get_definition_inductive_type_environment(environment, definition),
            syntax::Definition::FamilyDefinition {
                family_definition: _family_definition,
            } => Ok(TypeEnvironment::new()),
            syntax::Definition::FamilyDefinitionWhere {
                family_definition: _family_definition,
                where_: _where_,
                definition,
            } => self.get_definition_inductive_type_environment(environment, definition),
        }
    }

    pub fn eval_definition(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        local_type_env: &TypeEnvironment,
        definition: &Constituent<syntax::Definition>,
    ) -> Result<(), Error> {
        match &**definition {
            syntax::Definition::TypeDefinition { type_definition } => {
                self.eval_type_definition(
                    environment,
                    export_environment,
                    local_type_env,
                    type_definition,
                )?;

                Ok(())
            }
            syntax::Definition::TypeDefinitionWhere {
                type_definition,
                where_: _where_,
                definition,
            } => {
                self.eval_definition(environment, export_environment, local_type_env, definition)?;

                self.eval_type_definition(
                    environment,
                    export_environment,
                    local_type_env,
                    type_definition,
                )?;

                Ok(())
            }
            syntax::Definition::ValueDefinition { value_definition } => {
                self.eval_value_definition(
                    environment,
                    export_environment,
                    local_type_env,
                    value_definition,
                )?;

                Ok(())
            }
            syntax::Definition::ValueDefinitionWhere {
                value_definition,
                where_: _where_,
                definition,
            } => {
                self.eval_definition(environment, export_environment, local_type_env, definition)?;

                self.eval_value_definition(
                    environment,
                    export_environment,
                    local_type_env,
                    value_definition,
                )?;

                Ok(())
            }
            syntax::Definition::FamilyDefinition { family_definition } => {
                self.eval_family_definition(
                    environment,
                    export_environment,
                    local_type_env,
                    family_definition,
                )?;

                Ok(())
            }
            syntax::Definition::FamilyDefinitionWhere {
                family_definition,
                where_: _where_,
                definition,
            } => {
                self.eval_definition(environment, export_environment, local_type_env, definition)?;

                self.eval_family_definition(
                    environment,
                    export_environment,
                    local_type_env,
                    family_definition,
                )?;

                Ok(())
            }
        }
    }

    pub fn eval_definitions(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        definitions: &Constituent<syntax::Definitions>,
    ) -> Result<(), Error> {
        match &**definitions {
            syntax::Definitions::One { definition } => {
                let local_env =
                    self.get_definition_inductive_type_environment(environment, definition)?;

                self.eval_definition(environment, export_environment, &local_env, definition)?;

                Ok(())
            }
            syntax::Definitions::More {
                definition,
                definitions,
            } => {
                let local_env =
                    self.get_definition_inductive_type_environment(environment, definition)?;

                let mut local_environment = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: environment.type_env.clone(),
                    value_env: environment.value_env.clone(),
                    family_env: environment.family_env.clone(),
                    record_env: environment.record_env.clone(),
                    variant_env: environment.variant_env.clone(),
                };

                let mut exports = Environment {
                    family_path: environment.family_path.clone(),
                    family_parameter: environment.family_parameter.clone(),
                    family_parameter_kind: environment.family_parameter_kind.clone(),
                    family_parameter_type: environment.family_parameter_type.clone(),
                    family_type: environment.family_type.clone(),
                    type_env: TypeEnvironment::new(),
                    value_env: ValueEnvironment::new(),
                    family_env: FamilyEnvironment::new(),
                    record_env: RecordEnvironment::new(),
                    variant_env: VariantEnvironment::new(),
                };

                self.eval_definition(&environment, &mut exports, &local_env, definition)?;

                local_environment
                    .type_env
                    .append(&mut exports.type_env.clone());
                local_environment
                    .value_env
                    .append(&mut exports.value_env.clone());
                local_environment
                    .family_env
                    .append(&mut exports.family_env.clone());
                local_environment
                    .record_env
                    .append(&mut exports.record_env.clone());
                local_environment
                    .variant_env
                    .append(&mut exports.variant_env.clone());

                export_environment.type_env.append(&mut exports.type_env);
                export_environment.value_env.append(&mut exports.value_env);
                export_environment
                    .family_env
                    .append(&mut exports.family_env);
                export_environment
                    .record_env
                    .append(&mut exports.record_env);
                export_environment
                    .variant_env
                    .append(&mut exports.variant_env);

                self.eval_definitions(&local_environment, export_environment, definitions)?;

                Ok(())
            }
        }
    }

    pub fn eval_program(
        &mut self,
        environment: &Environment,
        export_environment: &mut Environment,
        program: &Constituent<syntax::Program>,
    ) -> Result<(), Error> {
        match &**program {
            syntax::Program::Definitions { definitions } => {
                self.eval_definitions(environment, export_environment, definitions)
            }
        }
    }
}
