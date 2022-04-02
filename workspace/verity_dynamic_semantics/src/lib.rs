extern crate verity_definition;

use verity_definition::common::*;
use verity_definition::dynamic_semantics;
use verity_definition::syntax;

type Environment = dynamic_semantics::Environment;
type Value = dynamic_semantics::Value;
type Error = dynamic_semantics::Error;

pub fn show_value(value: &Constituent<Value>) -> String {
    match &**value {
        Value::Bool(false) => String::from("false"),
        Value::Bool(true) => String::from("true"),
        Value::Int(integer) => format!("{}", integer),
        Value::Rational(rational) => {
            let zero = num_rational::BigRational::from(num_bigint::BigInt::from(0u32));
            let ten = num_rational::BigRational::from(num_bigint::BigInt::from(10u32));

            let t = rational.trunc().to_integer();
            let mut f = vec![];
            let mut r = rational.fract();

            while r != zero {
                f.push(format!(
                    "{}",
                    (r.clone() * ten.clone()).trunc().to_integer()
                ));
                r = (r * ten.clone()).fract();
            }

            if f.len() == 0 {
                format!("{}.0", t)
            } else {
                format!("{}.{}", t, f.join(""))
            }
        }
        Value::String(string) => {
            let mut cs = vec![];

            for c in string.chars() {
                if c == '"' {
                    cs.push('\\');
                    cs.push('"');
                } else if c == '\\' {
                    cs.push('\\');
                    cs.push('\\');
                } else {
                    cs.push(c);
                }
            }

            let s: String = std::iter::FromIterator::from_iter(cs);
            format!("\"{}\"", s)
        }
        Value::Binary(digits) => {
            let mut s = vec![];

            for digit in digits.iter() {
                s.push(format!("{:02X}", digit))
            }

            format!("#{}#", s.join(""))
        }
        Value::Abstraction(_, _, _) => String::from("<abstr>"),
        Value::Family(_) => String::from("<family>"),
        Value::Quasi(_, _) => String::from("<quasi>"),
        Value::Quasienv(_, _) => String::from("<quasi>"),
        Value::Lazy(_, _) => String::from("<lazy>"),
        Value::Variant(x, None) => format!("<{}>", x),
        Value::Variant(x, Some(value)) => format!("<{} := {}>", x, show_value(value)),
        Value::VariantInj(_) => String::from("<abstr>"),
        Value::RecordProj(_) => String::from("<abstr>"),
        Value::RecordProjDefault(_) => String::from("<abstr>"),
        Value::Array(values) => {
            let s = values
                .iter()
                .map(|value| show_value(value))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", s)
        }
        Value::Record(members) => {
            let s = members
                .iter()
                .map(|(id, value)| format!("{} := {}", id, show_value(value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", s)
        }
        Value::Reference(_) => String::from("<ref>"),
        Value::Pair(x, y) => {
            format!("({}, {})", show_value(x), show_value(y))
        }
        Value::ForeignFn(_) => String::from("<abstr>"),
    }
}

pub fn apply_atomic_value_parameter(
    location: (usize, usize),
    quasienvironment: &Environment,
    environment: &mut Environment,
    parameter: &Constituent<syntax::AtomicValueParameter>,
    argument: &Constituent<Value>,
) -> Result<(), Error> {
    match &**parameter {
        syntax::AtomicValueParameter::Identifier { identifier } => {
            environment.insert(String::from(&**identifier), argument.clone());
            Ok(())
        }
        syntax::AtomicValueParameter::Tuple {
            lparen: _lparen,
            value_parameters,
            rparen: _rparen,
        } => apply_value_parameters(
            location,
            quasienvironment,
            environment,
            value_parameters,
            argument,
        ),
    }
}

pub fn apply_value_parameter_member(
    location: (usize, usize),
    quasienvironment: &Environment,
    environment: &mut Environment,
    parameter: &Constituent<syntax::ValueParameterMember>,
    argument: &Constituent<Value>,
) -> Result<(), Error> {
    match &**parameter {
        syntax::ValueParameterMember::Identifier { identifier } => {
            if let Value::Record(r) = &**argument {
                if let Some(value) = r.get(&String::from(&**identifier)) {
                    environment.insert(String::from(&**identifier), value.clone());
                    Ok(())
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ValueParameterMember::Default {
            identifier,
            question: _question,
            value_expression,
        } => {
            if let Value::Record(r) = &**argument {
                if let Some(value) = r.get(&String::from(&**identifier)) {
                    environment.insert(String::from(&**identifier), value.clone());
                    Ok(())
                } else {
                    let value =
                        eval_value_expression(quasienvironment, environment, value_expression)?;
                    environment.insert(String::from(&**identifier), value);
                    Ok(())
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ValueParameterMember::DefaultMap {
            identifier,
            question: _question,
            value_expression1,
            colon: _colon,
            value_expression2,
        } => {
            if let Value::Record(r) = &**argument {
                if let Some(value) = r.get(&String::from(&**identifier)) {
                    let operation =
                        eval_value_expression(quasienvironment, environment, value_expression2)?;
                    let value = apply(location, quasienvironment, environment, &operation, &value)?;
                    environment.insert(String::from(&**identifier), value);
                    Ok(())
                } else {
                    let value =
                        eval_value_expression(quasienvironment, environment, value_expression1)?;
                    environment.insert(String::from(&**identifier), value);
                    Ok(())
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ValueParameterMember::Colon {
            identifier,
            colon: _colon,
            type_expression: _type_expression,
        } => {
            if let Value::Record(r) = &**argument {
                if let Some(value) = r.get(&String::from(&**identifier)) {
                    environment.insert(String::from(&**identifier), value.clone());
                    Ok(())
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ValueParameterMember::ColonDefault {
            identifier,
            colon: _colon,
            type_expression: _type_expression,
            question: _question,
            value_expression,
        } => {
            if let Value::Record(r) = &**argument {
                if let Some(value) = r.get(&String::from(&**identifier)) {
                    environment.insert(String::from(&**identifier), value.clone());
                    Ok(())
                } else {
                    let value =
                        eval_value_expression(quasienvironment, environment, value_expression)?;
                    environment.insert(String::from(&**identifier), value);
                    Ok(())
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ValueParameterMember::ColonDefaultMap {
            identifier,
            colon1: _colon1,
            type_expression: _type_expression,
            question: _question,
            value_expression1,
            colon2: _colon2,
            value_expression2,
        } => {
            if let Value::Record(r) = &**argument {
                if let Some(value) = r.get(&String::from(&**identifier)) {
                    let operation =
                        eval_value_expression(quasienvironment, environment, value_expression2)?;
                    let value = apply(location, quasienvironment, environment, &operation, &value)?;
                    environment.insert(String::from(&**identifier), value);
                    Ok(())
                } else {
                    let value =
                        eval_value_expression(quasienvironment, environment, value_expression1)?;
                    environment.insert(String::from(&**identifier), value);
                    Ok(())
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn apply_value_parameter_members(
    location: (usize, usize),
    quasienvironment: &Environment,
    environment: &mut Environment,
    parameter: &Constituent<syntax::ValueParameterMembers>,
    argument: &Constituent<Value>,
) -> Result<(), Error> {
    match &**parameter {
        syntax::ValueParameterMembers::One {
            value_parameter_member,
        } => apply_value_parameter_member(
            location,
            quasienvironment,
            environment,
            value_parameter_member,
            argument,
        ),
        syntax::ValueParameterMembers::More {
            value_parameter_member,
            comma: _comma,
            value_parameter_members,
        } => {
            apply_value_parameter_member(
                location,
                quasienvironment,
                environment,
                value_parameter_member,
                argument,
            )?;

            apply_value_parameter_members(
                location,
                quasienvironment,
                environment,
                value_parameter_members,
                argument,
            )
        }
    }
}

pub fn apply_value_parameter(
    location: (usize, usize),
    quasienvironment: &Environment,
    environment: &mut Environment,
    parameter: &Constituent<syntax::ValueParameter>,
    argument: &Constituent<Value>,
) -> Result<(), Error> {
    match &**parameter {
        syntax::ValueParameter::AtomicValueParameter {
            atomic_value_parameter,
        } => apply_atomic_value_parameter(
            location,
            quasienvironment,
            environment,
            atomic_value_parameter,
            argument,
        ),
        syntax::ValueParameter::Colon {
            identifier,
            colon: _colon,
            type_expression: _type_expression,
        } => {
            environment.insert(String::from(&**identifier), argument.clone());
            Ok(())
        }
        syntax::ValueParameter::EmptyRecord {
            qualified_identifier: _qualified_identifier,
            lbrace: _lbrace,
            rbrace: _rbrace,
        } => Ok(()),
        syntax::ValueParameter::NonEmptyRecord {
            qualified_identifier: _qualified_identifier,
            lbrace: _lbrace,
            value_parameter_members,
            rbrace: _rbrace,
        } => apply_value_parameter_members(
            location,
            quasienvironment,
            environment,
            value_parameter_members,
            argument,
        ),
        syntax::ValueParameter::Family {
            family: _family,
            qualified_identifier: _qualified_identifier,
            atomic_value_parameter,
        } => apply_atomic_value_parameter(
            location,
            quasienvironment,
            environment,
            atomic_value_parameter,
            argument,
        ),
    }
}

pub fn apply_value_parameters(
    location: (usize, usize),
    quasienvironment: &Environment,
    environment: &mut Environment,
    parameters: &Constituent<syntax::ValueParameters>,
    argument: &Constituent<Value>,
) -> Result<(), Error> {
    match &**parameters {
        syntax::ValueParameters::One { value_parameter } => apply_value_parameter(
            location,
            quasienvironment,
            environment,
            value_parameter,
            argument,
        ),
        syntax::ValueParameters::More {
            value_parameter,
            comma: _comma,
            value_parameters,
        } => match &**argument {
            Value::Pair(x, y) => {
                apply_value_parameter(location, quasienvironment, environment, value_parameter, x)?;
                apply_value_parameters(
                    location,
                    quasienvironment,
                    environment,
                    value_parameters,
                    y,
                )?;
                Ok(())
            }
            _ => Err(Error::Unsound { location }),
        },
    }
}

pub fn apply(
    location: (usize, usize),
    quasienvironment: &Environment,
    environment: &Environment,
    operation: &Constituent<Value>,
    argument: &Constituent<Value>,
) -> Result<Constituent<Value>, Error> {
    if let Value::Abstraction(environment, parameter, expression) = &**operation {
        let mut environment = environment.clone();
        apply_atomic_value_parameter(
            location,
            quasienvironment,
            &mut environment,
            parameter,
            argument,
        )?;
        eval_value_expression(quasienvironment, &environment, expression)
    } else if let Value::VariantInj(string) = &**operation {
        Ok(Constituent::new(
            location,
            Value::Variant(string.clone(), Some(argument.clone())),
        ))
    } else if let Value::RecordProj(string) = &**operation {
        if let Value::Record(r) = &**argument {
            if let Some(v) = r.get(string) {
                Ok(v.clone())
            } else {
                Err(Error::Unsound { location })
            }
        } else {
            Err(Error::Unsound { location })
        }
    } else if let Value::RecordProjDefault(string) = &**operation {
        if let Value::Pair(r, x) = &**argument {
            if let Value::Record(r) = &**r {
                if let Some(y) = r.get(string) {
                    Ok(y.clone())
                } else {
                    Ok(x.clone())
                }
            } else {
                Err(Error::Unsound { location })
            }
        } else {
            Err(Error::Unsound { location })
        }
    } else if let Value::Quasienv(quasienvironment, value) = &**operation {
        let value = apply(location, quasienvironment, environment, value, argument)?;

        if let Value::Abstraction(_, _, _) = &*value {
            Ok(Constituent::new(
                operation.location(),
                Value::Quasienv(quasienvironment.clone(), value),
            ))
        } else if let Value::Lazy(_, _) = &*value {
            Ok(Constituent::new(
                operation.location(),
                Value::Quasienv(quasienvironment.clone(), value),
            ))
        } else {
            Ok(value)
        }
    } else if let Value::Quasi(parameters, value) = &**operation {
        let mut quasienvironment = Environment::new();

        let mut parameter = parameters[0].clone();

        for parameter1 in &parameters[1..] {
            parameter = Constituent::new(
                location,
                syntax::ValueParameters::More {
                    value_parameter: Constituent::new(
                        location,
                        syntax::ValueParameter::AtomicValueParameter {
                            atomic_value_parameter: Constituent::new(
                                location,
                                syntax::AtomicValueParameter::Tuple {
                                    lparen: Lexeme::new(location, String::from("(")),
                                    value_parameters: parameter.clone(),
                                    rparen: Lexeme::new(location, String::from(")")),
                                },
                            ),
                        },
                    ),
                    comma: Lexeme::new(location, String::from(",")),
                    value_parameters: parameter1.clone(),
                },
            );
        }

        apply_value_parameters(
            location,
            &Environment::new(),
            &mut quasienvironment,
            &parameter,
            argument,
        )?;

        let value = if let Value::Lazy(environment, expression) = &**value {
            eval_value_expression(&quasienvironment, environment, expression)?
        } else {
            value.clone()
        };

        if let Value::Abstraction(_, _, _) = &*value {
            Ok(Constituent::new(
                operation.location(),
                Value::Quasienv(quasienvironment, value),
            ))
        } else if let Value::Lazy(_, _) = &*value {
            Ok(Constituent::new(
                operation.location(),
                Value::Quasienv(quasienvironment, value),
            ))
        } else {
            Ok(value)
        }
    } else if let Value::ForeignFn(function) = &**operation {
        let environment = Environment::new();
        function(&environment, argument)
    } else {
        Err(Error::Unsound { location })
    }
}

pub fn eval_identifier(
    quasienvironment: &Environment,
    environment: &Environment,
    identifier: &Lexeme,
) -> Result<Constituent<Value>, Error> {
    if let Some(value) = environment.get(&**identifier) {
        if let Value::Lazy(environment, expression) = &**value {
            eval_value_expression(quasienvironment, environment, expression)
        } else {
            Ok(value.clone())
        }
    } else if let Some(value) = quasienvironment.get(&**identifier) {
        if let Value::Lazy(environment, expression) = &**value {
            eval_value_expression(quasienvironment, environment, expression)
        } else {
            Ok(value.clone())
        }
    } else {
        Err(Error::Unsound {
            location: identifier.location(),
        })
    }
}

pub fn eval_qualified_identifier(
    quasienvironment: &Environment,
    environment: &Environment,
    qualified_identifier: &Constituent<syntax::QualifiedIdentifier>,
) -> Result<Constituent<Value>, Error> {
    match &**qualified_identifier {
        syntax::QualifiedIdentifier::Identifier { identifier } => {
            eval_identifier(quasienvironment, environment, identifier)
        }
        syntax::QualifiedIdentifier::Dot {
            qualified_identifier,
            dot: _dot,
            identifier,
        } => {
            let value =
                eval_qualified_identifier(quasienvironment, environment, qualified_identifier)?;

            match &*value {
                Value::Family(family_value) => {
                    if let Some(value) = family_value.get(&**identifier) {
                        if let Value::Lazy(environment, expression) = &**value {
                            eval_value_expression(quasienvironment, environment, expression)
                        } else {
                            Ok(value.clone())
                        }
                    } else {
                        Err(Error::Unsound {
                            location: identifier.location(),
                        })
                    }
                }
                _ => Err(Error::Unsound {
                    location: identifier.location(),
                }),
            }
        }
    }
}

pub fn eval_atomic_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    atomic_value_expression: &Constituent<syntax::AtomicValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = atomic_value_expression.location();

    match &**atomic_value_expression {
        syntax::AtomicValueExpression::False { false_: _false_ } => {
            Ok(Constituent::new(location, Value::Bool(false)))
        }
        syntax::AtomicValueExpression::True { true_: _true_ } => {
            Ok(Constituent::new(location, Value::Bool(true)))
        }
        syntax::AtomicValueExpression::Integer { integer } => Ok(Constituent::new(
            location,
            Value::Int(String::from(&**integer).parse().unwrap()),
        )),
        syntax::AtomicValueExpression::Rational { rational } => {
            let mut parts = rational.splitn(2, '.');
            let x = parts.next().unwrap();
            let y = parts.next().unwrap();
            let n = y.len() as u32;

            let x: num_bigint::BigInt = x.parse().unwrap();
            let y: num_bigint::BigInt = y.parse().unwrap();
            let x = num_rational::BigRational::from_integer(x);
            let y = num_rational::BigRational::from_integer(y);

            let m = num_bigint::BigInt::from(10usize);
            let y = y / num_rational::BigRational::from_integer(m.pow(n));

            Ok(Constituent::new(location, Value::Rational(x + y)))
        }
        syntax::AtomicValueExpression::String { string } => {
            let mut cs = vec![];
            let mut backslash = false;

            for c in string.chars() {
                if backslash {
                    if c == '"' {
                        cs.push('"');
                    } else if c == '\\' {
                        cs.push('\\');
                    }

                    backslash = false;
                } else {
                    if c == '"' {
                    } else if c == '\\' {
                        backslash = true;
                    } else {
                        cs.push(c);
                    }
                }
            }

            Ok(Constituent::new(
                location,
                Value::String(std::iter::FromIterator::from_iter(cs)),
            ))
        }
        syntax::AtomicValueExpression::Binary { binary } => {
            let mut even = true;
            let mut digit: u8 = 0;
            let mut digits: Vec<u8> = vec![];

            for c in binary.chars() {
                match c {
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'a' | 'b' | 'c'
                    | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' => {
                        if even {
                            digit = if c == '0' {
                                0
                            } else if c == '1' {
                                1
                            } else if c == '2' {
                                2
                            } else if c == '3' {
                                3
                            } else if c == '4' {
                                4
                            } else if c == '5' {
                                5
                            } else if c == '6' {
                                6
                            } else if c == '7' {
                                7
                            } else if c == '8' {
                                8
                            } else if c == '9' {
                                9
                            } else if c == 'a' || c == 'A' {
                                10
                            } else if c == 'b' || c == 'B' {
                                11
                            } else if c == 'c' || c == 'C' {
                                12
                            } else if c == 'd' || c == 'D' {
                                13
                            } else if c == 'e' || c == 'E' {
                                14
                            } else if c == 'f' || c == 'F' {
                                15
                            } else {
                                0
                            };

                            digit = digit << 4;
                            even = false;
                        } else {
                            digit += if c == '0' {
                                0
                            } else if c == '1' {
                                1
                            } else if c == '2' {
                                2
                            } else if c == '3' {
                                3
                            } else if c == '4' {
                                4
                            } else if c == '5' {
                                5
                            } else if c == '6' {
                                6
                            } else if c == '7' {
                                7
                            } else if c == '8' {
                                8
                            } else if c == '9' {
                                9
                            } else if c == 'a' || c == 'A' {
                                10
                            } else if c == 'b' || c == 'B' {
                                11
                            } else if c == 'c' || c == 'C' {
                                12
                            } else if c == 'd' || c == 'D' {
                                13
                            } else if c == 'e' || c == 'E' {
                                14
                            } else if c == 'f' || c == 'F' {
                                15
                            } else {
                                0
                            };

                            digits.push(digit);
                            digit = 0;
                            even = true;
                        }
                    }
                    _ => {}
                }
            }

            if !even {
                digits.push(digit);
            }

            Ok(Constituent::new(location, Value::Binary(digits)))
        }
        syntax::AtomicValueExpression::QualifiedIdentifier {
            qualified_identifier,
        } => eval_qualified_identifier(quasienvironment, environment, qualified_identifier),
        syntax::AtomicValueExpression::QualifiedIdentifierDotEq {
            qualified_identifier,
            dot,
            eq,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: eq.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotNoteq {
            qualified_identifier,
            dot,
            noteq,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: noteq.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotLte {
            qualified_identifier,
            dot,
            lte,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: lte.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotLt {
            qualified_identifier,
            dot,
            lt,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: lt.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotGte {
            qualified_identifier,
            dot,
            gte,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: gte.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotGt {
            qualified_identifier,
            dot,
            gt,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: gt.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotPlus {
            qualified_identifier,
            dot,
            plus,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: plus.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotMinus {
            qualified_identifier,
            dot,
            minus,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: minus.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotAsterisk {
            qualified_identifier,
            dot,
            asterisk,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: asterisk.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotSlash {
            qualified_identifier,
            dot,
            slash,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: slash.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierDotPercent {
            qualified_identifier,
            dot,
            percent,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: dot.clone(),
                    identifier: percent.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::QualifiedIdentifierColoncolonIdentifier {
            qualified_identifier,
            coloncolon,
            identifier,
        } => eval_qualified_identifier(
            quasienvironment,
            environment,
            &Constituent::new(
                location,
                syntax::QualifiedIdentifier::Dot {
                    qualified_identifier: qualified_identifier.clone(),
                    dot: coloncolon.clone(),
                    identifier: identifier.clone(),
                },
            ),
        ),
        syntax::AtomicValueExpression::EmptyArray {
            lbracket: _lbracket,
            rbracket: _rbracket,
        } => Ok(Constituent::new(location, Value::Array(vec![]))),
        syntax::AtomicValueExpression::NonEmptyArray {
            lbracket: _lbracket,
            value_expressions,
            rbracket: _rbracket,
        } => Ok(Constituent::new(
            location,
            Value::Array(eval_value_expressions_array(
                quasienvironment,
                environment,
                value_expressions,
            )?),
        )),
        syntax::AtomicValueExpression::MapArray {
            lbracket: _lbracket,
            value_expression1,
            pipe: _pipe,
            value_parameter,
            in_: _in_,
            value_expression2,
            rbracket: _rbracket,
        } => {
            let value1 = eval_value_expression(quasienvironment, environment, value_expression2)?;

            if let Value::Array(array) = &*value1 {
                let mut result = vec![];

                for x in array {
                    let mut environment = environment.clone();

                    apply_value_parameter(
                        location,
                        quasienvironment,
                        &mut environment,
                        value_parameter,
                        x,
                    )?;

                    let value1 =
                        eval_value_expression(quasienvironment, &environment, value_expression1)?;

                    result.push(value1);
                }

                Ok(Constituent::new(location, Value::Array(result)))
            } else {
                Err(Error::Unsound { location })
            }
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
            let value1 = eval_value_expression(quasienvironment, environment, value_expression2)?;
            let value2 = eval_value_expression(quasienvironment, environment, value_expression3)?;

            if let Value::Int(n) = &*value1 {
                if let Value::Int(m) = &*value2 {
                    let mut result = vec![];

                    let mut i = n.clone();
                    let m = m.clone();

                    while i < m {
                        let mut environment = environment.clone();

                        apply_value_parameter(
                            location,
                            quasienvironment,
                            &mut environment,
                            value_parameter,
                            &Constituent::new(location, Value::Int(i.clone())),
                        )?;

                        let value1 = eval_value_expression(
                            quasienvironment,
                            &environment,
                            value_expression1,
                        )?;

                        result.push(value1);
                        i += num_bigint::BigInt::from(1u32);
                    }

                    Ok(Constituent::new(location, Value::Array(result)))
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::AtomicValueExpression::Tuple {
            lparen: _lparen,
            value_expressions,
            rparen: _rparen,
        } => eval_value_expressions(quasienvironment, environment, value_expressions),
    }
}

pub fn eval_applicative_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    applicative_value_expression: &Constituent<syntax::ApplicativeValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = applicative_value_expression.location();

    match &**applicative_value_expression {
        syntax::ApplicativeValueExpression::AtomicValueExpression {
            atomic_value_expression,
        } => eval_atomic_value_expression(quasienvironment, environment, atomic_value_expression),
        syntax::ApplicativeValueExpression::Apply {
            applicative_value_expression,
            atomic_value_expression,
        } => {
            let operation = eval_applicative_value_expression(
                quasienvironment,
                environment,
                applicative_value_expression,
            )?;
            let argument = eval_atomic_value_expression(
                quasienvironment,
                environment,
                atomic_value_expression,
            )?;
            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
    }
}

pub fn eval_value_member(
    quasienvironment: &Environment,
    environment: &Environment,
    value_member: &Constituent<syntax::ValueMember>,
) -> Result<std::collections::BTreeMap<String, Constituent<Value>>, Error> {
    let mut members = std::collections::BTreeMap::new();

    match &**value_member {
        syntax::ValueMember::Identifier { identifier } => {
            let value = eval_identifier(quasienvironment, environment, identifier)?;

            members.insert(String::from(&**identifier), value);

            Ok(members)
        }
        syntax::ValueMember::Coloneq {
            identifier,
            coloneq: _coloneq,
            value_expression,
        } => {
            let value = eval_value_expression(quasienvironment, environment, value_expression)?;

            members.insert(String::from(&**identifier), value);

            Ok(members)
        }
    }
}

pub fn eval_value_members(
    quasienvironment: &Environment,
    environment: &Environment,
    value_members: &Constituent<syntax::ValueMembers>,
) -> Result<std::collections::BTreeMap<String, Constituent<Value>>, Error> {
    match &**value_members {
        syntax::ValueMembers::One { value_member } => {
            eval_value_member(quasienvironment, environment, value_member)
        }
        syntax::ValueMembers::More {
            value_member,
            comma: _comma,
            value_members,
        } => {
            let mut members1 = eval_value_member(quasienvironment, environment, value_member)?;
            let mut members2 = eval_value_members(quasienvironment, environment, value_members)?;
            members1.append(&mut members2);
            Ok(members1)
        }
    }
}

pub fn eval_record_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    record_value_expression: &Constituent<syntax::RecordValueExpression>,
) -> Result<Constituent<Value>, Error> {
    match &**record_value_expression {
        syntax::RecordValueExpression::ApplicativeValueExpression {
            applicative_value_expression,
        } => eval_applicative_value_expression(
            quasienvironment,
            environment,
            applicative_value_expression,
        ),
        syntax::RecordValueExpression::EmptyRecord {
            qualified_identifier: _qualified_identifier,
            lbrace: _lbrace,
            rbrace: _rbrace,
        } => Ok(Constituent::new(
            record_value_expression.location(),
            Value::Record(std::collections::BTreeMap::new()),
        )),
        syntax::RecordValueExpression::NonEmptyRecord {
            qualified_identifier: _qualified_identifier,
            lbrace: _lbrace,
            value_members,
            rbrace: _rbrace,
        } => {
            let members = eval_value_members(quasienvironment, environment, value_members)?;

            Ok(Constituent::new(
                record_value_expression.location(),
                Value::Record(members),
            ))
        }
    }
}

pub fn eval_complementary_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    complementary_value_expression: &Constituent<syntax::ComplementaryValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = complementary_value_expression.location();

    match &**complementary_value_expression {
        syntax::ComplementaryValueExpression::RecordValueExpression {
            record_value_expression,
        } => eval_record_value_expression(quasienvironment, environment, record_value_expression),
        syntax::ComplementaryValueExpression::Exclamation {
            exclamation: _exclamation,
            complementary_value_expression,
        } => {
            let value = eval_complementary_value_expression(
                quasienvironment,
                environment,
                complementary_value_expression,
            )?;

            if let Value::Bool(b) = &*value {
                Ok(Constituent::new(location, Value::Bool(!b)))
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ComplementaryValueExpression::Amp {
            amp: _amp,
            complementary_value_expression,
        } => {
            let value = eval_complementary_value_expression(
                quasienvironment,
                environment,
                complementary_value_expression,
            )?;

            let r = std::rc::Rc::new(std::cell::RefCell::new(value));
            Ok(Constituent::new(location, Value::Reference(r)))
        }
        syntax::ComplementaryValueExpression::Asterisk {
            asterisk: _asterisk,
            complementary_value_expression,
        } => {
            let value = eval_complementary_value_expression(
                quasienvironment,
                environment,
                complementary_value_expression,
            )?;

            if let Value::Reference(r) = &*value {
                Ok(r.as_ref().borrow().clone())
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn eval_multiplicative_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    multiplicative_value_expression: &Constituent<syntax::MultiplicativeValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = multiplicative_value_expression.location();

    match &**multiplicative_value_expression {
        syntax::MultiplicativeValueExpression::ComplementaryValueExpression {
            complementary_value_expression,
        } => eval_complementary_value_expression(
            quasienvironment,
            environment,
            complementary_value_expression,
        ),
        syntax::MultiplicativeValueExpression::Asterisk {
            multiplicative_value_expression,
            asterisk,
            complementary_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, asterisk)?;
            let lhs = eval_multiplicative_value_expression(
                quasienvironment,
                environment,
                multiplicative_value_expression,
            )?;
            let rhs = eval_complementary_value_expression(
                quasienvironment,
                environment,
                complementary_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::MultiplicativeValueExpression::Slash {
            multiplicative_value_expression,
            slash,
            complementary_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, slash)?;
            let lhs = eval_multiplicative_value_expression(
                quasienvironment,
                environment,
                multiplicative_value_expression,
            )?;
            let rhs = eval_complementary_value_expression(
                quasienvironment,
                environment,
                complementary_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::MultiplicativeValueExpression::Percent {
            multiplicative_value_expression,
            percent,
            complementary_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, percent)?;
            let lhs = eval_multiplicative_value_expression(
                quasienvironment,
                environment,
                multiplicative_value_expression,
            )?;
            let rhs = eval_complementary_value_expression(
                quasienvironment,
                environment,
                complementary_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
    }
}

pub fn eval_additive_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    additive_value_expression: &Constituent<syntax::AdditiveValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = additive_value_expression.location();

    match &**additive_value_expression {
        syntax::AdditiveValueExpression::MultiplicativeValueExpression {
            multiplicative_value_expression,
        } => eval_multiplicative_value_expression(
            quasienvironment,
            environment,
            multiplicative_value_expression,
        ),
        syntax::AdditiveValueExpression::Plus {
            additive_value_expression,
            plus,
            multiplicative_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, plus)?;
            let lhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let rhs = eval_multiplicative_value_expression(
                quasienvironment,
                environment,
                multiplicative_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::AdditiveValueExpression::Minus {
            additive_value_expression,
            minus,
            multiplicative_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, minus)?;
            let lhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let rhs = eval_multiplicative_value_expression(
                quasienvironment,
                environment,
                multiplicative_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
    }
}

pub fn eval_comparative_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    comparative_value_expression: &Constituent<syntax::ComparativeValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = comparative_value_expression.location();

    match &**comparative_value_expression {
        syntax::ComparativeValueExpression::AdditiveValueExpression {
            additive_value_expression,
        } => {
            eval_additive_value_expression(quasienvironment, environment, additive_value_expression)
        }
        syntax::ComparativeValueExpression::Eq {
            comparative_value_expression,
            eq,
            additive_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, eq)?;
            let lhs = eval_comparative_value_expression(
                quasienvironment,
                environment,
                comparative_value_expression,
            )?;
            let rhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::ComparativeValueExpression::Noteq {
            comparative_value_expression,
            noteq,
            additive_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, noteq)?;
            let lhs = eval_comparative_value_expression(
                quasienvironment,
                environment,
                comparative_value_expression,
            )?;
            let rhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::ComparativeValueExpression::Lte {
            comparative_value_expression,
            lte,
            additive_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, lte)?;
            let lhs = eval_comparative_value_expression(
                quasienvironment,
                environment,
                comparative_value_expression,
            )?;
            let rhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::ComparativeValueExpression::Lt {
            comparative_value_expression,
            lt,
            additive_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, lt)?;
            let lhs = eval_comparative_value_expression(
                quasienvironment,
                environment,
                comparative_value_expression,
            )?;
            let rhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::ComparativeValueExpression::Gte {
            comparative_value_expression,
            gte,
            additive_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, gte)?;
            let lhs = eval_comparative_value_expression(
                quasienvironment,
                environment,
                comparative_value_expression,
            )?;
            let rhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
        syntax::ComparativeValueExpression::Gt {
            comparative_value_expression,
            gt,
            additive_value_expression,
        } => {
            let operation = eval_identifier(quasienvironment, environment, gt)?;
            let lhs = eval_comparative_value_expression(
                quasienvironment,
                environment,
                comparative_value_expression,
            )?;
            let rhs = eval_additive_value_expression(
                quasienvironment,
                environment,
                additive_value_expression,
            )?;
            let argument = Constituent::new(location, Value::Pair(lhs, rhs));

            apply(
                location,
                quasienvironment,
                environment,
                &operation,
                &argument,
            )
        }
    }
}

pub fn eval_conjunctive_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    conjunctive_value_expression: &Constituent<syntax::ConjunctiveValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = conjunctive_value_expression.location();

    match &**conjunctive_value_expression {
        syntax::ConjunctiveValueExpression::ComparativeValueExpression {
            comparative_value_expression,
        } => eval_comparative_value_expression(
            quasienvironment,
            environment,
            comparative_value_expression,
        ),
        syntax::ConjunctiveValueExpression::And {
            conjunctive_value_expression,
            and: _and,
            comparative_value_expression,
        } => {
            let value = eval_conjunctive_value_expression(
                quasienvironment,
                environment,
                conjunctive_value_expression,
            )?;

            if let Value::Bool(b) = &*value {
                if *b {
                    eval_comparative_value_expression(
                        quasienvironment,
                        environment,
                        comparative_value_expression,
                    )
                } else {
                    Ok(Constituent::new(location, Value::Bool(false)))
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn eval_disjunctive_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    disjunctive_value_expression: &Constituent<syntax::DisjunctiveValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = disjunctive_value_expression.location();

    match &**disjunctive_value_expression {
        syntax::DisjunctiveValueExpression::ConjunctiveValueExpression {
            conjunctive_value_expression,
        } => eval_conjunctive_value_expression(
            quasienvironment,
            environment,
            conjunctive_value_expression,
        ),
        syntax::DisjunctiveValueExpression::Or {
            disjunctive_value_expression,
            or: _or,
            conjunctive_value_expression,
        } => {
            let value = eval_disjunctive_value_expression(
                quasienvironment,
                environment,
                disjunctive_value_expression,
            )?;

            if let Value::Bool(b) = &*value {
                if *b {
                    Ok(Constituent::new(location, Value::Bool(true)))
                } else {
                    eval_conjunctive_value_expression(
                        quasienvironment,
                        environment,
                        conjunctive_value_expression,
                    )
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn eval_assignment_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    assignment_value_expression: &Constituent<syntax::AssignmentValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = assignment_value_expression.location();

    match &**assignment_value_expression {
        syntax::AssignmentValueExpression::DisjunctiveValueExpression {
            disjunctive_value_expression,
        } => eval_disjunctive_value_expression(
            quasienvironment,
            environment,
            disjunctive_value_expression,
        ),
        syntax::AssignmentValueExpression::Coloneq {
            disjunctive_value_expression,
            coloneq: _coloneq,
            assignment_value_expression,
        } => {
            let r = eval_disjunctive_value_expression(
                quasienvironment,
                environment,
                disjunctive_value_expression,
            )?;

            let value = eval_assignment_value_expression(
                quasienvironment,
                environment,
                assignment_value_expression,
            )?;

            if let Value::Reference(r) = &*r {
                *r.as_ref().borrow_mut() = value;
                Ok(Constituent::new(location, Value::Bool(true)))
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn eval_value_implication(
    quasienvironment: &Environment,
    environment: &Environment,
    value_implication: &Constituent<syntax::ValueImplication>,
    label: &str,
    value: &Option<Constituent<Value>>,
) -> Result<Option<Constituent<Value>>, Error> {
    let location = value_implication.location();

    match &**value_implication {
        syntax::ValueImplication::Variant {
            identifier,
            darrow: _darrow,
            value_expression,
        } => {
            if String::from(label) == String::from(&**identifier) {
                if let None = value {
                    Ok(Some(eval_value_expression(
                        quasienvironment,
                        environment,
                        value_expression,
                    )?))
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Ok(None)
            }
        }
        syntax::ValueImplication::VariantOf {
            identifier,
            atomic_value_parameter,
            darrow: _darrow,
            value_expression,
        } => {
            if String::from(label) == String::from(&**identifier) {
                if let Some(value) = value {
                    let mut environment = environment.clone();

                    apply_atomic_value_parameter(
                        location,
                        quasienvironment,
                        &mut environment,
                        atomic_value_parameter,
                        value,
                    )?;

                    Ok(Some(eval_value_expression(
                        quasienvironment,
                        &environment,
                        value_expression,
                    )?))
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Ok(None)
            }
        }
    }
}

pub fn eval_value_implications(
    quasienvironment: &Environment,
    environment: &Environment,
    value_implications: &Constituent<syntax::ValueImplications>,
    label: &str,
    value: &Option<Constituent<Value>>,
) -> Result<Constituent<Value>, Error> {
    let location = value_implications.location();

    match &**value_implications {
        syntax::ValueImplications::One { value_implication } => {
            if let Some(value) = eval_value_implication(
                quasienvironment,
                environment,
                value_implication,
                label,
                value,
            )? {
                Ok(value)
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ValueImplications::More {
            value_implication,
            pipe: _pipe,
            value_implications,
        } => {
            if let Some(value) = eval_value_implication(
                quasienvironment,
                environment,
                value_implication,
                label,
                value,
            )? {
                Ok(value)
            } else {
                eval_value_implications(
                    quasienvironment,
                    environment,
                    value_implications,
                    label,
                    value,
                )
            }
        }
    }
}

pub fn eval_control_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    control_value_expression: &Constituent<syntax::ControlValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = control_value_expression.location();

    match &**control_value_expression {
        syntax::ControlValueExpression::AssignmentValueExpression {
            assignment_value_expression,
        } => eval_assignment_value_expression(
            quasienvironment,
            environment,
            assignment_value_expression,
        ),
        syntax::ControlValueExpression::EmptyMatch {
            qualified_identifier: _qualified_identifier,
            dot: _dot,
            match_: _match_,
            value_expression,
            with: _with,
            end: _end,
        } => {
            let _value = eval_value_expression(quasienvironment, environment, value_expression)?;

            Err(Error::Unsound { location })
        }
        syntax::ControlValueExpression::NonEmptyMatch {
            qualified_identifier: _qualified_identifier,
            dot: _dot,
            match_: _match_,
            value_expression,
            with: _with,
            value_implications,
            end: _end,
        } => {
            let value = eval_value_expression(quasienvironment, environment, value_expression)?;

            if let Value::Variant(label, value) = &*value {
                eval_value_implications(
                    quasienvironment,
                    environment,
                    value_implications,
                    &label,
                    value,
                )
            } else {
                Err(Error::Unsound { location })
            }
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
            let value1 = eval_value_expression(quasienvironment, environment, value_expression1)?;

            if let Value::Array(array) = &*value1 {
                let mut result = true;

                for x in array {
                    let mut environment = environment.clone();

                    apply_value_parameter(
                        location,
                        quasienvironment,
                        &mut environment,
                        value_parameter,
                        x,
                    )?;

                    let value1 =
                        eval_value_expression(quasienvironment, &environment, value_expression2)?;

                    if let Value::Bool(b) = &*value1 {
                        if !*b {
                            result = false;
                            break;
                        }
                    } else {
                        return Err(Error::Unsound { location });
                    }
                }

                Ok(Constituent::new(location, Value::Bool(result)))
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ControlValueExpression::ForArrayWhere {
            for_: _for_,
            value_parameter1,
            in_: _in_,
            value_expression1,
            where_: _where_,
            value_parameter2,
            coloneq: _coloneq,
            value_expression2,
            do_: _do_,
            value_expression3,
            end: _end,
        } => {
            let value1 = eval_value_expression(quasienvironment, environment, value_expression1)?;
            let value2 = eval_value_expression(quasienvironment, environment, value_expression2)?;

            if let Value::Array(array) = &*value1 {
                let mut result = value2;

                for x in array {
                    let mut environment = environment.clone();

                    apply_value_parameter(
                        location,
                        quasienvironment,
                        &mut environment,
                        value_parameter1,
                        x,
                    )?;

                    apply_value_parameter(
                        location,
                        quasienvironment,
                        &mut environment,
                        value_parameter2,
                        &result,
                    )?;

                    let value3 =
                        eval_value_expression(quasienvironment, &environment, value_expression3)?;

                    result = value3;
                }

                Ok(result)
            } else {
                Err(Error::Unsound { location })
            }
        }
        syntax::ControlValueExpression::ForRangeWhere {
            for_: _for_,
            value_parameter1,
            in_: _in_,
            value_expression1,
            dotdot: _dotdot,
            value_expression2,
            where_: _where_,
            value_parameter2,
            coloneq: _coloneq,
            value_expression3,
            do_: _do_,
            value_expression4,
            end: _end,
        } => {
            let value1 = eval_value_expression(quasienvironment, environment, value_expression1)?;
            let value2 = eval_value_expression(quasienvironment, environment, value_expression2)?;
            let value3 = eval_value_expression(quasienvironment, environment, value_expression3)?;

            if let Value::Int(n) = &*value1 {
                if let Value::Int(m) = &*value2 {
                    let mut result = value3;

                    let mut i = n.clone();
                    let m = m.clone();

                    while i < m {
                        let mut environment = environment.clone();

                        apply_value_parameter(
                            location,
                            quasienvironment,
                            &mut environment,
                            value_parameter1,
                            &Constituent::new(location, Value::Int(i.clone())),
                        )?;

                        apply_value_parameter(
                            location,
                            quasienvironment,
                            &mut environment,
                            value_parameter2,
                            &result,
                        )?;

                        let value4 = eval_value_expression(
                            quasienvironment,
                            &environment,
                            value_expression4,
                        )?;

                        result = value4;

                        i += num_bigint::BigInt::from(1u32);
                    }

                    Ok(result)
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Err(Error::Unsound { location })
            }
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
            let value1 = eval_value_expression(quasienvironment, environment, value_expression1)?;
            let value2 = eval_value_expression(quasienvironment, environment, value_expression2)?;

            if let Value::Int(n) = &*value1 {
                if let Value::Int(m) = &*value2 {
                    let mut result = true;

                    let mut i = n.clone();
                    let m = m.clone();

                    while i < m {
                        let mut environment = environment.clone();

                        apply_value_parameter(
                            location,
                            quasienvironment,
                            &mut environment,
                            value_parameter,
                            &Constituent::new(location, Value::Int(i.clone())),
                        )?;

                        let value1 = eval_value_expression(
                            quasienvironment,
                            &environment,
                            value_expression3,
                        )?;

                        if let Value::Bool(b) = &*value1 {
                            if !*b {
                                result = false;
                                break;
                            }
                        } else {
                            return Err(Error::Unsound { location });
                        }

                        i += num_bigint::BigInt::from(1u32);
                    }

                    Ok(Constituent::new(location, Value::Bool(result)))
                } else {
                    Err(Error::Unsound { location })
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn eval_value_expression(
    quasienvironment: &Environment,
    environment: &Environment,
    value_expression: &Constituent<syntax::ValueExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = value_expression.location();

    match &**value_expression {
        syntax::ValueExpression::ControlValueExpression {
            control_value_expression,
        } => eval_control_value_expression(quasienvironment, environment, control_value_expression),
        syntax::ValueExpression::Semicolon {
            control_value_expression,
            semicolon: _semicolon,
            value_expression,
        } => {
            let _value1 = eval_control_value_expression(
                quasienvironment,
                environment,
                control_value_expression,
            )?;
            eval_value_expression(quasienvironment, environment, value_expression)
        }
        syntax::ValueExpression::Abstraction {
            hat: _hat,
            atomic_value_parameter,
            dot: _dot,
            value_expression,
        } => Ok(Constituent::new(
            location,
            Value::Abstraction(
                environment.clone(),
                atomic_value_parameter.clone(),
                value_expression.clone(),
            ),
        )),
        syntax::ValueExpression::Let {
            let_: _let_,
            identifier,
            coloneq: _coloneq,
            value_expression1,
            in_: _in_,
            value_expression2,
        } => {
            let value1 = eval_value_expression(quasienvironment, environment, value_expression1)?;

            let mut environment = environment.clone();
            environment.insert(String::from(&**identifier), value1);

            let value2 = eval_value_expression(quasienvironment, &environment, value_expression2)?;

            Ok(value2)
        }
        syntax::ValueExpression::If {
            if_: _if_,
            value_expression1,
            then: _then,
            value_expression2,
            else_: _else_,
            value_expression3,
        } => {
            let value1 = eval_value_expression(quasienvironment, environment, value_expression1)?;

            if let Value::Bool(b) = &*value1 {
                if *b {
                    eval_value_expression(quasienvironment, &environment, value_expression2)
                } else {
                    eval_value_expression(quasienvironment, &environment, value_expression3)
                }
            } else {
                Err(Error::Unsound { location })
            }
        }
    }
}

pub fn eval_value_expressions_array(
    quasienvironment: &Environment,
    environment: &Environment,
    value_expressions: &Constituent<syntax::ValueExpressions>,
) -> Result<Vec<Constituent<Value>>, Error> {
    match &**value_expressions {
        syntax::ValueExpressions::One { value_expression } => {
            let value = eval_value_expression(quasienvironment, environment, value_expression)?;
            Ok(vec![value])
        }
        syntax::ValueExpressions::More {
            value_expression,
            comma: _comma,
            value_expressions,
        } => {
            let value = eval_value_expression(quasienvironment, environment, value_expression)?;
            let values =
                eval_value_expressions_array(quasienvironment, environment, value_expressions)?;
            Ok(vec![vec![value], values].concat())
        }
    }
}

pub fn eval_value_expressions(
    quasienvironment: &Environment,
    environment: &Environment,
    value_expressions: &Constituent<syntax::ValueExpressions>,
) -> Result<Constituent<Value>, Error> {
    let location = value_expressions.location();

    match &**value_expressions {
        syntax::ValueExpressions::One { value_expression } => {
            eval_value_expression(quasienvironment, environment, value_expression)
        }
        syntax::ValueExpressions::More {
            value_expression,
            comma: _comma,
            value_expressions,
        } => {
            let value1 = eval_value_expression(quasienvironment, environment, value_expression)?;
            let value2 = eval_value_expressions(quasienvironment, environment, value_expressions)?;
            Ok(Constituent::new(location, Value::Pair(value1, value2)))
        }
    }
}

pub fn eval_type_member(
    _environment: &Environment,
    type_member: &Constituent<syntax::TypeMember>,
) -> Result<Environment, Error> {
    let mut exports = Environment::new();

    match &**type_member {
        syntax::TypeMember::Required {
            identifier,
            colon: _colon,
            type_expression: _type_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::RecordProj(String::from(&**identifier)),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeMember::Optional {
            identifier,
            question: _question,
            colon: _colon,
            type_expression: _type_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::RecordProjDefault(String::from(&**identifier)),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
    }
}

pub fn eval_type_members(
    environment: &Environment,
    type_members: &Constituent<syntax::TypeMembers>,
) -> Result<Environment, Error> {
    match &**type_members {
        syntax::TypeMembers::One { type_member } => eval_type_member(environment, type_member),
        syntax::TypeMembers::More {
            type_member,
            comma: _comma,
            type_members,
        } => {
            let mut exports1 = eval_type_member(environment, type_member)?;
            let mut exports2 = eval_type_members(environment, type_members)?;
            exports1.append(&mut exports2);
            Ok(exports1)
        }
    }
}

pub fn eval_type_variant(
    _environment: &Environment,
    type_variant: &Constituent<syntax::TypeVariant>,
) -> Result<Environment, Error> {
    let mut exports = Environment::new();

    match &**type_variant {
        syntax::TypeVariant::Identifier { identifier } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Variant(String::from(&**identifier), None),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeVariant::Of {
            identifier,
            of: _of,
            type_expression: _type_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::VariantInj(String::from(&**identifier)),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
    }
}

pub fn eval_type_variants(
    environment: &Environment,
    type_variants: &Constituent<syntax::TypeVariants>,
) -> Result<Environment, Error> {
    match &**type_variants {
        syntax::TypeVariants::One { type_variant } => eval_type_variant(environment, type_variant),
        syntax::TypeVariants::More {
            type_variant,
            pipe: _pipe,
            type_variants,
        } => {
            let mut exports1 = eval_type_variant(environment, type_variant)?;
            let mut exports2 = eval_type_variants(environment, type_variants)?;
            exports1.append(&mut exports2);
            Ok(exports1)
        }
    }
}

pub fn eval_record_expression(
    environment: &Environment,
    record_expression: &Constituent<syntax::RecordExpression>,
) -> Result<Environment, Error> {
    match &**record_expression {
        syntax::RecordExpression::Empty {
            lbrace: _lbrace,
            rbrace: _rbrace,
        } => Ok(Environment::new()),
        syntax::RecordExpression::NonEmpty {
            lbrace: _lbrace,
            type_members,
            rbrace: _rbracket,
        } => eval_type_members(environment, type_members),
    }
}

pub fn eval_variant_expression(
    environment: &Environment,
    variant_expression: &Constituent<syntax::VariantExpression>,
) -> Result<Environment, Error> {
    match &**variant_expression {
        syntax::VariantExpression::Empty {
            lbracket: _lbracket,
            rbracket: _rbracket,
        } => Ok(Environment::new()),
        syntax::VariantExpression::NonEmpty {
            lbracket: _lbracket,
            type_variants,
            rbracket: _rbracket,
        } => eval_type_variants(environment, type_variants),
    }
}

pub fn eval_structure_expression(
    environment: &Environment,
    structure_expression: &Constituent<syntax::StructureExpression>,
) -> Result<Environment, Error> {
    match &**structure_expression {
        syntax::StructureExpression::RecordExpression { record_expression } => {
            eval_record_expression(environment, record_expression)
        }
        syntax::StructureExpression::VariantExpression { variant_expression } => {
            eval_variant_expression(environment, variant_expression)
        }
    }
}

pub fn eval_type_definition(
    environment: &Environment,
    type_definition: &Constituent<syntax::TypeDefinition>,
) -> Result<Environment, Error> {
    let mut exports = Environment::new();

    match &**type_definition {
        syntax::TypeDefinition::Struct {
            struct_: _struct_,
            identifier,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::StructColon {
            struct_: _struct_,
            identifier,
            colon: _colon,
            kind_expression: _kind_expression,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::StructFn {
            struct_: _struct_,
            identifier,
            lparen: _lparen,
            type_parameters: _type_parameters,
            rparen: _rparen,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::StructFnColon {
            struct_: _struct_,
            identifier,
            lparen: _lparen,
            type_parameters: _type_parameters,
            rparen: _rparen,
            colon: _colon,
            kind_expression: _kind_expression,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::Inductive {
            inductive: _inductive,
            identifier,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::InductiveColon {
            inductive: _inductive,
            identifier,
            colon: _colon,
            kind_expression: _kind_expression,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::InductiveFn {
            inductive: _inductive,
            identifier,
            lparen: _lparen,
            type_parameters: _type_parameters,
            rparen: _rparen,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::TypeDefinition::InductiveFnColon {
            inductive: _inductive,
            identifier,
            lparen: _lparen,
            type_parameters: _type_parameters,
            rparen: _rparen,
            colon: _colon,
            kind_expression: _kind_expression,
            coloneq: _coloneq,
            structure_expression,
        } => {
            let value = Constituent::new(
                identifier.location(),
                Value::Family(eval_structure_expression(
                    environment,
                    structure_expression,
                )?),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        _ => Ok(exports),
    }
}

pub fn eval_value_definition(
    environment: &Environment,
    family_value_parameters: &[Constituent<syntax::ValueParameters>],
    value_definition: &Constituent<syntax::ValueDefinition>,
) -> Result<Environment, Error> {
    let mut exports = Environment::new();

    match &**value_definition {
        syntax::ValueDefinition::Def {
            def: _def,
            identifier,
            coloneq: _coloneq,
            value_expression,
        } => {
            let value = if family_value_parameters.len() == 0 {
                eval_value_expression(&Environment::new(), environment, value_expression)?
            } else {
                Constituent::new(
                    identifier.location(),
                    Value::Lazy(environment.clone(), value_expression.clone()),
                )
            };

            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefColon {
            def: _def,
            identifier,
            colon: _colon,
            type_expression: _type_expression,
            coloneq: _coloneq,
            value_expression,
        } => {
            let value = if family_value_parameters.len() == 0 {
                eval_value_expression(&Environment::new(), environment, value_expression)?
            } else {
                Constituent::new(
                    identifier.location(),
                    Value::Lazy(environment.clone(), value_expression.clone()),
                )
            };

            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefFn {
            def: _def,
            identifier,
            lparen,
            value_parameters,
            rparen,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                identifier.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: lparen.clone(),
                    value_parameters: value_parameters.clone(),
                    rparen: rparen.clone(),
                },
            );

            let value = Constituent::new(
                identifier.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefFnColon {
            def: _def,
            identifier,
            lparen,
            value_parameters,
            rparen,
            colon: _colon,
            type_expression: _type_expression,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                identifier.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: lparen.clone(),
                    value_parameters: value_parameters.clone(),
                    rparen: rparen.clone(),
                },
            );

            let value = Constituent::new(
                identifier.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefPoly {
            def: _def,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            coloneq: _coloneq,
            value_expression,
        } => {
            let value = if family_value_parameters.len() == 0 {
                eval_value_expression(&Environment::new(), environment, value_expression)?
            } else {
                Constituent::new(
                    identifier.location(),
                    Value::Lazy(environment.clone(), value_expression.clone()),
                )
            };

            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefPolyColon {
            def: _def,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            colon: _colon,
            type_expression: _type_expression,
            coloneq: _coloneq,
            value_expression,
        } => {
            let value = if family_value_parameters.len() == 0 {
                eval_value_expression(&Environment::new(), environment, value_expression)?
            } else {
                Constituent::new(
                    identifier.location(),
                    Value::Lazy(environment.clone(), value_expression.clone()),
                )
            };

            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefPolyFn {
            def: _def,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            lparen,
            value_parameters,
            rparen,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                identifier.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: lparen.clone(),
                    value_parameters: value_parameters.clone(),
                    rparen: rparen.clone(),
                },
            );

            let value = Constituent::new(
                identifier.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefPolyFnColon {
            def: _def,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            lparen,
            value_parameters,
            rparen,
            colon: _colon,
            type_expression: _type_expression,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                identifier.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: lparen.clone(),
                    value_parameters: value_parameters.clone(),
                    rparen: rparen.clone(),
                },
            );

            let value = Constituent::new(
                identifier.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefEq {
            def: _def,
            identifier1,
            eq,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                eq.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(eq.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(eq.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(eq.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                eq.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**eq), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefNoteq {
            def: _def,
            identifier1,
            noteq,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                noteq.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(noteq.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(noteq.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(noteq.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                noteq.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**noteq), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefLte {
            def: _def,
            identifier1,
            lte,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                lte.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(lte.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(lte.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(lte.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                lte.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**lte), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefLt {
            def: _def,
            identifier1,
            lt,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                lt.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(lt.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(lt.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(lt.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                lt.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**lt), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefGte {
            def: _def,
            identifier1,
            gte,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                gte.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(gte.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(gte.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(gte.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                gte.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**gte), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefGt {
            def: _def,
            identifier1,
            gt,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                gt.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(gt.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(gt.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(gt.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                gt.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**gt), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefPlus {
            def: _def,
            identifier1,
            plus,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                plus.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(plus.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(plus.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(plus.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                plus.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**plus), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefMinus {
            def: _def,
            identifier1,
            minus,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                minus.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(minus.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(minus.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(minus.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                minus.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**minus), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefAsterisk {
            def: _def,
            identifier1,
            asterisk,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                asterisk.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(asterisk.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(asterisk.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(asterisk.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                asterisk.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**asterisk), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefSlash {
            def: _def,
            identifier1,
            slash,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                slash.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(slash.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(slash.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(slash.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                slash.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**slash), value);
            Ok(exports)
        }
        syntax::ValueDefinition::DefPercent {
            def: _def,
            identifier1,
            percent,
            identifier2,
            coloneq: _coloneq,
            value_expression,
        } => {
            let atomic_value_parameter = Constituent::new(
                percent.location(),
                syntax::AtomicValueParameter::Tuple {
                    lparen: Lexeme::new(percent.location(), String::from("(")),
                    value_parameters: Constituent::new(
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
                            comma: Lexeme::new(percent.location(), String::from(",")),
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
                    ),
                    rparen: Lexeme::new(percent.location(), String::from(")")),
                },
            );

            let value = Constituent::new(
                percent.location(),
                Value::Abstraction(
                    environment.clone(),
                    atomic_value_parameter.clone(),
                    value_expression.clone(),
                ),
            );
            exports.insert(String::from(&**percent), value);
            Ok(exports)
        }
    }
}

pub fn eval_family_expression(
    environment: &Environment,
    family_value_parameters: &[Constituent<syntax::ValueParameters>],
    family_expression: &Constituent<syntax::FamilyExpression>,
) -> Result<Constituent<Value>, Error> {
    let location = family_expression.location();

    match &**family_expression {
        syntax::FamilyExpression::Family {
            family: _family,
            definitions,
            end: _end,
        } => {
            let exports = eval_definitions(environment, family_value_parameters, definitions)?;

            let exports = if family_value_parameters.len() == 0 {
                exports
            } else {
                exports
                    .into_iter()
                    .map(|(id, value)| {
                        if let Value::Family(_) = &*value {
                            (id, value)
                        } else {
                            (
                                id,
                                Constituent::new(
                                    family_expression.location(),
                                    Value::Quasi(Vec::from(family_value_parameters.clone()), value),
                                ),
                            )
                        }
                    })
                    .collect()
            };

            Ok(Constituent::new(location, Value::Family(exports)))
        }
    }
}

pub fn eval_family_definition(
    environment: &Environment,
    family_value_parameters: &[Constituent<syntax::ValueParameters>],
    family_definition: &Constituent<syntax::FamilyDefinition>,
) -> Result<Environment, Error> {
    let mut exports = Environment::new();

    match &**family_definition {
        syntax::FamilyDefinition::Family {
            family: _family,
            identifier,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value =
                eval_family_expression(environment, family_value_parameters, family_expression)?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::FamilySubtype {
            family: _family,
            identifier,
            subtype: _subtype,
            type_expression: _type_expression,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value =
                eval_family_expression(environment, family_value_parameters, family_expression)?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::FamilyFn {
            family: _family,
            identifier,
            lparen: _lparen,
            value_parameters,
            rparen: _rparen,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value = eval_family_expression(
                environment,
                &[family_value_parameters, &[value_parameters.clone()]].concat(),
                family_expression,
            )?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::FamilyFnSubtype {
            family: _family,
            identifier,
            lparen: _lparen,
            value_parameters,
            rparen: _rparen,
            subtype: _subtype,
            type_expression: _type_expression,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value = eval_family_expression(
                environment,
                &[family_value_parameters, &[value_parameters.clone()]].concat(),
                family_expression,
            )?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::PolyFamily {
            family: _family,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value =
                eval_family_expression(environment, family_value_parameters, family_expression)?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::PolyFamilySubtype {
            family: _family,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            subtype: _subtype,
            type_expression: _type_expression,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value =
                eval_family_expression(environment, family_value_parameters, family_expression)?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::PolyFamilyFn {
            family: _family,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            lparen: _lparen,
            value_parameters,
            rparen: _rparen,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value = eval_family_expression(
                environment,
                &[family_value_parameters, &[value_parameters.clone()]].concat(),
                family_expression,
            )?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
        syntax::FamilyDefinition::PolyFamilyFnSubtype {
            family: _family,
            identifier,
            lbracket: _lbracket,
            type_parameters: _type_parameters,
            rbracket: _rbracket,
            lparen: _lparen,
            value_parameters,
            rparen: _rparen,
            subtype: _subtype,
            type_expression: _type_expression,
            coloneq: _coloneq,
            family_expression,
        } => {
            let value = eval_family_expression(
                environment,
                &[family_value_parameters, &[value_parameters.clone()]].concat(),
                family_expression,
            )?;
            exports.insert(String::from(&**identifier), value);
            Ok(exports)
        }
    }
}

pub fn eval_definition(
    environment: &Environment,
    family_value_parameters: &[Constituent<syntax::ValueParameters>],
    definition: &Constituent<syntax::Definition>,
) -> Result<Environment, Error> {
    match &**definition {
        syntax::Definition::TypeDefinition { type_definition } => {
            eval_type_definition(environment, type_definition)
        }
        syntax::Definition::ValueDefinition { value_definition } => {
            eval_value_definition(environment, family_value_parameters, value_definition)
        }
        syntax::Definition::FamilyDefinition { family_definition } => {
            eval_family_definition(environment, family_value_parameters, family_definition)
        }
        syntax::Definition::TypeDefinitionWhere {
            type_definition,
            where_: _where_,
            definition,
        } => {
            let mut exports1 = eval_definition(environment, family_value_parameters, definition)?;
            let mut exports2 = eval_type_definition(environment, type_definition)?;
            exports1.append(&mut exports2);
            Ok(exports1)
        }
        syntax::Definition::ValueDefinitionWhere {
            value_definition,
            where_: _where_,
            definition,
        } => {
            let mut exports1 = eval_definition(environment, family_value_parameters, definition)?;
            let mut exports2 =
                eval_value_definition(environment, family_value_parameters, value_definition)?;
            exports1.append(&mut exports2);
            Ok(exports1)
        }
        syntax::Definition::FamilyDefinitionWhere {
            family_definition,
            where_: _where_,
            definition,
        } => {
            let mut exports1 = eval_definition(environment, family_value_parameters, definition)?;
            let mut exports2 =
                eval_family_definition(environment, family_value_parameters, family_definition)?;
            exports1.append(&mut exports2);
            Ok(exports1)
        }
    }
}

pub fn eval_definitions(
    environment: &Environment,
    family_value_parameters: &[Constituent<syntax::ValueParameters>],
    definitions: &Constituent<syntax::Definitions>,
) -> Result<Environment, Error> {
    match &**definitions {
        syntax::Definitions::One { definition } => {
            eval_definition(environment, family_value_parameters, definition)
        }
        syntax::Definitions::More {
            definition,
            definitions,
        } => {
            let mut exports1 = eval_definition(environment, family_value_parameters, definition)?;
            let mut environment = environment.clone();
            environment.append(&mut exports1.clone());
            let mut exports2 =
                eval_definitions(&environment, family_value_parameters, definitions)?;
            exports1.append(&mut exports2);
            Ok(exports1)
        }
    }
}

pub fn eval_program(
    environment: &Environment,
    program: &Constituent<syntax::Program>,
) -> Result<Environment, Error> {
    match &**program {
        syntax::Program::Definitions { definitions } => {
            eval_definitions(environment, &[], definitions)
        }
    }
}
