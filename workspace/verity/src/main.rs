fn int_eq(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x == y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_noteq(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x != y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_lte(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x <= y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_lt(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x < y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_gte(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x >= y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_gt(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x > y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_plus(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Int(x + y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_minus(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Int(x - y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_mult(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Int(x * y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_div(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Int(x / y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn int_mod(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Int(x),
                verity_definition::dynamic_semantics::Value::Int(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Int(x % y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_eq(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x == y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_noteq(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x != y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_lte(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x <= y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_lt(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x < y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_gte(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x >= y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_gt(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(x > y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_plus(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Rational(x + y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_minus(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Rational(x - y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_mult(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Rational(x * y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_div(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Rational(x / y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn rational_mod(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::Pair(x, y) => match (&**x, &**y) {
            (
                verity_definition::dynamic_semantics::Value::Rational(x),
                verity_definition::dynamic_semantics::Value::Rational(y),
            ) => Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Rational(x % y),
            )),
            _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
                location: parameter.location(),
            }),
        },
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn stdout_puts(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::String(x) => {
            println!("{}", x);

            Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(true),
            ))
        }
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn stderr_puts(
    _environment: &verity_definition::dynamic_semantics::Environment,
    parameter: &verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
) -> Result<
    verity_definition::common::Constituent<verity_definition::dynamic_semantics::Value>,
    verity_definition::dynamic_semantics::Error,
> {
    match &**parameter {
        verity_definition::dynamic_semantics::Value::String(x) => {
            eprintln!("{}", x);

            Ok(verity_definition::common::Constituent::new(
                parameter.location(),
                verity_definition::dynamic_semantics::Value::Bool(true),
            ))
        }
        _ => Err(verity_definition::dynamic_semantics::Error::Unsound {
            location: parameter.location(),
        }),
    }
}

fn prim_environment() -> verity::static_semantics::FamilyValue {
    let mut prim = verity::static_semantics::FamilyValue {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    let mut int = verity::static_semantics::FamilyValue {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    let mut rational = verity::static_semantics::FamilyValue {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    let mut stdout = verity::static_semantics::FamilyValue {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    let mut stderr = verity::static_semantics::FamilyValue {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    int.value_env.insert(
        String::from("=="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("!="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("<="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("<"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from(">="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from(">"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("+"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Int,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("-"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Int,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("*"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Int,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("/"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Int,
                ),
            ),
        ),
    );

    int.value_env.insert(
        String::from("%"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Int,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Int,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("=="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("!="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("<="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("<"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from(">="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from(">"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Bool,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("+"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Rational,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("-"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Rational,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("*"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Rational,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("/"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Rational,
                ),
            ),
        ),
    );

    rational.value_env.insert(
        String::from("%"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Product(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Rational,
                        ),
                    ),
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Rational,
                ),
            ),
        ),
    );

    stdout.value_env.insert(
        String::from("puts"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::String,
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Effect(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Bool,
                        ),
                    ),
                ),
            ),
        ),
    );

    stderr.value_env.insert(
        String::from("puts"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::static_semantics::TypeValue::Arrow(
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::String,
                ),
                verity_definition::common::Constituent::new(
                    (0, 0),
                    verity_definition::static_semantics::TypeValue::Effect(
                        verity_definition::common::Constituent::new(
                            (0, 0),
                            verity_definition::static_semantics::TypeValue::Bool,
                        ),
                    ),
                ),
            ),
        ),
    );

    prim.type_env.insert(
        String::from("bool"),
        (
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::TypeValue::Bool,
            ),
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::KindValue::Type,
            ),
        ),
    );

    prim.type_env.insert(
        String::from("int"),
        (
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::TypeValue::Int,
            ),
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::KindValue::Type,
            ),
        ),
    );

    prim.type_env.insert(
        String::from("rational"),
        (
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::TypeValue::Rational,
            ),
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::KindValue::Type,
            ),
        ),
    );

    prim.type_env.insert(
        String::from("array"),
        (
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::TypeValue::Array,
            ),
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::KindValue::Arrow(
                    verity_definition::common::Constituent::new(
                        (0, 0),
                        verity_definition::static_semantics::KindValue::Type,
                    ),
                    verity_definition::common::Constituent::new(
                        (0, 0),
                        verity_definition::static_semantics::KindValue::Type,
                    ),
                ),
            ),
        ),
    );

    prim.type_env.insert(
        String::from("string"),
        (
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::TypeValue::String,
            ),
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::KindValue::Type,
            ),
        ),
    );

    prim.type_env.insert(
        String::from("binary"),
        (
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::TypeValue::Binary,
            ),
            verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::static_semantics::KindValue::Type,
            ),
        ),
    );

    prim.family_env.insert(
        String::from("Int"),
        verity_definition::common::Constituent::new((0, 0), int),
    );

    prim.family_env.insert(
        String::from("Rational"),
        verity_definition::common::Constituent::new((0, 0), rational),
    );

    prim.family_env.insert(
        String::from("stdout"),
        verity_definition::common::Constituent::new((0, 0), stdout),
    );

    prim.family_env.insert(
        String::from("stderr"),
        verity_definition::common::Constituent::new((0, 0), stderr),
    );

    prim
}

fn prim_dynamic_environment() -> verity_definition::dynamic_semantics::Environment {
    let mut dynamic_environment = verity_definition::dynamic_semantics::Environment::new();
    let mut prim = verity_definition::dynamic_semantics::Environment::new();
    let mut int = verity_definition::dynamic_semantics::Environment::new();
    let mut rational = verity_definition::dynamic_semantics::Environment::new();
    let mut stdout = verity_definition::dynamic_semantics::Environment::new();
    let mut stderr = verity_definition::dynamic_semantics::Environment::new();

    int.insert(
        String::from("=="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_eq),
        ),
    );

    int.insert(
        String::from("!="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_noteq),
        ),
    );

    int.insert(
        String::from("<="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_lte),
        ),
    );

    int.insert(
        String::from("<"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_lt),
        ),
    );

    int.insert(
        String::from(">="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_gte),
        ),
    );

    int.insert(
        String::from(">"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_gt),
        ),
    );

    int.insert(
        String::from("+"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_plus),
        ),
    );

    int.insert(
        String::from("-"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_minus),
        ),
    );

    int.insert(
        String::from("*"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_mult),
        ),
    );

    int.insert(
        String::from("/"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_div),
        ),
    );

    int.insert(
        String::from("%"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(int_mod),
        ),
    );

    rational.insert(
        String::from("=="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_eq),
        ),
    );

    rational.insert(
        String::from("!="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_noteq),
        ),
    );

    rational.insert(
        String::from("<="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_lte),
        ),
    );

    rational.insert(
        String::from("<"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_lt),
        ),
    );

    rational.insert(
        String::from(">="),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_gte),
        ),
    );

    rational.insert(
        String::from(">"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_gt),
        ),
    );

    rational.insert(
        String::from("+"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_plus),
        ),
    );

    rational.insert(
        String::from("-"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_minus),
        ),
    );

    rational.insert(
        String::from("*"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_mult),
        ),
    );

    rational.insert(
        String::from("/"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_div),
        ),
    );

    rational.insert(
        String::from("%"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(rational_mod),
        ),
    );

    stdout.insert(
        String::from("puts"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(stdout_puts),
        ),
    );

    stderr.insert(
        String::from("puts"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::ForeignFn(stderr_puts),
        ),
    );

    prim.insert(
        String::from("Int"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::Family(int),
        ),
    );

    prim.insert(
        String::from("Rational"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::Family(rational),
        ),
    );

    prim.insert(
        String::from("stdout"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::Family(stdout),
        ),
    );

    prim.insert(
        String::from("stderr"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::Family(stderr),
        ),
    );

    dynamic_environment.insert(
        String::from("prim"),
        verity_definition::common::Constituent::new(
            (0, 0),
            verity_definition::dynamic_semantics::Value::Family(prim),
        ),
    );

    dynamic_environment
}

fn prelude() -> &'static str {
    include_str!("../../../lib/prelude.verity")
}

fn repl() {
    let mut rl = rustyline::Editor::<()>::new();
    let mut i = 0;

    let mut verifier = verity::static_semantics::Verifier::new();

    let mut environment = verity_definition::static_semantics::Environment {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    let prim = prim_environment();

    environment.family_env.insert(
        String::from("prim"),
        verity_definition::common::Constituent::new((0, 0), prim),
    );

    let mut prelude_environment = verity_definition::static_semantics::Environment {
        family_path: vec![],
        family_parameter: None,
        family_parameter_kind: None,
        family_parameter_type: None,
        family_type: None,
        type_env: std::collections::BTreeMap::new(),
        value_env: std::collections::BTreeMap::new(),
        family_env: std::collections::BTreeMap::new(),
        record_env: std::collections::BTreeMap::new(),
        variant_env: std::collections::BTreeMap::new(),
    };

    let dynamic_environment = prim_dynamic_environment();
    let mut dynamic_exports = verity_definition::dynamic_semantics::Environment::new();

    eval_program(
        &mut verifier,
        &environment,
        &mut prelude_environment,
        &dynamic_environment,
        &mut dynamic_exports,
        "prelude",
        1,
        1,
        prelude(),
    );

    let mut environment = prelude_environment;
    let mut dynamic_environment = dynamic_exports;

    loop {
        let result = rl.readline("> ");

        match result {
            Ok(line) => {
                let mut exports = verity_definition::static_semantics::Environment {
                    family_path: vec![],
                    family_parameter: None,
                    family_parameter_kind: None,
                    family_parameter_type: None,
                    family_type: None,
                    type_env: std::collections::BTreeMap::new(),
                    value_env: std::collections::BTreeMap::new(),
                    family_env: std::collections::BTreeMap::new(),
                    record_env: std::collections::BTreeMap::new(),
                    variant_env: std::collections::BTreeMap::new(),
                };

                let mut dynamic_exports = verity_definition::dynamic_semantics::Environment::new();

                eval_program(
                    &mut verifier,
                    &environment,
                    &mut exports,
                    &dynamic_environment,
                    &mut dynamic_exports,
                    "verity",
                    i + 1,
                    1,
                    &line,
                );

                for (id, (type_value, kind_value)) in exports.type_env.iter() {
                    eprintln!(
                        "type {} : {} := {}",
                        String::from(&**id),
                        verity::static_semantics::pp::show_kind(&kind_value),
                        verity::static_semantics::pp::show_type(&type_value)
                    );
                }

                for (id, type_value) in exports.value_env.iter() {
                    eprintln!(
                        "val {} : {}",
                        String::from(&**id),
                        verity::static_semantics::pp::show_type(&type_value)
                    );
                }

                for (id, x) in dynamic_exports.iter() {
                    eprintln!("def {} := {}", id, verity::dynamic_semantics::show_value(x));
                }

                environment.type_env.append(&mut exports.type_env);
                environment.value_env.append(&mut exports.value_env);
                environment.family_env.append(&mut exports.family_env);
                environment.record_env.append(&mut exports.record_env);
                environment.variant_env.append(&mut exports.variant_env);
                dynamic_environment.append(&mut dynamic_exports);
            }
            Err(e) => match e {
                rustyline::error::ReadlineError::Interrupted => {
                    break;
                }
                rustyline::error::ReadlineError::Eof => {
                    break;
                }
                _ => {
                    eprintln!("{:?}", e);
                    break;
                }
            },
        }

        i += 1;
    }
}

fn eval_program(
    verifier: &mut verity::static_semantics::Verifier,
    environment: &verity::static_semantics::Environment,
    exports: &mut verity::static_semantics::Environment,
    dynamic_environment: &verity_definition::dynamic_semantics::Environment,
    dynamic_exports: &mut verity_definition::dynamic_semantics::Environment,
    subject: &str,
    lineno: usize,
    colno: usize,
    contents: &str,
) {
    match verity::syntax::parse(&contents) {
        Ok(program) => {
            let result = verifier.eval_program(environment, exports, &program);

            match result {
                Ok(()) => {
                    let result =
                        verity::dynamic_semantics::eval_program(dynamic_environment, &program);

                    match result {
                        Ok(dynamic_exports1) => {
                            for (id, x) in dynamic_exports1.iter() {
                                dynamic_exports.insert(id.clone(), x.clone());
                            }
                        }
                        Err(e) => {
                            let e = match e {
                                verity_definition::dynamic_semantics::Error::Unsound {
                                    location,
                                } => verity::definition::error::Error::between(
                                    &contents,
                                    location,
                                    "*UNSOUND*",
                                ),
                            };

                            verity::definition::error::eprintln_error(
                                &subject, lineno, colno, &contents, e,
                            );
                        }
                    }
                }
                Err(e) => {
                    let e = match e {
                        verity::static_semantics::Error::TypeNotInScope { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "type not in scope",
                            )
                        }
                        verity::static_semantics::Error::TypeNotInFamily { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "type not in family",
                            )
                        }
                        verity::static_semantics::Error::ValueNotInScope { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "value not in scope",
                            )
                        }
                        verity::static_semantics::Error::ValueNotInFamily { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "value not in family",
                            )
                        }
                        verity::static_semantics::Error::FamilyNotInScope { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "family not in scope",
                            )
                        }
                        verity::static_semantics::Error::FamilyNotInFamily { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "family not in family",
                            )
                        }
                        verity::static_semantics::Error::RecordNotInScope { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "record not in scope",
                            )
                        }
                        verity::static_semantics::Error::RecordNotInFamily { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "record not in family",
                            )
                        }
                        verity::static_semantics::Error::VariantNotInScope { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "variant not in scope",
                            )
                        }
                        verity::static_semantics::Error::VariantNotInFamily { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "variant not in family",
                            )
                        }
                        verity::static_semantics::Error::KindMismatch { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "kind mismatch",
                            )
                        }
                        verity::static_semantics::Error::TypeMismatch { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "type mismatch",
                            )
                        }
                        verity::static_semantics::Error::ValueAlreadyDefined { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "value already defined",
                            )
                        }
                        verity::static_semantics::Error::TypeAlreadyDefined { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "type already defined",
                            )
                        }
                        verity::static_semantics::Error::RecordAlreadyDefined { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "record already defined",
                            )
                        }
                        verity::static_semantics::Error::VariantAlreadyDefined { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "variant already defined",
                            )
                        }
                        verity::static_semantics::Error::FamilyAlreadyDefined { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "family already defined",
                            )
                        }
                        verity::static_semantics::Error::Cyclic { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "cyclic type",
                            )
                        }
                        verity::static_semantics::Error::InvalidPattern { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "invalid pattern",
                            )
                        }
                        verity::static_semantics::Error::InvalidRecord { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "invalid record",
                            )
                        }
                        verity::static_semantics::Error::ToplevelEffect { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "toplevel effect",
                            )
                        }
                        verity::static_semantics::Error::EffectInPattern { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "effect in pattern",
                            )
                        }
                    };

                    verity::definition::error::eprintln_error(
                        &subject, lineno, colno, &contents, e,
                    );
                }
            }
        }
        Err(e) => {
            verity::definition::error::eprintln_error(&subject, lineno, colno, &contents, e);
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = std::env::args().collect();

    for arg in &args[1..] {
        if arg == &String::from("--version") {
            eprintln!("0.1.0");
            return Ok(());
        }
    }

    if args.len() <= 1 {
        repl();
    }

    if args.len() >= 2 {
        use std::io::prelude::*;

        let path = &args[1];
        let mut file = std::fs::File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let mut verifier = verity::static_semantics::Verifier::new();

        let mut environment = verity_definition::static_semantics::Environment {
            family_path: vec![],
            family_parameter: None,
            family_parameter_kind: None,
            family_parameter_type: None,
            family_type: None,
            type_env: std::collections::BTreeMap::new(),
            value_env: std::collections::BTreeMap::new(),
            family_env: std::collections::BTreeMap::new(),
            record_env: std::collections::BTreeMap::new(),
            variant_env: std::collections::BTreeMap::new(),
        };

        let prim = prim_environment();

        environment.family_env.insert(
            String::from("prim"),
            verity_definition::common::Constituent::new((0, 0), prim),
        );

        let mut prelude_environment = verity_definition::static_semantics::Environment {
            family_path: vec![],
            family_parameter: None,
            family_parameter_kind: None,
            family_parameter_type: None,
            family_type: None,
            type_env: std::collections::BTreeMap::new(),
            value_env: std::collections::BTreeMap::new(),
            family_env: std::collections::BTreeMap::new(),
            record_env: std::collections::BTreeMap::new(),
            variant_env: std::collections::BTreeMap::new(),
        };

        let mut exports = verity_definition::static_semantics::Environment {
            family_path: vec![],
            family_parameter: None,
            family_parameter_kind: None,
            family_parameter_type: None,
            family_type: None,
            type_env: std::collections::BTreeMap::new(),
            value_env: std::collections::BTreeMap::new(),
            family_env: std::collections::BTreeMap::new(),
            record_env: std::collections::BTreeMap::new(),
            variant_env: std::collections::BTreeMap::new(),
        };

        let dynamic_environment = prim_dynamic_environment();
        let mut prelude_dynamic_environment =
            verity_definition::dynamic_semantics::Environment::new();
        let mut dynamic_exports = verity_definition::dynamic_semantics::Environment::new();

        eval_program(
            &mut verifier,
            &environment,
            &mut prelude_environment,
            &dynamic_environment,
            &mut prelude_dynamic_environment,
            "prelude",
            1,
            1,
            prelude(),
        );

        eval_program(
            &mut verifier,
            &prelude_environment,
            &mut exports,
            &prelude_dynamic_environment,
            &mut dynamic_exports,
            &path,
            1,
            1,
            &contents,
        );

        if let Some(type_value) = exports.value_env.get(&String::from("main")) {
            let type_value = verifier.instantiate_type(&type_value);

            let main_type = verity_definition::common::Constituent::new(
                type_value.location(),
                verity_definition::static_semantics::TypeValue::Arrow(
                    verity_definition::common::Constituent::new(
                        type_value.location(),
                        verity_definition::static_semantics::TypeValue::Application(
                            verity_definition::common::Constituent::new(
                                type_value.location(),
                                verity_definition::static_semantics::TypeValue::Array,
                            ),
                            verity_definition::common::Constituent::new(
                                type_value.location(),
                                verity_definition::static_semantics::TypeValue::String,
                            ),
                        ),
                    ),
                    verity_definition::common::Constituent::new(
                        type_value.location(),
                        verity_definition::static_semantics::TypeValue::Effect(
                            verity_definition::common::Constituent::new(
                                type_value.location(),
                                verity_definition::static_semantics::TypeValue::Bool,
                            ),
                        ),
                    ),
                ),
            );

            if let Err(e) =
                verity::static_semantics::unify(type_value.location(), &type_value, &main_type)
            {
                let e = match e {
                    verity::static_semantics::Error::TypeNotInScope { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "type not in scope",
                        )
                    }
                    verity::static_semantics::Error::TypeNotInFamily { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "type not in family",
                        )
                    }
                    verity::static_semantics::Error::ValueNotInScope { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "value not in scope",
                        )
                    }
                    verity::static_semantics::Error::ValueNotInFamily { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "value not in family",
                        )
                    }
                    verity::static_semantics::Error::FamilyNotInScope { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "family not in scope",
                        )
                    }
                    verity::static_semantics::Error::FamilyNotInFamily { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "family not in family",
                        )
                    }
                    verity::static_semantics::Error::RecordNotInScope { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "record not in scope",
                        )
                    }
                    verity::static_semantics::Error::RecordNotInFamily { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "record not in family",
                        )
                    }
                    verity::static_semantics::Error::VariantNotInScope { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "variant not in scope",
                        )
                    }
                    verity::static_semantics::Error::VariantNotInFamily { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "variant not in family",
                        )
                    }
                    verity::static_semantics::Error::KindMismatch { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "kind mismatch",
                        )
                    }
                    verity::static_semantics::Error::TypeMismatch { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "type mismatch",
                        )
                    }
                    verity::static_semantics::Error::ValueAlreadyDefined { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "value already defined",
                        )
                    }
                    verity::static_semantics::Error::TypeAlreadyDefined { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "type already defined",
                        )
                    }
                    verity::static_semantics::Error::RecordAlreadyDefined { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "record already defined",
                        )
                    }
                    verity::static_semantics::Error::VariantAlreadyDefined { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "variant already defined",
                        )
                    }
                    verity::static_semantics::Error::FamilyAlreadyDefined { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "family already defined",
                        )
                    }
                    verity::static_semantics::Error::Cyclic { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "cyclic type",
                        )
                    }
                    verity::static_semantics::Error::InvalidPattern { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "invalid pattern",
                        )
                    }
                    verity::static_semantics::Error::InvalidRecord { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "invalid record",
                        )
                    }
                    verity::static_semantics::Error::ToplevelEffect { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "toplevel effect",
                        )
                    }
                    verity::static_semantics::Error::EffectInPattern { location } => {
                        verity::definition::error::Error::between(
                            &contents,
                            location,
                            "effect in pattern",
                        )
                    }
                };

                verity::definition::error::eprintln_error(path, 1, 1, &contents, e);

                return Ok(());
            }
        }

        if let Some(main) = dynamic_exports.get(&String::from("main")) {
            let args = verity_definition::common::Constituent::new(
                (0, 0),
                verity_definition::dynamic_semantics::Value::Array(
                    Vec::from(&args[2..])
                        .into_iter()
                        .map(|x| {
                            verity_definition::common::Constituent::new(
                                (0, 0),
                                verity_definition::dynamic_semantics::Value::String(x),
                            )
                        })
                        .collect(),
                ),
            );

            let result = verity::dynamic_semantics::apply(
                (0, 0),
                &verity_definition::dynamic_semantics::Environment::new(),
                &verity_definition::dynamic_semantics::Environment::new(),
                main,
                &args,
            );

            match result {
                Ok(value) => {
                    if let verity_definition::dynamic_semantics::Value::Bool(status) = &*value {
                        if !status {
                            std::process::exit(1);
                        }
                    }
                }
                Err(e) => {
                    let e = match e {
                        verity_definition::dynamic_semantics::Error::Unsound { location } => {
                            verity::definition::error::Error::between(
                                &contents,
                                location,
                                "*UNSOUND*",
                            )
                        }
                    };

                    verity::definition::error::eprintln_error(path, 1, 1, &contents, e);
                }
            }
        }
    }

    Ok(())
}
