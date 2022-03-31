#[macro_use]
extern crate lalrpop_util;
extern crate verity_definition as definition;

lalrpop_mod!(parser);

fn rename_token(token: &str) -> String {
    let terminal = match token {
        "EXCLAMATION" => Some("!"),
        "NOTEQ" => Some("!="),
        "PERCENT" => Some("%"),
        "AMP" => Some("&"),
        "AND" => Some("&&"),
        "LPAREN" => Some("("),
        "RPAREN" => Some(")"),
        "ASTERISK" => Some("*"),
        "PLUS" => Some("+"),
        "COMMA" => Some(","),
        "MINUS" => Some("-"),
        "ARROW" => Some("->"),
        "DOT" => Some("."),
        "SLASH" => Some("/"),
        "COLON" => Some(":"),
        "COLONEQ" => Some(":="),
        "SEMICOLON" => Some(";"),
        "LT" => Some("<"),
        "SUBTYPE" => Some("<:"),
        "LTE" => Some("<="),
        "EQ" => Some("=="),
        "DARROW" => Some("=>"),
        "GT" => Some(">"),
        "GTE" => Some(">="),
        "QUESTION" => Some("?"),
        "HAT" => Some("^"),
        "ELSE" => Some("else"),
        "FALSE" => Some("false"),
        "FORALL" => Some("forall"),
        "DEF" => Some("def"),
        "DO" => Some("do"),
        "END" => Some("end"),
        "EXISTS" => Some("exists"),
        "FAMILY" => Some("family"),
        "FOR" => Some("for"),
        "IF" => Some("if"),
        "IN" => Some("in"),
        "INDUCTIVE" => Some("inductive"),
        "LET" => Some("let"),
        "MATCH" => Some("match"),
        "OF" => Some("of"),
        "STRUCT" => Some("struct"),
        "THEN" => Some("then"),
        "TRUE" => Some("true"),
        "TYPE" => Some("type"),
        "WITH" => Some("with"),
        "WHERE" => Some("where"),
        "LBRACKET" => Some("["),
        "RBRACKET" => Some("]"),
        "LBRACE" => Some("{"),
        "PIPE" => Some("|"),
        "OR" => Some("||"),
        "RBRACE" => Some("}"),
        _ => None,
    };

    if let Some(s) = terminal {
        format!("`{}'", s)
    } else {
        match token {
            "STRING" => String::from("string"),
            "BINARY" => String::from("binary"),
            "INTEGER" => String::from("integer"),
            "RATIONAL" => String::from("rational"),
            "IDENTIFIER" => String::from("identifier"),
            _ => String::from(token),
        }
    }
}

pub fn parse(
    contents: &str,
) -> Result<definition::common::Constituent<definition::syntax::Program>, definition::error::Error>
{
    let parser = parser::ProgramParser::new();

    match parser.parse(contents) {
        Ok(program) => Ok(program),
        Err(e) => match e {
            lalrpop_util::ParseError::InvalidToken { location } => Err(
                definition::error::Error::at(contents, location, "invalid token"),
            ),
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                let message = format!(
                    "unrecognized eof: one of {} expected.",
                    expected
                        .into_iter()
                        .map(|token| rename_token(&token))
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                Err(definition::error::Error::at(contents, location, &message))
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let (left, raw, right) = token;

                let message = format!(
                    "unrecognized token: one of {} expected but got `{}'.",
                    expected
                        .into_iter()
                        .map(|token| rename_token(&token))
                        .collect::<Vec<String>>()
                        .join(", "),
                    raw
                );

                Err(definition::error::Error::between(
                    contents,
                    (left, right),
                    &message,
                ))
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                let (left, raw, right) = token;
                let message = format!("extra token `{}'", raw);

                Err(definition::error::Error::between(
                    contents,
                    (left, right),
                    &message,
                ))
            }
            lalrpop_util::ParseError::User { error } => Err(definition::error::Error {
                lineno: None,
                colno: None,
                message: Some(String::from(error)),
                error_line: None,
            }),
        },
    }
}
