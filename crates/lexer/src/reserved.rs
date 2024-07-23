use super::token::{BuiltInType, Literal, ReservedWord, Token};
use crate::ByteResult;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::{map_res, recognize},
    multi::many0,
    sequence::pair,
};
use shared::{util::complete_byte_slice_str_from_utf8, BytesSpan, Positioned, Span};

pub fn lex_reserved_ident(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    map_res(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: BytesSpan| {
            let c = complete_byte_slice_str_from_utf8(s);
            let s: Span = s.into();

            c.map(|syntax| {
                s.wrap(match syntax {
                    "var" => Token::ReservedWord(ReservedWord::Var),
                    "enum" => Token::ReservedWord(ReservedWord::Enum),
                    "async" => Token::ReservedWord(ReservedWord::Async),
                    "await" => Token::ReservedWord(ReservedWord::Await),
                    "while" => Token::ReservedWord(ReservedWord::While),
                    "match" => Token::ReservedWord(ReservedWord::Match),
                    "return" => Token::ReservedWord(ReservedWord::Return),
                    "func" => Token::ReservedWord(ReservedWord::Func),
                    "null" => Token::ReservedWord(ReservedWord::Null),
                    "use" => Token::ReservedWord(ReservedWord::Use),
                    "as" => Token::ReservedWord(ReservedWord::As),
                    "for" => Token::ReservedWord(ReservedWord::For),
                    "in" => Token::ReservedWord(ReservedWord::In),
                    "if" => Token::ReservedWord(ReservedWord::If),
                    "else" => Token::ReservedWord(ReservedWord::Else),
                    "from" => Token::ReservedWord(ReservedWord::From),
                    "break" => Token::ReservedWord(ReservedWord::Break),
                    "continue" => Token::ReservedWord(ReservedWord::Continue),

                    "number" => Token::BuiltInType(BuiltInType::Number),
                    "float" => Token::BuiltInType(BuiltInType::Float),
                    "boolean" => Token::BuiltInType(BuiltInType::Bool),
                    "string" => Token::BuiltInType(BuiltInType::String),
                    "void" => Token::BuiltInType(BuiltInType::Void),

                    "true" => Token::Literal(Literal::Boolean(true)),
                    "false" => Token::Literal(Literal::Boolean(false)),

                    _ => Token::Ident(syntax.to_string()),
                })
            })
        },
    )(input)
}
