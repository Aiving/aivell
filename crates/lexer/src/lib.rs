#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
#![allow(
    clippy::cargo_common_metadata,
    clippy::too_many_lines,
    clippy::module_name_repetitions,
    clippy::missing_panics_doc,
    clippy::missing_errors_doc
)]

use self::{
    delimiters::lex_delimiter,
    literal::lex_literal,
    operators::lex_operator,
    punctuation::{double_slash_punctuation, lex_punctuation},
    reserved::lex_reserved_ident,
    token::Token,
};
use shared::{BytesSpan, Positioned, Span, util::convert_vec_utf8};
use nom::{
    branch::alt,
    bytes::complete::{is_not, take},
    character::complete::multispace0,
    combinator::map,
    multi::many0,
    sequence::{delimited, pair}, IResult,
};

pub mod delimiters;
pub mod literal;
pub mod operators;
pub mod punctuation;
pub mod reserved;
pub mod token;
pub mod tokens;

pub type ByteResult<'a, T> = IResult<BytesSpan<'a>, T>;

#[macro_export]
macro_rules! syntax {
    ($func_name:ident: $tag_string:literal => $output_token:expr) => {
        pub fn $func_name(input: BytesSpan) -> ByteResult<Positioned<Token>> {
            let (input, position) = tag($tag_string)(input)?;

            Ok((input, Positioned::new($output_token, position.into())))
        }
    };

    ($($func_name:ident: $tag_string:literal => $output_token:expr);*;) => {
        use nom::bytes::complete::tag;

        $(
            pub fn $func_name(input: BytesSpan) -> ByteResult<Positioned<Token>> {
                let (input, position) = tag($tag_string)(input)?;

                Ok((input, Positioned::new($output_token, position.into())))
            }
        )*
    };
}

pub fn lex_comment(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    let (input, (_, (position, comment))) = pair(
        double_slash_punctuation,
        map(is_not("\r\n"), |s: BytesSpan| {
            (s, convert_vec_utf8(s.fragment()).unwrap())
        }),
    )(input)?;
    let position: Span = position.into();

    Ok((input, position.wrap(Token::Comment(comment))))
}

fn lex_illegal(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    map(take(1usize), |s: BytesSpan| {
        Span::from(s).wrap(Token::Illegal)
    })(input)
}

pub fn lex_token(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((
        lex_comment,
        lex_reserved_ident,
        lex_literal,
        lex_punctuation,
        lex_operator,
        lex_delimiter,
        lex_illegal,
    ))(input)
}

pub fn lex_tokens(input: BytesSpan) -> ByteResult<Vec<Positioned<Token>>> {
    many0(delimited(multispace0, lex_token, multispace0))(input)
}

pub struct Lexer;

impl Lexer {
    pub fn parse<'a>(source: &'a &str) -> ByteResult<'a, Vec<Positioned<Token>>> {
        lex_tokens(source.as_bytes().into()).map(|(slice, result)| {
            (
                slice,
                [
                    &result[..],
                    &vec![Positioned::new(Token::EOF, slice.into())][..],
                ]
                .concat()
                .into_iter()
                .filter(|token| !matches!(token.value, Token::Comment(_)))
                .collect::<Vec<_>>(),
            )
        })
    }
}