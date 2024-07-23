use super::{
    punctuation::double_quote_punctuation,
    token::{Literal, Token},
};
use crate::ByteResult;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::digit1,
    combinator::{map, map_res, opt},
    sequence::{delimited, pair},
    AsBytes,
};
use nom_locate::position;
use shared::{
    util::{
        complete_byte_slice_str_from_utf8, complete_str_from_str, concat_slice_vec,
        convert_vec_utf8,
    },
    BytesSpan, Positioned, Span,
};

pub fn lex_integer(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    let (input, start) = position(input)?;
    let (input, value) = alt((map(
        map_res(
            map_res(pair(opt(tag("-")), digit1), |(is_negative, data)| {
                complete_byte_slice_str_from_utf8(data).map(|number| (is_negative, number))
            }),
            |(is_negative, data)| {
                complete_str_from_str(data).map(|number: i64| {
                    if is_negative.is_some() {
                        -number
                    } else {
                        number
                    }
                })
            },
        ),
        Literal::Number,
    ),))(input)?;
    let (input, end) = position(input)?;
    let start: Span = start.into();
    let end: Span = end.into();

    Ok((input, start.between(end).wrap(Token::Literal(value))))
}

fn pis(input: BytesSpan) -> ByteResult<Vec<u8>> {
    let (second_input, character) = take(1usize)(input)?;

    match character.as_bytes() {
        b"\"" => Ok((input, vec![])),
        b"\\" => {
            let (i2, c2) = take(1usize)(second_input)?;

            pis(i2).map(|(slice, done)| (slice, concat_slice_vec(c2.fragment(), &done)))
        }
        c => pis(second_input).map(|(slice, done)| (slice, concat_slice_vec(c, &done))),
    }
}

pub fn lex_string(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    let (input, start) = position(input)?;
    let (input, value) = delimited(
        double_quote_punctuation,
        map_res(pis, |r| convert_vec_utf8(&r)),
        double_quote_punctuation,
    )(input)?;
    let (input, end) = position(input)?;
    let start: Span = start.into();
    let end: Span = end.into();

    Ok((
        input,
        start
            .between(end)
            .wrap(Token::Literal(Literal::String(value))),
    ))
}

pub fn lex_literal(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((lex_string, lex_integer))(input)
}
