use super::token::{Punctuation, Token};
use crate::{syntax, token::Operator, ByteResult};
use nom::branch::alt;
use shared::{BytesSpan, Positioned};

syntax! {
    comma_punctuation: "," => Token::Punctuation(Punctuation::Comma);
    dot_punctuation: "." => Token::Punctuation(Punctuation::Dot);
    ellipsis_punctuation: "..." => Token::Punctuation(Punctuation::Ellipsis);
    r_arrow_punctuation: "->" => Token::Punctuation(Punctuation::RArrow);
    fat_arrow_punctuation: "=>" => Token::Punctuation(Punctuation::FatArrow);
    double_slash_punctuation: "//" => Token::Punctuation(Punctuation::DoubleSlash);
    double_quote_punctuation: "\"" => Token::Punctuation(Punctuation::DoubleQuote);
    colon_punctuation: ":" => Token::Punctuation(Punctuation::Colon);
    semi_punctuation: ";" => Token::Punctuation(Punctuation::Semi);
    question_punctuation: "?" => Token::Punctuation(Punctuation::Question);
    pound_punctuation: "#" => Token::Punctuation(Punctuation::Pound);
    range_inclusive_operator: "..=" => Token::Operator(Operator::RangeInclusive);
    range_operator: ".." => Token::Operator(Operator::Range);
}

pub fn lex_punctuation(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((
        comma_punctuation,
        ellipsis_punctuation,
        range_inclusive_operator,
        range_operator,
        dot_punctuation,
        r_arrow_punctuation,
        fat_arrow_punctuation,
        double_slash_punctuation,
        double_quote_punctuation,
        colon_punctuation,
        semi_punctuation,
        question_punctuation,
        pound_punctuation,
    ))(input)
}
