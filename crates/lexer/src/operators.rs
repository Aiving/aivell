use super::token::{Operator, Token};
use crate::{syntax, ByteResult};
use nom::branch::alt;
use shared::{BytesSpan, Positioned};

syntax! {
    and_operator: "&" => Token::Operator(Operator::And);
    and_and_operator: "&&" => Token::Operator(Operator::AndAnd);
    equal_operator: "==" => Token::Operator(Operator::EqEq);
    not_equal_operator: "!=" => Token::Operator(Operator::Ne);
    or_operator: "|" => Token::Operator(Operator::Or);
    or_or_operator: "||" => Token::Operator(Operator::OrOr);
    assign_operator: "=" => Token::Operator(Operator::Eq);
    plus_plus_operator: "++" => Token::Operator(Operator::PlusPlus);
    plus_operator: "+" => Token::Operator(Operator::Plus);
    plus_eq_operator: "+=" => Token::Operator(Operator::PlusEq);
    minus_minus_operator: "--" => Token::Operator(Operator::MinusMinus);
    minus_operator: "-=" => Token::Operator(Operator::Minus);
    minus_eq_operator: "-=" => Token::Operator(Operator::MinusEq);
    multiply_operator: "*" => Token::Operator(Operator::Star);
    multiply_eq_operator: "*=" => Token::Operator(Operator::StarEq);
    divide_operator: "/" => Token::Operator(Operator::Slash);
    divide_eq_operator: "/=" => Token::Operator(Operator::SlashEq);
    not_operator: "!" => Token::Operator(Operator::Not);
    greater_equal_operator: ">=" => Token::Operator(Operator::Ge);
    lesser_equal_operator: "<=" => Token::Operator(Operator::Le);
    greater_operator: ">" => Token::Operator(Operator::Gt);
    lesser_operator: "<" => Token::Operator(Operator::Lt);
    percent_operator: "%" => Token::Operator(Operator::Percent);
    percent_eq_operator: "%=" => Token::Operator(Operator::PercentEq);
}

pub fn lex_operator(input: BytesSpan) -> ByteResult<Positioned<Token>> {
    alt((
        alt((percent_eq_operator, percent_operator)),
        alt((and_and_operator, and_operator)),
        equal_operator,
        not_equal_operator,
        alt((or_or_operator, or_operator)),
        assign_operator,
        alt((plus_plus_operator, plus_eq_operator, plus_operator)),
        alt((minus_minus_operator, minus_eq_operator, minus_operator)),
        alt((multiply_eq_operator, multiply_operator)),
        alt((divide_eq_operator, divide_operator)),
        not_operator,
        alt((greater_equal_operator, greater_operator)),
        alt((lesser_equal_operator, lesser_operator)),
    ))(input)
}
