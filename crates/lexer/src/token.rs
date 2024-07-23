use std::fmt::{self, Write};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltInType {
    Number,
    Float,
    Bool,
    String,
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(i64),
    Float(f64),
    Boolean(bool),
}

impl Eq for Literal {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReservedWord {
    Var,      // var
    Return,   // return
    While,    // while
    Match,    // match
    Async,    // async
    Await,    // await
    Enum,     // enum
    Func,     // func
    Null,     // null
    Use,      // use
    As,       // as
    For,      // for
    In,       // in
    If,       // if
    Else,     // else
    From,     // from
    Break,    // break,
    Continue, // continue
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Punctuation {
    Comma,       // ,
    Dot,         // .
    Ellipsis,    // ...
    RArrow,      // ->
    FatArrow,    // =>
    DoubleSlash, // //
    DoubleQuote, // "
    Colon,       // :
    Semi,        // ;
    Question,    // ?
    Pound,       // #
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    And,            // &
    AndAnd,         // &&
    Plus,           // +
    PlusEq,         // +=
    Minus,          // -
    MinusEq,        // -=
    Star,           // *
    StarEq,         // *=
    Slash,          // /
    SlashEq,        // /=
    Or,             // |
    OrOr,           // ||
    PlusPlus,       // ++
    MinusMinus,     // --
    EqEq,           // ==
    Eq,             // =
    Ne,             // !=
    Le,             // <=
    Ge,             // >=
    Lt,             // <
    Gt,             // >
    Not,            // !
    Range,          //..
    RangeInclusive, // ..=
    Percent,        // %
    PercentEq,      // %=
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    BraceOpen,    // {
    BraceClose,   // }
    BracketOpen,  // [
    BracketClose, // ]
    ParenOpen,    // (
    ParenClose,   // )
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Illegal,
    EOF,
    Comment(String),
    Delimiter(Delimiter),
    Ident(String),
    Literal(Literal),
    BuiltInType(BuiltInType),
    ReservedWord(ReservedWord),
    Punctuation(Punctuation),
    Operator(Operator),
}

impl Token {
    #[must_use]
    pub const fn is_end(&self) -> bool {
        matches!(self, Self::EOF | Self::Delimiter(Delimiter::BraceClose))
    }

    #[must_use]
    pub const fn is_indexer(&self) -> bool {
        matches!(
            self,
            Self::Punctuation(Punctuation::Dot)
                | Self::Delimiter(Delimiter::BracketOpen | Delimiter::ParenOpen)
        )
    }

    #[must_use]
    pub const fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(_))
    }

    #[must_use]
    pub const fn is_literal(&self) -> bool {
        matches!(self, Self::Literal(_))
    }

    #[must_use]
    pub const fn is_number(&self) -> bool {
        matches!(self, Self::Literal(Literal::Number(_)))
    }

    #[must_use]
    pub const fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }

    #[must_use]
    pub fn into_ident(self) -> String {
        if let Self::Ident(value) = self {
            value
        } else {
            unreachable!()
        }
    }

    #[must_use]
    pub fn into_literal(self) -> Literal {
        if let Self::Literal(value) = self {
            value
        } else {
            unreachable!()
        }
    }

    #[must_use]
    pub fn into_number(self) -> i64 {
        if let Self::Literal(Literal::Number(value)) = self {
            value
        } else {
            unreachable!()
        }
    }

    #[must_use]
    pub fn into_comment(self) -> String {
        if let Self::Comment(value) = self {
            value
        } else {
            unreachable!()
        }
    }

    #[must_use]
    pub const fn as_number(&self) -> Option<i64> {
        if let Self::Literal(Literal::Number(value)) = self {
            Some(*value)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_usize(&self) -> Option<usize> {
        if let Self::Literal(Literal::Number(value)) = self {
            usize::try_from(*value).ok()
        } else {
            None
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Illegal => f.write_str("<unknown>"),
            Self::EOF => f.write_str("<EOF>"),
            Self::Comment(_) => f.write_str("comment"),
            Self::Delimiter(delimiter) => f.write_char(match delimiter {
                Delimiter::BraceOpen => '{',
                Delimiter::BraceClose => '}',
                Delimiter::BracketOpen => '[',
                Delimiter::BracketClose => ']',
                Delimiter::ParenOpen => '(',
                Delimiter::ParenClose => ')',
            }),
            Self::Ident(_) => f.write_str("ident"),
            Self::Literal(literal) => f.write_str(match literal {
                Literal::String(_) => "string literal",
                Literal::Number(_) => "number literal",
                Literal::Float(_) => "float literal",
                Literal::Boolean(_) => "boolean literal",
            }),
            Self::BuiltInType(ty) => f.write_str(match ty {
                BuiltInType::Number => "number",
                BuiltInType::Float => "float",
                BuiltInType::Bool => "bool",
                BuiltInType::String => "string",
                BuiltInType::Void => "void",
            }),
            Self::ReservedWord(word) => f.write_str(match word {
                ReservedWord::Var => "var",
                ReservedWord::Return => "return",
                ReservedWord::While => "while",
                ReservedWord::Match => "match",
                ReservedWord::Enum => "enum",
                ReservedWord::Async => "async",
                ReservedWord::Await => "await",
                ReservedWord::Func => "func",
                ReservedWord::Null => "null",
                ReservedWord::Use => "use",
                ReservedWord::As => "as",
                ReservedWord::For => "for",
                ReservedWord::In => "in",
                ReservedWord::If => "if",
                ReservedWord::Else => "else",
                ReservedWord::From => "from",
                ReservedWord::Break => "break",
                ReservedWord::Continue => "continue",
            }),
            Self::Punctuation(punct) => match punct {
                Punctuation::Comma => f.write_char(','),
                Punctuation::Dot => f.write_char('.'),
                Punctuation::Ellipsis => f.write_str("..."),
                Punctuation::RArrow => f.write_str("->"),
                Punctuation::FatArrow => f.write_str("=>"),
                Punctuation::DoubleSlash => f.write_str("//"),
                Punctuation::DoubleQuote => f.write_char('"'),
                Punctuation::Colon => f.write_char(':'),
                Punctuation::Semi => f.write_char(';'),
                Punctuation::Question => f.write_char('?'),
                Punctuation::Pound => f.write_char('#'),
            },
            Self::Operator(operator) => match operator {
                Operator::And => f.write_char('&'),
                Operator::AndAnd => f.write_str("&&"),
                Operator::Star => f.write_char('*'),
                Operator::StarEq => f.write_str("*="),
                Operator::Slash => f.write_char('/'),
                Operator::SlashEq => f.write_str("/="),
                Operator::Or => f.write_char('|'),
                Operator::OrOr => f.write_str("||"),
                Operator::Plus => f.write_char('+'),
                Operator::PlusEq => f.write_str("+="),
                Operator::PlusPlus => f.write_str("++"),
                Operator::Minus => f.write_char('-'),
                Operator::MinusEq => f.write_str("-="),
                Operator::MinusMinus => f.write_str("--"),
                Operator::EqEq => f.write_str("=="),
                Operator::Eq => f.write_char('='),
                Operator::Ne => f.write_str("!="),
                Operator::Le => f.write_str("<="),
                Operator::Ge => f.write_str(">="),
                Operator::Lt => f.write_char('<'),
                Operator::Gt => f.write_char('>'),
                Operator::Not => f.write_char('!'),
                Operator::Range => f.write_str(".."),
                Operator::RangeInclusive => f.write_str("..="),
                Operator::Percent => f.write_char('%'),
                Operator::PercentEq => f.write_str("%="),
            },
        }
    }
}
