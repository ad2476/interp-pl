use std::fmt;

mod scan;
pub use scan::Scanner;

pub struct Error {
    line: usize,
    message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.line, self.message)
    }
}

#[derive(Debug, Clone)]
pub enum Lexeme {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    DoubleEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Hash,

    Identifier(String),
    StringLiteral(String),
    Number(f64),

    And,
    Or,
    If,
    Else,
    True,
    False,
    Fun,
    For,
    Return,
    Let,
    While,

    EOF,
}

impl Lexeme {
    // n.b. Lexemes must be checked in order of maximal munch.
    // for symbols, e.g. '!=' must be checked before '!' to avoid
    // consuming '!','=' instead of '!='.
    const SYMBOL_LEXEMES: [Lexeme; 19] = [
        Lexeme::BangEqual,
        Lexeme::DoubleEqual,
        Lexeme::GreaterEqual,
        Lexeme::LessEqual,
        Lexeme::LeftParen,
        Lexeme::RightParen,
        Lexeme::LeftBrace,
        Lexeme::RightBrace,
        Lexeme::Comma,
        Lexeme::Dot,
        Lexeme::Minus,
        Lexeme::Plus,
        Lexeme::Semicolon,
        Lexeme::Slash,
        Lexeme::Star,
        Lexeme::Bang,
        Lexeme::Equal,
        Lexeme::Greater,
        Lexeme::Less,
    ];

    pub fn as_str(&self) -> Option<&'static str> {
        match &self {
            Lexeme::LeftParen => Some("("),
            Lexeme::RightParen => Some(")"),
            Lexeme::LeftBrace => Some("["),
            Lexeme::RightBrace => Some("]"),
            Lexeme::Comma => Some(","),
            Lexeme::Dot => Some("."),
            Lexeme::Minus => Some("-"),
            Lexeme::Plus => Some("+"),
            Lexeme::Semicolon => Some(";"),
            Lexeme::Slash => Some("/"),
            Lexeme::Star => Some("*"),
            Lexeme::Bang => Some("!"),
            Lexeme::BangEqual => Some("!="),
            Lexeme::Equal => Some("="),
            Lexeme::DoubleEqual => Some("=="),
            Lexeme::Greater => Some(">"),
            Lexeme::GreaterEqual => Some(">="),
            Lexeme::Less => Some("<"),
            Lexeme::LessEqual => Some("<="),
            Lexeme::Hash => Some("#"),

            Lexeme::And => Some("and"),
            Lexeme::Or => Some("or"),
            Lexeme::If => Some("if"),
            Lexeme::Else => Some("else"),
            Lexeme::True => Some("true"),
            Lexeme::False => Some("false"),
            Lexeme::Fun => Some("fun"),
            Lexeme::For => Some("for"),
            Lexeme::Return => Some("return"),
            Lexeme::Let => Some("let"),
            Lexeme::While => Some("while"),

            _ => None,
        }
    }

    pub fn from_literal(s: &str) -> Lexeme {
        match s {
            "and" => Lexeme::And,
            "or" => Lexeme::Or,
            "if" => Lexeme::If,
            "else" => Lexeme::Else,
            "true" => Lexeme::True,
            "false" => Lexeme::False,
            "fun" => Lexeme::Fun,
            "for" => Lexeme::For,
            "return" => Lexeme::Return,
            "let" => Lexeme::Let,
            "while" => Lexeme::While,
            _ => Lexeme::Identifier(String::from(s)),
        }
    }

    pub fn is_literal(&self) -> bool {
        match &self {
            Lexeme::Identifier(_) => true,
            Lexeme::StringLiteral(_) => true,
            Lexeme::Number(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    lexeme: Lexeme,
    span: &'a str,
    line: usize,
}

impl Token<'_> {
    pub fn new<'a>(lexeme: Lexeme, span: &'a str, line: usize) -> Token<'a> {
        Token { lexeme, span, line }
    }
}

#[derive(Debug, Clone)]
enum UnaryOp {
    LogicalNot,
    Negate,
}

#[derive(Debug, Clone)]
enum BinaryOp {
    Equals,
    NotEquals,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    EString(String),
    Bool(bool),
    Group(Box<Expr>),
    UnOp(UnaryOp, Box<Expr>),
    BinOp(BinaryOp, Box<Expr>, Box<Expr>),
}
