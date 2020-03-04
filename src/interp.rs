use std::fmt;

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

pub struct Scanner {
    current_line: usize,
}

fn empty_to_none<'a>(option_str: Option<&'a str>) -> Option<&'a str> {
    option_str.filter(|s| !s.is_empty())
}

impl Scanner {
    pub fn new() -> Scanner {
        Scanner { current_line: 0 }
    }

    pub fn scan<'a>(&mut self, source: &'a str) -> Result<Vec<Token<'a>>, Vec<Error>> {
        let mut tokens: Vec<Token<'a>> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();

        let mut scan_head = empty_to_none(Some(source));
        while let Some(scan_input) = scan_head.map(Scanner::eat_whitespace) {
            if scan_input.starts_with("\n") {
                self.current_line += 1;
                scan_head = Scanner::advance_one(scan_input).1;
            } else if scan_input.starts_with(Lexeme::Hash.as_str().unwrap()) {
                match scan_input.find(|c: char| c == '\n') {
                    Some(i) => scan_head = scan_input.get(i..),
                    None => scan_head = None,
                };
            } else {
                let (token_res, rest) = self.scan_token(scan_input);
                match token_res {
                    Ok(t) => tokens.push(t),
                    Err(e) => errors.push(e),
                }
                scan_head = rest;
            }
        }
        tokens.push(Token::new(
            Lexeme::EOF,
            &source[source.len()..],
            self.current_line,
        ));

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn scan_token<'a>(&mut self, source: &'a str) -> (Result<Token<'a>, Error>, Option<&'a str>) {
        let prefix_matcher = Scanner::match_prefix(source);
        let symbol_matcher = |lex: &Lexeme| {
            lex.as_str()
                .and_then(|s| prefix_matcher(s))
                .map(|(s, rest)| (Token::new(lex.clone(), s, self.current_line), rest))
        };

        // try matching to a symbol lexeme, returning the first match
        for lex in &Lexeme::SYMBOL_LEXEMES {
            if let Some((token, rest)) = symbol_matcher(lex) {
                return (Ok(token), rest);
            }
        }

        let (span, rest) = Scanner::advance_one(source);
        match span {
            '"' => self.scan_string(rest),
            _ if span.is_ascii_digit() => self.scan_number(source),
            _ if span.is_alphabetic() => self.scan_literal(source),
            _ => (
                Err(Error {
                    line: self.current_line,
                    message: String::from("Unexpected token : ")
                        + &source[..span.len_utf8()].escape_debug().to_string(),
                }),
                rest,
            ),
        }
    }

    fn is_valid_literal(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    // Given a source starting at (and including) an alphabetic char, scan all following
    // alphanumeric chars as a name literal. If the name is reserved, return the appropriate Lexeme
    // for it.
    fn scan_literal<'a>(&self, source: &'a str) -> (Result<Token<'a>, Error>, Option<&'a str>) {
        let end = source
            .find(|c| !Scanner::is_valid_literal(c))
            .unwrap_or(source.len());
        let span = &source[..end];
        (
            Ok(Token::new(
                Lexeme::from_literal(span),
                span,
                self.current_line,
            )),
            empty_to_none(source.get(end..)),
        )
    }

    // Given a source starting at (and including) a digit, try to parse it into a float.
    // All numbers are floats internally.
    fn scan_number<'a>(&self, source: &'a str) -> (Result<Token<'a>, Error>, Option<&'a str>) {
        // find the first char that isn't a digit or a .
        let end = source
            .find(|c: char| !(c.is_ascii_digit() || c == '.'))
            .unwrap_or(source.len());
        let token_res = source[..end]
            .parse::<f64>()
            .map(|n| Token::new(Lexeme::Number(n), &source[..end], self.current_line))
            .map_err(|e| Error {
                line: self.current_line,
                message: e.to_string(),
            });
        (token_res, empty_to_none(source.get(end..)))
    }

    // Given a source starting at (but not including) a double-quote delimiter,
    // scan the string literal or return an error if unmatched quote.
    // Does not support escape sequences.
    fn scan_string<'a>(
        &mut self,
        source_opt: Option<&'a str>,
    ) -> (Result<Token<'a>, Error>, Option<&'a str>) {
        let unmatched_quote_err = Err(Error {
            line: self.current_line,
            message: String::from("Unmatched '\"'"),
        });
        match source_opt {
            Some(source) => match source.find('"') {
                Some(i) => {
                    let token = Token::new(
                        Lexeme::StringLiteral(String::from(&source[..i])),
                        &source[..i],
                        self.current_line,
                    );
                    self.current_line += token.span.lines().count() - 1;
                    (Ok(token), source.get((i + 1)..)) // skips over end quote
                }
                None => (unmatched_quote_err, source_opt),
            },
            None => (unmatched_quote_err, source_opt),
        }
    }

    // Given a source string, return a closure that matches on a given string prefix.
    fn match_prefix<'a>(source: &'a str) -> impl Fn(&'a str) -> Option<(&'a str, Option<&'a str>)> {
        move |s| {
            if source.starts_with(s) {
                Some((&source[..s.len()], empty_to_none(source.get(s.len()..))))
            } else {
                None
            }
        }
    }

    fn eat_whitespace<'a>(source: &'a str) -> &'a str {
        source.trim_start_matches(|c: char| c.is_whitespace() && c != '\n')
    }

    fn peek<'a>(source: &'a str) -> Option<char> {
        source.chars().next()
    }

    fn advance_one<'a>(source: &'a str) -> (char, Option<&'a str>) {
        let peek = Scanner::peek(source).unwrap();
        (peek, empty_to_none(source.get(peek.len_utf8()..)))
    }
}
