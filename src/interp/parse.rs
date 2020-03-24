use crate::interp::*;

/// The Parser.
///
/// Left-recursive descent parser. Grammar w. precedence looks like:
/// ```
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
/// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
/// multiplication → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "false" | "true" | "nil"
///                               | "(" expression ")" ;
/// ```
pub struct Parser {}

type RuleMatchResult<'a> = Result<(Box<Expr>, Option<&'a [Token<'a>]>), Error>;
type GrammarRule<'a> = fn(&'a [Token]) -> RuleMatchResult<'a>;

impl Parser {
    // TODO: synchronise errors at statement boundaries.

    const UNMATCHED_LEFT_PAREN: &'static str = "Unmatched left paren";

    /// Construct a new Parser. Unused for now.
    pub fn new() -> Parser {
        Parser {}
    }

    /// Parse a sequence of tokens.
    /// Returns either the root AST node or an error.
    pub fn parse(tokens: &[Token]) -> Result<Box<Expr>, Error> {
        let (expr, rest) = Parser::expression(tokens)?;
        
        let rest = rest.ok_or(Error { line: tokens[tokens.len() - 1].line, message: String::from("Missing EOF. THIS SHOULD NOT HAPPEN") })?;
        match &rest[0].lexeme {
            Lexeme::EOF => Ok(expr),
            _ => Err(Error { line: rest[0].line, message: String::from("Unexpected token ") + &rest[0].span.escape_debug().to_string() })
        }
    }

    fn expression<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // expression → equality
        Parser::equality(tokens)
    }

    fn equality<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // equality → comparison ( ( "!=" | "==" ) comparison )*
        Parser::binop_leftassoc(tokens, |l: &Lexeme| match l {
            Lexeme::DoubleEqual => Some(BinaryOp::Equals),
            Lexeme::BangEqual => Some(BinaryOp::NotEquals),
            _ => None,
        }, Parser::comparison)
    }

    fn comparison<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )*
        Parser::binop_leftassoc(tokens, |l: &Lexeme| match l {
            Lexeme::Greater => Some(BinaryOp::Greater),
            Lexeme::GreaterEqual => Some(BinaryOp::GreaterEqual),
            Lexeme::Less => Some(BinaryOp::Less),
            Lexeme::LessEqual => Some(BinaryOp::LessEqual),
            _ => None,
        }, Parser::addition)
    }

    fn addition<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // addition → multiplication ( ( "-" | "+" ) multiplication )*
        Parser::binop_leftassoc(tokens, |l: &Lexeme| match l {
            Lexeme::Plus=> Some(BinaryOp::Add),
            Lexeme::Minus => Some(BinaryOp::Subtract),
            _ => None,
        }, Parser::multiplication)
    }

    fn multiplication<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // multiplication → unary ( ( "/" | "*" ) unary )*
        Parser::binop_leftassoc(tokens, |l: &Lexeme| match l {
            Lexeme::Star => Some(BinaryOp::Multiply),
            Lexeme::Slash => Some(BinaryOp::Divide),
            _ => None,
        }, Parser::unary)
    }

    // Parses a left-associative series of binary operators:
    //   binop_leftassoc → descend ( ( op_matcher ) descend)*
    // where op_matcher takes a Lexeme and figures out its BinaryOp variant,
    // if one exists.
    fn binop_leftassoc<'a, F>(tokens: &'a [Token], op_matcher: F, descend: GrammarRule<'a>)
        -> RuleMatchResult<'a>
        where F: Fn(&Lexeme) -> Option<BinaryOp>
    {
        // 1. Parse the lhs expr.
        let (mut expr, mut rest_tokens) = descend(tokens)?;

        // 2. While there are tokens to the right of the lhs:
        while let Some(rest_) = rest_tokens {
            // 3. Check the next token is a valid operator.
            let op_token = &rest_[0];
            if let Some(op) = op_matcher(&op_token.lexeme) {
                // 4. Check the rhs tokens exist.
                let rhs_slice = rest_.get(1..).ok_or(Error {
                    line: op_token.line,
                    message: String::from("Expected rhs for binary op"),
                })?;
                // 5. Parse the rhs expr.
                let (rhs, rest) = descend(rhs_slice)?;
                // 6. Construct an expr of the (op, lhs, rhs) and call this the lhs.
                expr = Box::new(Expr::BinOp(op, expr, rhs));
                rest_tokens = rest; // whatever remains for the next iteration.
            } else {
                break; // 3.5. Stop parsing if not.
            }
        }

        Ok((expr, rest_tokens))
    }

    fn unary<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // unary → ( "!" | "-" ) unary | primary ;
        let token = &tokens[0];
        let op = match &token.lexeme {
            Lexeme::Bang => Some(UnaryOp::LogicalNot),
            Lexeme::Minus => Some(UnaryOp::Negate),
            _ => None,
        };

        if let Some(op_) = op {
            let operand_slice = tokens.get(1..).ok_or(Error {
                line: token.line,
                message: String::from("Expected operand for unary op") + &token.span.escape_debug().to_string(),
            })?;
            let (rhs, rest) = Parser::unary(operand_slice)?;
            Ok((Box::new(Expr::UnOp(op_, rhs)), rest))
        } else {
            Parser::primary(tokens)
        }
    }

    fn primary<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // primary → NUMBER | STRING | "false" | "true" | "nil" | "(" expression ")"
        let token = &tokens[0];
        let rest = tokens.get(1..);
        let rule_match = match &token.lexeme {
            Lexeme::Number(v) => Some((Box::new(Expr::Number(*v)), rest)),
            Lexeme::StringLiteral(s) => Some((Box::new(Expr::EString(s.clone())), rest)),
            Lexeme::False => Some((Box::new(Expr::Bool(false)), rest)),
            Lexeme::True => Some((Box::new(Expr::Bool(true)), rest)),
            Lexeme::LeftParen => {
                // Make sure there are tokens left for us to parse.
                let rest = rest.ok_or(Parser::unmatched_paren_err(token.line))?;
                // Parse the expression in the parens group.
                let (expr, rest_) = Parser::parens_group(rest)?;
                Some((Box::new(Expr::Group(expr)), rest_))
            },
            _ => None,
        };

        rule_match.ok_or(Error {
            line: token.line,
            message: String::from("Expected literal or group, found ")
                + &token.span.escape_debug().to_string(),
        })
    }

    fn parens_group<'a>(tokens: &'a [Token]) -> RuleMatchResult<'a> {
        // Try to parse the expression.
        let (expr, rest) = Parser::expression(tokens)?;

        // Make sure there are still tokens left.
        let rest = rest.ok_or(Parser::unmatched_paren_err(tokens[0].line))?;
        let token = &rest[0];
        match &token.lexeme {
            // Check this token is a closing parenthesis.
            Lexeme::RightParen => Ok((expr, rest.get(1..))),
            _ => Err(Parser::unmatched_paren_err(token.line)),
        }
    }

    fn unmatched_paren_err(line: usize) -> Error {
        Error {
            line,
            message: String::from(Parser::UNMATCHED_LEFT_PAREN)
        }
    }
}

#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_primary() -> Result<(), Error> {
        let mut scanner = Scanner::new();
        let tokens = scanner.scan("true or false").unwrap();

        assert_eq!(
            Parser::primary(&tokens)?,
            (Box::new(Expr::Bool(true)), Some(&tokens[1..]))
        );
        Ok(())
    }

    #[test]
    fn test_unary() -> Result<(), Error> {
        let mut scanner = Scanner::new();
        let tokens = scanner.scan("!true").unwrap();

        assert_eq!(
            Parser::unary(&tokens)?,
            (
                Box::new(Expr::UnOp(UnaryOp::LogicalNot, Box::new(Expr::Bool(true)))),
                Some(&tokens[2..])
            )
        );

        let tokens = scanner.scan("-5 + 2").unwrap();

        assert_eq!(
            Parser::unary(&tokens)?,
            (
                Box::new(Expr::UnOp(UnaryOp::Negate, Box::new(Expr::Number(5.0)))),
                Some(&tokens[2..])
            )
        );
        Ok(())
    }

    #[test]
    fn test_multiplication() -> Result<(), Error> {
        let mut scanner = Scanner::new();

        let tokens = scanner.scan("5 * 2").unwrap();
        assert_eq!(
            Parser::multiplication(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::Multiply, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );
        let tokens = scanner.scan("5 / 2").unwrap();
        assert_eq!(
            Parser::multiplication(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::Divide, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );

        Ok(())
    }

    #[test]
    fn test_addition() -> Result<(), Error> {
        let mut scanner = Scanner::new();

        let tokens = scanner.scan("5 + 2").unwrap();
        assert_eq!(
            Parser::addition(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::Add, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );

        let tokens = scanner.scan("5 - 2").unwrap();
        assert_eq!(
            Parser::addition(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::Subtract, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );
        Ok(())
    }

    #[test]
    fn test_comparison() -> Result<(), Error> {
        let mut scanner = Scanner::new();

        let tokens = scanner.scan("5 < 2").unwrap();
        assert_eq!(
            Parser::comparison(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::Less, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );
        let tokens = scanner.scan("5 > 2").unwrap();
        assert_eq!(
            Parser::comparison(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::Greater, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );
        let tokens = scanner.scan("5 <= 2").unwrap();
        assert_eq!(
            Parser::comparison(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::LessEqual, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );
        let tokens = scanner.scan("5 >= 2").unwrap();
        assert_eq!(
            Parser::comparison(&tokens)?,
            (
                Box::new(Expr::BinOp(BinaryOp::GreaterEqual, Box::new(Expr::Number(5.0)), Box::new(Expr::Number(2.0)))),
                Some(&tokens[3..])
            )
        );

        Ok(())
    }
}
