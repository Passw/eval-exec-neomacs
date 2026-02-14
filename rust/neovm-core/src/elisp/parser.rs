//! Lisp reader / parser.
//!
//! Supports: integers, floats, strings (with escapes), symbols, keywords,
//! character literals (?a), lists, dotted pairs, vectors, quote ('), function (#'),
//! backquote (`), unquote (,), splice (,@), line comments (;), block comments (#|..|#).

use super::expr::{Expr, ParseError};

pub fn parse_forms(input: &str) -> Result<Vec<Expr>, ParseError> {
    let mut parser = Parser::new(input);
    let mut forms = Vec::new();
    while parser.skip_ws_and_comments() {
        forms.push(parser.parse_expr()?);
    }
    Ok(forms)
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    // -- Whitespace & comments -----------------------------------------------

    fn skip_ws_and_comments(&mut self) -> bool {
        loop {
            let Some(ch) = self.current() else {
                return false;
            };
            if ch.is_ascii_whitespace() {
                self.bump();
                continue;
            }
            if ch == ';' {
                // Line comment
                while let Some(c) = self.current() {
                    self.bump();
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            if ch == '#' && self.peek_at(1) == Some('|') {
                // Block comment #| ... |#
                self.bump(); // #
                self.bump(); // |
                let mut depth = 1;
                while depth > 0 {
                    match self.current() {
                        None => return false,
                        Some('#') if self.peek_at(1) == Some('|') => {
                            self.bump();
                            self.bump();
                            depth += 1;
                        }
                        Some('|') if self.peek_at(1) == Some('#') => {
                            self.bump();
                            self.bump();
                            depth -= 1;
                        }
                        _ => self.bump(),
                    }
                }
                continue;
            }
            return true;
        }
    }

    // -- Main parse dispatch -------------------------------------------------

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.skip_ws_and_comments();
        let Some(ch) = self.current() else {
            return Err(self.error("unexpected end of input"));
        };

        match ch {
            '(' => self.parse_list_or_dotted(),
            ')' => Err(self.error("unexpected ')'")),
            '[' => self.parse_vector(),
            '\'' => {
                self.bump();
                let quoted = self.parse_expr()?;
                Ok(Expr::List(vec![Expr::Symbol("quote".into()), quoted]))
            }
            '`' => {
                self.bump();
                let quoted = self.parse_expr()?;
                Ok(Expr::List(vec![Expr::Symbol("\\`".into()), quoted]))
            }
            ',' => {
                self.bump();
                if self.current() == Some('@') {
                    self.bump();
                    let expr = self.parse_expr()?;
                    Ok(Expr::List(vec![Expr::Symbol("\\,@".into()), expr]))
                } else {
                    let expr = self.parse_expr()?;
                    Ok(Expr::List(vec![Expr::Symbol("\\,".into()), expr]))
                }
            }
            '"' => self.parse_string(),
            '?' => self.parse_char_literal(),
            '#' => self.parse_hash_syntax(),
            _ => self.parse_atom(),
        }
    }

    // -- Lists and dotted pairs ----------------------------------------------

    fn parse_list_or_dotted(&mut self) -> Result<Expr, ParseError> {
        self.expect('(')?;
        let mut items = Vec::new();
        loop {
            self.skip_ws_and_comments();
            match self.current() {
                Some(')') => {
                    self.bump();
                    return Ok(Expr::List(items));
                }
                Some('.') if self.is_dot_separator() => {
                    // Dotted pair
                    self.bump(); // consume '.'
                    let cdr = self.parse_expr()?;
                    self.skip_ws_and_comments();
                    match self.current() {
                        Some(')') => {
                            self.bump();
                            return Ok(Expr::DottedList(items, Box::new(cdr)));
                        }
                        _ => return Err(self.error("expected ')' after dotted pair")),
                    }
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error("unterminated list")),
            }
        }
    }

    /// Check if current '.' is a dot separator (not part of a number like 1.5).
    fn is_dot_separator(&self) -> bool {
        // A dot is a separator if the next char is whitespace, ')', or EOF
        match self.peek_at(1) {
            None => true,
            Some(c) => c.is_ascii_whitespace() || c == ')' || c == '(' || c == ';',
        }
    }

    // -- Vectors [1 2 3] ----------------------------------------------------

    fn parse_vector(&mut self) -> Result<Expr, ParseError> {
        self.expect('[')?;
        let mut items = Vec::new();
        loop {
            self.skip_ws_and_comments();
            match self.current() {
                Some(']') => {
                    self.bump();
                    return Ok(Expr::Vector(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error("unterminated vector")),
            }
        }
    }

    // -- Strings "..." -------------------------------------------------------

    fn parse_string(&mut self) -> Result<Expr, ParseError> {
        self.expect('"')?;
        let mut s = String::new();
        loop {
            let Some(ch) = self.current() else {
                return Err(self.error("unterminated string"));
            };
            self.bump();
            match ch {
                '"' => return Ok(Expr::Str(s)),
                '\\' => {
                    let Some(esc) = self.current() else {
                        return Err(self.error("unterminated escape in string"));
                    };
                    self.bump();
                    match esc {
                        'n' => s.push('\n'),
                        'r' => s.push('\r'),
                        't' => s.push('\t'),
                        '\\' => s.push('\\'),
                        '"' => s.push('"'),
                        'a' => s.push('\x07'), // bell
                        'b' => s.push('\x08'), // backspace
                        'f' => s.push('\x0C'), // form feed
                        'e' => s.push('\x1B'), // escape
                        's' => s.push(' '),    // space
                        'd' => s.push('\x7F'), // delete
                        'x' => {
                            let hex = self.read_hex_digits()?;
                            if let Some(c) = char::from_u32(hex) {
                                s.push(c);
                            } else {
                                return Err(self.error("invalid unicode codepoint in \\x escape"));
                            }
                        }
                        'u' => {
                            let hex = self.read_fixed_hex(4)?;
                            if let Some(c) = char::from_u32(hex) {
                                s.push(c);
                            } else {
                                return Err(self.error("invalid unicode codepoint in \\u escape"));
                            }
                        }
                        'U' => {
                            let hex = self.read_fixed_hex(8)?;
                            if let Some(c) = char::from_u32(hex) {
                                s.push(c);
                            } else {
                                return Err(self.error("invalid unicode codepoint in \\U escape"));
                            }
                        }
                        '0'..='7' => {
                            // Octal escape
                            let mut val = (esc as u32) - ('0' as u32);
                            for _ in 0..2 {
                                match self.current() {
                                    Some(c @ '0'..='7') => {
                                        self.bump();
                                        val = val * 8 + (c as u32 - '0' as u32);
                                    }
                                    _ => break,
                                }
                            }
                            if let Some(c) = char::from_u32(val) {
                                s.push(c);
                            }
                        }
                        '\n' => {
                            // Line continuation — skip newline
                        }
                        other => {
                            // Unknown escape — keep the character
                            s.push(other);
                        }
                    }
                }
                other => s.push(other),
            }
        }
    }

    fn read_hex_digits(&mut self) -> Result<u32, ParseError> {
        let start = self.pos;
        while let Some(c) = self.current() {
            if c.is_ascii_hexdigit() {
                self.bump();
            } else {
                // Emacs \\x terminates at first non-hex or at ';'
                if c == ';' {
                    self.bump(); // consume terminating semicolon
                }
                break;
            }
        }
        let hex_str = &self.input[start..self.pos].trim_end_matches(';');
        if hex_str.is_empty() {
            return Err(self.error("expected hex digits after \\x"));
        }
        u32::from_str_radix(hex_str, 16).map_err(|_| self.error("invalid hex escape"))
    }

    fn read_fixed_hex(&mut self, count: usize) -> Result<u32, ParseError> {
        let start = self.pos;
        for _ in 0..count {
            match self.current() {
                Some(c) if c.is_ascii_hexdigit() => self.bump(),
                _ => return Err(self.error(&format!("expected {} hex digits", count))),
            }
        }
        u32::from_str_radix(&self.input[start..self.pos], 16)
            .map_err(|_| self.error("invalid hex escape"))
    }

    // -- Character literals ?a -----------------------------------------------

    fn parse_char_literal(&mut self) -> Result<Expr, ParseError> {
        self.expect('?')?;
        let Some(ch) = self.current() else {
            return Err(self.error("expected character after '?'"));
        };
        self.bump();

        if ch == '\\' {
            // Escape sequence
            let Some(esc) = self.current() else {
                return Err(self.error("unterminated character escape"));
            };
            self.bump();
            let c = match esc {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '\'' => '\'',
                '"' => '"',
                'a' => '\x07',
                'b' => '\x08',
                'f' => '\x0C',
                'e' => '\x1B',
                's' => ' ',
                'd' => '\x7F',
                'x' => {
                    let val = self.read_hex_digits()?;
                    char::from_u32(val).ok_or_else(|| self.error("invalid unicode codepoint"))?
                }
                'u' => {
                    let val = self.read_fixed_hex(4)?;
                    char::from_u32(val).ok_or_else(|| self.error("invalid unicode codepoint"))?
                }
                'U' => {
                    let val = self.read_fixed_hex(8)?;
                    char::from_u32(val).ok_or_else(|| self.error("invalid unicode codepoint"))?
                }
                '0'..='7' => {
                    let mut val = (esc as u32) - ('0' as u32);
                    for _ in 0..2 {
                        match self.current() {
                            Some(c @ '0'..='7') => {
                                self.bump();
                                val = val * 8 + (c as u32 - '0' as u32);
                            }
                            _ => break,
                        }
                    }
                    char::from_u32(val).ok_or_else(|| self.error("invalid octal character"))?
                }
                // Modifier keys (Emacs-style)
                'C' if self.current() == Some('-') => {
                    self.bump(); // -
                    let Some(base) = self.current() else {
                        return Err(self.error("expected char after \\C-"));
                    };
                    self.bump();
                    // Control character
                    char::from_u32((base as u32) & 0x1F)
                        .ok_or_else(|| self.error("invalid control character"))?
                }
                'M' if self.current() == Some('-') => {
                    self.bump(); // -
                    let Some(base) = self.current() else {
                        return Err(self.error("expected char after \\M-"));
                    };
                    self.bump();
                    // Meta character (set bit 27)
                    char::from_u32((base as u32) | (1 << 27)).unwrap_or(base)
                }
                'S' if self.current() == Some('-') => {
                    self.bump(); // -
                    let Some(base) = self.current() else {
                        return Err(self.error("expected char after \\S-"));
                    };
                    self.bump();
                    // Shift modifier (set bit 25)
                    char::from_u32((base as u32) | (1 << 25)).unwrap_or(base)
                }
                other => other,
            };
            Ok(Expr::Char(c))
        } else {
            Ok(Expr::Char(ch))
        }
    }

    // -- Hash syntax #' #( etc -----------------------------------------------

    fn parse_hash_syntax(&mut self) -> Result<Expr, ParseError> {
        self.expect('#')?;
        let Some(ch) = self.current() else {
            return Err(self.error("unexpected end after '#'"));
        };

        match ch {
            '\'' => {
                // #'function
                self.bump();
                let expr = self.parse_expr()?;
                Ok(Expr::List(vec![Expr::Symbol("function".into()), expr]))
            }
            '(' => {
                // #(...) — historical compiled-function literal.
                let vector = self.parse_vector_paren()?;
                Ok(Expr::List(vec![Expr::Symbol("quote".into()), vector]))
            }
            '[' => {
                // #[...] — compiled-function literal in .elc.
                let vector = self.parse_vector()?;
                Ok(Expr::List(vec![
                    Expr::Symbol("byte-code-literal".into()),
                    vector,
                ]))
            }
            '@' => {
                // #@N<bytes> — reader skip used by .elc for inline data blocks.
                self.parse_hash_skip_bytes()
            }
            '$' => {
                // #$ — expands to the current load file name during read.
                // We model it as the special variable symbol and let eval resolve it.
                self.bump();
                Ok(Expr::Symbol("load-file-name".into()))
            }
            'b' | 'B' => {
                // #b... binary integer
                self.bump();
                self.parse_radix_number(2)
            }
            'o' | 'O' => {
                // #o... octal integer
                self.bump();
                self.parse_radix_number(8)
            }
            'x' | 'X' => {
                // #x... hex integer
                self.bump();
                self.parse_radix_number(16)
            }
            's' => {
                // #s(hash-table ...) — simplified reader
                self.bump();
                if self.current() == Some('(') {
                    self.parse_hash_table_literal()
                } else {
                    Err(self.error("expected '(' after #s"))
                }
            }
            _ => {
                // Unknown # syntax — try to read as #N or just return error
                Err(self.error(&format!("unknown reader syntax: #{}", ch)))
            }
        }
    }

    fn parse_vector_paren(&mut self) -> Result<Expr, ParseError> {
        self.expect('(')?;
        let mut items = Vec::new();
        loop {
            self.skip_ws_and_comments();
            match self.current() {
                Some(')') => {
                    self.bump();
                    return Ok(Expr::Vector(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error("unterminated vector")),
            }
        }
    }

    fn parse_hash_skip_bytes(&mut self) -> Result<Expr, ParseError> {
        self.expect('@')?;
        let len = self.parse_decimal_usize()?;
        self.skip_exact_bytes(len)?;
        self.parse_expr()
    }

    fn parse_radix_number(&mut self, radix: u32) -> Result<Expr, ParseError> {
        let start = self.pos;
        let negative = if self.current() == Some('-') {
            self.bump();
            true
        } else if self.current() == Some('+') {
            self.bump();
            false
        } else {
            false
        };

        while let Some(c) = self.current() {
            if c.is_digit(radix) || c == '_' {
                self.bump();
            } else {
                break;
            }
        }

        let digits: String = self.input[start..self.pos]
            .chars()
            .filter(|c| *c != '_' && *c != '-' && *c != '+')
            .collect();
        if digits.is_empty() {
            return Err(self.error("expected digits after radix prefix"));
        }

        let val =
            i64::from_str_radix(&digits, radix).map_err(|_| self.error("invalid radix number"))?;
        Ok(Expr::Int(if negative { -val } else { val }))
    }

    fn parse_hash_table_literal(&mut self) -> Result<Expr, ParseError> {
        // #s(hash-table size N test T data (k1 v1 k2 v2 ...))
        // For now, parse the entire thing as a list and let eval handle it
        let list = self.parse_list_or_dotted()?;
        Ok(Expr::List(vec![
            Expr::Symbol("make-hash-table-from-literal".into()),
            Expr::List(vec![Expr::Symbol("quote".into()), list]),
        ]))
    }

    // -- Atoms (numbers, symbols) --------------------------------------------

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let start = self.pos;
        while let Some(ch) = self.current() {
            if ch.is_ascii_whitespace()
                || matches!(ch, '(' | ')' | '[' | ']' | '\'' | '`' | ',' | '"' | ';')
            {
                break;
            }
            self.bump();
        }

        if self.pos == start {
            return Err(self.error("expected atom"));
        }

        let token = &self.input[start..self.pos];

        // Keywords (:foo)
        if token.starts_with(':') && token.len() > 1 {
            return Ok(Expr::Keyword(token.to_string()));
        }

        // Try integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr::Int(n));
        }

        // Try float — handles 1.5, 1e10, .5, 1.5e-3, etc.
        if looks_like_float(token) {
            if let Ok(f) = token.parse::<f64>() {
                return Ok(Expr::Float(f));
            }
        }

        // Hex integer: 0xFF
        if token.starts_with("0x") || token.starts_with("0X") {
            if let Ok(n) = i64::from_str_radix(&token[2..], 16) {
                return Ok(Expr::Int(n));
            }
        }

        // Boolean
        if token == "nil" || token == "t" {
            return Ok(Expr::Symbol(token.to_string()));
        }

        Ok(Expr::Symbol(token.to_string()))
    }

    // -- Helpers -------------------------------------------------------------

    fn expect(&mut self, expected: char) -> Result<(), ParseError> {
        match self.current() {
            Some(ch) if ch == expected => {
                self.bump();
                Ok(())
            }
            _ => Err(self.error(&format!("expected '{}'", expected))),
        }
    }

    fn current(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.input[self.pos..].chars().nth(offset)
    }

    fn bump(&mut self) {
        if let Some(ch) = self.current() {
            self.pos += ch.len_utf8();
        }
    }

    fn error(&self, message: &str) -> ParseError {
        ParseError {
            position: self.pos,
            message: message.to_string(),
        }
    }

    fn parse_decimal_usize(&mut self) -> Result<usize, ParseError> {
        let start = self.pos;
        while matches!(self.current(), Some(c) if c.is_ascii_digit()) {
            self.bump();
        }
        if self.pos == start {
            return Err(self.error("expected decimal length"));
        }
        self.input[start..self.pos]
            .parse::<usize>()
            .map_err(|_| self.error("invalid decimal length"))
    }

    fn skip_exact_bytes(&mut self, len: usize) -> Result<(), ParseError> {
        let Some(new_pos) = self.pos.checked_add(len) else {
            return Err(self.error("byte skip overflow"));
        };
        if new_pos > self.input.len() {
            return Err(self.error("byte skip past end of input"));
        }
        if !self.input.is_char_boundary(new_pos) {
            return Err(self.error("byte skip ended mid-character"));
        }
        self.pos = new_pos;
        Ok(())
    }
}

fn looks_like_float(s: &str) -> bool {
    // Must contain a decimal point or exponent marker, and not be purely a symbol
    let s = if s.starts_with('+') || s.starts_with('-') {
        &s[1..]
    } else {
        s
    };
    if s.is_empty() {
        return false;
    }
    // Must start with a digit or '.'
    let first = s.as_bytes()[0];
    if !first.is_ascii_digit() && first != b'.' {
        return false;
    }
    s.contains('.') || s.contains('e') || s.contains('E')
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_integers() {
        let forms = parse_forms("42 -7 0").unwrap();
        assert_eq!(forms, vec![Expr::Int(42), Expr::Int(-7), Expr::Int(0)]);
    }

    #[test]
    fn parse_floats() {
        let forms = parse_forms("3.14 1e10 .5 -2.5").unwrap();
        assert_eq!(
            forms,
            vec![
                Expr::Float(3.14),
                Expr::Float(1e10),
                Expr::Float(0.5),
                Expr::Float(-2.5),
            ]
        );
    }

    #[test]
    fn parse_strings() {
        let forms = parse_forms(r#""hello" "world\n" "tab\there" "quote\"d""#).unwrap();
        assert_eq!(
            forms,
            vec![
                Expr::Str("hello".into()),
                Expr::Str("world\n".into()),
                Expr::Str("tab\there".into()),
                Expr::Str("quote\"d".into()),
            ]
        );
    }

    #[test]
    fn parse_string_hex_escape() {
        let forms = parse_forms(r#""\x41""#).unwrap();
        assert_eq!(forms, vec![Expr::Str("A".into())]);
    }

    #[test]
    fn parse_char_literals() {
        let forms = parse_forms("?a ?\\n ?\\t").unwrap();
        assert_eq!(
            forms,
            vec![Expr::Char('a'), Expr::Char('\n'), Expr::Char('\t')]
        );
    }

    #[test]
    fn parse_keywords() {
        let forms = parse_forms(":test :size").unwrap();
        assert_eq!(
            forms,
            vec![Expr::Keyword(":test".into()), Expr::Keyword(":size".into()),]
        );
    }

    #[test]
    fn parse_lists() {
        let forms = parse_forms("(+ 1 2) ()").unwrap();
        assert_eq!(
            forms,
            vec![
                Expr::List(vec![Expr::Symbol("+".into()), Expr::Int(1), Expr::Int(2),]),
                Expr::List(vec![]),
            ]
        );
    }

    #[test]
    fn parse_dotted_pair() {
        let forms = parse_forms("(a . b)").unwrap();
        assert_eq!(
            forms,
            vec![Expr::DottedList(
                vec![Expr::Symbol("a".into())],
                Box::new(Expr::Symbol("b".into())),
            )]
        );
    }

    #[test]
    fn parse_vectors() {
        let forms = parse_forms("[1 2 3]").unwrap();
        assert_eq!(
            forms,
            vec![Expr::Vector(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)])]
        );
    }

    #[test]
    fn parse_quote_shorthand() {
        let forms = parse_forms("'foo '(1 2)").unwrap();
        assert_eq!(
            forms,
            vec![
                Expr::List(vec![
                    Expr::Symbol("quote".into()),
                    Expr::Symbol("foo".into())
                ]),
                Expr::List(vec![
                    Expr::Symbol("quote".into()),
                    Expr::List(vec![Expr::Int(1), Expr::Int(2)]),
                ]),
            ]
        );
    }

    #[test]
    fn parse_function_shorthand() {
        let forms = parse_forms("#'car").unwrap();
        assert_eq!(
            forms,
            vec![Expr::List(vec![
                Expr::Symbol("function".into()),
                Expr::Symbol("car".into()),
            ])]
        );
    }

    #[test]
    fn parse_backquote() {
        let forms = parse_forms("`(a ,b ,@c)").unwrap();
        assert_eq!(forms.len(), 1);
    }

    #[test]
    fn parse_hex_literal() {
        let forms = parse_forms("#xff #b1010 #o17").unwrap();
        assert_eq!(forms, vec![Expr::Int(255), Expr::Int(10), Expr::Int(15)]);
    }

    #[test]
    fn parse_line_comment() {
        let forms = parse_forms("42 ; this is a comment\n7").unwrap();
        assert_eq!(forms, vec![Expr::Int(42), Expr::Int(7)]);
    }

    #[test]
    fn parse_block_comment() {
        let forms = parse_forms("42 #| block comment |# 7").unwrap();
        assert_eq!(forms, vec![Expr::Int(42), Expr::Int(7)]);
    }

    #[test]
    fn parse_nested_block_comment() {
        let forms = parse_forms("42 #| outer #| inner |# still outer |# 7").unwrap();
        assert_eq!(forms, vec![Expr::Int(42), Expr::Int(7)]);
    }

    #[test]
    fn parse_bytecode_literal_vector_uses_byte_code_literal_form() {
        let forms = parse_forms("#[(x) \"\\bT\\207\" [x] 1 (#$ . 83)]").unwrap();
        assert_eq!(forms.len(), 1);
        let Expr::List(items) = &forms[0] else {
            panic!("expected byte-code-literal form");
        };
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], Expr::Symbol("byte-code-literal".into()));

        let Expr::Vector(values) = &items[1] else {
            panic!("expected vector body");
        };
        let Expr::DottedList(cons_items, cdr) = &values[4] else {
            panic!("expected source-loc dotted pair");
        };
        assert_eq!(cons_items, &vec![Expr::Symbol("load-file-name".into())]);
        assert_eq!(**cdr, Expr::Int(83));
    }

    #[test]
    fn parse_paren_bytecode_literal_vector_is_quoted() {
        let forms = parse_forms("#((x) \"\\bT\\207\" [x] 1 (#$ . 83))").unwrap();
        assert_eq!(forms.len(), 1);
        let Expr::List(items) = &forms[0] else {
            panic!("expected quoted literal");
        };
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], Expr::Symbol("quote".into()));

        let Expr::Vector(values) = &items[1] else {
            panic!("expected vector body");
        };
        let Expr::DottedList(cons_items, cdr) = &values[4] else {
            panic!("expected source-loc dotted pair");
        };
        assert_eq!(cons_items, &vec![Expr::Symbol("load-file-name".into())]);
        assert_eq!(**cdr, Expr::Int(83));
    }

    #[test]
    fn parse_hash_skip_bytes_reads_next_form() {
        let forms = parse_forms("#@4data42").unwrap();
        assert_eq!(forms, vec![Expr::Int(42)]);
    }

    #[test]
    fn parse_hash_dollar_maps_to_load_file_name_symbol() {
        let forms = parse_forms("#$").unwrap();
        assert_eq!(forms, vec![Expr::Symbol("load-file-name".into())]);
    }
}
