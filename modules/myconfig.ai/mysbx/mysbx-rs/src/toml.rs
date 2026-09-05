// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! A minimal TOML parser for the subset `mysbx` configuration uses
//! (docs/design/config.md).
//!
//! Hand-rolled on purpose: the crate is zero-dependency by design
//! (docs/design/cli.md D5), which keeps `cargoLock` in `nix/mysbx.nix` free
//! of any `outputHashes`. The subset is deliberate, not accidental:
//!
//! * key/value pairs, dotted and quoted keys
//! * tables `[a.b]` and arrays of tables `[[a]]`
//! * basic strings (with the common escapes), literal strings
//! * integers, floats, booleans
//! * arrays (multi-line, trailing comma allowed) and inline tables
//! * `#` comments
//!
//! Everything else — multi-line strings, datetimes — is rejected with a
//! located error instead of being silently misparsed. Configuration that
//! cannot be understood must fail fast (docs/design/config.md D8).

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// A TOML table: keys in sorted order (iteration order is irrelevant for
/// configuration, determinism in test output is not).
pub type Table = BTreeMap<String, Value>;

/// A parsed TOML value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Array(Vec<Value>),
    Table(Table),
}

impl Value {
    /// TOML type name, for schema error messages.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Integer(_) => "integer",
            Value::Float(_) => "float",
            Value::Boolean(_) => "boolean",
            Value::Array(_) => "array",
            Value::Table(_) => "table",
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Value::Integer(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[Value]> {
        match self {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<&Table> {
        match self {
            Value::Table(t) => Some(t),
            _ => None,
        }
    }
}

/// A parse error, located at a 1-based line and column.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub line: usize,
    pub col: usize,
    pub message: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "line {}, column {}: {}",
            self.line, self.col, self.message
        )
    }
}

impl std::error::Error for Error {}

/// Parse a TOML document into its root table.
pub fn parse(input: &str) -> Result<Table, Error> {
    Parser::new(input).document()
}

struct Parser {
    chars: Vec<char>,
    i: usize,
    /// Headers already seen, to reject `[a]` twice.
    seen_headers: BTreeSet<String>,
}

impl Parser {
    fn new(input: &str) -> Self {
        Parser {
            chars: input.chars().collect(),
            i: 0,
            seen_headers: BTreeSet::new(),
        }
    }

    // ---- character helpers ------------------------------------------------

    fn peek(&self) -> Option<char> {
        self.chars.get(self.i).copied()
    }

    fn peek_at(&self, n: usize) -> Option<char> {
        self.chars.get(self.i + n).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek();
        if c.is_some() {
            self.i += 1;
        }
        c
    }

    fn eat(&mut self, want: char) -> bool {
        if self.peek() == Some(want) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    /// 1-based line/column of a character index.
    fn pos(&self, idx: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for c in self.chars.iter().take(idx) {
            if *c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }

    fn err_at<T>(&self, idx: usize, message: impl Into<String>) -> Result<T, Error> {
        let (line, col) = self.pos(idx);
        Err(Error {
            line,
            col,
            message: message.into(),
        })
    }

    fn err<T>(&self, message: impl Into<String>) -> Result<T, Error> {
        self.err_at(self.i, message)
    }

    /// Spaces and tabs only.
    fn skip_inline_ws(&mut self) {
        while matches!(self.peek(), Some(' ') | Some('\t')) {
            self.i += 1;
        }
    }

    /// A `#` comment up to (not including) the newline.
    fn skip_comment(&mut self) {
        if self.peek() == Some('#') {
            while !matches!(self.peek(), None | Some('\n')) {
                self.i += 1;
            }
        }
    }

    /// Whitespace, newlines and comments — used between items.
    fn skip_trivia(&mut self) {
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\n') | Some('\r') => {
                    self.i += 1;
                }
                Some('#') => self.skip_comment(),
                _ => return,
            }
        }
    }

    /// After a value: allow inline whitespace and a comment, then require
    /// end of line or end of input.
    fn expect_eol(&mut self) -> Result<(), Error> {
        self.skip_inline_ws();
        self.skip_comment();
        match self.peek() {
            None => Ok(()),
            Some('\n') => {
                self.i += 1;
                Ok(())
            }
            Some('\r') if self.peek_at(1) == Some('\n') => {
                self.i += 2;
                Ok(())
            }
            Some(c) => self.err(format!("unexpected `{c}` after value")),
        }
    }

    // ---- document --------------------------------------------------------

    fn document(&mut self) -> Result<Table, Error> {
        let mut root = Table::new();
        let mut current: Vec<String> = Vec::new();

        loop {
            self.skip_trivia();
            match self.peek() {
                None => return Ok(root),
                Some('[') => {
                    current = self.header(&mut root)?;
                }
                _ => {
                    let start = self.i;
                    let key = self.key_path()?;
                    self.skip_inline_ws();
                    if !self.eat('=') {
                        return self.err("expected `=` after key");
                    }
                    self.skip_inline_ws();
                    let value = self.value()?;
                    self.expect_eol()?;

                    let mut path = current.clone();
                    path.extend(key);
                    self.insert(&mut root, &path, value, start)?;
                }
            }
        }
    }

    /// `[a.b]` or `[[a.b]]`; returns the new current table path.
    fn header(&mut self, root: &mut Table) -> Result<Vec<String>, Error> {
        let start = self.i;
        self.bump(); // '['
        let array = self.eat('[');
        self.skip_inline_ws();
        let path = self.key_path()?;
        self.skip_inline_ws();
        if !self.eat(']') || (array && !self.eat(']')) {
            return self.err("unterminated table header");
        }
        self.expect_eol()?;

        if array {
            let (parent, last) = path.split_at(path.len() - 1);
            let parent_table = self.descend(root, parent, start)?;
            match parent_table
                .entry(last[0].clone())
                .or_insert_with(|| Value::Array(Vec::new()))
            {
                Value::Array(items) => items.push(Value::Table(Table::new())),
                other => {
                    let name = other.type_name();
                    return self.err_at(
                        start,
                        format!("`{}` is already defined as {name}", path.join(".")),
                    );
                }
            }
        } else {
            let key = path.join(".");
            if !self.seen_headers.insert(key.clone()) {
                return self.err_at(start, format!("table `{key}` is defined twice"));
            }
            self.descend(root, &path, start)?;
        }
        Ok(path)
    }

    /// Walk (creating as needed) to the table at `path`. Descends into the
    /// last element of an array of tables, which is what makes `[[a]]`
    /// followed by `key = ...` work.
    fn descend<'t>(
        &self,
        root: &'t mut Table,
        path: &[String],
        at: usize,
    ) -> Result<&'t mut Table, Error> {
        let mut table = root;
        for (n, seg) in path.iter().enumerate() {
            let entry = table
                .entry(seg.clone())
                .or_insert_with(|| Value::Table(Table::new()));
            table = match entry {
                Value::Table(t) => t,
                Value::Array(items) => match items.last_mut() {
                    Some(Value::Table(t)) => t,
                    _ => {
                        return self
                            .err_at(at, format!("`{}` is not a table", path[..=n].join(".")))
                    }
                },
                other => {
                    let name = other.type_name();
                    return self.err_at(
                        at,
                        format!("`{}` is already defined as {name}", path[..=n].join(".")),
                    );
                }
            };
        }
        Ok(table)
    }

    fn insert(
        &self,
        root: &mut Table,
        path: &[String],
        value: Value,
        at: usize,
    ) -> Result<(), Error> {
        let (parent, last) = path.split_at(path.len() - 1);
        let table = self.descend(root, parent, at)?;
        if table.contains_key(&last[0]) {
            return self.err_at(at, format!("key `{}` is defined twice", path.join(".")));
        }
        table.insert(last[0].clone(), value);
        Ok(())
    }

    // ---- keys ------------------------------------------------------------

    /// A possibly dotted key: `a`, `a.b`, `"a b".c`.
    fn key_path(&mut self) -> Result<Vec<String>, Error> {
        let mut path = vec![self.key()?];
        loop {
            self.skip_inline_ws();
            if self.eat('.') {
                self.skip_inline_ws();
                path.push(self.key()?);
            } else {
                return Ok(path);
            }
        }
    }

    fn key(&mut self) -> Result<String, Error> {
        match self.peek() {
            Some('"') => self.basic_string(),
            Some('\'') => self.literal_string(),
            Some(c) if is_bare_key_char(c) => {
                let mut s = String::new();
                while let Some(c) = self.peek() {
                    if is_bare_key_char(c) {
                        s.push(c);
                        self.i += 1;
                    } else {
                        break;
                    }
                }
                Ok(s)
            }
            Some(c) => self.err(format!("expected a key, found `{c}`")),
            None => self.err("expected a key, found end of input"),
        }
    }

    // ---- values ----------------------------------------------------------

    fn value(&mut self) -> Result<Value, Error> {
        match self.peek() {
            Some('"') => {
                if self.peek_at(1) == Some('"') && self.peek_at(2) == Some('"') {
                    return self.err("multi-line strings are not supported");
                }
                Ok(Value::String(self.basic_string()?))
            }
            Some('\'') => {
                if self.peek_at(1) == Some('\'') && self.peek_at(2) == Some('\'') {
                    return self.err("multi-line strings are not supported");
                }
                Ok(Value::String(self.literal_string()?))
            }
            Some('[') => self.array(),
            Some('{') => self.inline_table(),
            Some(_) => self.atom(),
            None => self.err("expected a value, found end of input"),
        }
    }

    fn basic_string(&mut self) -> Result<String, Error> {
        let start = self.i;
        self.bump(); // '"'
        let mut s = String::new();
        loop {
            match self.bump() {
                None | Some('\n') => return self.err_at(start, "unterminated string"),
                Some('"') => return Ok(s),
                Some('\\') => {
                    let esc_at = self.i;
                    match self.bump() {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('r') => s.push('\r'),
                        Some('"') => s.push('"'),
                        Some('\\') => s.push('\\'),
                        Some('0') => s.push('\0'),
                        Some('u') => s.push(self.unicode_escape(4, esc_at)?),
                        Some('U') => s.push(self.unicode_escape(8, esc_at)?),
                        Some(c) => {
                            return self.err_at(esc_at, format!("unknown escape `\\{c}`"));
                        }
                        None => return self.err_at(start, "unterminated string"),
                    }
                }
                Some(c) => s.push(c),
            }
        }
    }

    fn unicode_escape(&mut self, digits: usize, at: usize) -> Result<char, Error> {
        let mut n: u32 = 0;
        for _ in 0..digits {
            match self.bump().and_then(|c| c.to_digit(16)) {
                Some(d) => n = n * 16 + d,
                None => return self.err_at(at, "invalid unicode escape"),
            }
        }
        match char::from_u32(n) {
            Some(c) => Ok(c),
            None => self.err_at(at, "invalid unicode escape"),
        }
    }

    fn literal_string(&mut self) -> Result<String, Error> {
        let start = self.i;
        self.bump(); // '\''
        let mut s = String::new();
        loop {
            match self.bump() {
                None | Some('\n') => return self.err_at(start, "unterminated string"),
                Some('\'') => return Ok(s),
                Some(c) => s.push(c),
            }
        }
    }

    fn array(&mut self) -> Result<Value, Error> {
        let start = self.i;
        self.bump(); // '['
        let mut items = Vec::new();
        loop {
            self.skip_trivia();
            match self.peek() {
                None => return self.err_at(start, "unterminated array"),
                Some(']') => {
                    self.i += 1;
                    return Ok(Value::Array(items));
                }
                _ => {
                    items.push(self.value()?);
                    self.skip_trivia();
                    match self.peek() {
                        Some(',') => {
                            self.i += 1;
                        }
                        Some(']') => {}
                        None => return self.err_at(start, "unterminated array"),
                        Some(c) => return self.err(format!("expected `,` or `]`, found `{c}`")),
                    }
                }
            }
        }
    }

    fn inline_table(&mut self) -> Result<Value, Error> {
        let start = self.i;
        self.bump(); // '{'
        let mut table = Table::new();
        self.skip_inline_ws();
        if self.eat('}') {
            return Ok(Value::Table(table));
        }
        loop {
            self.skip_inline_ws();
            let at = self.i;
            let path = self.key_path()?;
            self.skip_inline_ws();
            if !self.eat('=') {
                return self.err("expected `=` after key");
            }
            self.skip_inline_ws();
            let value = self.value()?;
            self.insert(&mut table, &path, value, at)?;
            self.skip_inline_ws();
            match self.peek() {
                Some(',') => {
                    self.i += 1;
                }
                Some('}') => {
                    self.i += 1;
                    return Ok(Value::Table(table));
                }
                None => return self.err_at(start, "unterminated inline table"),
                Some(c) => return self.err(format!("expected `,` or `}}`, found `{c}`")),
            }
        }
    }

    /// A bare token: `true`, `false`, or a number.
    fn atom(&mut self) -> Result<Value, Error> {
        let start = self.i;
        let mut tok = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || matches!(c, ',' | ']' | '}' | '#') {
                break;
            }
            tok.push(c);
            self.i += 1;
        }
        if tok.is_empty() {
            return self.err_at(start, "expected a value");
        }
        match tok.as_str() {
            "true" => return Ok(Value::Boolean(true)),
            "false" => return Ok(Value::Boolean(false)),
            _ => {}
        }
        let cleaned = tok.replace('_', "");
        if let Ok(i) = cleaned.parse::<i64>() {
            return Ok(Value::Integer(i));
        }
        if (cleaned.contains('.') || cleaned.contains('e') || cleaned.contains('E'))
            && !cleaned.contains(':')
        {
            if let Ok(f) = cleaned.parse::<f64>() {
                return Ok(Value::Float(f));
            }
        }
        if tok.contains(':') || (tok.len() >= 10 && tok.as_bytes()[4] == b'-') {
            return self.err_at(start, format!("datetimes are not supported: `{tok}`"));
        }
        self.err_at(start, format!("invalid value: `{tok}`"))
    }
}

fn is_bare_key_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(input: &str) -> Table {
        parse(input).expect("parses")
    }

    #[test]
    fn scalars() {
        let t = t("s = \"a\"\ni = 42\nf = 1.5\nb = true\nneg = -7\nbig = 1_000\n");
        assert_eq!(t["s"], Value::String("a".into()));
        assert_eq!(t["i"], Value::Integer(42));
        assert_eq!(t["f"], Value::Float(1.5));
        assert_eq!(t["b"], Value::Boolean(true));
        assert_eq!(t["neg"], Value::Integer(-7));
        assert_eq!(t["big"], Value::Integer(1000));
    }

    #[test]
    fn strings_and_escapes() {
        let t = t(r#"a = "x\ty\n" # comment
b = 'raw \n stays'
c = "\u0041"
"quoted key" = "v"
"#);
        assert_eq!(t["a"].as_str(), Some("x\ty\n"));
        assert_eq!(t["b"].as_str(), Some(r"raw \n stays"));
        assert_eq!(t["c"].as_str(), Some("A"));
        assert_eq!(t["quoted key"].as_str(), Some("v"));
    }

    #[test]
    fn tables_dotted_keys_and_inline_tables() {
        let t = t("[a.b]\nx = 1\n\n[a]\ny.z = 2\n\nw = { p = \"q\", r = 3 }\n");
        let a = t["a"].as_table().unwrap();
        assert_eq!(a["b"].as_table().unwrap()["x"], Value::Integer(1));
        assert_eq!(a["y"].as_table().unwrap()["z"], Value::Integer(2));
        assert_eq!(a["w"].as_table().unwrap()["p"].as_str(), Some("q"));
    }

    #[test]
    fn arrays_and_arrays_of_tables() {
        let t = t("xs = [\n  1,\n  2, # trailing comma next\n  3,\n]\n\n[[m]]\np = \"a\"\n\n[[m]]\np = \"b\"\n");
        assert_eq!(t["xs"].as_array().unwrap().len(), 3);
        let m = t["m"].as_array().unwrap();
        assert_eq!(m.len(), 2);
        assert_eq!(m[1].as_table().unwrap()["p"].as_str(), Some("b"));
    }

    #[test]
    fn empty_and_comment_only_documents() {
        assert!(t("").is_empty());
        assert!(t("# nothing\n\n  # here\n").is_empty());
    }

    #[test]
    fn errors_are_located() {
        let e = parse("a = 1\nb = \n").unwrap_err();
        assert_eq!(e.line, 2, "{e}");
        assert!(e.message.contains("expected a value"), "{e}");
        let e = parse("a = 1\na = 2\n").unwrap_err();
        assert!(e.message.contains("defined twice"), "{e}");
        let e = parse("[a]\n[a]\n").unwrap_err();
        assert!(e.message.contains("defined twice"), "{e}");
        let e = parse("a = \"x\n").unwrap_err();
        assert!(e.message.contains("unterminated"), "{e}");
        let e = parse("a = 2024-01-01\n").unwrap_err();
        assert!(e.message.contains("datetimes"), "{e}");
        let e = parse("a = \"\"\"x\"\"\"\n").unwrap_err();
        assert!(e.message.contains("multi-line"), "{e}");
    }
}
