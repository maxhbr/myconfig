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

// ---- span editing (review-4 item 3) ---------------------------------
//
// `mysbx init --approve-git-dirs` edits an EXISTING sidecar config in
// place: the operator's comments and formatting must survive, so the
// document is not re-rendered from the parsed table but spliced at byte
// offsets. Doing that needs the same lexical rules the parser above
// implements — a `]` or a `#` inside a quoted path is data, not
// structure, and a key is only the top-level `git-dirs` when it appears
// BEFORE the first table header (TOML never returns to the root table,
// so an edit appended at EOF would land in `[env]` or in the last
// `[[mounts]]`).
//
// The scanner below walks the document once, byte by byte. It only ever
// matches ASCII delimiters, so every offset it reports is a UTF-8
// boundary and the splices are safe.

/// Where a top-level `git-dirs` approval may be written
/// (see [`add_git_dirs`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Layout {
    /// Byte offsets of the top-level `git-dirs` array, when the key
    /// exists: the `[` that opens it and the `]` that closes it.
    pub git_dirs: Option<(usize, usize)>,
    /// Byte offset at which a new TOP-LEVEL key may be inserted: the
    /// start of the first table header's line (minus the comment lines
    /// directly above it, which document that table), or the end of
    /// the document when it has no table headers at all.
    pub insert_at: usize,
}

/// Scan `text` for the [`Layout`] of a `git-dirs` approval edit.
///
/// The document is expected to have parsed with [`parse`] already; the
/// scan is deliberately lenient about everything it does not need, but
/// it never guesses: an unterminated string, an unterminated array or a
/// key without a `=` is an error, so an edit is refused rather than
/// written blind.
pub fn layout(text: &str) -> Result<Layout, String> {
    let b = text.as_bytes();
    let mut i = 0usize;
    let mut in_root = true;
    let mut insert_at = text.len();
    let mut git_dirs = None;
    while i < b.len() {
        match b[i] {
            b' ' | b'\t' | b'\r' | b'\n' => {
                i += 1;
                continue;
            }
            b'#' => {
                while i < b.len() && b[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
            // At this position a `[` can only open a table header: a
            // `[` that opens an ARRAY is consumed as part of a value by
            // `skip_value` below, never seen here.
            b'[' => {
                if in_root {
                    insert_at = insertion_point(text, i);
                    in_root = false;
                }
                i = skip_line(text, i)?;
                continue;
            }
            _ => {}
        }
        let key_start = i;
        let (key, after_key) = read_key_path(text, i)?;
        i = skip_blanks(b, after_key);
        if i >= b.len() || b[i] != b'=' {
            return Err(format!(
                "cannot edit this config: no `=` after the key at byte {key_start}"
            ));
        }
        i = skip_blanks(b, i + 1);
        let value_start = i;
        let value_end = skip_value(text, i)?;
        if in_root && key.len() == 1 && key[0] == "git-dirs" {
            if b[value_start] != b'[' {
                return Err("cannot edit this config: `git-dirs` is not an array".into());
            }
            // `skip_value` stops one past the closing `]`.
            git_dirs = Some((value_start, value_end - 1));
        }
        i = value_end;
    }
    Ok(Layout {
        git_dirs,
        insert_at,
    })
}

/// `text` with `entries` added to the top-level `git-dirs` array,
/// creating the key when it is missing. Everything else is preserved
/// byte for byte: this is a splice, not a re-render.
///
/// `entries` are raw host paths; the escaping into TOML basic strings
/// happens here, in one place, so a path containing `"`, `\`, `]` or
/// `#` round-trips through [`parse`] unchanged. `new_key_comment` is
/// written above a NEWLY created key only (an existing array keeps its
/// own documentation).
///
/// The caller is expected to re-parse the result before replacing the
/// file — `lib.rs` does, with the real `Config::parse`.
pub fn add_git_dirs(text: &str, entries: &[&str], new_key_comment: &str) -> Result<String, String> {
    let layout = layout(text)?;
    let rendered: Vec<String> = entries
        .iter()
        .map(|e| format!("  \"{}\",", escape_basic(e)))
        .collect();
    let Some((open, close)) = layout.git_dirs else {
        // No top-level key: write one BEFORE the first table header —
        // TOML has no way back to the root table, so appending at EOF
        // would silently make the key a field of `[env]` or of the
        // last `[[mounts]]` entry (review-4 item 3).
        let at = layout.insert_at;
        let mut out = String::with_capacity(text.len() + 128);
        out.push_str(&text[..at]);
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        if !out.is_empty() && !out.ends_with("\n\n") {
            out.push('\n');
        }
        out.push_str(new_key_comment);
        out.push_str("git-dirs = [\n");
        for entry in &rendered {
            out.push_str(entry);
            out.push('\n');
        }
        out.push_str("]\n");
        let rest = &text[at..];
        if !rest.is_empty() {
            out.push('\n');
            out.push_str(rest);
        }
        return Ok(out);
    };
    // The key exists: splice the new entries in before the closing
    // `]`, adding the separator the last existing element may be
    // missing (`git-dirs = ["a"]` has no trailing comma).
    let (last_significant, needs_comma) = array_tail(text, open, close);
    let comma_at = last_significant.map_or(close, |x| x + 1);
    let mut out = String::with_capacity(text.len() + 64 * rendered.len());
    out.push_str(&text[..comma_at]);
    if needs_comma {
        out.push(',');
    }
    out.push_str(&text[comma_at..close]);
    if !out.ends_with('\n') {
        out.push('\n');
    }
    for entry in &rendered {
        out.push_str(entry);
        out.push('\n');
    }
    out.push_str(&text[close..]);
    Ok(out)
}

/// TOML basic-string escaping of a raw path: a path may legally contain
/// `"` or `\`, and an unescaped one would make the file mysbx just
/// wrote unparsable on the next run. `]` and `#` need no escape — they
/// are ordinary characters INSIDE a string; what they must not do is
/// confuse the scanner, and it reads strings as strings.
fn escape_basic(raw: &str) -> String {
    raw.replace('\\', "\\\\").replace('"', "\\\"")
}

fn skip_blanks(b: &[u8], mut i: usize) -> usize {
    while i < b.len() && (b[i] == b' ' || b[i] == b'\t') {
        i += 1;
    }
    i
}

/// The offset at which a new top-level key is written when the first
/// table header sits at `header`: the start of that header's line, but
/// above the contiguous comment lines directly preceding it — those
/// document the table and must keep sitting on it.
fn insertion_point(text: &str, header: usize) -> usize {
    let mut at = line_start(text, header);
    loop {
        if at == 0 {
            return at;
        }
        let prev = line_start(text, at - 1);
        let line = text[prev..at].trim();
        if line.starts_with('#') {
            at = prev;
        } else {
            return at;
        }
    }
}

/// The offset of the first byte of the line `at` lies on.
fn line_start(text: &str, at: usize) -> usize {
    text[..at].rfind('\n').map_or(0, |n| n + 1)
}

/// Skip to just past the end of the line starting at `i`, ignoring
/// newlines inside quoted strings (a table header may carry a quoted
/// key containing anything at all).
fn skip_line(text: &str, mut i: usize) -> Result<usize, String> {
    let b = text.as_bytes();
    while i < b.len() {
        match b[i] {
            b'"' | b'\'' => i = skip_string(text, i)?,
            b'\n' => return Ok(i + 1),
            _ => i += 1,
        }
    }
    Ok(i)
}

/// Read the key at `i` — bare, quoted, or dotted — and return its
/// segments plus the offset just past it. A dotted key yields more than
/// one segment, which is how `[env]`-style `a.b = 1` at the root is
/// told apart from the top-level `git-dirs` this edit touches.
fn read_key_path(text: &str, mut i: usize) -> Result<(Vec<String>, usize), String> {
    let b = text.as_bytes();
    let mut segments = Vec::new();
    loop {
        i = skip_blanks(b, i);
        if i >= b.len() {
            return Err("cannot edit this config: truncated key".into());
        }
        match b[i] {
            b'"' | b'\'' => {
                let end = skip_string(text, i)?;
                // The scanner only needs to COMPARE the key, so the
                // raw inner text is enough for the bare spellings this
                // schema uses; an escaped quoted key simply never
                // equals `git-dirs`.
                segments.push(text[i + 1..end - 1].to_string());
                i = end;
            }
            _ => {
                let start = i;
                while i < b.len() && is_bare_key_char(b[i] as char) {
                    i += 1;
                }
                if i == start {
                    return Err(format!(
                        "cannot edit this config: unexpected `{}` at byte {start}",
                        &text[start..start + 1]
                    ));
                }
                segments.push(text[start..i].to_string());
            }
        }
        let after = skip_blanks(b, i);
        if after < b.len() && b[after] == b'.' {
            i = after + 1;
            continue;
        }
        return Ok((segments, i));
    }
}

/// Skip the string starting at `i` (`"` basic with escapes, or `'`
/// literal without) and return the offset just past its closing quote.
fn skip_string(text: &str, i: usize) -> Result<usize, String> {
    let b = text.as_bytes();
    let quote = b[i];
    let mut j = i + 1;
    while j < b.len() {
        if quote == b'"' && b[j] == b'\\' {
            j += 2;
            continue;
        }
        if b[j] == quote {
            return Ok(j + 1);
        }
        j += 1;
    }
    Err(format!(
        "cannot edit this config: unterminated string at byte {i}"
    ))
}

/// Skip the value starting at `i` and return the offset just past it.
/// Arrays and inline tables are skipped with their nesting, strings as
/// strings and comments as comments — so a `]` or `#` inside a quoted
/// path cannot end the value early.
fn skip_value(text: &str, i: usize) -> Result<usize, String> {
    let b = text.as_bytes();
    match b.get(i) {
        None => Err("cannot edit this config: missing value".into()),
        Some(b'"') | Some(b'\'') => skip_string(text, i),
        Some(b'[') | Some(b'{') => {
            let mut depth = 0usize;
            let mut j = i;
            while j < b.len() {
                match b[j] {
                    b'"' | b'\'' => {
                        j = skip_string(text, j)?;
                        continue;
                    }
                    b'#' => {
                        while j < b.len() && b[j] != b'\n' {
                            j += 1;
                        }
                        continue;
                    }
                    b'[' | b'{' => depth += 1,
                    b']' | b'}' => {
                        depth -= 1;
                        if depth == 0 {
                            return Ok(j + 1);
                        }
                    }
                    _ => {}
                }
                j += 1;
            }
            Err(format!(
                "cannot edit this config: unterminated array or inline table at byte {i}"
            ))
        }
        // A scalar runs to the end of its line or to a comment.
        Some(_) => {
            let mut j = i;
            while j < b.len() && b[j] != b'\n' && b[j] != b'#' {
                j += 1;
            }
            Ok(j)
        }
    }
}

/// The tail of the array `text[open..=close]`: the offset of its last
/// significant byte (outside strings and comments), and whether a
/// separating comma must be added before another element is appended.
fn array_tail(text: &str, open: usize, close: usize) -> (Option<usize>, bool) {
    let b = text.as_bytes();
    let mut j = open + 1;
    let mut last: Option<usize> = None;
    while j < close {
        match b[j] {
            b'"' | b'\'' => {
                // An unterminated string cannot occur here: `layout`
                // already scanned the array successfully.
                let end = skip_string(text, j).unwrap_or(close);
                last = Some(end - 1);
                j = end;
                continue;
            }
            b'#' => {
                while j < close && b[j] != b'\n' {
                    j += 1;
                }
                continue;
            }
            b' ' | b'\t' | b'\r' | b'\n' => {}
            _ => last = Some(j),
        }
        j += 1;
    }
    let needs_comma = last.is_some_and(|x| b[x] != b',');
    (last, needs_comma)
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

#[cfg(test)]
mod edit_tests {
    use super::*;

    const COMMENT: &str = "# approved\n";

    /// The edit, re-parsed: every test asserts on the VALUE the parser
    /// sees, not on the bytes alone — the point of the exercise is that
    /// the rewritten document still means what it says.
    fn add(text: &str, entries: &[&str]) -> (String, Table) {
        let out = add_git_dirs(text, entries, COMMENT).expect("edit succeeds");
        let table = parse(&out).unwrap_or_else(|e| panic!("re-parse failed: {e}\n---\n{out}"));
        (out, table)
    }

    fn git_dirs_of(table: &Table) -> Vec<String> {
        table["git-dirs"]
            .as_array()
            .expect("git-dirs is an array")
            .iter()
            .map(|v| v.as_str().expect("string").to_string())
            .collect()
    }

    #[test]
    fn a_config_ending_in_a_table_gets_a_top_level_key() {
        // The bug: appending at EOF made this an `env.git-dirs` key.
        let (out, table) = add(
            "backend = \"bubblewrap\"\n\n[env]\nEDITOR = \"nvim\"\n",
            &["/a"],
        );
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
        assert!(
            table["env"]
                .as_table()
                .expect("env")
                .get("git-dirs")
                .is_none(),
            "the key landed in [env]: {out}"
        );
    }

    #[test]
    fn a_config_ending_in_an_array_of_tables_gets_a_top_level_key() {
        let (out, table) = add(
            "backend = \"bubblewrap\"\n\n[[mounts]]\npath = \"/x\"\nmode = \"ro\"\n",
            &["/a"],
        );
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
        let mounts = table["mounts"].as_array().expect("mounts");
        assert_eq!(mounts.len(), 1, "{out}");
        assert!(
            mounts[0]
                .as_table()
                .expect("mount")
                .get("git-dirs")
                .is_none(),
            "the key landed in the mount: {out}"
        );
    }

    #[test]
    fn a_document_without_tables_keeps_the_key_at_the_end() {
        let (_, table) = add("backend = \"bubblewrap\"\n", &["/a"]);
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
    }

    #[test]
    fn an_existing_quoted_key_is_extended_not_duplicated() {
        // `"git-dirs"` is the same key as `git-dirs`; a second
        // definition would be a parse error ("duplicate key"), so
        // recognising the quoted spelling is what keeps the edit valid.
        let (_, table) = add("\"git-dirs\" = [\"/a\"]\n", &["/b"]);
        assert_eq!(
            git_dirs_of(&table),
            vec!["/a".to_string(), "/b".to_string()]
        );
    }

    #[test]
    fn a_same_named_key_in_another_table_is_not_touched() {
        let text = "backend = \"bubblewrap\"\n\n[env]\n\"git-dirs\" = \"not a path\"\n";
        let (out, table) = add(text, &["/a"]);
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
        assert_eq!(
            table["env"].as_table().expect("env")["git-dirs"],
            Value::String("not a path".into()),
            "{out}"
        );
    }

    #[test]
    fn an_array_without_a_trailing_comma_gets_one() {
        let (_, table) = add("git-dirs = [\"/a\"]\n", &["/b"]);
        assert_eq!(
            git_dirs_of(&table),
            vec!["/a".to_string(), "/b".to_string()]
        );
    }

    #[test]
    fn an_empty_array_is_filled() {
        let (_, table) = add("git-dirs = []\n", &["/a"]);
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
    }

    #[test]
    fn brackets_hashes_quotes_and_backslashes_in_paths_round_trip() {
        // The old locator scanned for `]` and `#` without knowing
        // about strings, so either character inside a path ended the
        // edit in the middle of the array.
        let weird = ["/sq[uare]", "/ha#sh", "/qu\"ote", "/back\\slash"];
        let (out, table) = add("git-dirs = [\"/keep]\"] # trailing ] comment\n", &weird);
        let mut expected = vec!["/keep]".to_string()];
        expected.extend(weird.iter().map(|s| (*s).to_string()));
        assert_eq!(git_dirs_of(&table), expected, "{out}");
    }

    #[test]
    fn unrelated_comments_and_sections_survive() {
        let text = "# top comment\nbackend = \"bubblewrap\"\n\n# about the mounts\n[[mounts]]\npath = \"/x\"\nmode = \"ro\"\n";
        let (out, table) = add(text, &["/a"]);
        assert!(out.contains("# top comment"), "{out}");
        // The comment documenting the table stays ON the table.
        let mounts_at = out.find("[[mounts]]").expect("mounts header");
        let about_at = out.find("# about the mounts").expect("mount comment");
        assert!(about_at < mounts_at, "{out}");
        assert!(
            out.find("git-dirs").expect("key") < about_at,
            "the new key must be top-level: {out}"
        );
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
    }

    #[test]
    fn comments_inside_the_array_survive() {
        let text = "git-dirs = [\n  \"/a\", # the main checkout\n]\n";
        let (out, table) = add(text, &["/b"]);
        assert!(out.contains("# the main checkout"), "{out}");
        assert_eq!(
            git_dirs_of(&table),
            vec!["/a".to_string(), "/b".to_string()]
        );
    }

    #[test]
    fn a_dotted_root_key_is_not_the_top_level_key() {
        // `env.git-dirs` at the root is a field of `env`, not the key
        // the approval edits.
        let (_, table) = add("env.\"git-dirs\" = \"x\"\n", &["/a"]);
        assert_eq!(git_dirs_of(&table), vec!["/a".to_string()]);
    }

    #[test]
    fn a_broken_document_is_refused_instead_of_spliced() {
        // Unterminated string: no offsets can be trusted, so no edit.
        assert!(add_git_dirs("git-dirs = [\"/a\n", &["/b"], COMMENT).is_err());
    }
}
