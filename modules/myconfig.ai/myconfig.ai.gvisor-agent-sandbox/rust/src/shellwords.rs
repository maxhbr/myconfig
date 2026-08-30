// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Shell word quoting and unquoting (the `meta` / `last-command` layer).
//!
//! `meta` keeps the historical shell-quoted `key=value` format so sessions
//! written by the bash CLI stay loadable (docs/spec.md §8, §12):
//!
//! - [`quote`] reproduces bash's `printf %q` byte-for-byte for every value
//!   the CLI can produce (verified against bash 5.3's `bstab` + `ansic_quote`
//!   in the tests);
//! - [`unquote`] is a general shell-word parser (bare words, backslash
//!   escapes, `'…'`, `"…"`, `$'…'` ANSI-C) so bash-written meta parses;
//! - [`split_ws`] is the IFS-whitespace splitter for the space-separated list
//!   environment variables.

/// Byte needs no `\` in the non-ANSI-C path: alphanumerics plus bash's
/// `@%+=:,./-_` — everything else printable is backslash-escaped.
fn is_bare_byte(b: u8) -> bool {
    matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'@' | b'%' | b'+' | b'=' | b':' | b'.' | b'/' | b'-' | b'_')
}

/// Push the ANSI-C escape for one control byte (`$'…'` mode).
fn push_control(out: &mut String, b: u8) {
    out.push_str(match b {
        0x07 => "\\a",
        0x08 => "\\b",
        0x1b => "\\E",
        0x0c => "\\f",
        0x0a => "\\n",
        0x0d => "\\r",
        0x09 => "\\t",
        0x0b => "\\v",
        _ => return out.push_str(&format!("\\{:03o}", b)),
    });
}

/// Quote like bash's `printf '%s'` … `printf %q`.
pub fn quote(s: &str) -> String {
    if s.is_empty() {
        return String::from("''");
    }
    // Any control byte ⇒ the whole value in $'…' (bash ansic_quote). The
    // control characters themselves are escaped, printable characters stay
    // raw — including printable multibyte sequences (verified against a
    // UTF-8 bash; LANG=C escapes them octally, but the spec pins UTF-8).
    if s.bytes().any(|b| b < 0x20 || b == 0x7f) {
        let mut out = String::from("$'");
        for c in s.chars() {
            match c {
                '\'' => out.push_str("\\'"),
                '\\' => out.push_str("\\\\"),
                c if (c as u32) < 0x20 || c as u32 == 0x7f => {
                    push_control(&mut out, c as u32 as u8)
                }
                c => out.push(c),
            }
        }
        out.push('\'');
        return out;
    }
    let bytes = s.as_bytes();
    let mut out = String::new();
    let mut prev = 0u8;
    for (i, c) in s.char_indices() {
        let b = *bytes.get(i + c.len_utf8() - 1).unwrap();
        if c.is_ascii() {
            let b = c as u8;
            if is_bare_byte(b) {
                out.push(c);
            } else if b == b'#' {
                // `#` only starts a comment at the very beginning of a word.
                if i == 0 {
                    out.push_str("\\#");
                } else {
                    out.push(c);
                }
            } else if b == b'~' {
                // `~` is only a tilde-expansion at the start or after `:`/`=`.
                if i == 0 || prev == b':' || prev == b'=' {
                    out.push_str("\\~");
                } else {
                    out.push(c);
                }
            } else {
                out.push('\\');
                out.push(c);
            }
        } else {
            // Printable multibyte UTF-8 stays raw (the input is a &str, so
            // the sequence is well-formed).
            out.push(c);
        }
        prev = b;
    }
    out
}

/// Parse one shell word (assignment-value context: no word splitting, no
/// globbing). `Err` carries the `die` message.
pub fn unquote(s: &str) -> Result<String, String> {
    let bad = || format!("cannot parse shell-quoted value: {s:?}");
    let mut out = Vec::<u8>::new();
    let b = s.as_bytes();
    let mut i = 0;
    // (mode, byte) state machine: 0 = bare, 1 = '…', 2 = "…", 3 = $'…'.
    let mut mode = 0u8;
    while i < b.len() {
        let c = b[i];
        match mode {
            0 => match c {
                b'\'' => mode = 1,
                b'"' => mode = 2,
                b'$' if i + 1 < b.len() && b[i + 1] == b'\'' => {
                    mode = 3;
                    i += 1;
                }
                b'\\' if i + 1 < b.len() => {
                    i += 1; // consume the escaped byte (a \<newline> continuation drops it)
                    if b[i] != b'\n' {
                        out.push(b[i]);
                    }
                }
                _ => out.push(c),
            },
            1 => {
                if c == b'\'' {
                    mode = 0;
                } else {
                    out.push(c);
                }
            }
            2 => match c {
                b'"' => mode = 0,
                b'\\' if i + 1 < b.len() => match b[i + 1] {
                    b'"' | b'\\' | b'$' | b'`' => {
                        out.push(b[i + 1]);
                        i += 1;
                    }
                    b'\n' => i += 1, // line continuation
                    _ => out.push(c),
                },
                _ => out.push(c),
            },
            _ => {
                // $'…' ANSI-C: ' ends it, \ starts an escape.
                if c == b'\'' {
                    mode = 0;
                    i += 1;
                    continue;
                }
                if c != b'\\' {
                    out.push(c);
                    i += 1;
                    continue;
                }
                i += 1;
                if i >= b.len() {
                    return Err(bad());
                }
                let e = b[i];
                i += 1;
                match e {
                    b'a' => out.push(0x07),
                    b'b' => out.push(0x08),
                    b'e' | b'E' => out.push(0x1b),
                    b'f' => out.push(0x0c),
                    b'n' => out.push(0x0a),
                    b'r' => out.push(0x0d),
                    b't' => out.push(0x09),
                    b'v' => out.push(0x0b),
                    b'\\' | b'\'' | b'"' => out.push(e),
                    b'0'..=b'7' => {
                        // up to three octal digits (bash allows \0NNN too;
                        // \0 alone is NUL)
                        let mut v = (e - b'0') as u32;
                        let mut n = 1;
                        while n < 3 && i < b.len() && (b'0'..=b'7').contains(&b[i]) {
                            v = v * 8 + (b[i] - b'0') as u32;
                            i += 1;
                            n += 1;
                        }
                        out.push((v & 0xff) as u8);
                    }
                    b'x' => {
                        let mut v = 0u32;
                        let mut n = 0;
                        while n < 2 && i < b.len() && b[i].is_ascii_hexdigit() {
                            v = v * 16 + (b[i] as char).to_digit(16).unwrap();
                            i += 1;
                            n += 1;
                        }
                        if n == 0 {
                            return Err(bad());
                        }
                        out.push((v & 0xff) as u8);
                    }
                    b'u' | b'U' => {
                        let max = if e == b'u' { 4 } else { 8 };
                        let mut v = 0u32;
                        let mut n = 0;
                        while n < max && i < b.len() && b[i].is_ascii_hexdigit() {
                            v = v * 16 + (b[i] as char).to_digit(16).unwrap();
                            i += 1;
                            n += 1;
                        }
                        if n == 0 {
                            return Err(bad());
                        }
                        match char::from_u32(v) {
                            Some(ch) => {
                                let mut buf = [0u8; 4];
                                out.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
                            }
                            None => return Err(bad()),
                        }
                    }
                    b'\n' => {} // line continuation
                    other => out.push(other),
                }
                continue;
            }
        }
        i += 1;
    }
    if mode != 0 {
        return Err(bad());
    }
    // Values are produced by %q (or the reader's own paths); keep
    // well-formed UTF-8 and fall back to a lossless byte-preserving
    // conversion otherwise, so odd-but-valid meta never dies here.
    Ok(match String::from_utf8(out) {
        Ok(s) => s,
        Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
    })
}

/// Split on IFS whitespace (space / tab / newline), dropping empty items.
pub fn split_ws(s: &str) -> Vec<String> {
    s.split([' ', '\t', '\n'])
        .filter(|w| !w.is_empty())
        .map(str::to_string)
        .collect()
}
