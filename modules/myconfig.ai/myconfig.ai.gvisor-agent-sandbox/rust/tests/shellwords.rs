// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `shellwords`: the writer reproduces bash's `printf %q` (docs/spec.md §12).
//! The quoting fixtures were generated with bash 5.3 itself.

use agent_gvisor::shellwords::{quote, split_ws, unquote};

/// (input, expected `printf %q` output) — bash 5.3, UTF-8 locale.
const FIXTURES: &[(&str, &str)] = &[
    ("", "''"),
    ("plain", "plain"),
    ("with space", "with\\ space"),
    ("single'quote", "single\\'quote"),
    ("double\"quote", "double\\\"quote"),
    ("back\\slash", "back\\\\slash"),
    ("a\tb", "$'a\\tb'"),
    ("a\nb", "$'a\\nb'"),
    ("bell\x07", "$'bell\\a'"),
    ("del\x7f", "$'del\\177'"),
    ("esc\x1b", "$'esc\\E'"),
    ("café\tx", "$'café\\tx'"),
    ("café", "café"),
    ("你好", "你好"),
    ("emoji😀", "emoji😀"),
    ("~start", "\\~start"),
    ("#hash", "\\#hash"),
    ("a#b", "a#b"),
    ("a:~b", "a:\\~b"),
    ("a=~b", "a=\\~b"),
    ("comma,dot.dot", "comma\\,dot.dot"),
    ("[x]", "\\[x\\]"),
    ("{a}", "\\{a\\}"),
    ("^caret", "\\^caret"),
    ("a=b", "a=b"),
    ("@%+-./_:x", "@%+-./_:x"),
    ("$dollar", "\\$dollar"),
    ("semi;colon", "semi\\;colon"),
    ("pipe|x", "pipe\\|x"),
    ("backtick`x", "backtick\\`x"),
    ("!(paren)", "\\!\\(paren\\)"),
    ("*glob*", "\\*glob\\*"),
    ("?q", "\\?q"),
    ("<>", "\\<\\>"),
    ("already 'quoted'", "already\\ \\'quoted\\'"),
];

#[test]
fn quote_matches_printf_q() {
    for (input, expected) in FIXTURES {
        assert_eq!(&quote(input), expected, "quote({input:?})");
    }
}

#[test]
fn quote_unquote_roundtrip() {
    for (input, _) in FIXTURES {
        let quoted = quote(input);
        assert_eq!(
            unquote(&quoted).unwrap_or_else(|e| panic!("unquote({quoted:?}): {e}")),
            *input,
            "roundtrip via {quoted:?}"
        );
    }
}

#[test]
fn unquote_parses_bash_forms() {
    let cases: &[(&str, &str)] = &[
        ("''", ""),
        ("''''", ""), // two adjacent quoted empties
        ("'a b'", "a b"),
        ("a\\ b", "a b"),
        ("\"a b\"", "a b"),
        ("$'a\\tb'", "a\tb"),
        ("$'a\\nb'", "a\nb"),
        ("$'a\\x7fb'", "a\u{7f}b"),
        ("$'caf\\303\\251'", "café"),
        ("$'caf\\u00e9'", "café"),
        ("x\\'y", "x'y"),
        ("already\\ \\'quoted\\'", "already 'quoted'"),
        ("\\\\a", "\\a"),
        ("a\\\nb", "ab"), // \<newline> continuation
        ("foo", "foo"),
    ];
    for (input, expected) in cases {
        assert_eq!(
            unquote(input).unwrap_or_else(|e| panic!("unquote({input:?}): {e}")),
            *expected
        );
    }
}

#[test]
fn unquote_rejects_garbage() {
    assert!(unquote("'unterminated").is_err());
    assert!(unquote("\"unterminated").is_err());
    assert!(unquote("$'unterminated").is_err());
    assert!(unquote("trailing'").is_err());
}

#[test]
fn split_ws_drops_empties() {
    assert_eq!(split_ws(" a  b\tc\nd "), vec!["a", "b", "c", "d"]);
    assert_eq!(split_ws(""), Vec::<String>::new());
    assert_eq!(split_ws("   "), Vec::<String>::new());
    assert_eq!(split_ws("one"), vec!["one"]);
}
