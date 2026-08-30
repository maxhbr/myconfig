# Replace fragile line-range citations in the herdr tier comparison

## Context

`doc/sandboxed-herdr-vs-agent-microvm-herdr.md` cites its evidence as
`<file>:<start>-<end>` line ranges (e.g. `sandboxed-herdr.README.md:187-196`,
`agent-microvm.md:765-784`, `flake.sandboxed-pi.nix:470`). Every edit to a
cited file silently invalidates them: the 2026-08 doc review found ~10 drifted
ranges, and the doc-review fixes (commit `04c9e05f0a`, branch `agent/gvisor-fun`) re-verified and
updated all of them by hand — the second such manual pass over this file.

## What to do

Replace the line-range citations with drift-resistant references:

- cite **section headings / anchor names** instead of line numbers
  (e.g. `` `agent-microvm.md`, "herdr specifics" `` — the doc already does
  this in some places), and/or
- quote the claim verbatim (the doc already pairs most citations with a
  quotation, so the line number is redundant) and drop the numeric range.

## How to verify

- `grep -nE ':[0-9]+(-[0-9]+)?' doc/sandboxed-herdr-vs-agent-microvm-herdr.md`
  returns no matches (or only matches where a line number is genuinely the
  stable identifier, e.g. a `default.nix:197` cited inside another doc's
  historical snapshot);
- the comparison still names every claim's source file and section.
