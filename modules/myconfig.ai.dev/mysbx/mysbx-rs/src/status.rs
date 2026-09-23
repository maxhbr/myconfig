// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `mysbx status` (cli.md D19) — the one-command overview of a
//! repo's mysbx setup: the init state, the effective configuration
//! and the two registries, on stdout with the `## ` prefix (D9).
//!
//! Host-side like the handoff and noun-group verbs: no sandbox is
//! started, the repo is the one the cwd resolves to (D1). Every line
//! it prints is a fact another verb already owns — the init state is
//! `require_initialized_sidecar`'s question, the effective
//! configuration is the merge of `merge::load_layers`, and the
//! session/worktree rows are the SAME lines `session list` /
//! `worktree list` print ([`crate::sessionverbs::list_lines`],
//! [`crate::worktreeverbs::list_lines`]) — so the overview cannot
//! drift from the verbs it summarizes.
//!
//! The verb takes no nested verbs: a stray argument is a usage error
//! naming the bare shape. `--verbose` is refused (no run to report
//! on, like the handoff verbs); `--dry-run` is accepted and behaves
//! as a plain print — the verb has no side effects to preview, the
//! same precedent the list verbs set.
//!
//! When the sidecar is not inited the sidecar layer is empty (an
//! absent file loads as one), so the effective configuration still
//! answers from the user layer — with a line naming the state and
//! the run refusal (D13) it implies.

use crate::merge;
use crate::repo;
use crate::sessionverbs;
use crate::worktreeverbs;
use std::path::Path;

/// The verb's own usage line, printed by its usage errors (the same
/// pairing rule usage.txt follows, cli.md D5).
const USAGE_STATUS: &str = "usage: mysbx status";

/// `mysbx status` (D19): print the `## `-prefixed overview and exit
/// `0` — a listing of facts, not a verdict; even a not-inited repo
/// is a successfully reported state, never a failure.
///
/// Exit codes (cli.md D8): `2` for a wrong command line, `70` when
/// the repo the cwd resolves to could not be resolved (the inside-
/// clone refusal fires there). A broken configuration file is `70`
/// too — the overview refuses to show a merge it cannot compute.
pub fn run(args: &[String], dry_run: bool) -> i32 {
    // `--dry-run` is accepted and ignored: the verb has no side
    // effects, so there is nothing to preview — the same precedent
    // the list verbs' plain print sets. Refusing it would suggest a
    // side effect that does not exist.
    let _ = dry_run;
    if let Some(arg) = args.first() {
        eprintln!("mysbx status: unexpected argument: {arg}");
        eprintln!("{USAGE_STATUS}");
        return 2;
    }
    let repo = match repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };

    // The init state: the config FILE is what a run requires (D13),
    // so it is what "inited" means here too.
    let sidecar_config = repo.sidecar.join("config.toml");
    let inited = sidecar_config.is_file();
    let sidecar_exists = repo.sidecar.is_dir();

    let mut out: Vec<String> = Vec::new();
    let mut p = |s: String| out.push(format!("## {s}"));

    // 1. repo + sidecar: the resolved paths and the init state.
    p(format!("repo:            {}", repo.root.display()));
    p(format!(
        "sidecar:         {} ({})",
        repo.sidecar.display(),
        if inited {
            "inited"
        } else {
            "not inited (run: mysbx init)"
        }
    ));
    if !inited {
        // A sidecar directory without a config.toml is named for
        // what it is: D13 requires the FILE, and status is the
        // command that says so without a refused run.
        if sidecar_exists {
            p(format!(
                "sidecar config:  {} missing — the config file is what a run requires (cli.md D13)",
                sidecar_config.display()
            ));
        }
    }

    // 2. the effective configuration: the merge a run would compute.
    // With the sidecar not inited the sidecar layer is EMPTY (an
    // absent file loads as the empty layer), so the merge still
    // answers from the user layer — what IS known — while the
    // heading says the sidecar is not inited and a run would be
    // refused (D13). A broken file is `70`: the overview refuses to
    // show a merge it cannot compute.
    let home = std::env::var_os("HOME").unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    let layers = match merge::load_layers(Path::new(&home), xdg.as_deref(), &repo.sidecar) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let merged = match merge::merge(
        layers.user.0.clone(),
        layers.sidecar.0.clone(),
        &layers.user.1,
        &layers.sidecar.1,
        Path::new(&home),
    ) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    if !inited {
        p("configuration:  effective from the user layer only — the sidecar is not inited, a run would be refused".to_string());
    }
    // Which layer contributed the backend: the merge takes the
    // sidecar's value over the user layer's (config.md D6), so the
    // attribution is exactly the layers' own answers.
    let backend_line = match (&merged.backend, layers.sidecar.0.backend.is_some()) {
        (Some(_), true) => format!(
            "backend:         {} [sidecar config]",
            merged.backend.as_deref().unwrap_or_default()
        ),
        (Some(_), false) => format!(
            "backend:         {} [user config]",
            merged.backend.as_deref().unwrap_or_default()
        ),
        (None, _) => {
            "backend:         (none — a run would be refused: neither layer names one)".to_string()
        }
    };
    p(backend_line);
    p(format!("multiplexer:     {}", merged.multiplexer.name()));
    // The display channel (config.md D18), right after the
    // multiplexer: the effective selection plus — when waypipe is
    // selected — whether this build could actually open it (the
    // wrapper's `MYSBX_WAYPIPE` pin; a run would refuse the selection
    // without it). The pin probed here is the BWRAP backend's; the
    // podman-gvisor backend's image pin (`MYSBX_GVISOR_WAYPIPE`) is
    // checked at run time by the refusal path, not here — status does
    // not know which backend a run will use.
    p(match merged.display {
        crate::config::Display::Off => "display:         off".to_string(),
        crate::config::Display::Waypipe => {
            let pinned = std::env::var("MYSBX_WAYPIPE")
                .ok()
                .filter(|v| !v.is_empty())
                .is_some();
            if pinned {
                "display:         waypipe".to_string()
            } else {
                "display:         waypipe (refused: no waypipe pinned — set myconfig.ai.dev.mysbx.display.package)".to_string()
            }
        }
    });
    p(format!(
        "network:         {}",
        if merged.network {
            "shared (--share-net)"
        } else {
            "denied (--unshare-all, no --share-net)"
        }
    ));

    // 3. sessions: the SAME lines `session list` prints (D19's
    // reuse rule — the listing formats are the list verbs'
    // contract).
    let sessions = sessionverbs::list_lines(&repo);
    if sessions.len() > 1 {
        p("sessions:".to_string());
        for line in &sessions {
            p(format!("  {line}"));
        }
    } else {
        p("sessions:        none (the clones/ registry is empty)".to_string());
    }

    // 4. worktrees: the SAME lines `worktree list` prints.
    let worktrees = worktreeverbs::list_lines(&repo);
    if worktrees.len() > 1 {
        p("worktrees:".to_string());
        for line in &worktrees {
            p(format!("  {line}"));
        }
    } else {
        p("worktrees:       none (no __worktrees sibling or an empty registry)".to_string());
    }

    // 5. the gvisor image pin, when this build carries one: the
    // trivial tail of D19 — the wrapper's pin, or its absence, is
    // what a `podman-gvisor` run would consume.
    if let Ok(image) = std::env::var("MYSBX_GVISOR_IMAGE") {
        if !image.is_empty() {
            p(format!("gvisor image:    {image} [wrapper pin]"));
        }
    }

    for line in &out {
        println!("{line}");
    }
    0
}
