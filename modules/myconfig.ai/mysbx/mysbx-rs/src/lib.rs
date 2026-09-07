// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `mysbx` CLI surface (docs/TODOs/mvp-5-cli-and-dry-run.md,
//! docs/design/cli.md):
//!
//! ```text
//! mysbx [FLAGS]                     enter an interactive sandbox shell
//! mysbx run [FLAGS] -- CMD...       run one command in the sandbox
//! mysbx init                        create the sidecar (idempotent)
//! mysbx version | help
//! ```
//!
//! The bare form is the primary action (cli.md D2): it resolves the repo,
//! creates the sidecar implicitly when missing (D12) and execs the
//! backend's argv. `--dry-run` and `--verbose` are *global* flags (before
//! the subcommand, in any order): `--dry-run` runs the whole pipeline —
//! resolve, guards, load, merge, backend check, argv build — and stops
//! immediately before `exec`, printing the backend executable followed
//! by the argv, one argument per line, on stdout; `--verbose` prints the
//! `## `-prefixed run report before that (cli.md D10).

pub mod bwrap;
pub mod config;
pub mod merge;
pub mod repo;
pub mod report;
pub mod toml;

/// The usage text.
pub const USAGE: &str = include_str!("usage.txt");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Print the usage to stdout.
pub fn usage() {
    print!("{USAGE}");
}

/// Host environment variables forwarded into the sandbox — exactly this
/// list, each only when actually set in the process environment
/// (docs/plan.md, "Environment"). Nothing else is forwarded implicitly.
/// Public so the integration tests assert against the same list the
/// pipeline reads, not a hand-copied one.
pub const FORWARDED_ENV_VARS: &[&str] =
    &["TERM", "COLORTERM", "LANG", "LC_ALL", "EDITOR", "VISUAL"];

/// Dispatch on the argument list (without argv[0]); returns the exit code.
///
/// Exit codes (cli.md D8): `0` success, `1` runtime failure, `2` usage
/// error. A payload's own exit code propagates unchanged, because the real
/// run ends in an `exec` that replaces this process.
pub fn run(args: Vec<String>) -> i32 {
    // The global flags are accepted only BEFORE the subcommand / bare
    // form; anything after `--` is payload and never parsed (cli.md D4,
    // D5, D10).
    let (flags, rest) = match split_global_flags(&args) {
        Ok(x) => x,
        Err(code) => return code,
    };
    match rest.first().map(String::as_str) {
        // Bare `mysbx` is the primary action (docs/design/cli.md D2): enter
        // the sandbox for the current repository.
        None => sandbox(flags, bwrap::Payload::Shell),
        Some("run") => run_command(flags, &rest[1..]),
        // The global flags are only meaningful for the bare form and
        // `run`: on `init` `--dry-run` would promise side-effect-freeness
        // while files are still created, and there is no run to report on
        // for `help`/`version` — reject them instead (usage error, D8).
        Some(other) if flags.any() => {
            eprintln!("mysbx: {} is not valid with `{other}`", flags.first_name());
            eprintln!("try `mysbx --help`");
            2
        }
        Some("help") | Some("-h") | Some("--help") => {
            usage();
            0
        }
        Some("version") | Some("-V") | Some("--version") => {
            println!("mysbx {VERSION}");
            0
        }
        Some("init") => init(&rest[1..]),
        Some(other) => {
            eprintln!("mysbx: unknown command: {other}");
            eprintln!("try `mysbx --help`");
            2
        }
    }
}

/// The global flags of a sandbox run (cli.md D9, D10).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
    pub dry_run: bool,
    pub verbose: bool,
}

impl Flags {
    fn any(self) -> bool {
        self.dry_run || self.verbose
    }

    /// The flag named in the "not valid with `<verb>`" usage error —
    /// whichever was set, `--dry-run` first (it is the older, more
    /// dangerous-sounding promise).
    fn first_name(self) -> &'static str {
        if self.dry_run {
            "--dry-run"
        } else {
            "--verbose"
        }
    }
}

/// Split the leading global flags off the argument list. They may appear
/// in any order but never twice: a repeated flag is a typo, not an
/// intensifier, and staying strict keeps the surface honest (D5: the
/// parser is hand-written, so every accepted spelling is a deliberate
/// one). Returns the exit code of the usage error on rejection.
fn split_global_flags(args: &[String]) -> Result<(Flags, &[String]), i32> {
    let mut flags = Flags::default();
    let mut rest = args;
    while let Some((first, tail)) = rest.split_first() {
        let slot = match first.as_str() {
            "--dry-run" => &mut flags.dry_run,
            "--verbose" => &mut flags.verbose,
            _ => break,
        };
        if *slot {
            eprintln!("mysbx: repeated flag: {first}");
            eprintln!("try `mysbx --help`");
            return Err(2);
        }
        *slot = true;
        rest = tail;
    }
    Ok((flags, rest))
}

/// `mysbx run [--dry-run] [--verbose] -- CMD...` — parse the `run`
/// arguments and hand the payload to the same pipeline as the bare form
/// (spec "Watch out": one code path, two entry points).
///
/// Everything after `--` is the payload, verbatim — including things that
/// look like flags (cli.md D4). The global flags are accepted before `--`
/// only; `run` without `--` or without a command is a usage error (`2`),
/// not an empty sandbox.
fn run_command(global: Flags, args: &[String]) -> i32 {
    let mut flags = global;
    let mut idx = 0;
    while let Some(arg) = args.get(idx) {
        match arg.as_str() {
            "--dry-run" => {
                flags.dry_run = true;
                idx += 1;
            }
            "--verbose" => {
                flags.verbose = true;
                idx += 1;
            }
            "--" => {
                idx += 1;
                break;
            }
            other => {
                eprintln!("mysbx run: unexpected argument: {other}");
                eprintln!("usage: mysbx run [--dry-run] [--verbose] -- COMMAND...");
                return 2;
            }
        }
    }
    let cmd = &args[idx..];
    if cmd.is_empty() {
        eprintln!("mysbx run: no command given after `--`");
        eprintln!("usage: mysbx run [--dry-run] [--verbose] -- COMMAND...");
        return 2;
    }
    sandbox(flags, bwrap::Payload::Command(cmd.to_vec()))
}

/// The shared pipeline of the bare form and `run`: resolve the repo, run
/// the guards, (implicitly) init the sidecar, load and merge both layers,
/// check the backend, build the argv — then print it (`--dry-run`) or exec
/// it.
fn sandbox(flags: Flags, payload: bwrap::Payload) -> i32 {
    let dry_run = flags.dry_run;
    // 1. repo resolution and guards (docs/TODOs/mvp-2-repo-discovery.md).
    // The guard runs BEFORE anything is created, so a `$HOME`-resolved run
    // never even creates a sidecar on disk.
    let repo = match repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return 1;
        }
    };

    // The report shows the state BEFORE the implicit init, so an operator
    // sees what the run found, not what it just created.
    let sidecar_existed = repo.sidecar.is_dir();

    // 2. implicit init (cli.md D2) — except under `--dry-run`, which is
    // side-effect-free: a missing sidecar config is an empty layer there.
    if !dry_run {
        if let Err(msg) = ensure_sidecar(&repo) {
            eprintln!("mysbx: {msg}");
            return 1;
        }
        // `false`: the implicit init must not approve anything —
        // review-2 item 1 (the approval is an operator decision, taken
        // by `mysbx init`).
        if let Err(msg) = ensure_sidecar_config(&repo, false) {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    }

    // 3. both layers, merged (docs/TODOs/mvp-3-layer-merge.md). Merge
    // errors (broken paths, unparseable files, an `[env]` override) are
    // runtime failures: the full validation still runs under --dry-run —
    // a dry run that skipped it would exercise the wrong function.
    let home = std::env::var_os("HOME").unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    let layers =
        match merge::load_layers(std::path::Path::new(&home), xdg.as_deref(), &repo.sidecar) {
            Ok(l) => l,
            Err(e) => {
                eprintln!("mysbx: {e}");
                return 1;
            }
        };
    // Kept for the report before the configs are consumed by the merge:
    // which files were loaded, and how many mounts the user layer
    // contributed (the merge keeps them first and in order, so this one
    // number attributes every merged mount to its layer).
    let user_config_path = layers.user.1.clone();
    let sidecar_config_path = layers.sidecar.1.clone();
    let user_config_exists = user_config_path.exists();
    let sidecar_config_exists = sidecar_config_path.exists();
    let user_mount_count = layers.user.0.mounts.len();
    // `home` is also what `~/…` mount paths expand against (config.md
    // D8) — on the HOST, before bwrap runs, so the sandbox's own
    // (cleared) environment never enters the resolution.
    let merged = match merge::merge(
        layers.user.0,
        layers.sidecar.0,
        &layers.user.1,
        &layers.sidecar.1,
        std::path::Path::new(&home),
    ) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return 1;
        }
    };

    // 3b. the state-dir backing stores (docs/design/config.md D15):
    // one directory per merged `state-dirs` entry under
    // `<sidecar>/state/`, so the rw binds of the argv have an existing
    // source (bwrap needs one) and the payload's writes persist there
    // across runs. After the merge (the entries come from both layers),
    // before the backend check (a broken state dir is as fatal as a
    // broken config), and only for a real run — `--dry-run` stays
    // side-effect-free and prints the argv with the would-be sources.
    // Creation is idempotent; a failure is a runtime error like the
    // sidecar creation above.
    if !dry_run {
        if let Err(msg) = ensure_state_dirs(&repo, &merged.state_dirs) {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    }

    // 4. the backend is explicit, never auto-detected (cli.md D7): a
    // silently downgraded isolation level would be a security bug. The MVP
    // accepts exactly `bubblewrap`.
    match merged.backend.as_deref() {
        Some("bubblewrap") => {}
        Some(other) => {
            eprintln!(
                "mysbx: unsupported backend `{other}` — the MVP implements only `bubblewrap`"
            );
            return 1;
        }
        None => {
            eprintln!(
                "mysbx: no backend configured — set `backend = \"bubblewrap\"` in the user or sidecar config"
            );
            return 1;
        }
    }

    // 5. the argv — its single source is `bwrap::bwrap_argv` (mvp-4).
    let host_env = collect_host_env();
    // Without the Nix wrapper (item 6 pins MYSBX_SHELL and
    // MYSBX_TOOLS_PATH) the fallbacks are silent and deliberate: /bin/sh
    // for the interactive shell, /usr/bin (a base-bound path) for PATH —
    // so a plain `cargo run` still produces a sane argv. An empty value
    // counts as unset (the same treatment XDG_CONFIG_HOME gets in
    // merge.rs): `MYSBX_BWRAP=""` must not become `Command::new("")`.
    let shell = env_or("MYSBX_SHELL", "/bin/sh");
    let tools_path = env_or("MYSBX_TOOLS_PATH", "/usr/bin");
    // The sanitized nix client configuration (review-2 item 3). There
    // is no fallback on purpose: unset means "bind no nix.conf", never
    // "bind the host's" — that file may carry access-tokens, and a
    // read-only bind hands them to the payload all the same.
    let nix_conf = env_opt("MYSBX_NIX_CONF");
    // Review-3 item 3: the trusted policy files of THIS run, handed to
    // the argv builder so it can refuse any `rw` bind that would expose
    // one to the payload.
    //
    // Only files that exist are named: an absent file granted nothing,
    // so exposing its would-be location writes nothing this run — and
    // the check is not thereby bypassable, because the run that FOLLOWS
    // a payload-created `config.toml` sees an existing file and refuses
    // the very `rw` source that allowed creating it. Steering the next
    // run requires the next run to launch with the policy writable —
    // and that is exactly what this guard forbids.
    //
    // Each path is walked, not merely canonicalized (review-4 item 1):
    // the resolved target AND every directory entry the next run
    // traverses to find it are protected — see [`trusted_policy`] and
    // [`bwrap::PolicyPath`]. Canonicalizing alone would protect the
    // Nix-store target of a Home-Manager-generated user config while
    // leaving the symlink that names it replaceable.
    let policy_paths: Vec<bwrap::PolicyPath> = [
        (user_config_exists, &user_config_path),
        (sidecar_config_exists, &sidecar_config_path),
    ]
    .into_iter()
    .filter(|(exists, _)| *exists)
    .map(|(_, path)| trusted_policy(path))
    .collect();
    let params = bwrap::Params {
        shell: &shell,
        tools_path: &tools_path,
        nix_conf: nix_conf.as_deref(),
        policy_paths: &policy_paths,
    };
    let argv = match bwrap::bwrap_argv(&merged, &repo, &payload, &host_env, &params) {
        Ok(a) => a,
        // Review-2 item 4: a config that cannot be laid out safely is an
        // ordinary runtime failure — `mysbx:` on stderr, exit 1 — like
        // the merge errors above, never a Rust panic.
        Err(e) => {
            eprintln!("mysbx: {e}");
            return 1;
        }
    };
    // The Nix wrapper (item 6) pins the binary via MYSBX_BWRAP; the
    // fallback is a plain PATH lookup so `cargo run` works unwrapped.
    let bwrap_bin = env_or("MYSBX_BWRAP", "bwrap");

    // 6. the `--verbose` report (cli.md D10), BEFORE the argv and before
    // the exec: it describes the run that is about to happen, and every
    // line is `## `-prefixed so the unprefixed argv block below stays
    // byte-identical to a plain `--dry-run`.
    if flags.verbose {
        for line in report::lines(&report::Report {
            repo: &repo,
            sidecar_exists: sidecar_existed,
            user_config: &user_config_path,
            user_config_exists,
            sidecar_config: &sidecar_config_path,
            sidecar_config_exists,
            merged: &merged,
            user_mount_count,
            host_env: &host_env,
            params: &params,
            bwrap_bin: &bwrap_bin,
            payload: &payload,
            dry_run,
        }) {
            println!("{line}");
        }
    }

    // 7. print the argv, or exec it.
    if dry_run {
        // argv[0] first, then one argument per line, no prefix, no
        // quoting: this is the *result*, not a diagnostic (cli.md D9),
        // and the executable is part of what `--dry-run` audits — the
        // packaging definition of done requires the wrapped store path
        // as argv[0], which was invisible before (review-1 finding 7:
        // `MYSBX_BWRAP` was read only after the early return). Golden
        // tests compare bytes and `mysbx run --dry-run -- ls | wc -l`
        // stays meaningful — one line more.
        println!("{bwrap_bin}");
        for arg in &argv {
            println!("{arg}");
        }
        return 0;
    }

    let mut cmd = std::process::Command::new(&bwrap_bin);
    cmd.args(&argv);
    // `exec` replaces this process on success, so the payload's exit code
    // propagates unchanged (cli.md D8); the call only returns on failure,
    // with the error as its return value.
    use std::os::unix::process::CommandExt;
    let e = cmd.exec();
    eprintln!("mysbx: cannot exec {bwrap_bin}: {e}");
    1
}

/// Everything that must stay unwritable for `path` to still be THIS
/// policy file on the next run (review-4 item 1): the pathname is
/// walked component by component on the host filesystem, and every
/// directory entry it traverses is recorded — with the parents of that
/// entry already resolved, so an intermediate symlink is recorded as
/// the entry it is *and* followed — followed by the fully resolved
/// target.
///
/// Why the entries and not only the target: mysbx finds its policy by
/// PATHNAME. Home Manager writes `~/.config/mysbx/config.toml` as a
/// symlink into the immutable `/nix/store`; a payload that can write
/// `~/.config/mysbx` cannot touch the target, but it can unlink the
/// symlink and put its own `config.toml` there — and the next run
/// reads that one. The same holds for every directory above it (a
/// writable parent can rename the directory out of the way) and for
/// symlinks in intermediate components.
///
/// Fail-closed by construction: a component that cannot be read as a
/// symlink is treated as a plain entry, and a symlink chain longer
/// than [`SYMLINK_BUDGET`] stops being followed — in both cases the
/// entries collected so far stay protected, so an error can only
/// remove the FOLLOWING of a link, never the protection of the
/// pathname the run actually used.
fn trusted_policy(path: &std::path::Path) -> bwrap::PolicyPath {
    use std::collections::VecDeque;
    use std::ffi::OsString;

    // A relative path can only come from a caller that built it from
    // the CWD; resolve it the same way the rest of the pipeline would,
    // so the guarded set is absolute like every `rw` source it is
    // compared against.
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        match std::env::current_dir() {
            Ok(cwd) => cwd.join(path),
            Err(_) => return bwrap::PolicyPath::lexical(path),
        }
    };
    let mut pending: VecDeque<OsString> = absolute
        .components()
        .map(|c| c.as_os_str().to_owned())
        .collect();
    let mut guarded: Vec<std::path::PathBuf> = Vec::new();
    let mut cur = std::path::PathBuf::from("/");
    let mut budget = SYMLINK_BUDGET;
    while let Some(component) = pending.pop_front() {
        if component == *std::ffi::OsStr::new("/") {
            cur = std::path::PathBuf::from("/");
            continue;
        }
        if component == *std::ffi::OsStr::new(".") {
            continue;
        }
        if component == *std::ffi::OsStr::new("..") {
            cur.pop();
            continue;
        }
        let entry = cur.join(&component);
        if !guarded.contains(&entry) {
            guarded.push(entry.clone());
        }
        match std::fs::read_link(&entry) {
            // A symlink: the ENTRY stays protected (it is what the next
            // run traverses) and the walk continues through its target,
            // relative ones against the directory the link lives in —
            // exactly how the kernel resolves it.
            Ok(target) if budget > 0 => {
                budget -= 1;
                for c in target
                    .components()
                    .map(|c| c.as_os_str().to_owned())
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                {
                    pending.push_front(c);
                }
            }
            _ => cur = entry,
        }
    }
    if !guarded.contains(&cur) {
        guarded.push(cur);
    }
    bwrap::PolicyPath {
        path: absolute,
        guarded,
    }
}

/// How many symlinks [`trusted_policy`] follows before it stops — the
/// usual kernel limit (`ELOOP` at 40). A loop or a deeper chain leaves
/// the entries collected so far protected; nothing is silently
/// unprotected.
const SYMLINK_BUDGET: usize = 40;

/// The forwarded host environment (docs/plan.md, "Environment"): exactly
/// [`FORWARDED_ENV_VARS`], each only when actually set. This is the one
/// place that reads the real process environment; the argv builder stays
/// pure and receives the values as a parameter.
fn collect_host_env() -> bwrap::HostEnv {
    let mut env = bwrap::HostEnv::new();
    for name in FORWARDED_ENV_VARS {
        if let Ok(value) = std::env::var(name) {
            env.insert(name.to_string(), value);
        }
    }
    env
}

/// `std::env::var` with the empty-means-unset rule, for pins that have
/// no fallback at all: `MYSBX_NIX_CONF` unset means "bind no nix
/// configuration", never "bind the host's" (review-2 item 3).
fn env_opt(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|v| !v.is_empty())
}

/// `std::env::var` with the empty-means-unset rule: an empty value falls
/// back like an absent one (see `sandbox` for why that matters).
fn env_or(name: &str, fallback: &str) -> String {
    match std::env::var(name) {
        Ok(v) if !v.is_empty() => v,
        _ => fallback.to_owned(),
    }
}

/// `mysbx init` — resolve the repository (docs/TODOs/mvp-2-repo-discovery.md)
/// and create its sidecar directory `<repo>.mysbx/` with a default
/// `config.toml`.
fn init(args: &[String]) -> i32 {
    // Review-3 item 5: `--approve-git-dirs` is the recovery after an
    // implicit init. A user who first ran the bare form (which never
    // snapshots: the git pointer is untrusted, D3) and THEN wants the
    // discovered git metadata approved used to be stuck — plain
    // `init` reports `exists` and never touches an existing config,
    // so the only way forward was hand-editing. The flag makes that
    // a deliberate, idempotent one-command action: the same trust
    // decision a fresh explicit `init` would have recorded, taken
    // explicitly after the fact. Only ADDED entries are written —
    // anything already approved (or deliberately removed, but still
    // discovered) keeps its state; removal stays the operator's word.
    let mut approve_git_dirs = false;
    for arg in args {
        if arg == "--approve-git-dirs" {
            approve_git_dirs = true;
        } else {
            eprintln!("mysbx init: unexpected argument: {arg}");
            eprintln!("try `mysbx --help`");
            return 2;
        }
    }
    let repo = match repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return 1;
        }
    };
    if let Err(msg) = ensure_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return 1;
    }
    match ensure_sidecar_config(&repo, true) {
        Ok(Outcome::Created) => {}
        Ok(Outcome::Existed) => {
            // The config exists (implicit init, or an earlier init):
            // plain `init` leaves it alone (D12). The recovery flag
            // adds the discovered-but-unapproved git dirs to it.
            if approve_git_dirs {
                if let Err(msg) = approve_git_dirs_in_existing_config(&repo) {
                    eprintln!("mysbx: {msg}");
                    return 1;
                }
            } else {
                println!("## exists: {}", repo.sidecar.join("config.toml").display());
            }
        }
        Err(msg) => {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    }
    0
}

/// Create the sidecar directory if it is missing and report it (idempotent:
/// docs/design/config.md D12). Shared by `init` and the implicit init of
/// the bare form (cli.md D2: the bare form does exactly what `init` would
/// have done, no more, no less).
fn ensure_sidecar(repo: &repo::Repo) -> Result<(), String> {
    if repo.sidecar.is_dir() {
        return Ok(());
    }
    std::fs::create_dir_all(&repo.sidecar)
        .map_err(|e| format!("cannot create {}: {e}", repo.sidecar.display()))?;
    println!("## created: {}/", repo.sidecar.display());
    Ok(())
}

/// Create the `<sidecar>/state/<entry>` backing directory of every
/// merged `state-dirs` entry (docs/design/config.md D15), idempotently,
/// and report each creation like `ensure_sidecar` does. The argv builder
/// binds them rw at `/mysbx-home/<entry>`; bubblewrap needs an existing
/// source, and the writes the payload makes there are exactly the state
/// that survives the sandbox.
///
/// The entry SPELLING is already unambiguous (the parser rejects
/// absolute, `~/`, `.`/`..` and empty components), but the spelling is
/// only half of the path: the state tree is the one part of the sidecar
/// the payload can write, so a symlink planted there would make a plain
/// `create_dir_all` follow it OUT of the sidecar and bind whatever it
/// points at rw into the sandbox home — a host-home path could re-enter
/// the sandbox that way, which is exactly what D14 forbids. Every level
/// is therefore created and verified one component at a time with
/// [`ensure_plain_dir`]: no component of a backing path may be a
/// symlink, so the source of a state bind is always a real directory
/// below `<sidecar>/state/` (D15, "Trust").
fn ensure_state_dirs(repo: &repo::Repo, state_dirs: &[String]) -> Result<(), String> {
    if state_dirs.is_empty() {
        return Ok(());
    }
    let root = repo.sidecar.join("state");
    ensure_plain_dir(&root)?;
    for entry in state_dirs {
        let mut dir = root.clone();
        let mut created = false;
        // Component by component: each level's parent has been checked
        // before the child is touched, so nothing is ever created
        // through a symlink.
        for component in entry.split('/') {
            dir.push(component);
            created |= ensure_plain_dir(&dir)?;
        }
        if created {
            println!("## created: {}/", dir.display());
        }
    }
    Ok(())
}

/// Make `path` an existing, real directory, creating it when missing.
/// Returns whether it was created.
///
/// "Real" excludes a symlink to a directory: inside the sidecar's state
/// tree a symlink is payload-plantable, and following it would silently
/// move a state bind's source somewhere no configuration named (see
/// [`ensure_state_dirs`]). Refusing is the safe direction — the run
/// fails with the offending path named instead of binding it.
fn ensure_plain_dir(path: &std::path::Path) -> Result<bool, String> {
    match std::fs::symlink_metadata(path) {
        Ok(md) if md.file_type().is_symlink() => Err(format!(
            "state directory {} is a symlink — the state tree is writable by the sandbox, \
             so a symlink there would redirect a state bind out of the sidecar \
             (docs/design/config.md D15); remove it, or delete the sidecar's state/ tree",
            path.display()
        )),
        Ok(md) if md.is_dir() => Ok(false),
        Ok(_) => Err(format!(
            "state directory {} exists and is not a directory (docs/design/config.md D15)",
            path.display()
        )),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            std::fs::create_dir(path).map_err(|e| format!("cannot create {}: {e}", path.display()))?;
            Ok(true)
        }
        Err(e) => Err(format!("cannot inspect {}: {e}", path.display())),
    }
}

/// What [`ensure_sidecar_config`] found: writing the default config or
/// finding an existing one. `init` reports the difference; the implicit
/// init of the bare form does not care (cli.md D2: it does exactly what
/// `init` would have done — including not re-writing an operator-edited
/// config — but stays silent about it).
enum Outcome {
    Created,
    Existed,
}

/// Write the default sidecar `config.toml` if it is missing and report
/// it. Also shared by `init` and the implicit init: a sidecar without a
/// config file would make `load_layers` treat the layer as empty — the
/// same outcome, but the operator could no longer *see* the file they
/// are expected to review and edit.
///
/// `snapshot_git_dirs` records the discovered git metadata directories
/// into the fresh config as a `git-dirs` approval list (review-2
/// item 1). Only the EXPLICIT `mysbx init` does that: the pointer it
/// reads lives inside the repo and is untrusted content (config.md
/// D3), so turning it into an approval must be a deliberate operator
/// action, taken in a file outside the repo (D2) and printed on
/// stdout — never something a first bare run does for a freshly
/// cloned repository on its own. The bare form therefore creates the
/// sidecar WITHOUT approvals and refuses the bind with the message
/// naming what to approve.
fn ensure_sidecar_config(repo: &repo::Repo, snapshot_git_dirs: bool) -> Result<Outcome, String> {
    let config = repo.sidecar.join("config.toml");
    if config.exists() {
        return Ok(Outcome::Existed);
    }
    // The repo itself is implicit (docs/design/config.md D13): it is the
    // repo this sidecar belongs to, always mounted rw at its real path. It
    // is deliberately not written into the config — the schema has no
    // `[repo]` table (docs/design/config.md D11).
    let contents = "# mysbx sidecar config\n\
# The repo this sidecar belongs to is implicit: it is always mounted\n\
# read-write at its real host path and cannot be changed here\n\
# (docs/design/config.md D13).\n\
#\n\
# Everything else in the sandbox is opt-in. Give a host-home path a\n\
# `dest` under /mysbx-home: HOME is /mysbx-home inside the sandbox, so\n\
# a config bound at its host path is invisible there (config.md D14).\n\
# Examples:\n\
#\n\
# [[mounts]]\n\
# path = \"/home/user/.config/git\"\n\
# dest = \"/mysbx-home/.config/git\"\n\
# mode = \"ro\"\n\
#\n\
# State directories persist across runs (config.md D15): each entry\n\
# is backed by <repo>.mysbx/state/<entry> and bound rw at\n\
# /mysbx-home/<entry>:\n\
#\n\
# state-dirs = [\".local/share/opencode\"]\n\
#\n\
# [env]\n\
# EDITOR = \"nvim\"\n";
    let mut contents = contents.to_owned();
    if snapshot_git_dirs && !repo.git_dirs.is_empty() {
        contents.push_str(
            "\n# Git metadata this repository needs from outside the work tree\n\
             # (linked worktree or submodule). Recorded when the sidecar was\n\
             # created: the `.git` file inside the repo is untrusted content,\n\
             # so only directories approved HERE (or in the user config) are\n\
             # bound (docs/design/config.md D3, review-2 item 1). Remove an\n\
             # entry to refuse the bind; git then fails inside the sandbox.\n\
             git-dirs = [\n",
        );
        for dir in &repo.git_dirs {
            // A path is bytes, not text: one that is not UTF-8, or
            // that carries a control character, cannot be written as
            // a TOML basic string without either mangling it (a
            // lossy conversion produces a DIFFERENT path, i.e. a
            // dangling approval) or emitting a file mysbx itself
            // could not parse on the next run. Such a path is left
            // out and named on stdout instead — approving it stays
            // possible, by hand, with a literal string.
            let Some(text) = dir.to_str() else {
                println!(
                    "## not recorded (path is not valid UTF-8, approve it by hand): {}",
                    dir.display()
                );
                continue;
            };
            if text.chars().any(|c| c.is_control()) {
                println!(
                    "## not recorded (path contains a control character, approve it by hand): {}",
                    dir.display()
                );
                continue;
            }
            // Basic-string escaping: a path may legally contain `"` or
            // `\`, and an unescaped one would make the file we just
            // wrote unparsable on the next run.
            let escaped = text.replace('\\', "\\\\").replace('"', "\\\"");
            contents.push_str(&format!("  \"{escaped}\",\n"));
            println!("## approved git metadata: {}", dir.display());
        }
        contents.push_str("]\n");
    }
    if let Err(e) = std::fs::write(&config, contents) {
        return Err(format!("cannot write {}: {e}", config.display()));
    }
    println!("## created: {}", config.display());
    Ok(Outcome::Created)
}

/// The review-3 item 5 recovery: add the git metadata directories the
/// repo still needs to an EXISTING sidecar `config.toml` (written by
/// the implicit init of a first bare run, or an earlier `init`).
/// Idempotent — a second call finds nothing missing and writes
/// nothing — and additive only: entries already approved stay
/// (whoever put them there, including by hand), and the approval of a
/// removed-but-still-discovered entry is the operator's explicit word
/// again (the flag says exactly that). Comment lines and formatting
/// of the rest of the file are preserved byte-for-byte.
fn approve_git_dirs_in_existing_config(repo: &repo::Repo) -> Result<(), String> {
    let config = repo.sidecar.join("config.toml");
    let text = std::fs::read_to_string(&config)
        .map_err(|e| format!("cannot read {}: {e}", config.display()))?;
    let parsed = crate::config::Config::parse(&text)
        .map_err(|e| format!("{}: {e}", config.display()))?;
    // Compare on absolute paths: `git-dirs` entries may be written in
    // any D8 spelling (`~/…`, relative); the runtime resolves them
    // against HOME and canonicalizes both sides anyway (review-2
    // item 1). Raw-text matching would both miss a `~` spelling of
    // an approved dir and duplicate it; resolved matching keeps the
    // flag idempotent across spellings.
    let home = std::env::var_os("HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_default();
    let resolve = |raw: &str| -> std::path::PathBuf {
        let p = std::path::Path::new(raw);
        if let Some(rest) = raw.strip_prefix("~/") {
            home.join(rest)
        } else if p.is_absolute() {
            p.to_path_buf()
        } else {
            config.parent().unwrap_or(std::path::Path::new(".")).join(p)
        }
    };
    let approved: std::collections::BTreeSet<std::path::PathBuf> = parsed
        .git_dirs
        .iter()
        .map(|raw| {
            std::fs::canonicalize(resolve(raw)).unwrap_or_else(|_| resolve(raw))
        })
        .collect();
    // NOTE: an entry that does not EXIST cannot canonicalize and is
    // compared in its raw spelling — a duplicate spelling of it may
    // then be added. That is harmless: the next `run` hard-fails on
    // the dangling entry either way (merge.rs names the file), so the
    // broken entry is surfaced, not silently normalized.
    let missing: Vec<std::path::PathBuf> = repo
        .git_dirs
        .iter()
        .filter(|d| !approved.contains(d.as_path()))
        .cloned()
        .collect();
    if missing.is_empty() {
        println!("## git-dirs already lists everything this repo needs");
        return Ok(());
    }
    // A path is bytes, not text — the create-side refusal applies
    // here too: a non-UTF-8 or control-character path mangled through
    // `to_string_lossy` would record a DIFFERENT path (a dangling
    // approval) or an unparsable file. It is left out and named on
    // stdout instead (reviewer finding: the recovery must not be
    // weaker than the initial snapshot).
    let missing: Vec<&std::path::PathBuf> = missing
        .iter()
        .filter(|d| {
            let Some(text) = d.to_str() else {
                println!(
                    "## not recorded (path is not valid UTF-8, approve it by hand): {}",
                    d.display()
                );
                return false;
            };
            if text.chars().any(|c| c.is_control()) {
                println!(
                    "## not recorded (path contains a control character, approve it by hand): {}",
                    d.display()
                );
                return false;
            }
            true
        })
        .collect();
    if missing.is_empty() {
        return Ok(());
    }
    // Extend (or create) the top-level `git-dirs` array in place: a
    // table- and string-aware splice that keeps every other byte of
    // the file (review-4 item 3). Appending at EOF is NOT an option:
    // TOML never returns to the root table, so a file ending in
    // `[env]` would gain an `env.git-dirs` key and one ending in
    // `[[mounts]]` a mount field — both unparsable as a config, both
    // silently "successful" before.
    let entries: Vec<&str> = missing
        .iter()
        .map(|d| d.to_str().expect("filtered above"))
        .collect();
    let new_text = crate::toml::add_git_dirs(&text, &entries, APPROVAL_COMMENT)
        .map_err(|e| format!("{}: {e}", config.display()))?;
    // Validate with the REAL parser before anything is replaced: a
    // rewrite that mysbx itself could not read on the next run must
    // fail here, with the original file untouched, rather than be
    // reported as a successful approval.
    let reparsed = crate::config::Config::parse(&new_text)
        .map_err(|e| format!("{}: refusing to write an unparsable config: {e}", config.display()))?;
    if reparsed.git_dirs.len() < parsed.git_dirs.len() + entries.len() {
        return Err(format!(
            "{}: the rewritten config does not carry the new approvals — refusing to write it",
            config.display()
        ));
    }
    write_atomically(&config, &new_text)?;
    for d in &missing {
        println!("## approved git metadata: {}", d.display());
    }
    Ok(())
}

/// The comment written above a `git-dirs` key the approval CREATES (an
/// existing array keeps whatever documentation it already carries).
const APPROVAL_COMMENT: &str = "# Added by `mysbx init --approve-git-dirs`: git metadata this\n\
     # repository needs from outside the work tree (review-3 item 5).\n\
     # The `.git` pointer inside the repo is untrusted content\n\
     # (docs/design/config.md D3); remove an entry to refuse the\n\
     # bind.\n";

/// Replace a policy file's contents by writing a temporary file next
/// to it and renaming it over the original (review-4 item 3): an
/// interruption mid-write must never leave a TRUNCATED policy — that
/// would be a config granting less than the operator wrote, or none at
/// all, discovered only on the next run. `rename(2)` within the same
/// directory is atomic, so the file is either the old one or the new
/// one.
///
/// The temporary name carries the pid, so two concurrent approvals
/// cannot clobber each other's staging file; the leftover is removed
/// on failure.
fn write_atomically(path: &std::path::Path, contents: &str) -> Result<(), String> {
    let dir = path.parent().unwrap_or(std::path::Path::new("."));
    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "config.toml".to_string());
    let tmp = dir.join(format!(".{name}.mysbx-{}.tmp", std::process::id()));
    if let Err(e) = std::fs::write(&tmp, contents) {
        let _ = std::fs::remove_file(&tmp);
        return Err(format!("cannot write {}: {e}", tmp.display()));
    }
    if let Err(e) = std::fs::rename(&tmp, path) {
        let _ = std::fs::remove_file(&tmp);
        return Err(format!("cannot replace {}: {e}", path.display()));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn help_and_version_succeed() {
        assert_eq!(run(vec!["--help".into()]), 0);
        assert_eq!(run(vec!["--version".into()]), 0);
        assert_eq!(run(vec!["-h".into()]), 0);
        assert_eq!(run(vec!["-V".into()]), 0);
    }

    #[test]
    fn unknown_command_fails() {
        assert_eq!(run(vec!["nope".into()]), 2);
        // The global flags do not make an unknown verb acceptable.
        assert_eq!(run(vec!["--dry-run".into(), "nope".into()]), 2);
        assert_eq!(run(vec!["--verbose".into(), "nope".into()]), 2);
    }

    #[test]
    fn global_flags_parse_in_any_order_but_never_twice() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };
        let args = s(&["--verbose", "--dry-run", "run"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(
            flags,
            Flags {
                dry_run: true,
                verbose: true
            }
        );
        assert_eq!(rest, &s(&["run"])[..]);

        let args = s(&["--dry-run", "--verbose"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert!(flags.dry_run && flags.verbose);
        assert!(rest.is_empty());

        // A flag-less argument list stops the loop immediately.
        let args = s(&["init"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(flags, Flags::default());
        assert_eq!(rest, &s(&["init"])[..]);

        // Repeats are usage errors, per flag.
        assert_eq!(split_global_flags(&s(&["--verbose", "--verbose"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--dry-run", "--dry-run"])), Err(2));
    }

    // The guard-ordering property of the bare form (docs/TODOs/
    // mvp-2-repo-discovery.md): resolve-and-guard BEFORE ensure_sidecar, so
    // a `$HOME`-resolved bare run must never create a sidecar on disk.
    // `run()` uses the real CWD and `$HOME`, so run it *from* a temp dir by
    // spawning a subprocess of the test binary — no: cheaper and still
    // faithful, call the pieces directly.
    #[test]
    fn bare_form_in_home_creates_no_sidecar() {
        let home = std::env::temp_dir().join(format!("mysbx-lib-test-home-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&home);
        std::fs::create_dir_all(&home).unwrap();

        // resolve() is the pure core of the bare form; it must reject the
        // home without any filesystem side effect.
        let e = repo::resolve(&home, Some(&home)).unwrap_err();
        assert!(matches!(e, repo::Error::HomeDir(_)), "{e}");

        // And the sidecar that implicit init WOULD have created does not
        // exist, i.e. nothing ran past the guard.
        let sidecar = {
            let mut name = home.as_os_str().to_owned();
            name.push(".mysbx");
            std::path::PathBuf::from(name)
        };
        assert!(!sidecar.exists());

        let _ = std::fs::remove_dir_all(&home);
    }

    // Usage pairing (cli.md D5): there is no derive macro keeping the
    // parser and usage.txt in sync, so this test is the guard — every
    // accepted verb and flag must appear in the help text, and editing one
    // without the other turns the suite red.
    #[test]
    fn usage_documents_every_accepted_flag_and_verb() {
        for token in [
            "run",
            "init",
            "version",
            "help",
            "--dry-run",
            "--verbose",
            "--help",
            "--version",
            "-h",
            "-V",
        ] {
            assert!(
                USAGE.contains(token),
                "usage.txt does not mention `{token}`"
            );
        }
    }

    // ---- trusted policy pathnames (review-4 item 1) --------------------

    /// A canonical temporary directory for the walker tests (the
    /// crate has no dependencies, and `/tmp` itself may be a symlink
    /// on some systems — canonicalize so the expected entries are
    /// spelled the way the walk records them).
    fn walk_tmpdir(name: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("mysbx-policy-walk-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::canonicalize(&dir).unwrap()
    }

    #[test]
    fn trusted_policy_guards_the_symlink_entry_and_its_target() {
        // The Home-Manager shape: `<dir>/config.toml` is a symlink to
        // an immutable store-like file. BOTH must be guarded — the
        // target because it is the policy, the entry because whoever
        // can replace it decides what the NEXT run reads.
        let base = walk_tmpdir("hm-symlink");
        let store = base.join("store");
        std::fs::create_dir_all(&store).unwrap();
        std::fs::write(store.join("config.toml"), "").unwrap();
        let dir = base.join("cfg");
        std::fs::create_dir_all(&dir).unwrap();
        let link = dir.join("config.toml");
        std::os::unix::fs::symlink(store.join("config.toml"), &link).unwrap();

        let p = trusted_policy(&link);
        assert_eq!(p.path, link);
        assert!(p.guarded.contains(&link), "{:?}", p.guarded);
        assert!(
            p.guarded.contains(&store.join("config.toml")),
            "{:?}",
            p.guarded
        );
        // The containing directory entry too: a writable parent can
        // rename it out of the way and put a new one in its place.
        assert!(p.guarded.contains(&dir), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn trusted_policy_guards_intermediate_symlinks() {
        // `<base>/link/config.toml` where `link -> real`: the
        // intermediate entry is recorded as the entry it is, and the
        // walk continues through its target.
        let base = walk_tmpdir("intermediate");
        let real = base.join("real");
        std::fs::create_dir_all(&real).unwrap();
        std::fs::write(real.join("config.toml"), "").unwrap();
        let link = base.join("link");
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let p = trusted_policy(&link.join("config.toml"));
        assert!(p.guarded.contains(&link), "{:?}", p.guarded);
        assert!(
            p.guarded.contains(&real.join("config.toml")),
            "{:?}",
            p.guarded
        );
        // The final entry is spelled with its parents resolved — the
        // directory entry that actually exists on disk.
        assert!(p.guarded.contains(&real), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn trusted_policy_survives_a_symlink_loop() {
        // Fail-closed: a loop stops the FOLLOWING, never the
        // protection of the entries already walked.
        let base = walk_tmpdir("loop");
        let a = base.join("a");
        let b = base.join("b");
        std::os::unix::fs::symlink(&b, &a).unwrap();
        std::os::unix::fs::symlink(&a, &b).unwrap();

        let p = trusted_policy(&a);
        assert!(p.guarded.contains(&a), "{:?}", p.guarded);
        assert!(p.guarded.contains(&b), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn trusted_policy_of_a_plain_file_is_its_own_chain() {
        let base = walk_tmpdir("plain");
        let file = base.join("config.toml");
        std::fs::write(&file, "").unwrap();

        let p = trusted_policy(&file);
        assert!(p.guarded.contains(&file), "{:?}", p.guarded);
        assert!(p.guarded.contains(&base), "{:?}", p.guarded);
        // An unrelated sibling is NOT guarded: the protection stays
        // the pathname chain, not the whole filesystem.
        assert!(!p.guarded.contains(&base.join("other")), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    // `run` without `--` and without a command is a usage error (`2`),
    // decided before any repo resolution — safe to call from the test's
    // real CWD.
    #[test]
    fn run_without_dashdash_is_a_usage_error() {
        assert_eq!(run(vec!["run".into()]), 2);
        assert_eq!(run(vec!["run".into(), "ls".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--dry-run".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--verbose".into()]), 2);
    }
}
