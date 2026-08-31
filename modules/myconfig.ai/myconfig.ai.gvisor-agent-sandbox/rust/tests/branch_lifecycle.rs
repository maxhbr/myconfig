// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Branch-lifecycle regression tests against REAL Git repositories
//! (docs/spec.md §15): the recording stub cannot model clone ref layout or
//! ref-deletion failures, so `--delete-branch` and the branch-start
//! precedence are tested here end to end. Only `podman` is stubbed.

mod common;

use common::{expected_repo_id, git_in, git_try_in, Scenario};
use std::fs;
use std::path::{Path, PathBuf};

fn agent_root_of(s: &Scenario) -> PathBuf {
    let repo = s.repo.canonicalize().unwrap();
    repo.parent().unwrap().join(format!(
        "{}__agent-gvisor",
        repo.file_name().unwrap().to_string_lossy()
    ))
}

fn worktree_of(s: &Scenario, name: &str) -> PathBuf {
    agent_root_of(s).join(name)
}

fn meta_dir_of(s: &Scenario, name: &str) -> PathBuf {
    agent_root_of(s).join("__sessions").join(name)
}

fn commit_file(repo: &Path, file: &str) {
    fs::write(repo.join(file), format!("{file}\n")).unwrap();
    git_in(repo, &["add", file]);
    git_in(repo, &["commit", "-qm", &format!("add {file}")]);
}

/// Assert the session is COMPLETELY gone: clone, metadata, registry entry.
fn assert_session_gone(s: &Scenario, name: &str) {
    assert!(!worktree_of(s, name).exists(), "session clone still exists");
    assert!(!meta_dir_of(s, name).exists(), "session metadata still exists");
    assert!(
        fs::symlink_metadata(s.state.join("sessions").join(name)).is_err(),
        "registry entry still exists"
    );
}

/// Drop all recorded calls of `tool` so only the calls made AFTER this
/// point are visible to `recorded*` — the stub keeps its container state
/// (marker files under the same directory) intact.
fn clear_recordings(s: &Scenario, tool: &str) {
    for entry in fs::read_dir(&s.record).unwrap() {
        let p = entry.unwrap().path();
        if p
            .file_name()
            .unwrap()
            .to_string_lossy()
            .starts_with(&format!("{tool}-"))
            && p.extension().map(|e| e == "argv").unwrap_or(false)
        {
            fs::remove_file(&p).unwrap();
        }
    }
}

// --- branch-start precedence ----------------------------------------------

#[test]
fn start_on_existing_non_current_host_branch_uses_its_tip() {
    let s = Scenario::new_real_git("start-existing-non-current-branch");
    // `feature` exists in the host but is NOT the host's current branch,
    // and its tip is a distinct commit — the session must start THERE,
    // not at the base commit.
    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt");
    let feature_tip = git_in(&s.repo, &["rev-parse", "feature"]);
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);

    let wt = worktree_of(&s, "s1");
    assert_eq!(git_in(&wt, &["rev-parse", "feature"]), feature_tip);
    assert_eq!(git_in(&wt, &["rev-parse", "--abbrev-ref", "HEAD"]), "feature");
    // The session branch does not track origin/<branch>: the session owns
    // it and the host repository is not its upstream.
    assert!(!git_try_in(&wt, &["config", "--get", "branch.feature.remote"]));
}

#[test]
fn start_of_branch_absent_from_host_starts_at_requested_base() {
    let s = Scenario::new_real_git("start-branch-absent-from-host");
    commit_file(&s.repo, "second.txt"); // master: base + second
    let base = git_in(&s.repo, &["rev-parse", "HEAD~1"]);

    s.run_ok(&["start", "s2", "--branch", "brand-new", "--base", &base, "--detach"]);

    let wt = worktree_of(&s, "s2");
    assert_eq!(git_in(&wt, &["rev-parse", "brand-new"]), base);
    assert_eq!(git_in(&wt, &["rev-parse", "--abbrev-ref", "HEAD"]), "brand-new");
}

#[test]
fn start_branch_name_with_slashes() {
    let s = Scenario::new_real_git("start-branch-with-slashes");
    git_in(&s.repo, &["checkout", "-qb", "agent/gvisor/example"]);
    commit_file(&s.repo, "ex.txt");
    let tip = git_in(&s.repo, &["rev-parse", "agent/gvisor/example"]);
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "agent/gvisor/example", "--detach"]);

    let wt = worktree_of(&s, "s1");
    assert_eq!(git_in(&wt, &["rev-parse", "agent/gvisor/example"]), tip);
    assert_eq!(
        git_in(&wt, &["rev-parse", "--abbrev-ref", "HEAD"]),
        "agent/gvisor/example"
    );
}

#[test]
fn session_fetch_preserves_existing_host_branch_history() {
    // The session-to-host fetch is FAST-FORWARD-ONLY: it must ADVANCE an
    // existing host branch, never replace it with a branch incorrectly
    // created at the base commit.
    let s = Scenario::new_real_git("fetch-preserves-branch-history");
    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt");
    let feature_tip = git_in(&s.repo, &["rev-parse", "feature"]);
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "f2.txt");
    let session_tip = git_in(&wt, &["rev-parse", "feature"]);

    s.run_ok(&["fetch", "s1"]);

    assert_eq!(git_in(&s.repo, &["rev-parse", "feature"]), session_tip);
    // The original tip stayed in the history: the branch was advanced,
    // not replaced.
    assert!(git_try_in(
        &s.repo,
        &["merge-base", "--is-ancestor", &feature_tip, "feature"]
    ));
    assert_eq!(
        git_in(&s.repo, &["rev-parse", "--abbrev-ref", "HEAD"]),
        "master"
    );
}

// --- session-to-host transfer (FAST-FORWARD-ONLY) -------------------------

/// An absent host-local branch is CREATED at the session tip by the
/// transfer.
#[test]
fn fetch_creates_absent_host_branch_at_session_tip() {
    let s = Scenario::new_real_git("fetch-creates-absent-host-branch");
    s.run_ok(&["start", "s1", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "work.txt");
    let session_tip = git_in(&wt, &["rev-parse", "agent/gvisor/s1"]);

    s.run_ok(&["fetch", "s1"]);

    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/heads/agent/gvisor/s1"]),
        session_tip,
        "the absent host branch must be created at the session tip"
    );
}

/// The transfer is FAST-FORWARD-ONLY: a host branch that diverged from
/// the session branch must NOT be replaced. Without the fix, the forced
/// refspec discarded the host-only commit without warning.
#[test]
fn fetch_rejects_divergent_host_branch_and_leaves_it_unchanged() {
    let s = Scenario::new_real_git("fetch-rejects-divergent-host-branch");
    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt"); // the common base
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "session.txt"); // session: S
    let session_tip = git_in(&wt, &["rev-parse", "feature"]);

    // The host branch advances INDEPENDENTLY: H.
    git_in(&s.repo, &["checkout", "feature"]);
    commit_file(&s.repo, "host.txt");
    let host_tip = git_in(&s.repo, &["rev-parse", "refs/heads/feature"]);
    // Switch the host checkout AWAY from feature, so the rejection below
    // is the fast-forward check, not checked-out-branch protection.
    git_in(&s.repo, &["checkout", "master"]);

    let out = s.run(&["fetch", "s1"]);
    assert!(
        !out.status.success(),
        "fetch must fail on a diverged host branch"
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("may have diverged"),
        "stderr must explain possible divergence: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // The host ref is UNCHANGED, byte for byte: S did not replace H.
    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/heads/feature"]),
        host_tip,
        "the diverged host ref must not move"
    );
    assert_ne!(
        git_in(&s.repo, &["rev-parse", "refs/heads/feature"]),
        session_tip
    );
}

/// `merge` runs the transfer FIRST: when it is rejected, NO merge happens
/// — no merge state, no merge commit, no moved refs.
#[test]
fn merge_stops_when_the_transfer_is_rejected() {
    let s = Scenario::new_real_git("merge-stops-on-divergent-transfer");
    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt");
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "session.txt");

    git_in(&s.repo, &["checkout", "feature"]);
    commit_file(&s.repo, "host.txt"); // host: H
    let host_tip = git_in(&s.repo, &["rev-parse", "refs/heads/feature"]);
    // The merge target: master, clean, checked out.
    git_in(&s.repo, &["checkout", "master"]);
    let master_tip = git_in(&s.repo, &["rev-parse", "refs/heads/master"]);

    let out = s.run(&["merge", "s1"]);
    assert!(
        !out.status.success(),
        "merge must fail when the session-to-host transfer is rejected"
    );
    assert!(String::from_utf8_lossy(&out.stderr).contains("may have diverged"));
    assert_eq!(git_in(&s.repo, &["rev-parse", "refs/heads/feature"]), host_tip);
    assert_eq!(git_in(&s.repo, &["rev-parse", "refs/heads/master"]), master_tip);
    // No merge state was created.
    assert!(!git_try_in(
        &s.repo,
        &["rev-parse", "-q", "--verify", "MERGE_HEAD"]
    ));
}

/// `push` runs the implicit transfer FIRST: when it is rejected, nothing
/// is published — not even probed. A real bare remote receives no ref at
/// all.
#[test]
fn push_stops_when_the_transfer_is_rejected() {
    let s = Scenario::new_real_git("push-stops-on-divergent-transfer");
    // A real bare remote for the host repository (the push target).
    git_in(&s.root, &["init", "-q", "--bare", "remote.git"]);
    let bare = s.root.join("remote.git");
    git_in(
        &s.repo,
        &["remote", "add", "origin", &bare.display().to_string()],
    );

    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt");
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "session.txt");

    git_in(&s.repo, &["checkout", "feature"]);
    commit_file(&s.repo, "host.txt"); // host: H
    let host_tip = git_in(&s.repo, &["rev-parse", "refs/heads/feature"]);
    git_in(&s.repo, &["checkout", "master"]);

    let out = s.run(&["push", "s1"]);
    assert!(
        !out.status.success(),
        "push must fail when the session-to-host transfer is rejected"
    );
    assert!(String::from_utf8_lossy(&out.stderr).contains("may have diverged"));
    assert_eq!(git_in(&s.repo, &["rev-parse", "refs/heads/feature"]), host_tip);
    // The remote received NOTHING.
    assert_eq!(
        git_in(&bare, &["for-each-ref"]),
        "",
        "the bare remote must still be empty"
    );
}

/// Rewritten (amended/rebased) session history is NOT published by
/// default: the non-fast-forward update is rejected and the host ref
/// stays where the last fast-forward transfer left it. Reconcile such
/// history explicitly.
#[test]
fn fetch_rejects_rewritten_session_history() {
    let s = Scenario::new_real_git("fetch-rejects-rewritten-history");
    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt");
    git_in(&s.repo, &["checkout", "master"]);

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "work.txt");
    // An ordinary advance transfers fine.
    s.run_ok(&["fetch", "s1"]);
    let fetched_tip = git_in(&s.repo, &["rev-parse", "refs/heads/feature"]);

    // Amend the session commit: the next update is non-fast-forward.
    git_in(&wt, &["commit", "--amend", "-qm", "rewritten work"]);

    let out = s.run(&["fetch", "s1"]);
    assert!(
        !out.status.success(),
        "fetch must reject rewritten session history by default"
    );
    assert!(String::from_utf8_lossy(&out.stderr).contains("may have diverged"));
    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/heads/feature"]),
        fetched_tip,
        "the host ref must stay at the last fast-forward state"
    );
}

/// A branch literally named `+topic` must be transferred as ITSELF: with
/// a short source ref, the refspec would read `+topic:refs/heads/+topic`
/// and git would parse the leading `+` as a FORCE marker — fetching
/// `topic` instead, transferring the wrong branch and bypassing the
/// fast-forward-only policy. Fully qualified refs close the hole.
#[test]
fn fetch_transfers_the_plus_named_branch_itself() {
    let s = Scenario::new_real_git("fetch-plus-named-branch");
    // Session branch `+topic`, absent in the host, starts at the base.
    s.run_ok(&["start", "s1", "--branch", "+topic", "--detach"]);
    let wt = worktree_of(&s, "s1");

    // A DISTRACTER branch `topic` with a DIFFERENT tip exists in the
    // session clone: a misparsed refspec would transfer this one.
    git_in(&wt, &["checkout", "-qb", "topic"]);
    commit_file(&wt, "wrong.txt");
    let topic_tip = git_in(&wt, &["rev-parse", "refs/heads/topic"]);
    git_in(&wt, &["checkout", "+topic"]);
    commit_file(&wt, "right.txt");
    let plus_tip = git_in(&wt, &["rev-parse", "refs/heads/+topic"]);

    // The transfer picks `+topic`, the SESSION branch — not `topic`.
    s.run_ok(&["fetch", "s1"]);
    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/heads/+topic"]),
        plus_tip
    );
    assert_ne!(git_in(&s.repo, &["rev-parse", "refs/heads/+topic"]), topic_tip);
    // The distracter branch did not leak into the host either.
    assert!(!git_try_in(
        &s.repo,
        &["rev-parse", "-q", "--verify", "refs/heads/topic"]
    ));

    // Diverge the host `+topic` and retry: the rejection proves the
    // leading `+` can no longer become a force marker.
    git_in(&s.repo, &["checkout", "+topic"]);
    commit_file(&s.repo, "host.txt");
    let host_tip = git_in(&s.repo, &["rev-parse", "refs/heads/+topic"]);
    git_in(&s.repo, &["checkout", "master"]);

    let out = s.run(&["fetch", "s1"]);
    assert!(
        !out.status.success(),
        "a diverged host +topic must be rejected, not force-updated"
    );
    assert!(String::from_utf8_lossy(&out.stderr).contains("may have diverged"));
    assert_eq!(git_in(&s.repo, &["rev-parse", "refs/heads/+topic"]), host_tip);
}

/// A short source ref is AMBIGUOUS: git's DWIM order checks
/// `refs/tags/<name>` BEFORE `refs/heads/<name>`, so a session branch
/// `nested` with a same-named tag would transfer the TAG tip. Fully
/// qualifying the source pins the branch.
#[test]
fn fetch_prefers_the_branch_over_a_same_named_tag() {
    let s = Scenario::new_real_git("fetch-branch-not-tag");
    s.run_ok(&["start", "s1", "--branch", "nested", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "branch.txt");
    let branch_tip = git_in(&wt, &["rev-parse", "refs/heads/nested"]);
    // A same-named TAG pointing at the BASE commit (a different tip).
    git_in(&wt, &["tag", "nested", "HEAD~1"]);
    let tag_tip = git_in(&wt, &["rev-parse", "refs/tags/nested"]);
    assert_ne!(branch_tip, tag_tip);

    s.run_ok(&["fetch", "s1"]);

    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/heads/nested"]),
        branch_tip,
        "the BRANCH tip must be transferred, not the tag's"
    );
    assert_ne!(git_in(&s.repo, &["rev-parse", "refs/heads/nested"]), tag_tip);
}

/// `merge` must consume the EXACT fetched ref: with a host tag named
/// like the session branch, a bare `git merge <name>` resolves the TAG
/// (git's DWIM order checks refs/tags/ first), reports success WITHOUT
/// merging the session branch, and then deletes the fetched branch.
/// The ambiguity lives in the HOST ref database — the merge runs there.
#[test]
fn merge_uses_the_branch_not_a_same_named_tag() {
    let s = Scenario::new_real_git("merge-branch-not-tag");
    // A second commit on master, so the tag can point "elsewhere" than
    // the session base.
    commit_file(&s.repo, "host.txt");
    let tag_target = git_in(&s.repo, &["rev-parse", "HEAD~1"]);

    s.run_ok(&["start", "s1", "--branch", "nested", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "session.txt");

    // A HOST tag `nested` pointing elsewhere.
    git_in(&s.repo, &["tag", "nested", &tag_target]);

    s.run_ok(&["merge", "s1"]);

    // The session work IS merged into master …
    assert!(
        git_try_in(&s.repo, &["cat-file", "-e", "refs/heads/master:session.txt"]),
        "the session branch commit must be merged, not the tag's"
    );
    // … the tag still points elsewhere, untouched …
    assert_eq!(git_in(&s.repo, &["rev-parse", "refs/tags/nested"]), tag_target);
    // … and the temporary fetched branch is cleaned up.
    assert!(!git_try_in(
        &s.repo,
        &["rev-parse", "-q", "--verify", "refs/heads/nested"]
    ));
}

/// `push` must publish the session branch via an explicit, NON-FORCED,
/// fully qualified refspec: a bare `git push origin +topic` would parse
/// the leading `+` as a FORCE marker and push the branch `topic`
/// instead — the wrong branch, or an outright failure.
#[test]
fn push_publishes_the_plus_named_branch_itself() {
    let s = Scenario::new_real_git("push-plus-named-branch");
    // A real bare remote for the host repository (the push target).
    git_in(&s.root, &["init", "-q", "--bare", "remote.git"]);
    let bare = s.root.join("remote.git");
    git_in(
        &s.repo,
        &["remote", "add", "origin", &bare.display().to_string()],
    );

    s.run_ok(&["start", "s1", "--branch", "+topic", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "right.txt");
    let plus_tip = git_in(&wt, &["rev-parse", "refs/heads/+topic"]);
    // A DISTRACTER branch `topic` with a DIFFERENT tip in the session
    // clone (NOT transferred to the host by the implicit fetch).
    git_in(&wt, &["checkout", "-qb", "topic"]);
    commit_file(&wt, "wrong.txt");
    git_in(&wt, &["checkout", "+topic"]);

    s.run_ok(&["push", "s1"]);

    // The remote got `+topic` — the session branch — and nothing else.
    assert_eq!(git_in(&bare, &["rev-parse", "refs/heads/+topic"]), plus_tip);
    assert!(!git_try_in(
        &bare,
        &["rev-parse", "-q", "--verify", "refs/heads/topic"]
    ));
}

// --- destruction ----------------------------------------------------------

/// `clone.defaultRemoteName` must not move the clone's remote: the CLI
/// pins the remote with `--origin origin`, so an existing non-current
/// host branch is still found at `refs/remotes/origin/<branch>` even
/// when the user's Git configuration defaults new clones to a different
/// remote name. Without the pin, the probe misses the branch and the
/// session silently starts at the base commit, diverging from the real
/// host branch.
#[test]
fn start_pins_the_clone_remote_name_against_default_remote_name() {
    let s = Scenario::new_real_git("clone-remote-name-pinned");
    git_in(&s.repo, &["checkout", "-qb", "feature"]);
    commit_file(&s.repo, "f1.txt");
    let feature_tip = git_in(&s.repo, &["rev-parse", "feature"]);
    git_in(&s.repo, &["checkout", "master"]);

    // The scenario's isolated HOME carries the hostile configuration —
    // without the `--origin origin` pin, `git clone` (run by the CLI with
    // this HOME) would name the remote `upstream`.
    fs::write(
        s.home.join(".gitconfig"),
        "[clone]\n\tdefaultRemoteName = upstream\n",
    )
    .unwrap();

    s.run_ok(&["start", "s1", "--branch", "feature", "--detach"]);
    let wt = worktree_of(&s, "s1");
    // The clone's remote is pinned to `origin` regardless of the
    // configuration.
    assert_eq!(git_in(&wt, &["remote"]), "origin");
    // The session branch starts at the HOST feature tip, not at the base
    // commit.
    assert_eq!(git_in(&wt, &["rev-parse", "feature"]), feature_tip);

    // The session-to-host fetch still ADVANCES the host branch, never
    // replaces it.
    commit_file(&wt, "f2.txt");
    let session_tip = git_in(&wt, &["rev-parse", "feature"]);
    s.run_ok(&["fetch", "s1"]);
    assert_eq!(git_in(&s.repo, &["rev-parse", "feature"]), session_tip);
    assert!(git_try_in(
        &s.repo,
        &["merge-base", "--is-ancestor", &feature_tip, "feature"]
    ));
}

#[test]
fn destroy_delete_branch_of_never_fetched_session_succeeds() {
    // The common case: the session branch exists ONLY in its clone, so
    // `--delete-branch` must succeed without a host-local branch and clean
    // everything up.
    let s = Scenario::new_real_git("destroy-never-fetched-session");
    s.run_ok(&["start", "s1", "--detach"]);

    s.run_ok(&["destroy", "s1", "--delete-branch"]);

    assert_session_gone(&s, "s1");
    assert!(!git_try_in(
        &s.repo,
        &["show-ref", "--verify", "--quiet", "refs/heads/agent/gvisor/s1"]
    ));
}

#[test]
fn start_force_replaces_a_never_fetched_session() {
    let s = Scenario::new_real_git("start-force-replacement");
    s.run_ok(&["start", "s1", "--detach"]);
    commit_file(&worktree_of(&s, "s1"), "wip.txt");

    // The replacement destroys the old session INCLUDING its clone-local
    // branch; no host-local branch exists, so this must succeed.
    s.run_ok(&["start", "s1", "--force", "--detach"]);

    let wt = worktree_of(&s, "s1");
    assert!(meta_dir_of(&s, "s1").join("meta").is_file());
    assert_eq!(
        git_in(&wt, &["rev-parse", "--abbrev-ref", "HEAD"]),
        "agent/gvisor/s1"
    );
    // The new session starts from the host's HEAD, not the old session's
    // (now destroyed) work.
    assert!(!wt.join("wip.txt").exists());
}

#[test]
fn destroy_delete_branch_removes_a_fetched_host_branch() {
    let s = Scenario::new_real_git("destroy-deletes-fetched-host-branch");
    s.run_ok(&["start", "s1", "--detach"]);
    commit_file(&worktree_of(&s, "s1"), "work.txt");
    s.run_ok(&["fetch", "s1"]);
    assert!(git_try_in(
        &s.repo,
        &["show-ref", "--verify", "--quiet", "refs/heads/agent/gvisor/s1"]
    ));

    s.run_ok(&["destroy", "s1", "--delete-branch"]);

    assert_session_gone(&s, "s1");
    assert!(!git_try_in(
        &s.repo,
        &["show-ref", "--verify", "--quiet", "refs/heads/agent/gvisor/s1"]
    ));
}

#[test]
fn destroy_delete_branch_succeeds_after_merge() {
    // The documented cleanup after a successful `merge` (which deletes the
    // temporary host ref itself) must succeed.
    let s = Scenario::new_real_git("destroy-after-merge");
    s.run_ok(&["start", "s1", "--detach"]);
    commit_file(&worktree_of(&s, "s1"), "merged.txt");

    s.run_ok(&["merge", "s1"]);

    let log = git_in(&s.repo, &["log", "--format=%s"]);
    assert!(log.contains("add merged.txt"), "session work not merged:\n{log}");
    assert!(!git_try_in(
        &s.repo,
        &["show-ref", "--verify", "--quiet", "refs/heads/agent/gvisor/s1"]
    ));

    s.run_ok(&["destroy", "s1", "--delete-branch"]);
    assert_session_gone(&s, "s1");
}

#[test]
fn destroy_reports_genuine_branch_deletion_failure_and_keeps_the_session() {
    let s = Scenario::new_real_git("destroy-genuine-failure-keeps-session");
    s.run_ok(&["start", "s1", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "work.txt");
    s.run_ok(&["fetch", "s1"]);

    // A host-local branch that is CHECKED OUT cannot be deleted by
    // `git branch -D`: a genuine Git failure, not "absent".
    git_in(&s.repo, &["checkout", "agent/gvisor/s1"]);
    let out = s.run(&["destroy", "s1", "--delete-branch"]);
    assert!(
        !out.status.success(),
        "destroy must fail when the host branch cannot be deleted"
    );
    assert!(String::from_utf8_lossy(&out.stderr)
        .contains("could not delete branch agent/gvisor/s1"));
    // The session stays RECOVERABLE: clone, metadata and registry entry
    // survive the failed destroy.
    assert!(wt.exists());
    assert!(meta_dir_of(&s, "s1").join("meta").is_file());
    assert!(fs::symlink_metadata(s.state.join("sessions").join("s1")).is_ok());

    // Once the genuine failure is resolved, the destroy completes.
    git_in(&s.repo, &["checkout", "master"]);
    s.run_ok(&["destroy", "s1", "--delete-branch"]);
    assert_session_gone(&s, "s1");
}

/// A genuine branch-deletion failure must abort the destroy BEFORE any
/// session resource is removed — including the container and the
/// persistent Nix store volume (docs/nix-in-sandbox.md), which no later
/// retry could bring back. The branch operation runs directly after the
/// dirty-worktree safety check; a failed `--delete-branch` destroy
/// therefore leaves a FULLY recoverable session.
#[test]
fn destroy_delete_branch_failure_runs_before_podman_cleanup() {
    let s = Scenario::new_real_git("destroy-failure-before-podman-cleanup");
    s.run_ok(&["start", "s1", "--nix", "--detach"]);
    let wt = worktree_of(&s, "s1");
    commit_file(&wt, "work.txt");
    s.run_ok(&["fetch", "s1"]);

    // A host-local branch that is CHECKED OUT cannot be deleted by
    // `git branch -D`: a genuine Git failure, not "absent".
    git_in(&s.repo, &["checkout", "agent/gvisor/s1"]);

    let repo_id = expected_repo_id(&s.repo);
    let container = format!("agent-{repo_id}-s1");

    // Only the destroy's own Podman calls are visible from here on.
    clear_recordings(&s, "podman");
    let out = s.run(&["destroy", "s1", "--delete-branch"]);
    assert!(
        !out.status.success(),
        "destroy must fail when the host branch cannot be deleted"
    );
    assert!(String::from_utf8_lossy(&out.stderr)
        .contains("could not delete branch agent/gvisor/s1"));

    // NO Podman cleanup ran: no `podman rm`, and not even a
    // `volume exists` probe.
    assert!(
        s.recorded_starting_with("podman", "rm").is_empty(),
        "podman rm must not run before the branch deletion succeeds"
    );
    assert!(
        s.recorded_starting_with("podman", "volume").is_empty(),
        "no volume probe or removal may run before the branch deletion \
         succeeds"
    );
    // The container and its Nix store volume survive, alongside the
    // clone, the metadata and the registry entry.
    assert!(
        s.record.join("containers").join(&container).is_dir(),
        "container state must survive the failed destroy"
    );
    assert!(wt.exists());
    assert!(meta_dir_of(&s, "s1").join("meta").is_file());
    assert!(fs::symlink_metadata(s.state.join("sessions").join("s1")).is_ok());

    // Once the genuine failure is resolved, the destroy completes,
    // INCLUDING the persistent Nix store volume removal.
    git_in(&s.repo, &["checkout", "master"]);
    s.run_ok(&["destroy", "s1", "--delete-branch"]);
    assert_session_gone(&s, "s1");
    let volume_rms = s
        .recorded_starting_with("podman", "volume")
        .into_iter()
        .filter(|c| c.iter().any(|a| a == "rm"))
        .count();
    assert_eq!(
        volume_rms, 1,
        "the nix volume must be removed exactly once by the retry"
    );
}

#[test]
fn destroy_delete_branch_never_touches_same_named_tags_or_remotes() {
    let s = Scenario::new_real_git("destroy-ignores-tags-and-remotes");
    // A tag named EXACTLY like the session branch …
    let tag_target = git_in(&s.repo, &["rev-parse", "HEAD"]);
    git_in(&s.repo, &["tag", "agent/gvisor/s1"]);
    // … and a host remote-tracking ref of the same name (a second
    // repository publishes a branch like the session branch).
    let r2 = s.root.join("r2");
    fs::create_dir_all(&r2).unwrap();
    git_in(&r2, &["init", "-q", "-b", "agent/gvisor/s1"]);
    git_in(&r2, &["config", "user.email", "r2@example.com"]);
    git_in(&r2, &["config", "user.name", "r2"]);
    fs::write(r2.join("r2.txt"), "r2\n").unwrap();
    git_in(&r2, &["add", "."]);
    git_in(&r2, &["commit", "-qm", "r2"]);
    git_in(&s.repo, &["remote", "add", "origin", r2.to_str().unwrap()]);
    git_in(&s.repo, &["fetch", "-q", "origin"]);
    let r2_tip = git_in(&s.repo, &["rev-parse", "refs/remotes/origin/agent/gvisor/s1"]);

    s.run_ok(&["start", "s1", "--detach"]);
    s.run_ok(&["destroy", "s1", "--delete-branch"]);

    // Neither the tag nor the remote-tracking ref was deleted …
    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/tags/agent/gvisor/s1"]),
        tag_target
    );
    assert_eq!(
        git_in(&s.repo, &["rev-parse", "refs/remotes/origin/agent/gvisor/s1"]),
        r2_tip
    );
    // … and no host-local branch was created for it either.
    assert!(!git_try_in(
        &s.repo,
        &["show-ref", "--verify", "--quiet", "refs/heads/agent/gvisor/s1"]
    ));
    assert_session_gone(&s, "s1");
}
