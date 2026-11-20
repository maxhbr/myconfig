// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// History view: the activity feed (changes observed since server
// start) and the per-bead commit history popup (bd history).
//
// The two sources are complementary and labelled as such: the feed
// shows what the watcher saw live (revision + wall-clock, no commit
// identity), the popup shows bd's committed history (commit hashes +
// commit dates, no actor — the events journal is disabled).

import { DATA, esc, fmtDate, h, liveIssue } from "./core.js";

let feedSeq = 0; // busts the per-render stub every render()

export function renderHistory() {
  const wrap = document.getElementById("history-wrap");
  wrap.innerHTML = "";
  const seq = ++feedSeq;
  wrap.append(h("div", "history-empty", "loading activity…"));

  // staleness guard: every render() bumps feedSeq, so a response is
  // only rendered while it is still the newest request
  fetch("/api/activity", { cache: "no-store" })
    .then((r) => (r.ok ? r.json() : null))
    .then((data) => {
      if (seq !== feedSeq) return;
      wrap.innerHTML = "";
      if (!data) {
        wrap.append(h("div", "history-empty", "failed to load /api/activity"));
        return;
      }
      buildFeed(wrap, data);
    })
    .catch(() => {
      if (seq !== feedSeq) return;
      wrap.innerHTML = "";
      wrap.append(h("div", "history-empty", "failed to load /api/activity"));
    });
}

function link(id) {
  const a = h("span", "dep-link", id);
  a.onclick = () => {
    const issue = liveIssue(id) || DATA.issues.find((i) => i.id === id);
    if (issue) window.showDetails(issue);
  };
  return a;
}

function fmtTime(seconds) {
  return new Date(seconds * 1000).toLocaleTimeString();
}

function changeList(changes) {
  const wrap = h("div", "history-changes");
  for (const c of changes) {
    const line = h("div", "history-change");
    line.append(h("span", "history-field", c.field));
    line.append(h("span", null, ": "));
    const from = formatValue(c.from);
    const to = formatValue(c.to);
    if (from !== "") line.append(h("span", "history-from", from));
    if (from !== "" && to !== "") line.append(h("span", null, " → "));
    if (to !== "") line.append(h("span", "history-to", to));
    wrap.append(line);
  }
  return wrap;
}

function formatValue(v) {
  if (v === null || v === undefined) return "";
  if (Array.isArray(v)) return v.join(", ");
  return String(v);
}

const KIND_LABEL = { created: "created", changed: "changed", touched: "touched", deleted: "deleted" };

function buildFeed(wrap, data) {
  if (!data.activity.length) {
    const empty = h("div", "history-empty");
    empty.append("no changes observed since server start (");
    empty.append(h("span", null, fmtDate(
      new Date(data.since * 1000).toISOString().replace("Z", "")
    )));
    empty.append(")");
    wrap.append(empty);
    return;
  }
  const list = h("div", "history-list");
  for (const e of data.activity) {
    const row = h("div", "history-row");
    row.append(h("span", "history-time", fmtTime(e.time)));
    const head = h("span", "history-head");
    head.append(link(e.id));
    head.append(h("span", null, " " + (KIND_LABEL[e.kind] || e.kind)));
    if (e.title) {
      head.append(h("span", null, " — "));
      head.append(h("span", "history-title", esc(e.title)));
    }
    row.append(head);
    if (e.changes.length) row.append(changeList(e.changes));
    row.append(historyButton(e.id));
    list.append(row);
  }
  wrap.append(list);
  const note = h("div", "history-note");
  note.textContent =
    "live feed: changes this server observed since it started " +
    "(revision numbers, no commit identity)";
  wrap.append(note);
}

function historyButton(id) {
  const b = h("button", "history-button", "history");
  b.title = "commit history of " + id + " (bd history)";
  b.onclick = (ev) => {
    ev.stopPropagation();
    showHistoryPopup(id, b);
  };
  return b;
}

// ---------------------------------------------------------------- popup

let popup = null;

function closePopup() {
  if (popup) {
    popup.remove();
    popup = null;
  }
}

export function showHistoryPopup(id, anchor) {
  closePopup();
  popup = h("div", "history-popup");
  const title = h("div", "history-popup-title");
  title.append(h("span", null, "history of "));
  title.append(h("span", "dep-link", id));
  const close = h("button", "history-popup-close", "✕");
  close.onclick = closePopup;
  title.append(close);
  popup.append(title);
  popup.append(h("div", "history-empty", "loading bd history…"));

  const rect = anchor.getBoundingClientRect();
  const width = 460;
  const height = 380;
  popup.style.left = Math.max(8, Math.min(
    rect.left, window.innerWidth - width - 16
  )) + "px";
  popup.style.top = Math.max(8, Math.min(
    rect.bottom + 6, window.innerHeight - height - 16
  )) + "px";
  document.body.append(popup);
  popup.onkeydown = (ev) => { if (ev.key === "Escape") closePopup(); };
  popup.tabIndex = -1;
  popup.focus();
  popup.addEventListener("focusout", () => {
    // a focusout that left the popup entirely (not just moved between
    // its own children)
    setTimeout(() => {
      if (popup && !popup.contains(document.activeElement)) closePopup();
    }, 0);
  });
  document.addEventListener("click", outsideClick, { capture: true });

  fetch(`/api/beads/issue/${encodeURIComponent(id)}/history`, { cache: "no-store" })
    .then((r) => (r.ok ? r.json() : null))
    .then((data) => {
      if (!popup) return;
      popup.innerHTML = "";
      popup.append(title);
      if (!data || data.history === null || data.history === undefined) {
        popup.append(h("div", "history-empty", "no commit history available"));
        return;
      }
      buildCommitHistory(popup, data);
    })
    .catch(() => {
      if (!popup) return;
      popup.innerHTML = "";
      popup.append(title);
      popup.append(h("div", "history-empty", "failed to load history"));
    });
}

function outsideClick(ev) {
  if (popup && !popup.contains(ev.target)) closePopup();
}

function buildCommitHistory(container, data) {
  const list = h("div", "history-popup-list");
  if (!data.history.length) {
    list.append(h("div", "history-empty", "no changes in commit history"));
  }
  for (const e of data.history) {
    const row = h("div", "history-row");
    const head = h("span", "history-head");
    const date = (e.date || "").replace("T", " ").replace("Z", "");
    head.append(h("span", "history-time", date));
    head.append(h("span", "history-commit", (e.commit || "").slice(0, 8)));
    if (e.committer) head.append(h("span", null, " " + e.committer));
    row.append(head);
    row.append(changeList(e.changes));
    list.append(row);
  }
  container.append(list);
  const note = h("div", "history-note");
  note.textContent =
    "committed history from bd (commit dates; no actor — the events " +
    "journal is disabled for this workspace)";
  container.append(note);
}

// Inline (details pane / deep-link) variant: async, appended into a
// container the caller owns; returns a cancel/ignore handle so a pane
// re-render never paints stale content into a rebuilt section.
export function loadHistorySection(id, container) {
  const placeholder = h("div", "history-empty", "loading bd history…");
  container.append(placeholder);
  let cancelled = false;
  fetch(`/api/beads/issue/${encodeURIComponent(id)}/history`, { cache: "no-store" })
    .then((r) => (r.ok ? r.json() : null))
    .then((data) => {
      if (cancelled) return;
      placeholder.remove();
      if (!data || data.history === null || data.history === undefined) {
        container.append(h("div", "history-empty", "no commit history available"));
        return;
      }
      buildCommitHistory(container, data);
    })
    .catch(() => {
      if (cancelled) return;
      placeholder.remove();
      container.append(h("div", "history-empty", "failed to load history"));
    });
  return () => { cancelled = true; };
}

document.addEventListener("keydown", (ev) => {
  if (ev.key === "Escape") closePopup();
});
