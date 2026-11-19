// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Edit controls: inline fields, description editor, comment composer,
// label editor, new-bead dialog, drag-and-drop. Only active when
// /api/capabilities advertises write; every mutation requires the
// per-run token from /api/session.

import { liveIssue } from "./core.js";

let TOKEN = null;
let CAPABILITIES = { write: false };

export async function init() {
  try {
    const r = await fetch("/api/capabilities");
    CAPABILITIES = await r.json();
  } catch {
    CAPABILITIES = { write: false };
  }
  if (CAPABILITIES.write) {
    try {
      const r = await fetch("/api/session");
      TOKEN = (await r.json()).token;
    } catch {
      CAPABILITIES.write = false;
    }
  }
  const banner = document.getElementById("edit-banner");
  banner.hidden = false;
  banner.textContent = CAPABILITIES.write
    ? "editing enabled — every save runs bd and may create a git commit"
    : "read-only — start the server with --allow-write to edit";
  banner.classList.toggle("enabled", CAPABILITIES.write);
}

async function post(path, body) {
  const r = await fetch(path, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "X-Beads-Server-Token": TOKEN || "",
    },
    body: JSON.stringify(body),
  });
  const data = await r.json().catch(() => ({}));
  if (r.status === 409 && data.issue) {
    // reconcile: install the fresh record and tell the open pane, so
    // the same control cannot keep sending the same stale guard
    window.reconcileConflict(data.issue);
  } else if (r.status === 409) {
    toast("conflict: the bead changed elsewhere — reloading it");
    window.refetchAll();
  } else if (!r.ok) {
    toast(`save failed: ${data.error || r.status}`);
  } else if (data.output) {
    // bd prints its commit/summary line; surface it so the git commit
    // auto_commit creates is never silent
    toast(data.output);
  }
  return { ok: r.ok, status: r.status, data };
}

export function canWrite() {
  return !!CAPABILITIES.write;
}

// ---------------------------------------------------------------- toast

let toastTimer = null;
export function toast(text) {
  const el = document.getElementById("toast");
  el.textContent = text;
  el.classList.add("visible");
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => el.classList.remove("visible"), 4000);
}

// ------------------------------------------------------- inline controls

export function inlineSelect(issue, field, values, current) {
  const sel = document.createElement("select");
  sel.className = "edit-select";
  for (const v of values) {
    const o = document.createElement("option");
    o.value = v; o.textContent = v;
    if (String(v) === String(current)) o.selected = true;
    sel.append(o);
  }
  sel.disabled = !canWrite();
  sel.onchange = async () => {
    const body = { [field]: sel.value, if_status: issue.status };
    await post(`/api/beads/issue/${issue.id}/update`, body);
  };
  return sel;
}

export function inlinePriority(issue) {
  return inlineSelect(issue, "priority", [0, 1, 2, 3, 4], issue.priority);
}

export function inlineAssignee(issue, assignees) {
  return inlineSelect(issue, "assignee", [...assignees, ""], issue.assignee || "");
}

export function inlineStatus(issue, statuses) {
  return inlineSelect(issue, "status", statuses, issue.status);
}

export function inlineText(issue, field, value) {
  const input = document.createElement("input");
  input.type = "text";
  input.className = "edit-input";
  input.value = value ?? "";
  input.disabled = !canWrite();
  input.onchange = async () => {
    await post(`/api/beads/issue/${issue.id}/update`, {
      [field]: input.value,
      if_status: issue.status,
    });
  };
  return input;
}

// ------------------------------------------------------ description edit

// panes register their draft probes here so a conflict reconcile can
// ask "is the user mid-edit?" without the pane rebuilding under them
const draftProbes = new Set();
export function registerDraftProbe(probe) {
  draftProbes.add(probe);
}
export function clearDraftProbes() {
  draftProbes.clear(); // the pane owning the probes is being rebuilt
}
window.detailsHasDraft = () => [...draftProbes].some((p) => p());

export function descriptionEditor(issue) {
  const wrap = document.createElement("div");
  const ta = document.createElement("textarea");
  ta.className = "edit-textarea";
  // The concurrency story, honestly: bd 1.3.4 guards only status and
  // assignee, so there is NO atomic description precondition. The
  // editor therefore tracks the LAST SAVED value and re-reads the
  // LIVE record (by id, never the captured object — live updates
  // replace it) before every save: if the live description differs
  // from what we last saved or started from, someone else wrote and
  // the draft is preserved for review instead of overwriting them.
  const liveIssueById = () => liveIssue(issue.id) || issue;
  let base = issue.description || "";
  ta.value = base;
  ta.disabled = !canWrite();
  ta.placeholder = "description (markdown)";
  // `pending` = the textarea holds text that is not saved yet — the
  // draft probe reads THIS, not the autosave flag, so a save that is
  // rejected (409) or still in flight keeps reporting a draft and
  // the pane is never rebuilt out from under the user.
  let pending = false;
  let saving = false;
  let queued = false;
  let conflict = null; // exact version shown to the user, including ""
  let timer;
  const save = async (explicit = false) => {
    if (!canWrite() || !pending || (conflict !== null && !explicit)) return;
    if (saving) {
      queued = true;
      return;
    }
    const live = liveIssueById();
    const theirs = live.description || "";
    if (theirs !== (conflict ?? base)) {
      showConflict(theirs);
      return;
    }
    saving = true;
    queued = false;
    const value = ta.value;
    try {
      const res = await post(`/api/beads/issue/${issue.id}/update`, {
        description: value,
        if_status: live.status,
      });
      if (res.ok) {
        base = value;
        // A replaced record belongs to SSE, even if its text was reverted
        // to the old value while this response was in flight.
        if (liveIssueById() === live) live.description = value;
        conflict = null;
        conflictBox.hidden = true;
        pending = ta.value !== value;
      } else if (res.status === 409 && res.data.issue) {
        showConflict(res.data.issue.description || "");
      }
      if (!res.ok) return; // never replay a rejected write
    } catch {
      toast("save failed — draft kept");
      return;
    } finally {
      saving = false;
    }
    // A debounce, blur or Ctrl-S may have already fired during the POST.
    // Flush the latest text once, but only after a successful write.
    if (queued && pending) return save();
  };
  // debounce: description edits arrive one keystroke at a time, and
  // each save is a bd run (potentially a git commit)
  registerDraftProbe(() => pending || saving);
  const arm = () => {
    if (!canWrite()) return;
    pending = saving || conflict !== null || ta.value !== base;
    clearTimeout(timer);
    if (pending && conflict === null) timer = setTimeout(save, 1500);
  };
  ta.oninput = arm;
  ta.onblur = () => {
    if (!canWrite()) return;
    // blur either flushes a pending edit or cancels the timer — it
    // must never submit on its own and never leave the timer armed
    clearTimeout(timer);
    save();
  };
  ta.onkeydown = (ev) => {
    if (ev.key === "s" && (ev.ctrlKey || ev.metaKey)) {
      ev.preventDefault();
      clearTimeout(timer);
      save(true);
    }
  };
  const hint = document.createElement("div");
  hint.className = "edit-hint";
  hint.textContent = canWrite()
    ? "autosaves 1.5 s after typing stops · Ctrl-S saves now"
    : "";
  const conflictBox = document.createElement("div");
  conflictBox.className = "edit-hint conflict-hint";
  conflictBox.hidden = true;
  const remoteText = document.createElement("pre");
  conflictBox.append("Changed elsewhere. Autosave paused; Ctrl-S overwrites this version:", remoteText);
  wrap.append(ta, conflictBox, hint);
  const showConflict = (description) => {
    clearTimeout(timer);
    conflict = description;
    remoteText.textContent = description || "(empty)";
    conflictBox.hidden = false;
    toast("conflict — draft kept");
  };
  return wrap;
}

// ------------------------------------------------------------ comments

export function commentComposer(issue) {
  const wrap = document.createElement("div");
  const ta = document.createElement("textarea");
  ta.className = "edit-textarea";
  ta.placeholder = "write a comment…";
  ta.disabled = !canWrite();
  registerDraftProbe(() => ta.value.trim() !== "");
  const btn = document.createElement("button");
  btn.textContent = "comment";
  btn.disabled = !canWrite();
  btn.onclick = async () => {
    if (!ta.value.trim()) return;
    const res = await post(`/api/beads/issue/${issue.id}/comment`, {
      text: ta.value,
    });
    if (res.ok) {
      ta.value = "";
      window.refetchAll();
    }
  };
  wrap.append(ta, btn);
  return wrap;
}

// --------------------------------------------------------------- labels

export function labelEditor(issue) {
  const wrap = document.createElement("div");
  const input = document.createElement("input");
  input.type = "text";
  input.placeholder = "add label…";
  input.disabled = !canWrite();
  input.onchange = async () => {
    for (const label of input.value.split(",").map((l) => l.trim()).filter(Boolean)) {
      await post(`/api/beads/issue/${issue.id}/update`, {
        add_labels: [label],
        if_status: issue.status,
      });
    }
    input.value = "";
  };
  for (const label of issue.labels || []) {
    const chip = document.createElement("span");
    chip.className = "label-chip selected";
    chip.textContent = label;
    if (canWrite()) {
      chip.title = "click to remove";
      chip.onclick = async () => {
        await post(`/api/beads/issue/${issue.id}/update`, {
          remove_labels: [label],
          if_status: issue.status,
        });
      };
    }
    wrap.append(chip);
  }
  wrap.append(input);
  return wrap;
}

// ------------------------------------------------------------ new bead

export function newBeadDialog() {
  const title = prompt("new bead title");
  if (!title) return;
  post("/api/beads", { title }).then(() => window.refetchAll());
}

// --------------------------------------------------------- drag-and-drop

export function makeDraggable(cardEl, issue) {
  if (!canWrite()) return;
  cardEl.draggable = true;
  cardEl.ondragstart = (ev) => {
    ev.dataTransfer.setData("text/plain", issue.id);
    ev.dataTransfer.effectAllowed = "move";
  };
}

export function makeDropTarget(colEl, status, lookupIssue) {
  if (!canWrite()) return;
  colEl.ondragover = (ev) => {
    ev.preventDefault();
    ev.dataTransfer.dropEffect = "move";
  };
  colEl.ondrop = async (ev) => {
    ev.preventDefault();
    const id = ev.dataTransfer.getData("text/plain");
    if (!id) return;
    const issue = lookupIssue(id);
    // guard the drag: if the card moved elsewhere meanwhile, 409 wins
    await post(`/api/beads/issue/${id}/update`, {
      status,
      ...(issue ? { if_status: issue.status } : {}),
    });
  };
}
