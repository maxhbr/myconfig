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
  const save = async () => {
    if (!dirty) return;
    const live = liveIssueById();
    if ((live.description || "") !== base) {
      // the record changed underneath: keep the draft and surface
      // both versions — never silently overwrite a concurrent edit
      dirty = false;
      clearTimeout(timer);
      showConflict(live);
      return;
    }
    dirty = false;
    const value = ta.value;
    try {
      const res = await post(`/api/beads/issue/${issue.id}/update`, {
        description: value,
        if_status: live.status,
      });
      if (res.ok) {
        base = value;
        // advance the live record too: the next edit in the same
        // pane must not read as a false conflict
        liveIssueById().description = value;
      } else {
        dirty = true; // 409/4xx: the draft is still pending
      }
    } catch {
      dirty = true; // network error: the edit is still pending
    }
  };
  // debounce: description edits arrive one keystroke at a time, and
  // each save is a bd run (potentially a git commit)
  let timer = null;
  let dirty = false;
  registerDraftProbe(() => dirty);
  const arm = () => {
    if (!canWrite()) return;
    dirty = true;
    clearTimeout(timer);
    timer = setTimeout(save, 1500);
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
      save();
    }
  };
  const hint = document.createElement("div");
  hint.className = "edit-hint";
  hint.textContent = canWrite()
    ? "autosaves 1.5 s after typing stops · Ctrl-S saves now"
    : "";
  wrap.append(ta, hint);
  // a concurrent writer beat us: show their version next to the
  // draft (kept intact above) instead of discarding either side
  const showConflict = (live) => {
    const box = document.createElement("div");
    box.className = "edit-hint conflict-hint";
    const theirs = document.createElement("pre");
    theirs.textContent = live.description || "(empty)";
    const label = document.createElement("div");
    label.textContent =
      "description changed elsewhere — their version (draft kept above, " +
      "Ctrl-S overwrites):";
    box.append(label, theirs);
    wrap.insertBefore(box, hint);
    base = live.description || ""; // next save overwrites consciously
    toast("description changed elsewhere — draft kept");
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
