// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Filter model, URL-hash state and presets.
//
// The label filter and the search box are deliberately kept separate:
// labels are curated vocabulary (52 of 384 issues carry `mysbx`), while
// search hits any mention in title or id (326 mention "mysbx" in
// text). Merging the two would make the label subset look broken.

import { DATA, CLUSTER } from "./core.js";

export const state = {
  labels: new Set(),      // selected labels (AND-of-OR: issue matches if it has ANY selected label)
  status: "",            // "" = all
  type: "",              // "" = all
  priority: "",          // "" = all
  lane: "",              // cluster root id, "" = all
  assignee: "",          // "" = all
  blockedOnly: false,
  showClosed: false,     // default off: most beads in a mature DB are closed
  search: "",            // free text over title + id
  searchDescriptions: false,
};

// ---------------------------------------------------------------- filtering

export function matches(issue) {
  if (!state.showClosed && issue.status === "closed") return false;
  if (state.status && issue.status !== state.status) return false;
  if (state.type && issue.issue_type !== state.type) return false;
  if (state.priority && String(issue.priority) !== state.priority) return false;
  if (state.lane && (CLUSTER.get(issue.id) || issue.id) !== state.lane) return false;
  if (state.assignee && (issue.assignee || "") !== state.assignee) return false;
  if (state.blockedOnly && !issue.is_blocked) return false;
  if (state.labels.size) {
    const labels = issue.labels || []; // sparse: key absent on most issues
    if (![...state.labels].some((l) => labels.includes(l))) return false;
  }
  if (state.search) {
    const q = state.search.toLowerCase();
    const inTitle = issue.title.toLowerCase().includes(q) || issue.id.toLowerCase().includes(q);
    const inDesc = state.searchDescriptions
      && (issue.description || "").toLowerCase().includes(q);
    if (!inTitle && !inDesc) return false;
  }
  return true;
}

// ---------------------------------------------------------------- URL hash

export function writeToHash() {
  const params = new URLSearchParams();
  if (state.labels.size) params.set("labels", [...state.labels].sort().join(","));
  if (state.status) params.set("status", state.status);
  if (state.type) params.set("type", state.type);
  if (state.priority) params.set("priority", state.priority);
  if (state.lane) params.set("lane", state.lane);
  if (state.assignee) params.set("assignee", state.assignee);
  if (state.blockedOnly) params.set("blocked", "1");
  if (state.showClosed) params.set("closed", "1");
  if (state.search) params.set("q", state.search);
  if (state.searchDescriptions) params.set("qd", "1");
  const hash = params.toString();
  history.replaceState(null, "", hash ? `#${hash}` : location.pathname + location.search);
}

export function readFromHash() {
  const params = new URLSearchParams(location.hash.replace(/^#/, ""));
  state.labels = new Set((params.get("labels") || "").split(",").filter(Boolean));
  state.status = params.get("status") || "";
  state.type = params.get("type") || "";
  state.priority = params.get("priority") || "";
  state.lane = params.get("lane") || "";
  state.assignee = params.get("assignee") || "";
  state.blockedOnly = params.get("blocked") === "1";
  state.showClosed = params.get("closed") === "1";
  state.search = params.get("q") || "";
  state.searchDescriptions = params.get("qd") === "1";
  // a label bookmark is a curated subset; closed beads belong in it
  if (state.labels.size && !params.has("closed")) state.showClosed = true;
}

// ---------------------------------------------------------------- presets

const BUILTIN_PRESETS = {
  mysbx: { labels: ["mysbx"], showClosed: true },
  all: {},
};

export function applyPreset(name, me) {
  Object.assign(state, {
    labels: new Set(), status: "", type: "", priority: "",
    lane: "", assignee: "", blockedOnly: false,
    showClosed: false, search: "", searchDescriptions: false,
  });
  const preset = BUILTIN_PRESETS[name];
  if (preset) {
    if (preset.labels) state.labels = new Set(preset.labels);
    if (preset.showClosed) state.showClosed = true;
  } else if (name === "mine") {
    state.assignee = me || "";
  } else {
    // user preset from localStorage
    const saved = localStorage.getItem("beads-server:preset:" + name);
    if (saved) {
      try {
        const parsed = JSON.parse(saved);
        Object.assign(state, parsed);
        state.labels = new Set(parsed.labels || []);
      } catch { /* corrupt preset: ignore */ }
    }
  }
}

export function savePreset(name) {
  localStorage.setItem(
    "beads-server:preset:" + name,
    JSON.stringify({ ...state, labels: [...state.labels] }),
  );
}

export function userPresets() {
  return Object.keys(localStorage)
    .filter((k) => k.startsWith("beads-server:preset:"))
    .map((k) => k.slice("beads-server:preset:".length));
}
