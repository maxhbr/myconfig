// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
export const STATUS_COLOR = {
  open: "#4a9eff",
  in_progress: "#f5a623",
  blocked: "#e74c3c",
  closed: "#6abf69",
  deferred: "#9b59b6",
  pinned: "#9b59b6",
  hooked: "#e67e22",
};
export const STATUS_ORDER = [
  "open", "in_progress", "blocked", "hooked", "deferred", "pinned", "closed",
];

// Board columns. Any status not listed falls into the first column and is
// marked on the card with its own colored dot.
export const COLUMNS = [
  { key: "open", label: "Open", statuses: ["open"] },
  { key: "in_progress", label: "In Progress", statuses: ["in_progress"] },
  { key: "blocked", label: "Blocked", statuses: ["blocked"] },
  { key: "deferred", label: "Deferred", statuses: ["deferred", "pinned"] },
  { key: "closed", label: "Closed", statuses: ["closed"] },
];
const COLUMN_STATUS = {}; // status -> column key
for (const c of COLUMNS) for (const s of c.statuses) COLUMN_STATUS[s] = c.key;
export function columnOf(status) {
  return COLUMN_STATUS[status] || "open";
}

export function statusColor(s) {
  return STATUS_COLOR[s] || "#888";
}

export function isClaimed(issue) {
  return issue.status === "in_progress" || !!issue.assignee;
}

export const svgNS = "http://www.w3.org/2000/svg";

export function el(tag, attrs = {}, text) {
  const e = document.createElementNS(svgNS, tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (text !== undefined) e.textContent = text;
  return e;
}

export function h(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

// ---------------------------------------------------------------- data

export let DATA = null; // { revision, issues: [...] }
export let SELECTED = null;
export let CLUSTER = null; // Map: issue id -> cluster root id

export function setData(data) {
  DATA = data;
  CLUSTER = new Map(data.issues.map((i) => [i.id, i.cluster_root || i.id]));
  window.DATA_ISSUES_BY_ID = Object.fromEntries(
    data.issues.map((i) => [i.id, i])
  );
}

export function liveIssue(id) {
  // the authoritative current record: module DATA first, then the
  // window map (kept for closures that captured a replaced object)
  const fromWindow = window.DATA_ISSUES_BY_ID && window.DATA_ISSUES_BY_ID[id];
  if (DATA) {
    const fromData = DATA.issues.find((i) => i.id === id);
    if (fromData) return fromData;
  }
  return fromWindow;
}

export function applySnapshotPatch({ upserted, removed, revision }) {
  const byIdMap = new Map(DATA.issues.map((i) => [i.id, i]));
  for (const issue of upserted) {
    byIdMap.set(issue.id, issue);
    const root = issue.cluster_root || issue.id;
    CLUSTER.set(issue.id, root);
  }
  for (const id of removed) {
    byIdMap.delete(id);
    CLUSTER.delete(id);
  }
  DATA.issues = [...byIdMap.values()];
  if (revision !== undefined) DATA.revision = revision;
  window.DATA_ISSUES_BY_ID = Object.fromEntries(
    DATA.issues.map((i) => [i.id, i])
  );
}

export function applyIndices(idx) {
  // only indices matching the snapshot's generation AND revision
  // describe it (a restart renumbers revisions from zero)
  if (DATA && idx && idx.generation === DATA.generation
      && idx.revision === DATA.revision) {
    DATA.reverse_deps = idx.reverse_deps;
    DATA.blockers = idx.blockers;
    DATA.blocked_by = idx.blocked_by;
  }
}

export function select(id) {
  SELECTED = id;
}

export function byId() {
  return new Map(DATA.issues.map((i) => [i.id, i]));
}

// ---------------------------------------------------------------- misc

export function fmtDate(iso) {
  return iso ? iso.replace("T", " ").replace("Z", "") : "—";
}

export function esc(s) {
  return String(s ?? "");
}

export function shorten(s, n) {
  s = s || "";
  return s.length > n ? s.slice(0, n - 1) + "…" : s;
}
