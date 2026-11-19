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
  computeClusters();
}

export function select(id) {
  SELECTED = id;
}

export function byId() {
  return new Map(DATA.issues.map((i) => [i.id, i]));
}

// ---------------------------------------------------------------- clusters

// A cluster groups beads that belong to one epic / big work area:
// parent-child links first, then discovered-from as a fallback so that
// beads discovered while working on an issue land in its lane.
function parentOf(issue) {
  const p = issue.parent;
  if (typeof p === "string") return p;
  if (p && typeof p === "object" && p.id) return p.id;
  for (const d of issue.dependencies || []) {
    if (d.type === "parent-child") return d.depends_on_id;
  }
  return null;
}

function computeClusters() {
  const map = new Map(DATA.issues.map((i) => [i.id, i]));

  // pass 1: strict parent chains (parent field / parent-child deps)
  const strictRoot = new Map();
  for (const issue of DATA.issues) {
    const seen = new Set([issue.id]);
    let cur = issue;
    while (true) {
      const p = parentOf(cur);
      if (!p || seen.has(p) || !map.has(p)) break;
      seen.add(p);
      cur = map.get(p);
    }
    strictRoot.set(issue.id, cur.id);
  }

  // a lane exists for every strict cluster with more than one member
  const clusterSize = new Map();
  for (const root of strictRoot.values())
    clusterSize.set(root, (clusterSize.get(root) || 0) + 1);
  const isLane = (root) => (clusterSize.get(root) || 0) > 1;

  // pass 2: beads discovered while working on a lane member join that
  // lane; everything discovered outside a lane stays ungrouped
  CLUSTER = new Map();
  for (const issue of DATA.issues) {
    let root = strictRoot.get(issue.id);
    if (!isLane(root)) {
      for (const d of issue.dependencies || []) {
        if (d.type !== "discovered-from") continue;
        const target = strictRoot.get(d.depends_on_id);
        if (target !== undefined && isLane(target)) { root = target; break; }
      }
    }
    CLUSTER.set(issue.id, root);
  }
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
