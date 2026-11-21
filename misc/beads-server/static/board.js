// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Board renderer

import {
  COLUMNS, CLUSTER, DATA, SELECTED, columnOf, h, isClaimed, statusColor,
} from "./core.js";
import { state } from "./filters.js";
import { makeDraggable, makeDropTarget } from "./edit.js";

// lane collapse state: root id -> explicit user decision (boolean);
// absent = follow the default, which collapses closed epic lanes.
const overrides = new Map();
// per-lane expand state for "X more" truncated columns:
// `${laneRoot}|${colKey}` -> boolean (expanded)
const expandedCols = new Set();
export function isCollapsed(laneRoot, closedDefault) {
  return overrides.has(laneRoot)
    ? overrides.get(laneRoot)
    : !!closedDefault;
}
export function toggleLane(laneRoot, closedDefault) {
  overrides.set(laneRoot, !isCollapsed(laneRoot, closedDefault));
}

const MAX_SHOWN_CARDS = 3;
function moreKey(laneRoot, colKey) {
  return (laneRoot || "_ungrouped") + "|" + colKey;
}

export function renderBoard() {
  const board = document.getElementById("board");
  board.innerHTML = "";

  const cols = COLUMNS.filter((c) => state.showClosed || c.key !== "closed");
  board.style.setProperty("--ncols", cols.length);

  // lane = cluster root id -> visible issues
  const byIdMap = new Map(DATA.issues.map((i) => [i.id, i]));
  const lanes = new Map();
  for (const card of window.visibleIssues()) {
    const rootId = CLUSTER.get(card.id) || card.id;
    if (!lanes.has(rootId)) lanes.set(rootId, []);
    lanes.get(rootId).push(card);
  }
  const named = [], ungroupedOpen = [], ungroupedClosed = [];
  for (const [rootId, members] of lanes) {
    if (members.length >= 2) named.push({ rootId, members });
    else for (const m of members)
      (m.status === "closed" ? ungroupedClosed : ungroupedOpen).push(m);
  }
  named.sort((a, b) =>
    b.members.length - a.members.length ||
    a.rootId.localeCompare(b.rootId));
  // demote lanes whose epic root is closed to the bottom, behind a separator
  const rootClosed = (rootId) => {
    const root = byIdMap.get(rootId);
    return !!root && root.status === "closed";
  };
  const openLanes = [], closedLanes = [];
  for (const lane of named)
    (rootClosed(lane.rootId) ? closedLanes : openLanes).push(lane);

  const issueRank = (i) => {
    const statusRank = i.status === "in_progress" ? 0
      : i.status === "closed" ? 2 : 1;
    return [statusRank, i.priority ?? 2, i.id];
  };
  const byRank = (a, b) => {
    const [sa, pa, ia] = issueRank(a), [sb, pb, ib] = issueRank(b);
    return sa - sb || pa - pb || ia.localeCompare(ib);
  };

  // column headers (sticky)
  const head = h("div", "board-grid board-head");
  head.append(h("div", "corner", "lane \\ column"));
  for (const c of cols) {
    const ch = h("div", "colhead");
    const sw = h("span", "swatch");
    sw.style.background = statusColor(c.statuses[0]);
    ch.append(sw, document.createTextNode(c.label));
    ch.append(h("span", "count", String(0)));
    ch.dataset.col = c.key;
    head.append(ch);
  }
  board.append(head);

  const laneCounts = {};
  for (const c of cols) laneCounts[c.key] = 0;

  const buildLane = (laneRoot, members, ungroupedLane) => {
    const lane = h("div", "lane" + (ungroupedLane ? " ungrouped" : ""));
    const headEl = h("div", "lane-head");
    const closed = laneRoot ? rootClosed(laneRoot) : false;
    if (closed) lane.classList.add("lane-closed");
    if (laneRoot) {
      const root = byIdMap.get(laneRoot);
      const rootLink = h("span", "lane-root", laneRoot);
      if (root) rootLink.onclick = (ev) => { ev.stopPropagation(); window.showDetails(root); };
      headEl.append(rootLink);
      if (closed) headEl.append(h("span", "lane-closed-badge", "✓ closed"));
      headEl.append(h("div", "lane-title", root ? root.title : ""));
    } else {
      headEl.append(h("div", "lane-title", "ungrouped"));
    }
    headEl.append(h("div", "lane-toggle", isCollapsed(laneRoot, closed) ? "▸" : "▾"));
    headEl.onclick = () => { toggleLane(laneRoot, closed); renderBoard(); };

    const stats = { open: 0, in_progress: 0, blocked: 0, deferred: 0, closed: 0 };
    for (const m of members) {
      const col = columnOf(m.status);
      if (stats[col] !== undefined) stats[col]++;
      laneCounts[col]++;
    }
    const statParts = [];
    if (stats.open) statParts.push(stats.open + " open");
    if (stats.in_progress) statParts.push(stats.in_progress + " wip");
    if (stats.blocked) statParts.push(stats.blocked + " blocked");
    if (stats.deferred) statParts.push(stats.deferred + " deferred");
    if (stats.closed) statParts.push(stats.closed + " closed");
    headEl.append(h("div", "lane-stats", statParts.join(" · ") || "—"));
    lane.append(headEl);
    board.append(lane);

    if (isCollapsed(laneRoot, closed)) {
      lane.classList.add("collapsed");
      return; // cards hidden, lane head stays for toggling back
    }

    // partition members into columns, in-progress first within each
    const perCol = {};
    for (const c of cols) perCol[c.key] = [];
    for (const m of [...members].sort(byRank)) perCol[columnOf(m.status)].push(m);

    for (const c of cols) {
      const col = h("div", "col col-" + c.key);
      const all = perCol[c.key];
      const key = moreKey(laneRoot, c.key);
      const expanded = expandedCols.has(key);
      // only the closed column is collapsed by default; others show all
      const shown = !expanded && c.key === "closed"
        ? all.slice(0, MAX_SHOWN_CARDS)
        : all;
      const hidden = all.length - shown.length;
      for (const m of shown) col.append(card(m));
      if (hidden > 0) {
        const more = h("div", "col-more", hidden + " more " + (hidden === 1 ? "bead" : "beads") + " …");
        more.onclick = () => { expandedCols.add(key); renderBoard(); };
        col.append(more);
      } else if (expanded && all.length > MAX_SHOWN_CARDS) {
        const less = h("div", "col-more", "show fewer");
        less.onclick = () => { expandedCols.delete(key); renderBoard(); };
        col.append(less);
      }
      makeDropTarget(col, c.statuses[0], (id) => DATA.issues.find((i) => i.id === id));
      lane.append(col);
    }
  };

  for (const { rootId, members } of openLanes) buildLane(rootId, members, false);
  if (ungroupedOpen.length) buildLane(null, ungroupedOpen, true);
  const afterSeparator = closedLanes.length > 0 || ungroupedClosed.length > 0;
  if (afterSeparator) board.append(h("div", "board-separator", "closed epics"));
  for (const { rootId, members } of closedLanes) buildLane(rootId, members, false);
  if (ungroupedClosed.length) buildLane(null, ungroupedClosed, true);

  for (const c of cols) {
    const ch = head.querySelector(`[data-col="${c.key}"] .count`);
    ch.textContent = String(laneCounts[c.key]);
  }
}

function card(issue) {
  const c = h("div", "card" + (SELECTED === issue.id ? " selected" : ""));
  c.dataset.id = issue.id;
  c.style.borderLeftColor = statusColor(issue.status);

  const top = h("div", "card-top");
  const dot = h("span", "card-status");
  dot.style.background = statusColor(issue.status);
  dot.title = issue.status;
  top.append(h("span", "card-id", issue.id), dot);
  if (isClaimed(issue)) {
    const claim = h("span", "card-claimed", "◉");
    claim.title = "claimed" + (issue.assignee ? " · " + issue.assignee : "");
    top.append(claim);
  }
  c.append(top);

  c.append(h("div", "card-title", issue.title || ""));

  const chips = h("div", "card-chips");
  const mk = (t, cls) => { const s = h("span", cls || "", t); chips.append(s); };
  mk(issue.issue_type || "—");
  mk("P" + (issue.priority ?? "—"));
  if (issue.status && !COLUMNS.some((col) => col.statuses.includes(issue.status)))
    mk(issue.status, "substatus");
  if (issue.is_blocked) mk("⛔ " + (DATA.blocked_by?.[issue.id] ?? ""), "blocked-chip");
  if (issue.comment_count) mk("💬 " + issue.comment_count);
  c.append(chips);

  c.onclick = () => window.showDetails(issue);
  makeDraggable(c, issue);
  return c;
}
