// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Board renderer, ported from misc/beads-viewer/index.html.

import {
  COLUMNS, CLUSTER, DATA, SELECTED, columnOf, h, isClaimed, statusColor,
} from "./core.js";

export function renderBoard() {
  const board = document.getElementById("board");
  board.innerHTML = "";

  const showClosed = document.getElementById("toggle-closed").checked;
  const cols = COLUMNS.filter((c) => showClosed || c.key !== "closed");
  board.style.setProperty("--ncols", cols.length);

  // lane = cluster root id -> visible issues
  const byIdMap = new Map(DATA.issues.map((i) => [i.id, i]));
  const lanes = new Map();
  for (const card of window.visibleIssues()) {
    const rootId = CLUSTER.get(card.id) || card.id;
    if (!lanes.has(rootId)) lanes.set(rootId, []);
    lanes.get(rootId).push(card);
  }
  const named = [], ungrouped = [];
  for (const [rootId, members] of lanes) {
    if (members.length >= 2) named.push({ rootId, members });
    else ungrouped.push(...members);
  }
  named.sort((a, b) =>
    b.members.length - a.members.length ||
    a.rootId.localeCompare(b.rootId));

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
    if (laneRoot) {
      const root = byIdMap.get(laneRoot);
      const rootLink = h("span", "lane-root", laneRoot);
      if (root) rootLink.onclick = () => window.showDetails(root);
      headEl.append(rootLink);
      headEl.append(h("div", "lane-title", root ? root.title : ""));
    } else {
      headEl.append(h("div", "lane-title", "ungrouped"));
    }

    const stats = { open: 0, in_progress: 0, closed: 0 };
    for (const m of members) {
      const col = columnOf(m.status);
      if (stats[col] !== undefined) stats[col]++;
      laneCounts[col]++;
    }
    const statParts = [];
    if (stats.open) statParts.push(stats.open + " open");
    if (stats.in_progress) statParts.push(stats.in_progress + " wip");
    if (stats.closed) statParts.push(stats.closed + " closed");
    headEl.append(h("div", "lane-stats", statParts.join(" · ") || "—"));
    lane.append(headEl);

    // partition members into columns, in-progress first within each
    const perCol = {};
    for (const c of cols) perCol[c.key] = [];
    for (const m of [...members].sort(byRank)) perCol[columnOf(m.status)].push(m);

    for (const c of cols) {
      const col = h("div", "col col-" + c.key);
      for (const m of perCol[c.key]) col.append(card(m));
      lane.append(col);
    }
    board.append(lane);
  };

  for (const { rootId, members } of named) buildLane(rootId, members, false);
  if (ungrouped.length) buildLane(null, ungrouped, true);

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
  if (issue.comment_count) mk("💬 " + issue.comment_count);
  c.append(chips);

  c.onclick = () => window.showDetails(issue);
  return c;
}
