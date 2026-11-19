// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// App shell: data load, toolbar wiring, filtering, render dispatch.

import {
  DATA, STATUS_ORDER, CLUSTER, setData, select, statusColor, h,
} from "./core.js";
import { renderBoard } from "./board.js";
import { renderGraph, zoomBy, resetView } from "./graph.js";
import { showDetails, refreshSelection } from "./detail.js";

// The board/graph modules read the current selection through this
// window global (kept from the viewer port) so card() and draw() can
// mark the selected item without a per-render import cycle.
window.SELECTED_ID = null;

let NODES = []; // filtered issue list = visible beads
let FOCUS = null; // issue id whose neighbourhood the graph shows

export function visibleIssues() {
  return NODES;
}
window.visibleIssues = visibleIssues;

function viewMode() {
  return document.getElementById("view-mode").value;
}

function render() {
  NODES = visibleIssues();
  document.getElementById("status-count").textContent =
    `${NODES.length} of ${DATA.issues.length} beads`;

  const board = document.getElementById("board");
  const graphWrap = document.getElementById("graph-wrap");
  const isBoard = viewMode() === "board";
  board.hidden = !isBoard;
  graphWrap.hidden = isBoard;
  document.getElementById("graph-controls").hidden = isBoard;
  document.getElementById("label-deps").style.display = isBoard ? "" : "none";

  const banner = document.getElementById("focus-banner");
  if (FOCUS !== null && !isBoard) {
    banner.classList.add("visible");
    document.getElementById("focus-text").textContent =
      "focused on " + FOCUS + " (1-2 hops)";
  } else {
    banner.classList.remove("visible");
  }

  if (isBoard) renderBoard();
  else renderGraph(NODES);
}
window.render = render;

function visibleIssuesImpl() {
  const showClosed = document.getElementById("toggle-closed").checked;
  const fType = document.getElementById("filter-type").value;
  const fStatus = document.getElementById("filter-status").value;
  const fLane = document.getElementById("filter-lane").value;
  const q = document.getElementById("search").value.trim().toLowerCase();
  let issues = DATA.issues.filter((i) => {
    if (!showClosed && i.status === "closed") return false;
    if (fType && i.issue_type !== fType) return false;
    if (fStatus && i.status !== fStatus) return false;
    if (fLane && (CLUSTER.get(i.id) || i.id) !== fLane) return false;
    if (q && !(i.title.toLowerCase().includes(q) || i.id.includes(q))) return false;
    return true;
  });
  if (FOCUS !== null && viewMode() === "graph") {
    const ids = neighbourhood(FOCUS, 2);
    issues = issues.filter((i) => ids.has(i.id));
  }
  return issues;
}

function neighbourhood(centerId, hops) {
  const ids = new Set([centerId]);
  for (let hop = 0; hop < hops; hop++) {
    for (const id of [...ids]) {
      const cur = DATA.issues.find((i) => i.id === id);
      if (!cur) continue;
      for (const d of cur.dependencies || []) ids.add(d.depends_on_id);
      for (const r of DATA.reverse_deps?.[id] || []) ids.add(r.issue_id);
    }
  }
  return ids;
}

export function setFocus(id) {
  FOCUS = id;
  if (viewMode() !== "graph") document.getElementById("view-mode").value = "graph";
  render();
}
window.setFocus = setFocus;
// visibleIssues dispatch: NODES is recomputed inside render()
visibleIssues = visibleIssuesImpl;

function buildLegend() {
  const legend = document.getElementById("legend");
  legend.innerHTML = "";
  for (const s of STATUS_ORDER) {
    const span = document.createElement("span");
    const sw = document.createElement("span");
    sw.className = "swatch";
    sw.style.background = statusColor(s);
    span.append(sw, document.createTextNode(s));
    legend.append(span);
  }
  const claim = document.createElement("span");
  claim.textContent = "◉ claimed = amber dot";
  legend.append(claim);
  for (const [type, note] of [
    ["blocks", "solid"], ["parent-child", "dashed"],
    ["discovered-from", "faint"], ["supersedes", "purple"],
    ["depends-on", "labelled"], ["related", "solid"],
    ["relates-to", "solid"],
  ]) {
    const span = document.createElement("span");
    span.textContent = `${type}: ${note}`;
    legend.append(span);
  }
}

function buildFilters() {
  const types = new Set(), statuses = new Set();
  for (const i of DATA.issues) { types.add(i.issue_type); statuses.add(i.status); }
  for (const [id, set] of [["filter-type", types], ["filter-status", statuses]]) {
    const sel = document.getElementById(id);
    for (const v of [...set].filter(Boolean).sort()) {
      const o = document.createElement("option");
      o.value = v; o.textContent = v;
      sel.append(o);
    }
  }
  const laneSel = document.getElementById("filter-lane");
  const laneSize = new Map();
  for (const i of DATA.issues) {
    const root = CLUSTER.get(i.id) || i.id;
    laneSize.set(root, (laneSize.get(root) || 0) + 1);
  }
  const lanes = [...laneSize.entries()]
    .filter(([root, n]) => n >= 2 && root !== "ungrouped")
    .sort((a, b) => b[1] - a[1]);
  for (const [root, n] of lanes) {
    const o = document.createElement("option");
    o.value = root;
    const title = DATA.issues.find((i) => i.id === root)?.title || root;
    o.textContent = `${root} (${n}) — ${title.slice(0, 30)}`;
    laneSel.append(o);
  }
}

async function fetchSnapshot() {
  // fetch both halves until their revisions match, so reverse_deps and
  // blockers always describe the same snapshot as issues
  for (let attempt = 0; attempt < 5; attempt++) {
    const [rBeads, rIdx] = await Promise.all([
      fetch("/api/beads"), fetch("/api/indices"),
    ]);
    if (!rBeads.ok) throw new Error("HTTP " + rBeads.status);
    const data = await rBeads.json();
    if (!rIdx.ok) return data;
    const idx = await rIdx.json();
    if (idx.revision === data.revision) {
      data.reverse_deps = idx.reverse_deps;
      data.blockers = idx.blockers;
      return data;
    }
  }
  const data = await (await fetch("/api/beads")).json();
  return data;
}

async function load() {
  try {
    const data = await fetchSnapshot();
    setData(data);
  } catch (e) {
    const error = document.getElementById("error");
    error.hidden = false;
    error.textContent = "failed to load /api/beads: " + e;
    return;
  }
  buildLegend();
  buildFilters();
  window.addEventListener("resize", () => { if (viewMode() === "graph") render(); });
  document.getElementById("view-mode").onchange = render;
  document.getElementById("toggle-closed").onchange = render;
  document.getElementById("toggle-deps").onchange = render;
  document.getElementById("filter-type").onchange = render;
  document.getElementById("filter-status").onchange = render;
  document.getElementById("search").oninput = render;
  document.getElementById("filter-lane").onchange = render;
  document.getElementById("focus-clear").onclick = () => { FOCUS = null; render(); };
  document.getElementById("zoom-in").onclick = () => zoomBy(1.25);
  document.getElementById("zoom-out").onclick = () => zoomBy(0.8);
  document.getElementById("zoom-fit").onclick = resetView;
  render();
}

load();
