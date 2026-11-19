// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// App shell: data load, toolbar wiring, filtering, render dispatch.

import {
  DATA, STATUS_ORDER, setData, select, statusColor, h,
} from "./core.js";
import { renderBoard } from "./board.js";
import { renderGraph, zoomBy, resetView } from "./graph.js";
import { showDetails, refreshSelection } from "./detail.js";

// The board/graph modules read the current selection through this
// window global (kept from the viewer port) so card() and draw() can
// mark the selected item without a per-render import cycle.
window.SELECTED_ID = null;

let NODES = []; // filtered issue list = visible beads

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

  if (isBoard) renderBoard();
  else renderGraph(NODES);
}
window.render = render;

function visibleIssuesImpl() {
  const showClosed = document.getElementById("toggle-closed").checked;
  const fType = document.getElementById("filter-type").value;
  const fStatus = document.getElementById("filter-status").value;
  const q = document.getElementById("search").value.trim().toLowerCase();
  return DATA.issues.filter((i) => {
    if (!showClosed && i.status === "closed") return false;
    if (fType && i.issue_type !== fType) return false;
    if (fStatus && i.status !== fStatus) return false;
    if (q && !(i.title.toLowerCase().includes(q) || i.id.includes(q))) return false;
    return true;
  });
}
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
}

async function load() {
  try {
    const r = await fetch("/api/beads");
    if (!r.ok) throw new Error("HTTP " + r.status);
    setData(await r.json());
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
  document.getElementById("zoom-in").onclick = () => zoomBy(1.25);
  document.getElementById("zoom-out").onclick = () => zoomBy(0.8);
  document.getElementById("zoom-fit").onclick = resetView;
  render();
}

load();
