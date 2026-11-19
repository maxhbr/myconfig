// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// App shell: data load, toolbar wiring, filtering, render dispatch.

import {
  DATA, STATUS_ORDER, CLUSTER, setData, statusColor,
} from "./core.js";
import { connect } from "./live.js";
import { init as initEdit, newBeadDialog, canWrite } from "./edit.js";
import { renderBoard } from "./board.js";
import { renderGraph, zoomBy, resetView } from "./graph.js";
import { showDetails, refreshSelection } from "./detail.js";
import * as filters from "./filters.js";
const { state, matches, writeToHash, readFromHash } = filters;

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
  NODES = DATA.issues.filter(matches);
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

function renderFiltered() {
  writeToHash();
  render();
}

// ---------------------------------------------------------------- toolbar

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

function labelCounts() {
  const counts = new Map();
  for (const i of DATA.issues) {
    for (const l of i.labels || []) counts.set(l, (counts.get(l) || 0) + 1);
  }
  return counts;
}

function buildLabelPicker() {
  const wrap = document.getElementById("label-picker");
  wrap.innerHTML = "";
  const counts = labelCounts();
  for (const [label, n] of [...counts.entries()].sort((a, b) => b[1] - a[1])) {
    const chip = document.createElement("span");
    chip.className = "label-chip" + (state.labels.has(label) ? " selected" : "");
    chip.textContent = `${label} (${n})`;
    chip.onclick = () => {
      if (state.labels.has(label)) state.labels.delete(label);
      else state.labels.add(label);
      chip.classList.toggle("selected");
      renderFiltered();
    };
    wrap.append(chip);
  }
}

function myAssignee() {
  // the "mine" preset filters to the most common assignee in the
  // snapshot — the server has no session/user concept to ask
  const counts = new Map();
  for (const i of DATA.issues) {
    if (i.assignee) counts.set(i.assignee, (counts.get(i.assignee) || 0) + 1);
  }
  return [...counts.entries()].sort((a, b) => b[1] - a[1])[0]?.[0] || "";
}

function buildFilters() {
  const fillSelect = (id, values, current) => {
    const sel = document.getElementById(id);
    for (const v of values.filter(Boolean)) {
      const o = document.createElement("option");
      o.value = v; o.textContent = v;
      if (v === current) o.selected = true;
      sel.append(o);
    }
  };
  const types = new Set(), statuses = new Set(), assignees = new Set();
  for (const i of DATA.issues) {
    types.add(i.issue_type);
    statuses.add(i.status);
    if (i.assignee) assignees.add(i.assignee);
  }
  fillSelect("filter-type", [...types].sort(), state.type);
  fillSelect("filter-status", [...statuses].sort(), state.status);
  fillSelect("filter-priority", ["0", "1", "2", "3", "4"], state.priority);
  fillSelect("filter-assignee", [...assignees].sort(), state.assignee);

  const laneSel = document.getElementById("filter-lane");
  const laneSize = new Map();
  for (const i of DATA.issues) {
    const root = CLUSTER.get(i.id) || i.id;
    laneSize.set(root, (laneSize.get(root) || 0) + 1);
  }
  const lanes = [...laneSize.entries()]
    .filter(([root, n]) => n >= 2)
    .sort((a, b) => b[1] - a[1]);
  const laneAll = document.createElement("option");
  laneAll.value = ""; laneAll.textContent = "all";
  laneSel.append(laneAll);
  for (const [root, n] of lanes) {
    const o = document.createElement("option");
    o.value = root;
    const title = DATA.issues.find((i) => i.id === root)?.title || root;
    o.textContent = `${root} (${n}) — ${title.slice(0, 30)}`;
    if (root === state.lane) o.selected = true;
    laneSel.append(o);
  }

  buildLabelPicker();
}

export function rebuildFilterOptions() {
  // live patches introduce new labels/assignees/statuses; rebuild the
  // dropdowns and chips (selection lives in filters.state, not the DOM)
  document.getElementById("filter-type").innerHTML =
    '<option value="">all</option>';
  document.getElementById("filter-status").innerHTML =
    '<option value="">all</option>';
  document.getElementById("filter-priority").innerHTML =
    '<option value="">all</option>';
  document.getElementById("filter-assignee").innerHTML =
    '<option value="">all</option>';
  document.getElementById("filter-lane").innerHTML =
    '<option value="">all</option>';
  buildFilters();
  syncToolbarFromState();
}

function syncToolbarFromState() {
  document.getElementById("toggle-closed").checked = state.showClosed;
  document.getElementById("search").value = state.search;
  document.getElementById("toggle-search-desc").checked = state.searchDescriptions;
  document.getElementById("toggle-blocked").checked = state.blockedOnly;
  document.getElementById("filter-type").value = state.type;
  document.getElementById("filter-status").value = state.status;
  document.getElementById("filter-priority").value = state.priority;
  document.getElementById("filter-assignee").value = state.assignee;
  document.getElementById("filter-lane").value = state.lane;
  buildLabelPicker();
}

function buildPresets() {
  const sel = document.getElementById("presets");
  sel.innerHTML = "";
  const builtins = [
    ["", "— preset —"],
    ["mysbx", "mysbx"],
    ["all", "all"],
  ];
  const assignee = DATA.issues.find((i) => i.assignee)?.assignee;
  if (assignee) builtins.push(["mine", `mine (${assignee})`]);
  for (const [v, label] of builtins) {
    const o = document.createElement("option");
    o.value = v; o.textContent = label;
    sel.append(o);
  }
  for (const name of filters.userPresets()) {
    const o = document.createElement("option");
    o.value = name; o.textContent = `★ ${name}`;
    sel.append(o);
  }
  const save = document.createElement("option");
  save.value = "__save__"; save.textContent = "save current as…";
  sel.append(save);
}

export async function applyIndices(data) {
  // attach reverse_deps/blockers only when they match the issues'
  // revision, so indices never describe a different snapshot
  const rIdx = await fetch("/api/indices");
  if (!rIdx.ok) return;
  const idx = await rIdx.json();
  if (idx.revision === data.revision) {
    data.reverse_deps = idx.reverse_deps;
    data.blockers = idx.blockers;
  }
}

export async function fetchSnapshot() {
  const rBeads = await fetch("/api/beads");
  if (!rBeads.ok) throw new Error("HTTP " + rBeads.status);
  const data = await rBeads.json();
  for (let attempt = 0; attempt < 5; attempt++) {
    await applyIndices(data);
    if (data.reverse_deps) return data;
    const fresh = await (await fetch("/api/beads")).json();
    if (fresh.revision === data.revision) return data;
    Object.assign(data, fresh);
  }
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
  readFromHash();
  buildFilters();
  syncToolbarFromState();
  buildPresets();

  window.addEventListener("resize", () => { if (viewMode() === "graph") render(); });
  document.getElementById("view-mode").onchange = render;
  document.getElementById("toggle-deps").onchange = render;

  const bindCheckbox = (id, key) => {
    document.getElementById(id).onchange = () => {
      state[key] = document.getElementById(id).checked;
      renderFiltered();
    };
  };
  const bindSelect = (id, key) => {
    document.getElementById(id).onchange = () => {
      state[key] = document.getElementById(id).value;
      renderFiltered();
    };
  };
  bindCheckbox("toggle-closed", "showClosed");
  bindCheckbox("toggle-blocked", "blockedOnly");
  bindCheckbox("toggle-search-desc", "searchDescriptions");
  bindSelect("filter-type", "type");
  bindSelect("filter-status", "status");
  bindSelect("filter-priority", "priority");
  bindSelect("filter-assignee", "assignee");
  bindSelect("filter-lane", "lane");
  document.getElementById("search").oninput = () => {
    state.search = document.getElementById("search").value.trim();
    renderFiltered();
  };
  document.getElementById("presets").onchange = () => {
    const sel = document.getElementById("presets");
    if (sel.value === "__save__") {
      const name = prompt("preset name?");
      if (name) {
        filters.savePreset(name);
        buildPresets();
        sel.value = name;
      }
    } else if (sel.value) {
      filters.applyPreset(sel.value, myAssignee());
      syncToolbarFromState();
      renderFiltered();
      sel.value = "";
    }
  };
  document.getElementById("focus-clear").onclick = () => { FOCUS = null; render(); };
  document.getElementById("zoom-in").onclick = () => zoomBy(1.25);
  document.getElementById("zoom-out").onclick = () => zoomBy(0.8);
  document.getElementById("zoom-fit").onclick = resetView;
  document.getElementById("new-bead").onclick = () => newBeadDialog();
  render();
  connect(DATA.revision);
  initEdit();
}

load();
