// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Dependency graph (dagre layout), ported from misc/beads-viewer/index.html.

import { DATA, SELECTED, el, isClaimed, shorten, statusColor } from "./core.js";

/* global dagre */

let graph = null;   // dagre graph with layout
let scale = null;   // null = fit-to-viewport
let tx = 0, ty = 0; // pan offset (screen px)

export function renderGraph(nodes) {
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const showDeps = document.getElementById("toggle-deps").checked;

  graph = new dagre.graphlib.Graph({ multigraph: true });
  graph.setGraph({ rankdir: "LR", nodesep: 25, ranksep: 60, marginx: 20, marginy: 20 });
  graph.setDefaultEdgeLabel(() => ({}));

  for (const n of nodes) graph.setNode(n.id, { width: 170, height: 40 });

  if (showDeps) {
    for (const n of nodes) {
      for (const dep of n.dependencies || []) {
        if (!byId.has(dep.depends_on_id)) continue;
        graph.setEdge(n.id, dep.depends_on_id, { label: dep.type });
      }
    }
  }

  dagre.layout(graph);
  draw(nodes);
}

function layoutSize() {
  let w = 0, hh = 0;
  for (const v of graph.nodes()) {
    const n = graph.node(v);
    w = Math.max(w, n.x + n.width / 2);
    hh = Math.max(hh, n.y + n.height / 2);
  }
  return [w + 20, hh + 20];
}

function draw(nodes) {
  const wrap = document.getElementById("graph-wrap");
  const old = wrap.querySelector("svg");
  if (old) old.remove();

  const W = wrap.clientWidth, H = wrap.clientHeight;
  const [gw, gh] = layoutSize();
  if (scale === null) scale = Math.min(1, W / gw, H / gh) * 0.95;

  const svg = el("svg", { viewBox: `0 0 ${W} ${H}` });
  const root = el("g", { transform: `translate(${tx},${ty}) scale(${scale})` });
  svg.append(root);

  // edges
  for (const e of graph.edges()) {
    const edge = graph.edge(e);
    const pts = edge.points;
    const d = "M" + pts.map((p) => `${p.x},${p.y}`).join(" L");
    const style = (edge.label || "").replace(/[^a-z-]/g, "");
    const g = el("g", { class: "edge " + style });
    g.append(el("path", { d, "marker-end": "url(#arrow)" }));
    if (edge.label && ["blocks", "supersedes", "depends-on"].includes(edge.label)) {
      const mid = pts[Math.floor(pts.length / 2)];
      g.append(el("text", {
        class: "edgelabel", x: mid.x, y: mid.y - 4,
        "text-anchor": "middle",
      }, edge.label));
    }
    root.append(g);
  }

  // nodes
  const byId = new Map(nodes.map((n) => [n.id, n]));
  for (const id of graph.nodes()) {
    const n = graph.node(id);
    const issue = byId.get(id);
    const g = el("g", {
      class: "node" + (SELECTED === id ? " selected" : ""),
      transform: `translate(${n.x - n.width / 2},${n.y - n.height / 2})`,
    });
    const box = el("rect", {
      class: "nodebox" + (isClaimed(issue) ? " claimed" : ""),
      width: n.width, height: n.height, rx: 6,
      fill: statusColor(issue.status), "fill-opacity": 0.85,
    });
    g.append(box);
    if (isClaimed(issue)) {
      g.append(el("circle", {
        cx: n.width - 12, cy: 12, r: 5, fill: "var(--claimed)",
      }));
    }
    const label = el("text", { class: "nodelabel", x: 8, y: 16 }, id);
    const title = el("text", { class: "nodelabel", x: 8, y: 30 }, shorten(issue.title, 26));
    title.setAttribute("fill", "#10131a");
    g.append(label, title);
    g.addEventListener("click", () => window.showDetails(issue));
    root.append(g);
  }

  // defs: arrow marker
  const defs = el("defs");
  const marker = el("marker", {
    id: "arrow", viewBox: "0 0 10 10", refX: 10, refY: 5,
    markerWidth: 6, markerHeight: 6, orient: "auto",
  });
  marker.append(el("path", { d: "M0,0 L10,5 L0,10 z", fill: "var(--muted)" }));
  defs.append(marker);
  svg.prepend(defs);

  wrap.append(svg);
  attachPanZoom(svg, root, wrap);
}

export function zoomBy(f) {
  scale = Math.max(0.05, Math.min(4, (scale || 1) * f));
  redraw();
}

export function resetView() {
  scale = null; tx = 0; ty = 0;
  redraw();
}

function redraw() {
  if (document.getElementById("view-mode").value !== "graph") return;
  if (!graph) return;
  draw(window.visibleIssues());
}

function attachPanZoom(svg, root, wrap) {
  let dragging = null;
  svg.addEventListener("mousedown", (ev) => {
    dragging = { x: ev.clientX, y: ev.clientY, tx, ty };
    svg.classList.add("dragging");
    ev.preventDefault();
  });
  svg.addEventListener("mousemove", (ev) => {
    if (!dragging) return;
    tx = dragging.tx + (ev.clientX - dragging.x);
    ty = dragging.ty + (ev.clientY - dragging.y);
    root.setAttribute("transform", `translate(${tx},${ty}) scale(${scale})`);
  });
  for (const end of ["mouseup", "mouseleave"]) {
    svg.addEventListener(end, () => {
      dragging = null;
      svg.classList.remove("dragging");
    });
  }
  svg.addEventListener("wheel", (ev) => {
    ev.preventDefault();
    const f = ev.deltaY < 0 ? 1.1 : 1 / 1.1;
    const old = scale;
    scale = Math.max(0.05, Math.min(4, scale * f));
    // zoom towards cursor
    const rect = wrap.getBoundingClientRect();
    const cx = ev.clientX - rect.left, cy = ev.clientY - rect.top;
    tx = cx - (cx - tx) * (scale / old);
    ty = cy - (cy - ty) * (scale / old);
    draw(window.visibleIssues());
  }, { passive: false });
}
