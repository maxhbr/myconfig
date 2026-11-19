// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Details pane

import {
  CLUSTER, DATA, esc, fmtDate, h, isClaimed, statusColor,
} from "./core.js";
import {
  canWrite, inlinePriority, inlineStatus, inlineText, inlineAssignee,
  descriptionEditor, commentComposer, labelEditor,
} from "./edit.js";

function chip(text, bg) {
  const c = document.createElement("span");
  c.className = "chip" + (bg ? " status" : "");
  if (bg) c.style.background = bg;
  c.textContent = text;
  return c;
}

function depLink(dep) {
  const a = document.createElement("span");
  a.className = "dep-link";
  a.textContent = dep.depends_on_id;
  a.onclick = () => {
    const issue = DATA.issues.find((i) => i.id === dep.depends_on_id);
    if (issue) showDetails(issue);
  };
  return a;
}

export function refreshSelection() {
  for (const c of document.querySelectorAll(".card"))
    c.classList.toggle("selected", c.dataset.id === window.SELECTED_ID);
}

export function showDetails(issue) {
  window.SELECTED_ID = issue.id;
  const pane = document.getElementById("details");
  pane.innerHTML = "";
  pane.classList.add("open");
  const loading = h("div", "kv", "loading " + issue.id + "…");
  pane.append(loading);
  refreshSelection();
  // the list record carries no comments; hydrate the full record
  // BEFORE rendering, so the pane is built once and no re-render can
  // wipe a draft the user started typing
  fetch(`/api/beads/issue/${encodeURIComponent(issue.id)}`)
    .then((r) => (r.ok ? r.json() : null))
    .then((data) => {
      if (window.SELECTED_ID !== issue.id) return; // moved on meanwhile
      renderDetails(data ? data.issue : issue);
    })
    .catch(() => {
      if (window.SELECTED_ID === issue.id) renderDetails(issue);
    });
}

function renderDetails(issue) {
  const pane = document.getElementById("details");
  pane.innerHTML = "";
  pane.classList.add("open");

  const close = document.createElement("button");
  close.className = "close";
  close.textContent = "✕";
  close.onclick = () => {
    pane.classList.remove("open");
    window.SELECTED_ID = null;
    refreshSelection();
  };
  const h2 = document.createElement("h2");
  if (canWrite()) {
    const titleInput = inlineText(issue, "title", issue.title);
    h2.append(titleInput);
  } else {
    h2.textContent = issue.title;
  }
  pane.append(close, h2);

  pane.append(chip(issue.id));
  const focusChip = chip("◀ focus");
  focusChip.style.cursor = "pointer";
  focusChip.onclick = () => window.setFocus(issue.id);
  pane.append(focusChip);
  if (canWrite()) {
    const statuses = [...new Set(DATA.issues.map((i) => i.status))];
    pane.append(inlineStatus(issue, statuses));
    const typeSel = inlineText(issue, "type", issue.issue_type);
    typeSel.style.width = "90px";
    typeSel.style.display = "inline-block";
    pane.append(typeSel);
    const pr = inlinePriority(issue);
    pr.style.width = "60px";
    pr.style.display = "inline-block";
    pane.append(pr);
    const assignees = [...new Set(DATA.issues.map((i) => i.assignee).filter(Boolean))];
    const asg = inlineAssignee(issue, assignees);
    asg.style.width = "110px";
    asg.style.display = "inline-block";
    pane.append(asg);
  } else {
    pane.append(chip(issue.status, statusColor(issue.status)));
    pane.append(chip(issue.issue_type || "—"));
    pane.append(chip("P" + issue.priority));
  }

  const claimed = isClaimed(issue);
  if (claimed) {
    const c = chip("claimed" + (issue.assignee ? " · " + esc(issue.assignee) : ""), "var(--claimed)");
    c.style.color = "#10131a";
    pane.append(c);
  }

  if (issue.parent) {
    const wrap = document.createElement("div");
    wrap.append(chip("lane: " + (CLUSTER.get(issue.id) || issue.id)));
    pane.append(wrap);
  }

  const kv = document.createElement("div");
  kv.className = "kv";
  const row = (k, v) => {
    const kd = document.createElement("div"); kd.className = "k"; kd.textContent = k;
    const vd = document.createElement("div"); vd.textContent = v;
    kv.append(kd, vd);
  };
  row("owner", esc(issue.owner) || "—");
  row("assignee", esc(issue.assignee) || "—");
  row("created", fmtDate(issue.created_at) + " by " + esc(issue.created_by));
  row("updated", fmtDate(issue.updated_at));
  if (issue.closed_at) row("closed", fmtDate(issue.closed_at));
  pane.append(kv);

  const section = (title, body) => {
    const s = document.createElement("div");
    s.className = "section";
    const hh = document.createElement("h3"); hh.textContent = title;
    s.append(hh);
    if (typeof body === "string") {
      const pre = document.createElement("pre");
      pre.textContent = body;
      s.append(pre);
    } else s.append(body);
    pane.append(s);
  };

  if (canWrite() || (issue.labels || []).length) {
    section("labels", labelEditor(issue));
  }

  if (canWrite()) {
    section("description", descriptionEditor(issue));
  } else if (issue.description) {
    section("description", issue.description);
  }
  if (issue.close_reason) section("close reason", issue.close_reason);
  if (issue.notes) section("notes", issue.notes);
  if (issue.acceptance_criteria) section("acceptance criteria", issue.acceptance_criteria);

  const deps = issue.dependencies || [];
  if (deps.length) {
    const wrap = document.createElement("div");
    for (const dep of deps) {
      const line = document.createElement("div");
      line.append(depLink(dep));
      const t = document.createElement("span");
      t.textContent = "  (" + dep.type + ")";
      t.style.color = "var(--muted)";
      line.append(t);
      wrap.append(line);
    }
    section("depends on", wrap);
  }

  const blockers = DATA.reverse_deps?.[issue.id] || [];
  if (blockers.length) {
    const wrap = document.createElement("div");
    for (const b of blockers) {
      const line = document.createElement("div");
      const a = document.createElement("span");
      a.className = "dep-link";
      a.textContent = b.issue_id;
      a.onclick = () => {
        const target = DATA.issues.find((i) => i.id === b.issue_id);
        if (target) showDetails(target);
      };
      line.append(a);
      const t = document.createElement("span");
      t.textContent = "  (" + b.type + ")";
      t.style.color = "var(--muted)";
      line.append(t);
      wrap.append(line);
    }
    section("blocks (reverse deps)", wrap);
  }

  const chain = [];
  let cur = issue;
  const seen = new Set([issue.id]);
  while (true) {
    const pid = typeof cur.parent === "string" ? cur.parent
      : cur.parent?.id
      || (cur.dependencies || []).find((d) => d.type === "parent-child")?.depends_on_id;
    if (!pid || seen.has(pid)) break;
    seen.add(pid);
    const parent = DATA.issues.find((i) => i.id === pid);
    if (!parent) break;
    chain.push(parent);
    cur = parent;
  }
  if (chain.length) {
    const wrap = document.createElement("div");
    for (const p of chain.slice().reverse()) {
      const line = document.createElement("div");
      const a = document.createElement("span");
      a.className = "dep-link";
      a.textContent = p.id;
      a.onclick = () => showDetails(p);
      line.append(a);
      const t = document.createElement("span");
      t.textContent = "  " + p.title;
      t.style.color = "var(--muted)";
      line.append(t);
      wrap.append(line);
    }
    section("parent chain", wrap);
  }

  if ((issue.comments || []).length) {
    const wrap = document.createElement("div");
    for (const c of issue.comments) {
      const div = document.createElement("div");
      div.className = "comment";
      const meta = document.createElement("div");
      meta.className = "meta";
      meta.textContent = `${c.author || "?"} · ${fmtDate(c.created_at || c.timestamp || "")}`;
      const pre = document.createElement("pre");
      pre.textContent = c.text || "";
      div.append(meta, pre);
      wrap.append(div);
    }
    section(`comments (${issue.comment_count})`, wrap);
  }
  if (canWrite()) section("add a comment", commentComposer(issue));

  refreshSelection(); // update selection highlight
}

window.showDetails = showDetails;
