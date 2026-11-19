// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Live updates: SSE client, patch application, coalesced render.
//
// Patch discipline: a patch only applies when it extends the installed
// revision by exactly one AND comes from the same server generation
// (a restart renumbers revisions from zero, so cross-generation
// comparison would accept or reject patches against the wrong base).
// Anything else — a gap, a foreign generation, a patch the recovery
// fetch already covers — resolves through a full refetch, never
// guesswork, and the stale badge stays until recovery provably
// installed current data.

import { setData, applySnapshotPatch, applyIndices } from "./core.js";
import { init as initEdit, selectSession, sessionReady } from "./edit.js";

let SOURCE = null;
let installed = 0; // revision whose full state the DOM reflects
let generation = null; // server process the installed revision belongs to
let renderScheduled = false;
let refreshControls;
let streamOK = false;
let recovery = null;

export async function fetchSnapshot() {
  const response = await fetch("/api/beads", { cache: "no-store" });
  if (!response.ok) throw new Error("HTTP " + response.status);
  return response.json();
}

function recover({ generation: expected, revision }) {
  clearTimeout(recovery?.retry);
  const minimum = expected === generation ? Math.max(revision, installed) : revision;
  const job = { generation: expected, pending: null };
  recovery = job;
  selectSession(expected); // invalidate old credentials before any asynchronous work
  setStale(true);
  async function attempt() {
    try {
      const data = await fetchSnapshot();
      if (job !== recovery) return; // a later SSE handshake/reset owns recovery
      if (data.generation !== expected || data.revision < minimum) {
        throw new Error("snapshot does not cover the stream position");
      }
      if (!sessionReady() && !(await initEdit(expected))) throw new Error("session not ready");
      if (job !== recovery) return;
      setData(data);
      installed = data.revision;
      generation = data.generation;
      scheduleRender();
    } catch {
      // An idle EventSource sends only heartbeat comments, so it cannot
      // be relied on to trigger another attempt after a transient failure.
      if (job === recovery) job.retry = setTimeout(attempt, 2000);
      return;
    }
    recovery = null;
    setStale(!streamOK);
    if (job.pending) applyPatch(job.pending);
  }
  return attempt();
}
function refetchAll() {
  if (!recovery) return recover({ generation, revision: installed });
}

function applyPatch(data) {
  if (recovery?.generation === data.generation) {
    recovery.pending = data; // checked for contiguity against the new base
    return;
  }
  if (recovery || data.generation !== generation || data.revision > installed + 1) {
    recover(data);
    return;
  }
  if (data.revision <= installed) return; // already covered
  installed = data.revision;
  applySnapshotPatch(data);
  if (data.indices) applyIndices(data.indices);
  scheduleRender();
  setStale(false);
}

function scheduleRender() {
  if (renderScheduled) return;
  renderScheduled = true;
  requestAnimationFrame(() => {
    renderScheduled = false;
    refreshControls();
    const board = document.getElementById("board");
    const keepScroll = board.scrollTop;
    window.render();
    board.scrollTop = keepScroll;
  });
}

export function connect(revision, gen, onRender) {
  installed = revision;
  generation = gen;
  refreshControls = onRender;
  window.refetchAll = refetchAll;
  if (SOURCE) SOURCE.close();
  SOURCE = new EventSource(
    `/api/events?rev=${installed}` + (generation ? `&gen=${generation}` : "")
  );

  SOURCE.addEventListener("hello", (ev) => {
    const data = JSON.parse(ev.data);
    // the server only hellos when generation AND revision match;
    // anything else is a protocol surprise — recover rather than
    // silently claim coverage
    streamOK = true;
    if (recovery || !sessionReady() || data.generation !== generation || data.revision !== installed) {
      recover(data);
      return;
    }
    setStale(false);
  });

  SOURCE.addEventListener("patch", (ev) => {
    streamOK = true;
    applyPatch(JSON.parse(ev.data));
  });

  SOURCE.addEventListener("reset", (ev) => {
    // server-side revision diverged (restart or gap): full refetch,
    // patches buffered until the base is in place
    streamOK = true;
    recover(JSON.parse(ev.data));
  });

  SOURCE.onerror = () => {
    streamOK = false;
    setStale(true);
  };
  SOURCE.addEventListener("stale", SOURCE.onerror);

  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") refetchAll();
  });
}

function setStale(flag) {
  const el = document.getElementById("stale-indicator");
  if (el) el.classList.toggle("visible", flag);
}
