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
import { fetchSnapshot, rebuildFilterOptions } from "./app.js";

let SOURCE = null;
let installed = 0; // revision whose full state the DOM reflects
let generation = null; // server process the installed revision belongs to
let renderScheduled = false;
let recovering = false; // a full snapshot fetch is in flight
let pendingPatch = null; // patch that arrived during recovery

async function refetchAll() {
  recovering = true;
  setStale(true); // not current until proven otherwise
  try {
    const data = await fetchSnapshot();
    // a foreign generation is ALWAYS a full reinstall: the numbers
    // mean different things per process. Same generation: install
    // unless something newer applied meanwhile.
    if (
      data.generation !== generation ||
      (data.generation === generation && data.revision >= installed)
    ) {
      setData(data);
      installed = data.revision;
      generation = data.generation;
      scheduleRender();
    }
    setStale(false); // recovery installed current data
  } catch {
    // leave the current board; the stale indicator stays on and the
    // next SSE event retries recovery
  } finally {
    recovering = false;
    const queued = pendingPatch;
    pendingPatch = null;
    if (queued) applyPatch(queued); // re-evaluated against the new base
  }
}
window.refetchAll = refetchAll;

function applyPatch(data) {
  if (recovering) {
    pendingPatch = data; // evaluated once recovery installed its base
    return;
  }
  if (data.generation !== generation) {
    // foreign process (restart): its patches describe a base we do
    // not have — recover instead of comparing meaningless numbers
    refetchAll();
    return;
  }
  if (data.revision <= installed) return; // already covered
  if (data.revision > installed + 1) {
    // gap (stalled queue, missed event): recover from a full snapshot
    refetchAll();
    return;
  }
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
    rebuildFilterOptions();
    const board = document.getElementById("board");
    const keepScroll = board.scrollTop;
    window.render();
    board.scrollTop = keepScroll;
  });
}

export function connect(revision, gen) {
  installed = revision;
  generation = gen || null;
  if (SOURCE) SOURCE.close();
  SOURCE = new EventSource(
    `/api/events?rev=${installed}` + (generation ? `&gen=${generation}` : "")
  );

  SOURCE.addEventListener("hello", (ev) => {
    const data = JSON.parse(ev.data);
    // the server only hellos when generation AND revision match;
    // anything else is a protocol surprise — recover rather than
    // silently claim coverage
    if (data.generation !== generation || data.revision !== installed) {
      refetchAll();
      return;
    }
    setStale(false);
  });

  SOURCE.addEventListener("patch", (ev) => {
    applyPatch(JSON.parse(ev.data));
  });

  SOURCE.addEventListener("reset", (ev) => {
    // server-side revision diverged (restart or gap): full refetch,
    // patches buffered until the base is in place
    refetchAll();
  });

  SOURCE.addEventListener("stale", () => setStale(true));

  SOURCE.onerror = () => {
    // EventSource retries on its own; the server suggests 2000 ms.
    setStale(true);
  };

  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") refetchAll();
  });
}

function setStale(flag) {
  const el = document.getElementById("stale-indicator");
  if (el) el.classList.toggle("visible", flag);
}
