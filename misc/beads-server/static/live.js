// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Live updates: SSE client, patch application, coalesced render.
//
// Patch discipline: a patch only applies when it extends the installed
// revision by exactly one (contiguous). Anything else — a gap, a patch
// the recovery fetch already covers, or an older patch racing a newer
// snapshot — resolves through a full refetch, never through guesswork,
// so the board is always a contiguous revision history.

import { setData, applySnapshotPatch, applyIndices } from "./core.js";
import { fetchSnapshot, rebuildFilterOptions } from "./app.js";

let SOURCE = null;
let installed = 0; // revision whose full state the DOM reflects
let renderScheduled = false;
let recovering = false; // a full snapshot fetch is in flight
let pendingPatch = null; // patch that arrived during recovery

async function refetchAll() {
  recovering = true;
  try {
    const data = await fetchSnapshot();
    // only install if nothing newer applied meanwhile; fetchSnapshot
    // data is at least as new as the revision it was fetched at
    if (data.revision >= installed) {
      setData(data);
      installed = data.revision;
      scheduleRender();
    }
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

export function connect(revision) {
  installed = revision;
  if (SOURCE) SOURCE.close();
  SOURCE = new EventSource(`/api/events?rev=${installed}`);

  SOURCE.addEventListener("hello", (ev) => {
    const rev = JSON.parse(ev.data).revision;
    // the server only hellos when revisions match; anything else is
    // a protocol surprise — recover rather than silently claim coverage
    if (rev !== installed) refetchAll();
    setStale(false);
  });

  SOURCE.addEventListener("patch", (ev) => {
    applyPatch(JSON.parse(ev.data));
    setStale(false);
  });

  SOURCE.addEventListener("reset", (ev) => {
    // server-side revision diverged (restart or gap): full refetch,
    // patches buffered until the base is in place
    setStale(false);
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
