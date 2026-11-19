// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
// Live updates: SSE client, patch application, coalesced render.

import { setData, applySnapshotPatch, applyIndices } from "./core.js";
import { fetchSnapshot, rebuildFilterOptions } from "./app.js";

let SOURCE = null;
let lastRevision = 0;
let renderScheduled = false;
let refetchSeq = 0; // a fetch started before a patch must not apply over it

async function refetchAll() {
  const seq = ++refetchSeq;
  const data = await fetchSnapshot();
  if (seq !== refetchSeq) return; // a newer fetch or patch superseded this one
  if (data.revision < lastRevision) return; // patch arrived meanwhile: keep it
  setData(data);
  lastRevision = data.revision;
  scheduleRender();
}
window.refetchAll = refetchAll;

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
  lastRevision = revision;
  if (SOURCE) SOURCE.close();
  SOURCE = new EventSource(`/api/events?rev=${lastRevision}`);

  SOURCE.addEventListener("hello", (ev) => {
    lastRevision = JSON.parse(ev.data).revision;
    setStale(false);
  });

  SOURCE.addEventListener("patch", (ev) => {
    const data = JSON.parse(ev.data);
    lastRevision = data.revision;
    refetchSeq++; // invalidate any in-flight full fetch from before this patch
    applySnapshotPatch(data);
    if (data.indices) applyIndices(data.indices);
    scheduleRender();
    setStale(false);
  });

  SOURCE.addEventListener("reset", (ev) => {
    lastRevision = JSON.parse(ev.data).revision;
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
