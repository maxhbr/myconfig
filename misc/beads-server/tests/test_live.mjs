// SPDX-License-Identifier: MIT
import assert from "node:assert/strict";
import test from "node:test";
import { setImmediate } from "node:timers/promises";
import { DATA, setData } from "../static/core.js";
globalThis.window = {};
const { connect } = await import("../static/live.js");
const { init, canWrite } = await import("../static/edit.js");

const snapshot = (generation, revision) => ({ generation, revision, issues: [], blocked_by: {} });

async function setup(t) {
  t.mock.timers.enable({ apis: ["setTimeout"] });
  let stale;
  const requests = [];
  const events = new Map();
  window.render = () => {};
  globalThis.document = {
    addEventListener() {},
    querySelectorAll: () => [],
    getElementById: (id) => ({ classList: { toggle: (_, flag) => {
      if (id === "stale-indicator") stale = flag;
    } } }),
  };
  globalThis.requestAnimationFrame = (f) => f();
  globalThis.EventSource = class {
    close() {}
    addEventListener(name, f) { events.set(name, f); }
  };
  let server = "A";
  const sessions = [];
  globalThis.fetch = (path) => {
    if (path === "/api/capabilities") return Promise.resolve(Response.json({ generation: server, write: true }));
    if (path === "/api/session") {
      sessions.push(server);
      return Promise.resolve(Response.json({ generation: server, token: server }));
    }
    const request = Promise.withResolvers();
    requests.push(request);
    return request.promise;
  };
  setData(snapshot("A", 9));
  await init("A");
  connect(9, "A", () => {});
  const send = (name, data) => events.get(name)({ data: JSON.stringify(data) });
  send("hello", snapshot("A", 9));
  return { send, requests, stale: () => stale, restart: () => { server = "B"; }, sessions };
}

test("a superseded fetch cannot roll back a restarted server or its buffered patch", async (t) => {
  const { send, requests, stale, restart, sessions } = await setup(t);
  window.refetchAll();
  restart();
  send("reset", snapshot("B", 1));
  assert.equal(canWrite(), false);
  send("patch", { generation: "B", revision: 2, upserted: [], removed: [],
    indices: { generation: "B", revision: 2, blocked_by: { one: 1 } } });
  requests[1].resolve(Response.json(snapshot("B", 1)));
  await setImmediate();
  requests[0].resolve(Response.json(snapshot("A", 10)));
  await setImmediate();
  assert.equal(DATA.generation, "B");
  assert.equal(DATA.revision, 2);
  assert.deepEqual(DATA.blocked_by, { one: 1 });
  assert.deepEqual(sessions, ["A", "B"]);
  assert.equal(canWrite(), true);
  assert.equal(stale(), false);
});

test("a failed, wrong-process or too-old recovery retries without another event", async (t) => {
  const { send, requests, stale, restart } = await setup(t);
  restart();
  send("reset", snapshot("B", 3));
  requests[0].reject(new Error("offline"));
  for (const [i, data] of [snapshot("A", 12), snapshot("B", 1), snapshot("B", 3)].entries()) {
    await setImmediate();
    assert.equal(stale(), true);
    assert.equal(DATA.generation, "A");
    t.mock.timers.tick(2000);
    requests[i + 1].resolve(Response.json(data));
  }
  await setImmediate();
  assert.equal(stale(), false);
  assert.equal(DATA.generation, "B");
  assert.equal(DATA.revision, 3);
});

test("only the installed restart snapshot can renew writes; credential failures do not block reads", async (t) => {
  const { send, requests, restart, sessions } = await setup(t);
  restart();
  const fetchOK = globalThis.fetch;
  globalThis.fetch = (path) => path === "/api/session" ? new Response(null, { status: 503 }) : fetchOK(path);
  send("reset", snapshot("B", 1));
  send("reset", snapshot("B", 2));
  requests[0].resolve(Response.json(snapshot("B", 1)));
  await setImmediate();
  assert.equal(DATA.generation, "A");
  assert.equal(canWrite(), false);
  requests[1].resolve(Response.json(snapshot("B", 2)));
  await setImmediate();
  assert.equal(DATA.generation, "B");
  assert.equal(canWrite(), false);
  send("patch", { generation: "B", revision: 3, upserted: [], removed: [] });
  assert.equal(DATA.revision, 3);
  globalThis.fetch = fetchOK;
  t.mock.timers.tick(2000);
  await setImmediate();
  assert.equal(canWrite(), true);
  assert.equal(requests.length, 2); // retry only the session, not the snapshot
  assert.deepEqual(sessions, ["A", "B"]);
});

test("a buffered patch cannot clear a later stale warning", async (t) => {
  const { send, requests, stale } = await setup(t);
  window.refetchAll();
  send("patch", { generation: "A", revision: 10, upserted: [], removed: [] });
  send("stale", {});
  requests[0].resolve(Response.json(snapshot("A", 9)));
  await setImmediate();
  assert.equal(DATA.revision, 10);
  assert.equal(stale(), true);
});
