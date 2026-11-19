// SPDX-License-Identifier: MIT
import assert from "node:assert/strict";
import test from "node:test";
import { setImmediate } from "node:timers/promises";
import { DATA, setData } from "../static/core.js";
import { connect } from "../static/live.js";

const snapshot = (generation, revision) => ({ generation, revision, issues: [], blocked_by: {} });

function setup(t) {
  t.mock.timers.enable({ apis: ["setTimeout"] });
  let stale;
  const requests = [];
  const events = new Map();
  globalThis.window = { render() {} };
  globalThis.document = {
    addEventListener() {},
    getElementById: () => ({ classList: { toggle: (_, flag) => { stale = flag; } } }),
  };
  globalThis.requestAnimationFrame = (f) => f();
  globalThis.EventSource = class {
    close() {}
    addEventListener(name, f) { events.set(name, f); }
  };
  globalThis.fetch = () => {
    const request = Promise.withResolvers();
    requests.push(request);
    return request.promise;
  };
  setData(snapshot("A", 9));
  connect(9, "A", () => {});
  const send = (name, data) => events.get(name)({ data: JSON.stringify(data) });
  send("hello", snapshot("A", 9));
  return { send, requests, stale: () => stale };
}

test("a superseded fetch cannot roll back a restarted server or its buffered patch", async (t) => {
  const { send, requests, stale } = setup(t);
  window.refetchAll();
  send("reset", snapshot("B", 1));
  send("patch", { generation: "B", revision: 2, upserted: [], removed: [],
    indices: { generation: "B", revision: 2, blocked_by: { one: 1 } } });
  requests[1].resolve(Response.json(snapshot("B", 1)));
  await setImmediate();
  requests[0].resolve(Response.json(snapshot("A", 10)));
  await setImmediate();
  assert.equal(DATA.generation, "B");
  assert.equal(DATA.revision, 2);
  assert.deepEqual(DATA.blocked_by, { one: 1 });
  assert.equal(stale(), false);
});

test("a failed, wrong-process or too-old recovery retries without another event", async (t) => {
  const { send, requests, stale } = setup(t);
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
