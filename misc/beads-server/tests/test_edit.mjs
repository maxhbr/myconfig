// SPDX-License-Identifier: MIT
import assert from "node:assert/strict";
import test from "node:test";
import { setImmediate } from "node:timers/promises";
import { setData, liveIssue } from "../static/core.js";

globalThis.window = {};
const { init, canWrite, descriptionEditor, clearDraftProbes } = await import("../static/edit.js");

function element() {
  return {
    children: [], append(...items) { this.children.push(...items); },
    classList: { toggle() {}, add() {}, remove() {} },
  };
}

async function setup(t) {
  t.mock.timers.enable({ apis: ["setTimeout"] });
  const controls = [];
  globalThis.document = { createElement: element, getElementById: element, querySelectorAll: () => controls };
  const posts = [];
  const server = { generation: t.name, write: true };
  globalThis.fetch = async (path, req) => {
    if (path === "/api/capabilities") return Response.json(server);
    if (path === "/api/session") return Response.json({ generation: server.generation, token: server.generation });
    const request = Promise.withResolvers();
    posts.push({ ...JSON.parse(req.body), request, token: req.headers["X-Beads-Server-Token"] });
    return request.promise;
  };
  await init(server.generation);
  clearDraftProbes();
  const issue = { id: "one", status: "open", description: "original" };
  const publish = (description) => setData({ issues: [{ ...issue, description }] });
  publish(issue.description);
  const [ta, conflict] = descriptionEditor(issue).children;
  controls.push(ta);
  const type = (text) => { ta.value = text; ta.oninput(); };
  const confirm = () => ta.onkeydown({ key: "s", ctrlKey: true, preventDefault() {} });
  return { ta, conflict, publish, posts, type, confirm, server };
}

test("blur and debounce cannot confirm a conflict, including an empty description", async (t) => {
  const { ta, conflict, publish, posts, type, confirm } = await setup(t);
  type("draft");
  publish("");
  t.mock.timers.tick(1500);
  assert.equal(conflict.hidden, false);
  ta.onblur();
  type("revised draft");
  t.mock.timers.tick(1500);
  assert.equal(posts.length, 0);
  publish("even newer");
  confirm(); // this version hasn't been shown yet
  assert.equal(posts.length, 0);
  confirm();
  assert.equal(posts[0].description, "revised draft");
  posts[0].request.resolve(Response.json({}));
  await setImmediate();
  assert.equal(conflict.hidden, true);
  assert.equal(window.detailsHasDraft(), false);
});

test("a slow save flushes the latest edit once, even after debounce and blur", async (t) => {
  const { ta, posts, type } = await setup(t);
  type("first");
  t.mock.timers.tick(1500);
  type("second");
  t.mock.timers.tick(1500);
  type("last");
  ta.onblur();
  assert.equal(posts.length, 1);
  posts[0].request.resolve(Response.json({}));
  await setImmediate();
  assert.deepEqual(posts.map(p => p.description), ["first", "last"]);
  posts[1].request.resolve(Response.json({}));
  await setImmediate();
  t.mock.timers.tick(5000);
  assert.equal(posts.length, 2);
  assert.equal(window.detailsHasDraft(), false);
});

test("a rejected save keeps the draft and does not drain a queued write", async (t) => {
  const { ta, posts, type } = await setup(t);
  window.reconcileConflict = () => assert.equal(window.detailsHasDraft(), true);
  type("first");
  ta.onblur();
  type("still editing");
  ta.onblur();
  posts[0].request.resolve(Response.json({ issue: { description: "remote" } }, { status: 409 }));
  await setImmediate();
  ta.onblur();
  assert.equal(posts.length, 1);
  assert.equal(ta.value, "still editing");
  assert.equal(window.detailsHasDraft(), true);
});

test("a delayed successful save cannot undo a live revert to the old text", async (t) => {
  const { ta, posts, type, publish } = await setup(t);
  type("my change");
  ta.onblur();
  publish("my change");
  publish("original");
  posts[0].request.resolve(Response.json({}));
  await setImmediate();
  assert.equal(liveIssue("one").description, "original");
});

test("a restart preserves unsent drafts without replaying old writes with the new token", async (t) => {
  const { ta, posts, type, confirm, server } = await setup(t);
  type("sending");
  ta.onblur();
  type("draft during request");
  server.generation = "read-only";
  server.write = false;
  await init(server.generation);
  assert.equal(ta.disabled, true);
  assert.equal(canWrite(), false);
  posts[0].request.resolve(Response.json({}));
  await setImmediate();
  server.generation = "writable-again";
  server.write = true;
  await init(server.generation);
  assert.equal(ta.disabled, false);
  t.mock.timers.tick(1500);
  ta.onblur();
  assert.equal(posts.length, 1);
  assert.equal(ta.value, "draft during request");
  confirm();
  assert.equal(posts[1].token, "writable-again");
  assert.equal(posts[1].description, ta.value);
});

test("a delayed session reply cannot enable writes on a newer read-only server", async (t) => {
  const { ta } = await setup(t);
  const tokenReply = Promise.withResolvers();
  globalThis.fetch = async (path) => path === "/api/capabilities"
    ? Response.json({ generation: "B", write: true }) : tokenReply.promise;
  const old = init("B");
  await setImmediate();
  assert.equal(ta.disabled, true);
  globalThis.fetch = async () => Response.json({ generation: "C", write: false });
  assert.equal(await init("C"), true);
  tokenReply.resolve(Response.json({ generation: "B", token: "B" }));
  assert.equal(await old, false);
  assert.equal(canWrite(), false);
  assert.equal(ta.disabled, true);
});
