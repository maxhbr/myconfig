// SPDX-License-Identifier: MIT
import assert from "node:assert/strict";
import test from "node:test";
import { setImmediate } from "node:timers/promises";
import { setData, liveIssue } from "../static/core.js";

globalThis.window = {};
const { init, descriptionEditor, clearDraftProbes } = await import("../static/edit.js");

function element() {
  return {
    children: [], append(...items) { this.children.push(...items); },
    classList: { toggle() {}, add() {}, remove() {} },
  };
}

async function setup(t) {
  t.mock.timers.enable({ apis: ["setTimeout"] });
  globalThis.document = { createElement: element, getElementById: element };
  const posts = [];
  globalThis.fetch = async (path, req) => {
    if (path === "/api/capabilities") return Response.json({ write: true });
    if (path === "/api/session") return Response.json({ token: "fixture" });
    const request = Promise.withResolvers();
    posts.push({ ...JSON.parse(req.body), request });
    return request.promise;
  };
  await init();
  clearDraftProbes();
  const issue = { id: "one", status: "open", description: "original" };
  const publish = (description) => setData({ issues: [{ ...issue, description }] });
  publish(issue.description);
  const [ta, conflict] = descriptionEditor(issue).children;
  const type = (text) => { ta.value = text; ta.oninput(); };
  const confirm = () => ta.onkeydown({ key: "s", ctrlKey: true, preventDefault() {} });
  return { ta, conflict, publish, posts, type, confirm };
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
