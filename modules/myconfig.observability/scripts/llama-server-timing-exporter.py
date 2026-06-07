#!/usr/bin/env python3
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Journal-tailing Prometheus textfile exporter for llama-server timing metrics.
#
# Parses print_timing log lines from the llama-cpp systemd unit and writes a
# textfile .prom for node_exporter.
#
# Two log formats are handled:
#
# 1. Periodic (every ~3 s during generation):
#    slot print_timing: id N | task T | n_decoded = D, tg = X t/s
#
# 2. Final summary (once per completed task, four consecutive lines):
#    slot print_timing: id N | task T | prompt eval time = P ms / Tp tokens (..., Xp tokens per second)
#    slot print_timing: id N | task T |        eval time = E ms / Te tokens (..., Xe tokens per second)
#    slot print_timing: id N | task T |       total time = M ms / Tt tokens
#    slot print_timing: id N | task T |    graphs reused = G
#
# Usage: llama-server-timing-exporter.py <outfile.prom> <systemd-unit-name>
#
# The script is intentionally stateless across restarts: it follows the
# journal from the current position (-n 0 -f) so it never replays old entries.

import re
import subprocess
import sys
import os
import time
import threading

OUTFILE = sys.argv[1]
UNIT_NAME = sys.argv[2]

# How long (seconds) a completed task's per-task metrics remain in the
# textfile after completion. Per-task metrics are point-in-time events:
# exposing them as persistent gauges makes Grafana draw a flat horizontal
# line from completion time to "now" (and overlap across tasks). By
# expiring them shortly after completion each task instead shows up as a
# discrete spike/point around the time it actually finished.
TASK_TTL_SECONDS = float(os.environ.get("LLAMA_TIMING_TASK_TTL_SECONDS", "60"))

# How often (seconds) to re-write the textfile in the background so that
# expired tasks drop out even when the journal is idle (the main loop only
# re-writes when a new line arrives).
REWRITE_INTERVAL_SECONDS = 15.0

# --- regexes -----------------------------------------------------------

# Periodic line (captures slot id + task so interleaved generations from
# multiple slots/tasks are tracked as independent series):
#   slot print_timing: id 0 | task 47721 | n_decoded =    100, tg =  56.60 t/s
RE_PERIODIC = re.compile(
    r"slot\s+print_timing:.*?id\s+(\d+).*?task\s+(\d+).*?n_decoded\s*=\s*(\d+),\s*tg\s*=\s*([\d.]+)\s*t/s"
)

# Final-summary lines (matched individually, state-machine assembled):
#   prompt eval time = P ms / Tp tokens (..., Xp tokens per second)
RE_PROMPT_EVAL = re.compile(
    r"slot\s+print_timing:.*?task\s+(\d+).*?prompt eval time\s*=\s*([\d.]+)\s*ms\s*/\s*(\d+)\s*tokens.*?([\d.]+)\s*tokens per second"
)
#        eval time = E ms / Te tokens (..., Xe tokens per second)
RE_EVAL = re.compile(
    r"slot\s+print_timing:.*?task\s+(\d+).*?(?<!prompt )eval time\s*=\s*([\d.]+)\s*ms\s*/\s*(\d+)\s*tokens.*?([\d.]+)\s*tokens per second"
)
#       total time = M ms / Tt tokens
RE_TOTAL = re.compile(
    r"slot\s+print_timing:.*?task\s+(\d+).*?total time\s*=\s*([\d.]+)\s*ms\s*/\s*(\d+)\s*tokens"
)

# --- state -------------------------------------------------------------

# Live (periodic) tg rate, tracked per active generation so interleaved
# tasks each get their own series. Keyed by (slot_id, task_id) -> dict with
# the latest "tg" value and a "_seen_at" wall-clock stamp used to expire
# series once a task stops emitting periodic lines (otherwise a finished
# generation would leave a permanent flat line).
live_tg = {}  # (slot_id (str), task_id (str)) -> {"tg": float, "_seen_at": float}

# How long (seconds) a live per-task tg series lingers after its last
# periodic print_timing line before being dropped from the textfile.
LIVE_TTL_SECONDS = float(os.environ.get("LLAMA_TIMING_LIVE_TTL_SECONDS", "15"))

# Pending per-task summary accumulator: task_id -> dict of partial fields.
pending = {}

# Completed task ring buffer — keep the last 1000 tasks so the .prom
# file never grows unboundedly even under pathological churn within the
# TTL window. Each entry additionally carries a "_done_at" wall-clock
# timestamp used for TTL-based expiry at write time.
MAX_TASKS = 1000
completed = {}  # task_id (str) -> result dict

# Guards `completed`/`live_tg`/`lines_total` against concurrent access
# from the main journal loop and the background re-write timer.
state_lock = threading.Lock()

# Simple counter of processed journal lines.
lines_total = 0

# --- helpers -----------------------------------------------------------


def write_prom(path, live, tasks, n_lines):
    # Only emit tasks completed within the TTL window. Expired tasks are
    # dropped so each per-task metric renders as a discrete point in time
    # rather than a permanent flat line.
    now = time.time()
    fresh = {
        tid: r
        for tid, r in tasks.items()
        if now - r.get("_done_at", now) <= TASK_TTL_SECONDS
    }
    # Live tg series, expired once a generation stops emitting periodic lines.
    fresh_live = {
        key: r
        for key, r in live.items()
        if now - r.get("_seen_at", now) <= LIVE_TTL_SECONDS
    }

    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        # live tg rate (Gauge — most recent periodic value, per slot/task)
        f.write(
            "# HELP llama_server_tg_tokens_per_second "
            "Rolling token-generation rate (t/s) from the most recent "
            "periodic print_timing log line, labelled by slot and task. "
            "Interleaved generations appear as independent series.\n"
        )
        f.write("# TYPE llama_server_tg_tokens_per_second gauge\n")
        for (slot_id, task_id), r in fresh_live.items():
            f.write(
                f'llama_server_tg_tokens_per_second'
                f'{{slot="{slot_id}",task="{task_id}"}} {r["tg"]:.4f}\n'
            )

        # per-task final metrics
        f.write(
            "# HELP llama_server_task_tg_tokens_per_second "
            "Token-generation rate (t/s) for each completed task.\n"
        )
        f.write("# TYPE llama_server_task_tg_tokens_per_second gauge\n")
        f.write(
            "# HELP llama_server_task_prompt_eval_tokens_per_second "
            "Prompt-eval (prefill) rate (t/s) for each completed task.\n"
        )
        f.write("# TYPE llama_server_task_prompt_eval_tokens_per_second gauge\n")
        f.write(
            "# HELP llama_server_task_total_time_ms "
            "Total wall time (ms) for each completed task.\n"
        )
        f.write("# TYPE llama_server_task_total_time_ms gauge\n")
        f.write(
            "# HELP llama_server_task_n_decoded_tokens "
            "Number of tokens generated (eval phase) per completed task.\n"
        )
        f.write("# TYPE llama_server_task_n_decoded_tokens gauge\n")
        f.write(
            "# HELP llama_server_task_n_prompt_tokens "
            "Number of prompt tokens processed per completed task.\n"
        )
        f.write("# TYPE llama_server_task_n_prompt_tokens gauge\n")

        for tid, r in fresh.items():
            lbl = f'task="{tid}"'
            if "tg_tps" in r:
                f.write(
                    f'llama_server_task_tg_tokens_per_second{{{lbl}}} {r["tg_tps"]:.4f}\n'
                )
            if "prompt_tps" in r:
                f.write(
                    f'llama_server_task_prompt_eval_tokens_per_second{{{lbl}}} {r["prompt_tps"]:.4f}\n'
                )
            if "total_ms" in r:
                f.write(
                    f'llama_server_task_total_time_ms{{{lbl}}} {r["total_ms"]:.3f}\n'
                )
            if "n_decoded" in r:
                f.write(
                    f'llama_server_task_n_decoded_tokens{{{lbl}}} {r["n_decoded"]}\n'
                )
            if "n_prompt" in r:
                f.write(
                    f'llama_server_task_n_prompt_tokens{{{lbl}}} {r["n_prompt"]}\n'
                )

        # counter
        f.write(
            "# HELP llama_server_timing_journal_lines_total "
            "Total journal lines processed by the timing exporter.\n"
        )
        f.write("# TYPE llama_server_timing_journal_lines_total counter\n")
        f.write(f"llama_server_timing_journal_lines_total {n_lines}\n")

    os.replace(tmp, path)


def task_complete(task_id):
    """Move a pending task to completed once we have all three fields."""
    r = pending.get(task_id, {})
    if "tg_tps" in r and "prompt_tps" in r and "total_ms" in r:
        r["_done_at"] = time.time()
        with state_lock:
            completed[task_id] = r
            # Evict oldest if over limit
            while len(completed) > MAX_TASKS:
                oldest = next(iter(completed))
                del completed[oldest]
        del pending[task_id]


def snapshot_and_write():
    """Take a consistent snapshot of shared state under the lock and
    re-write the textfile. Safe to call from multiple threads."""
    with state_lock:
        live = {k: dict(v) for k, v in live_tg.items()}
        tasks = dict(completed)
        n_lines = lines_total
    write_prom(OUTFILE, live, tasks, n_lines)


def background_rewriter():
    """Periodically re-write the textfile so TTL-expired tasks drop out
    even when the journal is idle (the main loop only writes on new lines)."""
    while True:
        time.sleep(REWRITE_INTERVAL_SECONDS)
        snapshot_and_write()


def process_line(line):
    global lines_total
    with state_lock:
        lines_total += 1

    # 1. Periodic tg line
    m = RE_PERIODIC.search(line)
    if m and "prompt eval time" not in line and "eval time" not in line and "total time" not in line:
        slot_id = m.group(1)
        task_id = m.group(2)
        tg = float(m.group(4))
        with state_lock:
            live_tg[(slot_id, task_id)] = {"tg": tg, "_seen_at": time.time()}
            # Bound the live map under pathological churn (matches MAX_TASKS).
            while len(live_tg) > MAX_TASKS:
                del live_tg[next(iter(live_tg))]
        return

    # 2. prompt eval line (first of the final-summary block)
    m = RE_PROMPT_EVAL.search(line)
    if m:
        tid = m.group(1)
        pending.setdefault(tid, {})
        pending[tid]["prompt_tps"] = float(m.group(4))
        pending[tid]["n_prompt"] = int(m.group(3))
        task_complete(tid)
        return

    # 3. eval line (tg t/s for the final summary)
    m = RE_EVAL.search(line)
    if m:
        tid = m.group(1)
        pending.setdefault(tid, {})
        pending[tid]["tg_tps"] = float(m.group(4))
        pending[tid]["n_decoded"] = int(m.group(3))
        task_complete(tid)
        return

    # 4. total time line
    m = RE_TOTAL.search(line)
    if m:
        tid = m.group(1)
        pending.setdefault(tid, {})
        pending[tid]["total_ms"] = float(m.group(2))
        task_complete(tid)
        return


# --- main --------------------------------------------------------------

# -n 0: start from the end (don't replay history)
# -f:   follow new entries
# -u:   filter to the llama-cpp unit
# -o cat: plain one-line format, no JSON overhead
cmd = [
    "journalctl",
    "-n", "0",
    "-f",
    "-u", UNIT_NAME,
    "-o", "cat",
]

print(f"llama-server-timing-exporter: tailing journal for {UNIT_NAME!r}", flush=True)
print(f"llama-server-timing-exporter: writing textfile to {OUTFILE!r}", flush=True)

# Write an initial (empty) file so node_exporter doesn't log a
# "file not found" warning before the first real line arrives.
snapshot_and_write()

# Background thread to expire stale tasks while the journal is idle.
threading.Thread(target=background_rewriter, daemon=True).start()

with subprocess.Popen(cmd, stdout=subprocess.PIPE, text=True, bufsize=1) as proc:
    for raw_line in proc.stdout:
        line = raw_line.rstrip("\n")
        process_line(line)
        # Re-write on every line (cheap — it's an atomic rename).
        snapshot_and_write()

print("llama-server-timing-exporter: journalctl exited, restarting via systemd", flush=True)
sys.exit(1)
