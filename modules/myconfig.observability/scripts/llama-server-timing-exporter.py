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

OUTFILE = sys.argv[1]
UNIT_NAME = sys.argv[2]

# --- regexes -----------------------------------------------------------

# Periodic line:
#   slot print_timing: id 0 | task 47721 | n_decoded =    100, tg =  56.60 t/s
RE_PERIODIC = re.compile(
    r"slot\s+print_timing:.*?task\s+(\d+).*?n_decoded\s*=\s*(\d+),\s*tg\s*=\s*([\d.]+)\s*t/s"
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

# Rolling "live" tg rate: last seen value (Gauge semantics — we just
# want the most recent value at scrape time).
last_tg_tps = 0.0

# Pending per-task summary accumulator: task_id -> dict of partial fields.
pending = {}

# Completed task ring buffer — keep the last 1000 tasks so the .prom
# file carries recent history without growing unboundedly.
MAX_TASKS = 1000
completed = {}  # task_id (str) -> result dict

# Simple counter of processed journal lines.
lines_total = 0

# --- helpers -----------------------------------------------------------


def write_prom(path, last_tg, tasks, n_lines):
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        # live tg rate (Gauge — last seen value)
        f.write(
            "# HELP llama_server_tg_tokens_per_second "
            "Rolling token-generation rate (t/s) from the most recent "
            "periodic print_timing log line.\n"
        )
        f.write("# TYPE llama_server_tg_tokens_per_second gauge\n")
        f.write(f"llama_server_tg_tokens_per_second {last_tg:.4f}\n")

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

        for tid, r in tasks.items():
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
        completed[task_id] = r
        del pending[task_id]
        # Evict oldest if over limit
        while len(completed) > MAX_TASKS:
            oldest = next(iter(completed))
            del completed[oldest]


def process_line(line):
    global last_tg_tps, lines_total
    lines_total += 1

    # 1. Periodic tg line
    m = RE_PERIODIC.search(line)
    if m and "prompt eval time" not in line and "eval time" not in line and "total time" not in line:
        last_tg_tps = float(m.group(3))
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
write_prom(OUTFILE, last_tg_tps, completed, lines_total)

with subprocess.Popen(cmd, stdout=subprocess.PIPE, text=True, bufsize=1) as proc:
    for raw_line in proc.stdout:
        line = raw_line.rstrip("\n")
        process_line(line)
        # Re-write on every line (cheap — it's an atomic rename).
        write_prom(OUTFILE, last_tg_tps, completed, lines_total)

print("llama-server-timing-exporter: journalctl exited, restarting via systemd", flush=True)
sys.exit(1)
