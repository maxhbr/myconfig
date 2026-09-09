---
description: Run a read-only research sub-agent to investigate a question in the codebase
---
Use the subagent tool to spawn the "research" agent to investigate: $@

The research agent does read-only codebase investigation and returns a
structured report with exact file paths and line ranges, so you can act on
its findings without re-reading the code yourself.

Do not investigate the codebase yourself. Let the research sub-agent do
the reading and return its report.
