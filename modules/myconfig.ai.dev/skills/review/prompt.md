---
description: Run a code review sub-agent
---
Spawn yourself as a sub-agent via bash to do a code review: $@

Use `pi --print` with appropriate arguments. If the user specifies a model,
use `--provider` and `--model` accordingly.

Pass a prompt to the sub-agent asking it to review the code for:
- Bugs and logic errors
- Security issues
- Error handling gaps

Do not read the code yourself. Let the sub-agent do that.

Report the sub-agent's findings.
