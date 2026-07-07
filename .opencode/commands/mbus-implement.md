---
description: Implement an M-Bus change with automatic protocol review
agent: protocol-orchestrator
---

Implement the accepted M-Bus / wM-Bus plan or task:

$ARGUMENTS

Use the implementation workflow:

- delegate code changes to `protocol-build`,
- run or request tests,
- invoke `protocol-review` on the actual git diff,
- if `protocol-review` returns `BLOCKED`, fix via `protocol-build` and repeat review,
- final response only after `protocol-review` returns `PASS` or `PASS_WITH_RISKS`.
