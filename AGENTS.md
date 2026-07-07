# Protocol Agent Rules

This repository contains common protocol infrastructure plus protocol-specific libraries.

## Scope

- `Protocol.Core` or equivalent shared modules: parser, validator, decoder, writer, diagnostics, trace, result types and source anchors.
- DLMS/COSEM modules: HDLC, wrapper, ACSE, xDLMS, COSEM, security/ciphering and DLMS-specific domain models.
- M-Bus modules: FT1.2 frames, C-field, A-field, CI-field, DIF/VIF, application records, security and M-Bus-specific domain models.

## Hard rules

- Confidential standards must never be committed.
- Local standard files live only under `.local/standards/`.
- The public repository may reference standards by document, clause, table or figure.
- Do not copy standard tables or long examples into source, tests, issues or pull requests.
- Domain models must exclude invalid combinations where possible.
- Parser layer is syntactic and fail-fast.
- Validator layer checks semantic/protocol rules.
- Decoder layer orchestrates parsing, validation, deciphering/unprotection and domain construction.
- Writer layer mirrors the validated/domain model and must not be able to emit invalid protocol combinations.
- OpenAI agents must not read `.local/standards/**`.
- DeepSeek standard/review agents may read `.local/standards/**`.
- No protocol-related implementation is done until `protocol-review` returns `PASS` or `PASS_WITH_RISKS`.

## Agent roles

- `protocol-orchestrator`: OpenAI, user-facing workflow agent for `/dlms-plan`, `/dlms-implement`, `/mbus-plan`, `/mbus-implement` and common protocol work.
- `protocol-build`: OpenAI, implementation subagent. It must not read local standards and must not make normative standard decisions.
- `dlms-standard`: DeepSeek, DLMS/COSEM standard expert.
- `mbus-standard`: DeepSeek, M-Bus standard expert.
- `protocol-review`: DeepSeek, post-implementation review gate. It reviews the actual diff and calls the relevant standard agents as needed.

## Routing rules

- DLMS/COSEM task: delegate standard questions to `dlms-standard`.
- M-Bus task: delegate standard questions to `mbus-standard`.
- Shared core task: ask both standard agents only for requirements that influence the shared abstraction.
- Never merge conclusions from DLMS and M-Bus without naming the source standard.
- If a task appears to involve DLMS-over-M-Bus, stop and ask whether a bridge-specific review should be introduced.

## Definition of done for protocol changes

A protocol-related change is not done until:

1. The implementation was made in a focused, reviewable diff.
2. Tests were run or a concrete reason for not running them was documented.
3. `protocol-review` reviewed the actual `git diff`.
4. The review verdict is `PASS` or `PASS_WITH_RISKS`.
5. All `BLOCKED` findings have been fixed.
6. No confidential standard material was committed.
