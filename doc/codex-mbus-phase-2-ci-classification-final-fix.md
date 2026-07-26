# Codex Task: M-Bus Decoder Phase 2 — CI Classification Final Fix

## Repository and baseline

- Repository: `florianhofmaier/meter-protocols`
- Branch: `rework_decoder`
- Reviewed baseline: current branch after commit `8e80d72491c4c1e2ac82da402c02eebc1bc02e53`
- Work from the current local workspace.
- Inspect the current code, project structure, compile order, test helpers, and naming conventions before changing anything.
- Do not recreate removed legacy types such as `FrameTpl`.
- Do not invent existing APIs.

Run the real build and test commands before and after the changes:

```bash
dotnet build src/Metering.sln
dotnet test src/Metering.sln --no-build
```

Use the actual solution path if it differs.

Do not report successful build or tests unless the commands were really executed.

---

# Goal

Correct the remaining normative errors in TPL-CI classification.

The current architecture is otherwise accepted and must not be redesigned.

This task shall:

1. classify CI `0x54`, `0x55`, and `0x56` correctly according to EN 13757-3:2025;
2. distinguish CI values that are standard-defined but not applicable to wired M-Bus from standard-defined wired variants that are merely unsupported by this decoder;
3. remove the false `Unknown` classification for `0x54` and `0x55`;
4. preserve all completed Phase-2 behavior.

Do not start a new decoder phase.

---

# Normative source priority

For application-layer CI assignments, use the specific current application standard:

- **EN 13757-3:2025**, Clause 7.3, Table 27

Use additionally where applicable:

- **EN 13757-7:2018**, Clause 5.2, Table 2

When assignments or descriptions differ, do not silently prefer an older referenced edition over EN 13757-3:2025 within the EN 13757-3 application-layer scope.

Do not classify a CI value as unknown or reserved merely because the current decoder does not implement its following structure.

OMS rules apply only when an OMS profile is explicitly selected.

---

# Current defect

The current `CiFieldTpl` implementation contains logic equivalent to:

```fsharp
| 0x54uy
| 0x55uy ->
    CiClassification.Unknown value
```

and the reserved ranges include `0x56`.

This is incorrect for EN 13757-3:2025, Clause 7.3, Table 27.

The current table defines:

```text
0x54
→ Request of selected application to device
→ no TPL header
→ applicable to M-Bus

0x56
→ Request of selected application to device
→ short TPL header
→ applicable to M-Bus

0x55
→ Request of selected application to device
→ long TPL header
→ applicable to M-Bus
```

Phase 2 does not need to implement these structures.

They must nevertheless be classified as:

```text
standard-defined
applicable to wired M-Bus
not supported by this decoder
```

They are not unknown and not reserved.

---

# Required classification model

Extend or refine the internal CI classification so it can represent at least:

```fsharp
[<RequireQualifiedAccess>]
type internal CiClassification =
    | Supported of CiFieldTpl
    | Afl
    | StandardDefinedUnsupportedForWired of byte
    | StandardDefinedNotApplicableToWired of byte
    | Reserved of byte
    | Unknown of byte
```

The exact names may be adapted to the repository conventions.

The semantics must remain distinct:

## Supported

The current semantic DU can represent and decode the following structure.

## AFL

A valid AFL discriminator handled separately by the complete-message parser.

## Standard-defined unsupported for wired

The value is defined by the current normative edition and applicable to M-Bus, but this decoder does not yet implement the selected structure or protocol.

Examples required for this task:

```text
0x54
0x55
0x56
```

## Standard-defined but not applicable to wired

The value is defined by the current standard, but Table 27 marks it as not applicable to M-Bus.

Example:

```text
0x67
```

Use the exact current table entry when selecting test values.

## Reserved

The applicable current normative table reserves the value.

## Unknown

Use only when the applicable current edition neither defines nor reserves the value, or when the repository explicitly cannot classify it from the available normative source.

Do not use `Unknown` as a fallback for values removed from the current implementation.

---

# Required implementation changes

## 1. Correct `0x54`, `0x55`, and `0x56`

In the central classification function:

```fsharp
0x54uy
0x55uy
0x56uy
```

must produce:

```fsharp
CiClassification.StandardDefinedUnsupportedForWired value
```

or the equivalent repository-specific case.

Remove:

- the comment claiming `0x54` and `0x55` are absent from EN 13757-3:2025;
- the explicit `Unknown` mapping for `0x54` and `0x55`;
- any reserved range that captures `0x56` before the current EN 13757-3:2025 assignment is applied.

The specific current standard assignment must take precedence over broader or older reserved ranges.

---

## 2. Add wired applicability classification

Do not classify every standard-defined value as a valid but unsupported wired variant.

Use the M-Bus applicability column from EN 13757-3:2025, Table 27.

For a value such as `0x67`, return:

```fsharp
CiClassification.StandardDefinedNotApplicableToWired value
```

or equivalent.

Required diagnostic:

```text
TPL CI value 0x67 is standard-defined but not applicable to wired M-Bus.
EN 13757-3:2025, Clause 7.3, Table 27.
```

This is distinct from:

```text
Unsupported, but standard-conformant wired TPL CI value 0x54.
```

Do not call a value “standard-conformant for wired M-Bus” when the standard marks it as not applicable to M-Bus.

---

## 3. Make EN 13757-3:2025 the authoritative application-CI table

The central classifier must explicitly encode the current application-CI assignments needed for classification.

Do not use this pattern:

```fsharp
if not supported and not reserved then
    StandardDefinedUnsupported
```

That default incorrectly treats unverified values as defined and wired-applicable.

Prefer a closed classification table or explicit groups:

```fsharp
let internal classify value =
    match trySupported value with
    | Some ci ->
        CiClassification.Supported ci

    | None ->
        match value with
        | 0x90uy ->
            CiClassification.Afl

        | 0x54uy
        | 0x55uy
        | 0x56uy ->
            CiClassification.StandardDefinedUnsupportedForWired value

        | 0x67uy ->
            CiClassification.StandardDefinedNotApplicableToWired value

        | value when isReservedCurrentEdition value ->
            CiClassification.Reserved value

        | value when isOtherDefinedCurrentEdition value ->
            classifyCurrentApplicability value

        | value ->
            CiClassification.Unknown value
```

This snippet is conceptual.

Use the complete normative table entries needed for all values currently classified by the code.

Do not guess ranges.

---

## 4. Correct diagnostic references

Diagnostics must cite the source that actually defines the classification.

For:

```text
0x54
0x55
0x56
0x67
```

reference:

```text
EN 13757-3:2025, Clause 7.3, Table 27
```

Do not cite only EN 13757-7:2018, Table 2 for a classification derived from EN 13757-3:2025.

For AFL `0x90`, keep the existing EN 13757-7:2018 reference.

For a reserved value, cite the actual table that marks it reserved.

For an unknown value, state clearly that the value is not assigned by the selected current edition.

---

## 5. Keep one authoritative CI module

Retain `CiFieldTpl` as the single owner of:

- supported CI mapping;
- value conversion;
- direction;
- current-edition classification;
- parser diagnostics.

Do not add a second CI classification table to:

```text
CompleteMessage.fs
Tpl.fs
Apl.fs
WiredMbusFrame.fs
```

`CompleteMessage.fs` may only ask:

```fsharp
CiFieldTpl.isAfl
```

for the current AFL pre-discriminator.

---

# Preserve the accepted cleanup

Do not regress the following completed work.

## Unsupported security modes

Keep:

- `AplExpansionRaw.NotExpandedForUnsupportedSecurityMode`;
- private `ExpandableAplRaw`;
- private `AplContentValidation`;
- exactly one authoritative security-mode issue from the TPL header validator;
- no public `"APL expansion was skipped"` diagnostic;
- no ordinary-flow `invalidOp`.

## Wired short-header Mode 5

Keep:

```text
short header + Mode 5
→ parser-stage failure
```

for both:

```text
CI 0x7A
CI 0x57
```

The parser must still structurally parse the short header before applying the wired Mode-5 rule.

## Supported CI behavior

Keep:

```text
0x50
0x51
0x52
0x53
0x57
0x72
0x75
0x7A
```

including:

- `value` roundtrip;
- parser roundtrip;
- direction mapping;
- APL selection.

## Validation architecture

Keep:

- one applicative root validator;
- DLL/TPL/payload/cross-layer accumulation;
- no invalid domain object for unsupported modes;
- protected APL invariants;
- partial Mode-5 behavior.

---

# Required tests

Follow the current xUnit/FsUnit conventions.

## Replace the false `0x54` test

Remove:

```text
CI 0x54 older-edition assignment is unknown for EN 13757-3 2025
```

Replace it with:

```text
CI 0x54 is standard-defined and wired-applicable but unsupported
```

Assert:

```text
Unsupported, but standard-conformant
CI value 0x54
applicable to wired M-Bus
EN 13757-3:2025
Clause 7.3
Table 27
```

Assert it does not contain:

```text
Unknown
Reserved
AFL
```

## Add `0x55`

Add:

```text
CI 0x55 is standard-defined and wired-applicable but unsupported
```

Use the same diagnostic category and normative reference.

## Add `0x56`

Add:

```text
CI 0x56 is standard-defined and wired-applicable but unsupported
```

Assert it is not classified as reserved.

## Add not-applicable-to-wired test

Add:

```text
CI 0x67 is standard-defined but not applicable to wired M-Bus
```

Assert a distinct diagnostic category/message.

Assert it does not contain:

```text
Unsupported, but standard-conformant wired TPL CI
Reserved
Unknown
AFL
```

## Keep a reserved-value test

Choose a value explicitly reserved by the current applicable table.

Test:

```text
reserved CI value is classified as reserved
```

Document the exact standard row or range in the test.

Do not reuse `0x56`.

## Keep or add an unknown-value test

Only add this test when the current normative tables genuinely leave a value unassigned and non-reserved.

When no such value exists in the relevant range, remove the `Unknown` case instead of inventing a test.

The implementation should not retain a type case with no normative meaning merely because the previous code had one.

## Regression tests

Retain:

```text
AFL 0x90 has its distinct diagnostic
CI 0x57 + short header + Mode 5 fails during parsing
CI 0x7A + short header + Mode 5 fails during parsing
CI 0x57 + Mode 0 succeeds
CI 0x7A + Mode 0 succeeds
unsupported security mode produces exactly one issue
reserved security mode produces exactly one issue
```

---

# Acceptance criteria

The task is complete when:

1. `0x54` is not classified as unknown.
2. `0x55` is not classified as unknown.
3. `0x56` is not classified as reserved.
4. `0x54`, `0x55`, and `0x56` are classified as current-standard, wired-applicable, unsupported variants.
5. CI values not applicable to M-Bus have a distinct classification.
6. `0x67` is not described as a valid unsupported wired variant.
7. diagnostic references use EN 13757-3:2025, Clause 7.3, Table 27 where that table defines the result.
8. AFL remains separate.
9. no parallel CI classification table is introduced.
10. no synthetic APL-expansion security-mode issue is reintroduced.
11. the root validator remains applicative.
12. all existing and new tests pass.
13. the complete solution builds successfully.

---

# Final report

After implementation, report:

1. changed files and modules;
2. final CI-classification DU;
3. classification of `0x54`, `0x55`, `0x56`, and `0x67`;
4. exact EN 13757-3:2025 Table 27 rows used;
5. reserved test value and normative source;
6. whether `Unknown` remains and why;
7. tests removed, added, or changed;
8. actual build and test commands;
9. actual build and test results;
10. remaining Phase-2 limitations.

Do not report successful build or tests unless the commands were actually executed.
