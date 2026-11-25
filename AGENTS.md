# Agent Instructions

## M-Bus Standards

For any task that touches M-Bus protocol behavior, parsing, writing, validation, tests, or code review, first consult the local standard PDFs before making or validating claims or code changes.

Local standards are expected at:

```text
doc/local-standards/mbus/
```

Use the relevant standard part:

- IEC 60870-5-2 for FT 1.2 frame formats, data-link transmission procedures, and link user data terminology inherited by wired M-Bus.
- EN 13757-2 for wired M-Bus data link and physical communication rules.
- EN 13757-3 for application protocol, CI/TPL/APL, records, and data encoding.
- EN 13757-4 for wireless M-Bus behavior.
- EN 13757-7 for transport security services.
- EN 13757-1 for general data exchange context.

When changing or reviewing protocol code:

- Do not rely only on memory or existing implementation patterns for standard-defined behavior.
- Verify the behavior against the applicable local standard PDF before proposing, implementing, editing, or reviewing protocol semantics.
- In the same turn, explicitly identify the standard part and the section/table/page that justifies the behavior before making the code change. If the relevant passage has not been located yet, stop and locate it first.
- Mention the standard file, section, table, or page used when giving conclusions about protocol semantics.
- If the required local standard PDF is missing or unreadable, say so and treat standard-dependent conclusions as blocked until the document is available.
- Keep the standards local-only. Do not add standard PDFs or copied standard text to git.

## Protocol Modeling

Follow the shared [Protocol Modeling rules in CONTRIBUTING.md](CONTRIBUTING.md#protocol-modeling).

## Architecture Documentation

- `doc/requirements.adoc` defines project capabilities and quality scenarios.
- `doc/architecture/architecture.adoc` describes the currently accepted architecture.
- `doc/architecture/decisions/` contains accepted decisions and their rationale.
- `doc/architecture/proposals/` contains open designs; do not treat them as accepted decisions.
- `doc/protocols/*/conformance.adoc` tracks standard coverage and must link claims to tests before marking them verified.

Keep target requirements, accepted architecture, open proposals, and implementation status distinct. Update the relevant document when a change alters one of them.

## Project Conventions

Follow [CONTRIBUTING.md](CONTRIBUTING.md) for formatting, shared code conventions,
and validation commands. Keep these rules in that shared document rather than
duplicating them here. Use `bash scripts/format.sh --check` to verify formatting.
