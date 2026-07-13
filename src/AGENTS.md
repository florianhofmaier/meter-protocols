# Agent Instructions

## M-Bus Standard

For any task that touches M-Bus protocol behavior, parsing, writing, validation, tests, or code review, first consult the local standard PDFs before making or validating claims.

Local standards are expected at:

```text
../doc/local-standards/mbus/
```

Use the relevant standard part:

- EN 13757-2 for wired M-Bus data link and physical communication rules.
- EN 13757-3 for application protocol, CI/TPL/APL, records, and data encoding.
- EN 13757-4 for wireless M-Bus behavior.
- EN 13757-7 for transport security services.
- EN 13757-1 for general data exchange context.

When changing or reviewing protocol code:

- Do not rely only on memory or existing implementation patterns for standard-defined behavior.
- Verify the behavior against the applicable local standard PDF.
- Mention the standard file, section, table, or page used when giving conclusions about protocol semantics.
- If the required local standard PDF is missing or unreadable, say so and treat standard-dependent conclusions as blocked until the document is available.
- Keep the standards local-only. Do not add standard PDFs or copied standard text to git.

