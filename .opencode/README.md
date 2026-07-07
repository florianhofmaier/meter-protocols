# OpenCode protocol workflow

This directory configures a protocol-specific OpenCode workflow for this repository.

The setup is intentionally split by responsibility:

- `protocol-orchestrator` uses OpenAI and is the only user-facing workflow agent.
- `protocol-build` uses OpenAI and implements accepted plans.
- `dlms-standard` uses DeepSeek and reads local DLMS/COSEM standards.
- `mbus-standard` uses DeepSeek and reads local M-Bus standards.
- `protocol-review` uses DeepSeek and reviews the actual diff after implementation.

## First-time setup

1. Connect providers in OpenCode:

   ```text
   /connect
   ```

   Connect OpenAI and DeepSeek.

2. Open model selection and copy the exact model IDs:

   ```text
   /models
   ```

3. Replace the placeholders in `.opencode/agents/*.md`:

   ```text
   openai/<REPLACE_WITH_OPENAI_MODEL_ID>
   deepseek/<REPLACE_WITH_DEEPSEEK_MODEL_ID>
   ```

   The required format is `provider/model-id`.

4. Keep confidential standards local only:

   ```text
   .local/standards/
   ```

   Do not commit `.local/`, PDFs, DOCX files or generated RAG indexes.

## Commands

Use these commands from the OpenCode TUI:

```text
/dlms-plan <task>
/dlms-implement <accepted plan or task>
/mbus-plan <task>
/mbus-implement <accepted plan or task>
/protocol-plan <shared core task>
/protocol-implement <accepted shared core plan or task>
```

## Intended workflow

Planning:

```text
/dlms-plan AARQ decoder: parse AARQ, validate, unprotect user-information, parse InitiateRequest, build domain object
```

Implementation:

```text
/dlms-implement implement the accepted AARQ decoder plan
```

The orchestrator must then:

1. delegate implementation to `protocol-build`,
2. run or request tests,
3. invoke `protocol-review` on the actual `git diff`,
4. delegate fixes back to `protocol-build` if the review is `BLOCKED`,
5. repeat review until it returns `PASS` or `PASS_WITH_RISKS`.

## Confidentiality rule

Do not commit standards or copied standard text. Public code may reference standards by document, clause, table or figure only.
