#!/usr/bin/env bash
set -euo pipefail

changed_files="${1:-}"

if [[ -z "$changed_files" ]]; then
  changed_files="$(git diff --cached --name-only)"
fi

if echo "$changed_files" | grep -Eiq '\.(pdf|docx|xlsx)$'; then
  echo "Refusing commit: binary standard/document file detected." >&2
  echo "$changed_files" | grep -Ei '\.(pdf|docx|xlsx)$' >&2 || true
  exit 1
fi

if echo "$changed_files" | grep -Eiq '(^|/)(green[-_ ]?book|blue[-_ ]?book|yellow[-_ ]?book|dlms[-_ ]?ua[-_ ]?1000|en[-_ ]?13757|iec[-_ ]?62056)'; then
  echo "Refusing commit: possible confidential standard material detected." >&2
  echo "$changed_files" | grep -Ei '(^|/)(green[-_ ]?book|blue[-_ ]?book|yellow[-_ ]?book|dlms[-_ ]?ua[-_ ]?1000|en[-_ ]?13757|iec[-_ ]?62056)' >&2 || true
  exit 1
fi

echo "No confidential standard files detected in staged file list."
