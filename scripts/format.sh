#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

mode="${1:---check}"
if [[ $# -gt 1 || ( "$mode" != "--check" && "$mode" != "--write" ) ]]; then
    echo "Usage: bash scripts/format.sh [--check|--write]" >&2
    exit 2
fi

mapfile -t csharp_files < <(
    find src -type d \( -name bin -o -name obj -o -name node_modules -o -name dist \) -prune \
        -o -type f -name '*.cs' -print
)

if [[ "$mode" == "--check" ]]; then
    dotnet fantomas --check src
    if (( ${#csharp_files[@]} )); then
        dotnet format whitespace . --folder --include "${csharp_files[@]}" --verify-no-changes
    fi
    pnpm --dir src/Apps/ProtocolInspector/Client format:check
else
    dotnet fantomas src
    if (( ${#csharp_files[@]} )); then
        dotnet format whitespace . --folder --include "${csharp_files[@]}"
    fi
    pnpm --dir src/Apps/ProtocolInspector/Client format
fi
