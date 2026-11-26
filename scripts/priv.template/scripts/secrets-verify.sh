#!/usr/bin/env nix-shell
#! nix-shell -i bash -p age git

# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Guard against accidental commits of plaintext secrets after the
# git-crypt -> age migration: fails if any **/secrets-plain/ file is
# staged or tracked, and reminds to re-run scripts/secrets-encrypt.sh
# when a plaintext has no committed .age counterpart yet.
#
# Install once (idempotent):
#   scripts/secrets-verify.sh --install-hooks
# Then it runs on every commit and stores a reminder via
# git notes on each commit that touched secrets-plain/.

set -euo pipefail

repo="$(git rev-parse --show-toplevel)"

install_hooks() {
    hook="$repo/.git/hooks/pre-commit"
    mkdir -p "$(dirname "$hook")"
    if [[ -x $hook ]] && ! grep -q secrets-verify "$hook"; then
        echo "NOTE: existing pre-commit hook at $hook" >&2
        echo "      append this line to it:" >&2
        echo "      \"$repo/scripts/secrets-verify.sh\" --check-index" >&2
    else
        cat >"$hook" <<EOF
#!/usr/bin/env bash
"$repo/scripts/secrets-verify.sh" --check-index
EOF
        chmod +x "$hook"
        echo "installed pre-commit hook at $hook"
    fi
}

check_index() {
    cd "$repo"
    bad=0
    while IFS= read -r -d '' f; do
        echo "ERROR: plaintext secret is tracked by git: $f" >&2
        bad=1
    done < <(git ls-files -z | grep -zE '(^|/)secrets-plain/' | grep -zvE '\.nix$')
    if ((bad)); then
        echo "untrack it with: git rm --cached -- <file>" >&2
        exit 1
    fi
    if git diff --cached --name-only -z | grep -zE '(^|/)secrets-plain/' | grep -zvE '\.nix$'; then
        echo "ERROR: plaintext secrets staged for commit — unstage them:" >&2
        exit 1
    fi
}

check_pairing() {
    cd "$repo"
    bad=0
    while IFS= read -r -d '' f; do
        rel="${f#./}"
        # nix files under secrets-plain/ are not secrets and stay
        # unencrypted — no .age counterpart expected
        [[ $rel == *.nix ]] && continue
        if [[ $rel == secrets-plain/* ]]; then
            age="secrets/${rel#secrets-plain/}.age"
        else
            age="${rel/secrets-plain/secrets}.age"
        fi
        if [[ ! -e $age ]]; then
            echo "WARN: no committed ciphertext for $rel (expected $age)" >&2
            echo "      run scripts/secrets-encrypt.sh" >&2
            bad=1
        fi
    done < <(find . -path ./.git -prune -o -type f -path '*/secrets-plain/*' -print0)
    if ((bad)); then
        exit 1
    fi
    echo "OK: every secrets-plain/ file has a committed .age counterpart"
}

case "${1:-}" in
--install-hooks)
    install_hooks
    ;;
--check-index)
    check_index
    check_pairing
    ;;
*)
    check_pairing
    ;;
esac
