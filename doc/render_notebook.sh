#!/usr/bin/env bash
# Render a pagoda2.1 tutorial .Rmd to .ipynb + .html AND apply the GitHub language_info fixup.
#
# ALWAYS use this instead of a bare `quarto render`. Quarto writes a Python kernelspec and R
# `language_info`; GitHub's notebook viewer then renders R code-cell input inside a CodeMirror
# container that COLLAPSES whitespace/indentation. The fix (see notebook_rendering.md) is to reset the
# kernelspec to R/ir44 and set language_info to plain text so GitHub wraps code in a <pre> block.
# This script bundles render + fixup so the fixup is never forgotten.
#
# Usage:  doc/render_notebook.sh pagoda2.1-citeseq        (name without extension; symlink data/ first)
set -euo pipefail
cd "$(dirname "$0")"
nb="${1:?usage: render_notebook.sh <name-without-ext>}"
PATH="$HOME/.local/quarto-1.9.38/bin:$PATH" XDG_CACHE_HOME=/tmp/quarto-xdg DENO_DIR=/tmp/deno-cache \
  quarto render "${nb}.Rmd" --execute
python3 - "${nb}.ipynb" <<'PY'
import json, sys
p = sys.argv[1]
d = json.load(open(p))
d.setdefault("metadata", {})["kernelspec"] = {"display_name": "R", "language": "R", "name": "ir44"}
d["metadata"]["language_info"] = {
    "name": "text", "codemirror_mode": "text", "mimetype": "text/plain",
    "file_extension": ".r", "pygments_lexer": "text", "version": "4.4.1",
}
json.dump(d, open(p, "w"), indent=1); open(p, "a").write("\n")
print("language_info fixup applied ->", p)
PY
