# Notebook Rendering

Render GitHub-viewable `.ipynb` notebooks with Quarto, not `rmarkdown::render()`
plus a custom converter.

For this notebook, visible R code is written as fenced markdown code blocks and
the matching executable R chunks are hidden with `echo: false`. This is
intentional: GitHub's notebook renderer can display executable R cell input
through a CodeMirror path that collapses or mis-highlights code, while fenced R
markdown blocks render through the normal syntax-highlighting path. The notebook
therefore keeps visible code and executed outputs adjacent, but does not expose
non-empty executable code-cell sources in the `.ipynb`.

## Quarto Install

Quarto can be installed system-wide or locally. This development checkout has
been tested with Quarto `1.9.38`.

Example local install:

```sh
version=1.9.38
mkdir -p "$HOME/.local/src" "$HOME/.local"
cd "$HOME/.local/src"
curl -L "https://github.com/quarto-dev/quarto-cli/releases/download/v${version}/quarto-${version}-linux-amd64.tar.gz" \
  -o "quarto-${version}-linux-amd64.tar.gz"
tar -xzf "quarto-${version}-linux-amd64.tar.gz" -C "$HOME/.local"
```

## Render The Workflow Notebook

The single-dataset notebook expects the GSM5746259 10x triplet files in a
folder named `data` next to the Rmd during execution. For local rendering, use a
temporary symlink or copy:

```sh
ln -sfn ../../tests/data/GSE192391/GSM5746259_MGI0369_1_SLAB-145-0 doc/data

PATH="$HOME/.local/quarto-1.9.38/bin:$PATH" \
XDG_CACHE_HOME=/tmp/quarto-xdg \
DENO_DIR=/tmp/deno-cache \
quarto render doc/pagoda2.1-single-dataset.Rmd \
  --to ipynb \
  --output pagoda2.1-single-dataset.ipynb \
  --execute

mv pagoda2.1-single-dataset.ipynb doc/pagoda2.1-single-dataset.ipynb
rm -f doc/data
```

Quarto may write Python kernelspec metadata. Reset it to R after rendering:

```sh
python3 - <<'PY'
import nbformat as nbf

path = "doc/pagoda2.1-single-dataset.ipynb"
nb = nbf.read(path, as_version=4)
nb.metadata["kernelspec"] = {"display_name": "R", "language": "R", "name": "ir44"}
nb.metadata["language_info"] = {
    "name": "R",
    "codemirror_mode": "r",
    "mimetype": "text/x-r-source",
    "file_extension": ".r",
    "pygments_lexer": "r",
    "version": "4.4.1",
}
for cell in nb.cells:
    if cell.cell_type == "code":
        cell.metadata.setdefault("vscode", {})["languageId"] = "r"
        cell.metadata["language"] = "R"
nbf.write(nb, path)
PY
```

Validate:

```sh
python3 - <<'PY'
import nbformat

nb = nbformat.read("doc/pagoda2.1-single-dataset.ipynb", as_version=4)
nbformat.validate(nb)
PY
```

Check the GitHub-facing structure:

```sh
python3 - <<'PY'
import json

with open("doc/pagoda2.1-single-dataset.ipynb") as f:
    nb = json.load(f)

def source_text(cell):
    src = cell.get("source", "")
    return "".join(src) if isinstance(src, list) else src

print("metadata:", nb.get("metadata", {}))
print("visible fenced R blocks:", sum(
    cell.get("cell_type") == "markdown" and "``` r" in source_text(cell)
    for cell in nb["cells"]
))
print("non-empty executable sources:", sum(
    cell.get("cell_type") == "code" and bool(source_text(cell).strip())
    for cell in nb["cells"]
))
PY
```

The expected result is at least one visible fenced R block and zero non-empty
executable sources.
