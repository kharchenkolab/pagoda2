# Notebook Rendering

Render GitHub-viewable `.ipynb` notebooks with Quarto, not `rmarkdown::render()`
plus a custom converter. Quarto emits real notebook code cells for R chunks.

GitHub's notebook renderer is sensitive to the notebook `language_info`
metadata. With R syntax metadata, GitHub can render the code input as raw text
inside a CodeMirror container, which collapses the formatting. The working
reference pattern is to keep an R kernelspec but set `language_info` to plain
text. GitHub then wraps code-cell input in a `<pre>` block, preserving layout.

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

Quarto may write Python kernelspec metadata. Reset the kernelspec to R, and set
the language metadata to plain text for GitHub rendering:

```sh
python3 - <<'PY'
import nbformat as nbf

path = "doc/pagoda2.1-single-dataset.ipynb"
nb = nbf.read(path, as_version=4)
nb.metadata["kernelspec"] = {"display_name": "R", "language": "R", "name": "ir44"}
nb.metadata["language_info"] = {
    "name": "text",
    "codemirror_mode": "text",
    "mimetype": "text/plain",
    "file_extension": ".r",
    "pygments_lexer": "text",
    "version": "4.4.1",
}
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

Check the GitHub-facing notebook structure:

```sh
python3 - <<'PY'
import json

with open("doc/pagoda2.1-single-dataset.ipynb") as f:
    nb = json.load(f)

def source_text(cell):
    src = cell.get("source", "")
    return "".join(src) if isinstance(src, list) else src

print("kernelspec:", nb["metadata"].get("kernelspec"))
print("language_info:", nb["metadata"].get("language_info"))
print("non-empty executable sources:", sum(
    cell.get("cell_type") == "code" and bool(source_text(cell).strip())
    for cell in nb["cells"]
))
PY
```

After pushing, verify GitHub's rendered HTML around the first code block. The
code should appear inside an inner `highlight hl-text` `<pre>` block, not as raw
text directly inside `cm-editor`.
