# Notebook Rendering

Render GitHub-viewable `.ipynb` notebooks with Quarto, not `rmarkdown::render()`
plus a custom converter. Quarto emits real notebook code cells for R chunks,
which avoids GitHub/Jupyter treating `$` and `_` in R code as markdown or math.

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

Quarto may write Python kernelspec metadata. Reset it to R after rendering. The committed notebook should advertise the public R kernelspec (`name: ir`), even if the local execution kernel is named differently:

```sh
python3 - <<'PY'
import nbformat as nbf

path = "doc/pagoda2.1-single-dataset.ipynb"
nb = nbf.read(path, as_version=4)
nb.metadata["kernelspec"] = {"display_name": "R", "language": "R", "name": "ir"}
nb.metadata["language_info"] = {
    "name": "R",
    "codemirror_mode": "r",
    "mimetype": "text/x-r-source",
    "file_extension": ".r",
    "pygments_lexer": "r",
    "version": "4.4.3",
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
