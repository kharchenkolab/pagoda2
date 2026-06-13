# Pagoda2.1 multimodal — testing & phased-validation plan

*Companion to `multimodal_proposal2.md` (the design of record). It makes the §10 rollout **testable**:
each phase gets an OODA pass that ends in a concrete, gated test set. Written 2026-06-13. Nothing here is
implemented yet — this is the validation plan the implementation is built against.*

## Principles (mirroring lstar's corpus discipline)

1. **Two tiers, like lstar (`lstar/python/tests/CORPUS.md`).** *Real, local* fixtures (the lstar
   corpus, gitignored) are the honest grounding; a small *synthetic-but-faithful* CITE-seq fixture is
   committed for CI so GitHub runs without large downloads. Same contract lstar holds: the synthetic
   stand-in is structurally representative, verified by the local real runs.
2. **Reuse the lstar corpus via its own format.** The test backbone is
   **lstar reads the source → writes a `.lstar.zarr` → pagoda2.1 `Pagoda2$from(zarr)`**. This exercises
   the lstar-mediated import decision (§0.4.2) *and* reuses lstar's vetted multi-omic datasets — no new
   fixture sourcing.
3. **Round-trip is the invariant.** Every phase that touches storage asserts the fixed point
   `pagoda2 → lstar.zarr → pagoda2` (and, where it composes, `pagoda2 → lstar → mudata`). Loss must be
   *recorded*, never silent (lstar's `dropped`).
4. **Determinism/invariance is a hard gate, not a tolerance.** Every view model (CLR, TF-IDF) ships the
   §6.2 gate: `identical()` results across thread counts + a float64 dense reference. `identical`, not
   `all.equal`.
5. **Single-RNA is the regression backstop.** The existing 7 testthat files must pass **unmodified**
   after Phase 0 (delegation), proving the facet refactor is invisible to single-RNA users.

## Fixtures

| fixture | source | use | tier |
|---|---|---|---|
| `GSM5746259` (single RNA) | existing pagoda2 fixture | regression backstop; single-facet path | local |
| **CITE-seq** `minipbcite.h5mu` (411 cells, RNA+ADT) | `lstar/testdata/minipbcite.h5mu` | Phase 2a import/workflow/round-trip | local |
| **CITE-seq** raw matrices | `lstar/testdata/citeseq/{rna,adt}.mtx` | Phase 2a `addFacet` from matrices | local |
| **CITE-seq 10x** `5k_pbmc_protein_v3.h5` | `lstar/testdata/citeseq_10x/` | Phase 2a 10x-H5 import | local |
| **Multiome** `pbmc_multiome_3k.h5mu` (RNA+ATAC) | `lstar/testdata/mudata_examples/` | Phase 2b partial-overlap / TF-IDF / LSI | local |
| **Multiome 10x** `pbmc_granulocyte_sorted_3k.h5` | `lstar/testdata/multiome_10x/` | Phase 2b multiome barcode translation | local |
| **synthetic mini CITE-seq** (~200 cells, RNA+ADT) | committed fixture (mirror lstar `synth.citeseq_*`) | CI for all ADT phases | **CI** |

**Export recipe (real fixtures → zarr), run once into a gitignored `tests/data/lstar/`:**

```sh
# CITE-seq h5mu -> lstar zarr (Python mudata profile)
python -c "import lstar; lstar.write(lstar.read_mudata('minipbcite.h5mu'), 'minipbcite.lstar.zarr')"
# multiome h5mu -> lstar zarr
python -c "import lstar; lstar.write(lstar.read_mudata('pbmc_multiome_3k.h5mu'), 'pbmc_multiome_3k.lstar.zarr')"
```

pagoda2.1 then reads the zarr via the lstar R reader (`Pagoda2$from(path, facets=...)`). CI uses the
committed synthetic fixture, skipping the real ones with a note (`skip_if(!dir.exists(...))`).

---

## OODA passes per phase

### Phase 0 — structure (`test_facets.R`, regression) — DONE & GREEN (2026-06-13)

- **Observe.** Facet view, `facets`, `defaultFacet`, `p2$cells`, delegation to the default facet's storage.
- **Orient.** The only risk is that delegation isn't transparent — single-RNA behavior must be identical.
- **Decide.** Regression backstop + delegation unit tests.
- **Act / gate (met).** All existing testthat files pass **unmodified**; `test_facets.R` (29 assertions)
  covers `addFacet`/`listFacets`/`getFacet`, `identical(p2$rawCounts, p2$facets$RNA$rawCounts)`, write-through
  in both directions, RNA untouched by `addFacet`, and copy-constructor + serialization preservation.

> **Plan refinements made during Phase 0 (with rationale):**
> 1. **Facet is an ephemeral *view*, not active-binding-converted storage.** Converting the 6 public
>    matrix-bundle fields to active bindings would break the copy constructor (it iterates *public* members
>    via `ls(x)`; facet storage would be private/invisible) and the read-only `facets`/`cells` bindings would
>    error under its `assign()` loop. Instead the default facet's storage **remains the top-level fields**
>    and `Pagoda2Facet` delegates to them (others delegate to `misc$facetStore[[name]]`). Equivalent
>    downstream (a facet still yields a uniform `(rawCounts, matrixViews, …)` bundle), zero risk to existing
>    paths. The copy constructor only needed `c("counts","facets","cells")` added to its exclusion list.
> 2. **`apiVersion` bump deferred** from Phase 0 to whenever the *observable* API changes (the `facet=`
>    threading), since Phase 0 is purely additive — no existing call changes. Keeps the gate literally
>    "suite passes unmodified."
> 3. **No varinfo/odgenes *storage* migration needed.** They stay in `misc`, but the primary facet view
>    already exposes them as `facet$varinfo`/`facet$odgenes` by delegation — so Phase 1 reads/writes them
>    uniformly through the facet without moving storage or rewriting `misc[["varinfo"]]` call sites.

### Phase 1 — facet API & keying

**1a — resolution & keying primitives (`test_facet_resolution.R`) — DONE & GREEN (2026-06-13).**
`resolveFacet`, `.pagoda2_parse_qualified` (split on first `:`), `.pagoda2_reduction_key` (default-facet bare
/ non-default qualified), `.pagoda2_validate_joint_name` (no-shadow + no-`:`), all unit-tested (17
assertions). This is the deterministic name-resolution layer §4.5.1 flagged as the subtle correctness surface.

**1b — facet= threading + registry re-keying (NEXT increment, not yet done).** Thread `facet=` into the
*actual* pipeline — `runVariance`/`runReduction`/`runGraph`/`findMarkers` operate on
`resolveFacet(facet)$rawCounts`/`$matrixViews` instead of top-level, and write results under the
facet-keyed (`markerResults[[facet]][[grouping]]`) / name-keyed (`.pagoda2_reduction_key`) registries; bump
`apiVersion` (the first observable API change) + update `test_api_version.R`. This is the larger, higher-risk
edit (it touches `.pagoda2_r6_run_*` and `markers.R`); it is the next OODA loop.
  - Gate: bare `PCA` → default facet; `ADT:PCA` verbatim; markers `[["RNA"]]` ≠ `[["ADT"]]` no collision;
    cross-facet feature ambiguity (`CD3` gene vs protein) errors and demands `facet=`/qualified name;
    existing behavioral tests still green; api-version assertion updated to the bumped value.

### Phase 1b — accessor contract (`test_accessors.R`) + consumer-port readiness

- **Observe.** The §8.5.2 accessor set, each facet-aware; this is the **contract freeze**.
- **Orient.** Downstream (conos/cacoa) binds here; the return *shapes* are the stable contract.
- **Decide.** Pin every accessor's signature, default-facet behavior, and return shape/orientation.
- **Act / gate.**
  - Each of `getRawCounts`/`getExpression`/`getReduction`/`getEmbedding`/`getClustering`/`getOdGenes`/
    `getVarInfo`/`getDepth`/`listFacets`/`getFacet` — facet default = RNA, qualified works, orientation
    honored; `getVarInfo()` returns the `qv`/`v`/`gsf` columns conos depends on.
  - A **fixture-level contract test** asserting the shapes conos's `access_wrappers.R` expects (so the
    later conos port has a target). The conos port itself is tracked separately (do not modify conos now).

> **Implementation status (2026-06-13).** Landed and **green** (`R CMD INSTALL .` → `test_dir`):
> Phase 0 (Facet + delegation), Phase 1a (resolution/keying primitives: `resolveFacet`,
> `.pagoda2_parse_qualified`, `.pagoda2_reduction_key`, `.pagoda2_validate_joint_name`), Phase 1b
> matrix layer (facet-aware `getRawCounts`/`getMatrixView`/`materializeView`/`getExpressionBlock`/
> `viewColMeanVar`/`viewColSumByFac`), **CLR** view model (`test_facet_clr.R`) and **TF-IDF** view model
> (`test_facet_tfidf.R`) — both with the §6.2 reference + thread-invariance gates (materialized in R for now;
> the streaming C++ `viewKernelValue` clr/tfidf branch is a perf follow-up validated against this R path).
> **Not yet done:** facet= threading into `runVariance`/`runReduction`/`findMarkers` + facet/name-keyed
> result registries + `apiVersion` bump + `runPCA` removal (the larger pipeline retrofit); WNN
> (`runGraph(method="wnn")`); LSI drop-first; partial-overlap membership/`index`; lstar import
> (skip-gated — lstar R absent here); Phase 3 joint-shape round-trip. These are the next OODA loops.

### Phase 2a — ADT / CITE-seq (the first multimodal release; `test_citeseq.R`)

- **Observe.** CLR view model; lstar-mediated CITE-seq import; union axis + complete-cases + `filterData`;
  WNN joint (the shipped integration method).
- **Orient.** Two hard correctness points: the CLR kernel's thread-invariance, and lossless import/round-trip.
- **Decide.** The §6.2 numerical gate + the import/round-trip backbone + a full workflow smoke.
- **Act / gate.**
  - **CLR invariance gate (§6.2):** on the ADT facet, `identical(viewColMeanVar(n.cores=1), …(n.cores=8))`
    for mean & var; `max(abs(view - float64 dense reference)) < 1e-10/1e-8`.
  - **Import:** `Pagoda2$from("minipbcite.lstar.zarr")` → facets `RNA`(27g)+`ADT`(29p), 411 cells, correct
    feature axes; `Pagoda2$from(..., facets="RNA")` subsets.
  - **Round-trip:** `pagoda2 → export(zarr) → Pagoda2$from` is a fixed point on counts, feature axes, and
    cellMeta; recipe params survive in provenance, CLR divisor as a `recipe_scalar` field (lstar S1).
  - **Workflow:** `runReduction(facet="ADT")`, `runGraph(method="wnn")` →
    `reductions[["WNN"]]`+`cellMeta$wnn_weight_{RNA,ADT}`+`graphs[["WNN"]]`; `runClustering`;
    `findMarkers(facet="ADT")` non-empty and distinct from RNA markers.
  - CI runs all of the above on the **synthetic** CITE-seq fixture.

### Phase 2b — ATAC / multiome (`test_multiome.R`)

- **Observe.** TF-IDF view + LSI reduction (`drop.first`); peak ranges in `featureMeta`; 10x-multiome
  barcode translation → membership mask → partial-coverage `index`.
- **Orient.** New hard points: TF-IDF invariance, the LSI drop-first numeric, and **partial-overlap
  round-trip** (the membership-mask ↔ `index` mapping, lstar S3, now over a derived/observed axis).
- **Decide.** Mirror Phase 2a's gates for TF-IDF, plus a dedicated partial-overlap round-trip.
- **Act / gate.**
  - **TF-IDF invariance gate (§6.2)** on the ATAC facet (per-column IDF as `c[]`).
  - **LSI:** `drop.first=TRUE` drops the depth-correlated component (assert correlation of comp-1 with
    depth, and that it's absent from `reductions[["ATAC:LSI"]]`).
  - **Partial overlap:** import `pbmc_multiome_3k` (GEX/ATAC barcode sets differ); assert the union
    `p2$cells`, per-facet masks, and a lossless `pagoda2 → zarr → pagoda2` round-trip where ATAC's measure
    carries a typed `index` (not zero-filled); `requireFacets(c("RNA","ATAC"))` yields the intersection.
  - Peak ranges (seqnames/start/end) round-trip in `facets$ATAC$featureMeta`; fragments held as a
    reference, not inlined.

### Phase 3 — further joint methods (`test_joint_shape.R`)

- **Observe.** MOFA+/totalVI on the reserved named-product shape (storage now, algorithms as demand warrants).
- **Orient.** The shape must round-trip even before an algorithm ships.
- **Decide.** Storage-shape round-trip only.
- **Act / gate.** Construct a MOFA-shaped product (scores `reductions[["MOFA"]]` + per-facet
  `facets$<F>$loadings[["MOFA"]]` + `provenance$input_axes=c("genes","proteins")`); round-trip through
  lstar; assert loadings land on each facet's feature axis and `input_axes` is preserved (lstar S5).

### Collection scale (deferred, with conos)

The storage-backed collection path (§8.6) is validated *with* conos against a real on-disk Conos store +
a benchmark — `collection_pseudobulk` correctness (streamed == materialized) is already proven on the
lstar side (`test_collection_reduce.py`); the pagoda2-facing test arrives when the conos port does.

---

## CI vs local

- **CI (GitHub):** the existing suite + synthetic CITE-seq fixture; all view-model invariance gates (they
  need no real data); resolution/keying/accessor tests. No real-corpus downloads.
- **Local:** the full real lstar corpus (CITE-seq + multiome) via the zarr export recipe; the perf
  benchmark stays separate (`tests/perf/`), now with an ADT/CLR variant.

## Vignette plan

A CITE-seq multimodal vignette mirrors `doc/pagoda2.1-single-dataset.Rmd` (rendered via the handoff's
Quarto `.ipynb` recipe). It is built **once Phase 2a lands** (it must actually run). Outline:

1. Load a small CITE-seq dataset (`Pagoda2$from(zarr)` or `addFacet` from matrices); `listFacets()`.
2. Per-facet QC (`pct_mito_RNA`, `n_molecules_ADT`); `filterData`; `requireFacets`.
3. `runReduction()` (RNA) and `runReduction(facet="ADT")`.
4. `runGraph(method="wnn")` → joint graph + modality weights; `runClustering()`; `runEmbedding()`.
5. `findMarkers()` (RNA genes) and `findMarkers(facet="ADT")` (proteins); dot/heatmap plots.
6. `plotEmbedding(color="MS4A1")` vs `plotEmbedding(color="ADT:CD3")`.
7. `export(zarr)` and a note on lstar round-trip.

A multiome (RNA+ATAC) vignette follows in Phase 2b.
