# Scoping: CCA / sparse-CCA / SNF / LIGER / Schema as pagoda2.1 facet-integration `method=`s

Companion to `multimodal_proposal2.md` §5/§5.1. The design question is settled: facet integration
is a *step with pluggable methods* — `runReduction(facets=, method=)` for reduction-level (joint
latent) integration and `runGraph(facets=, method=)` for graph-level (fused neighborhood) integration
— and every method produces the **same named-product byproducts** (§4.5/§5): a name-keyed
`cells × k` reduction (joint scores), optional per-facet `features × k` loadings, an integrated graph,
and provenance `{facets, method, params, seed}`. **WNN ships in 2.1** (graph-level, implemented). This
doc scopes five *additional* methods for the deferred list, so we know the order and the cost before we
commit to any. No implementation is requested here — this is the menu.

The recurring storage question for every method: does it emit **per-facet loadings**? If yes, the
factors are *one lstar coordinate axis* shared by a `cells × k` embedding and per-facet `features × k`
loadings (lstar S5 `input_axes` provenance, §5). If no (graph-only, or scores-only), it's a bare
named product — cheaper to store, but not round-trippable into per-facet feature interpretation.

## Dependency reality (this machine, 2026-06)

| package | needed by | installed | note |
|---|---|---|---|
| `irlba` | CCA (dense-SVD path), concat-PCA | **yes** | already a transitive dep; truncated SVD of the cross-product |
| `PMA` | sparse-CCA (`PMA::CCA`) | **no** | conos borrows it behind a `PMA=TRUE` flag; CRAN, pure-R, light |
| `SNFtool` | SNF | **no** | CRAN, pure-R, small; O(n²) dense affinity |
| `rliger` | LIGER / iNMF | **no** | CRAN/Bioc, heavier; brings its own object model |
| `reticulate` | Schema (Python `schema_learn`) | **yes** | but the Python pkg + a venv are **not** present |
| `RcppML` / `NMF` | a native iNMF alternative to rliger | **no** | only relevant if we want LIGER without `rliger` |

conos already carries CCA machinery worth borrowing rather than reinventing: `quickCCA`
(`conos/R/conos.R:327`) does `irlba::irlba(sm[[1]] %*% t(sm[[2]]), ncomps)` for the plain path and
`PMA::CCA(...)` behind `PMA=TRUE` — but note conos's CCA is **horizontal** (genes×genes cross-covariance
between two *samples*), whereas our facet case is **vertical** (same cells, cross-covariance between two
*modalities*). The algebra transfers; the orientation flips (cross-product over the shared cell axis,
not the shared gene axis). The loading-normalization and component-reweighting tricks
(`conos.R:349-366`) are directly reusable.

---

## CCA (canonical correlation analysis) — reduction-level

- **Entry point.** `runReduction(facets = c("RNA","ADT"), method = "cca")` → `reductions[["CCA"]]`
  (joint `cells × k`), per-facet `loadings[["CCA"]]` (`genes × k`, `proteins × k`).
- **Algorithm (vertical, 2 facets).** Take each facet's centered, variance-scaled reduction-input block
  on the **shared (common) cells** — `X₁ (n × p₁)`, `X₂ (n × p₂)`. The cross-covariance is `Cᵀ = X₁ᵀ X₂`
  (`p₁ × p₂`); truncated SVD `Cᵀ = U D Vᵀ` gives canonical loadings `U`, `V`; canonical variates
  (cell scores) are `X₁U` and `X₂V`. Joint scores = the averaged/concatenated variates, component-weighted
  by `√d` (conos's `cw` trick). This is base R + one `irlba::irlba` call — **no new hard dep.**
- **Inputs.** Two reduction-input blocks (the same scaled od-gene / scaled-feature matrices `runReduction`
  already builds per facet). Restricted to `common` cells by default (the §0.4.3 intersection rule).
- **Output shape.** Joint scores **and** per-facet loadings → full lstar `input_axes = c("genes","proteins")`
  round-trip. The cleanest "factor model" shape of the five.
- **Effort.** **Lowest of the five.** ~a day: a new `.pagoda2_r6_run_cca` next to `.pagoda2_r6_run_joint_reduction`,
  reusing the per-facet feature-axis plumbing already written for concat-PCA. No new dependency, no Python.
- **Scale.** SVD of a `p₁ × p₂` matrix — tiny (modalities have ≤ few thousand features after od-gene
  selection). The `X₁ᵀX₂` product is `O(n·p₁·p₂)` but streams over cells; disk-backed via the same lstar
  gather path as concat-PCA. Scales to large `n`.
- **Limitation.** Two-block by default. ≥3 facets needs a generalized-CCA variant (e.g. MAXVAR / sum of
  pairwise cross-covariances, or sequential deflation) — scope creep; ship 2-block first.

## sparse-CCA (penalized CCA, PMA) — reduction-level

- **Entry point.** Same as CCA but `method = "cca", sparse = TRUE` (or `method = "scca"`); a `penalty=`
  arg surfaces PMA's L1 sparsity. → `reductions[["CCA"]]` + **sparse** per-facet loadings.
- **Algorithm.** `PMA::CCA(X₁, X₂, K=k, penaltyx=, penaltyz=, standardize=FALSE)` — same cross-covariance
  SVD but with L1 penalties yielding sparse canonical loadings (most features get exactly 0 weight). The
  interpretability win: each factor names a small set of genes × a small set of proteins.
- **Inputs.** Identical to CCA.
- **Output shape.** Identical to CCA, but loadings are sparse — *better* for lstar (sparse `features × k`
  field) and for downstream marker interpretation.
- **Effort.** **Low, gated on a dep.** The code is a thin branch on top of the CCA path: if `sparse=TRUE`
  call `PMA::CCA` (with a `requireNamespace` guard + install hint, exactly as conos does at
  `conos.R:339`). PMA is light and pure-R. ~half a day *on top of CCA*.
- **Scale.** PMA's permutation-based penalty selection (`PMA::CCA.permute`) is the expensive part; the
  fit itself is cheap. Same `n`-scaling as CCA.
- **Recommendation.** Bundle with CCA as one method (`sparse=` flag) rather than a separate `method=`.
  Make PMA a *Suggests*, not a hard dep — borrow conos's exact guard pattern.

## SNF (similarity network fusion) — graph-level

- **Entry point.** `runGraph(facets = c("RNA","ADT"), method = "snf")` → `graphs[["SNF"]]` (fused
  affinity graph). Sits beside WNN as a graph-fusion alternative.
- **Algorithm.** Per facet: build a full `n × n` affinity matrix from the facet's reduction (Gaussian
  kernel on pairwise distances). Iteratively cross-diffuse each facet's affinity through the others'
  until they converge to one fused network. `SNFtool::SNF(list(W₁, W₂), K, t)`.
- **Inputs.** Per-facet distance/affinity matrices over the **common** cells.
- **Output shape.** A fused **graph only** — *no* joint reduction, *no* loadings, *no* per-cell weights.
  Cheapest named product (just `graphs[["SNF"]]`), but also the least interpretable: you get a
  neighborhood, not a latent space or feature attribution.
- **Effort.** **Low-medium, gated on dep + a scale caveat.** `SNFtool` is small/pure-R; the wrapper is a
  per-facet affinity build (reuse `.pagoda2_facet_knn_dist`) + one `SNF()` call + adapt the dense fused
  matrix into the same igraph/sparse-SNN shape WNN already emits. ~1 day.
- **Scale.** **The blocker.** SNF is **dense `O(n²)`** in both memory and time — fine to ~10–20k cells,
  infeasible at 10⁵–10⁶ without a sparse/anchored reimplementation. This contradicts the
  "main container, disk-backed, large collections" goal (handoff/§8.6). If we ship SNF, gate it behind an
  explicit cell-count check with a clear error, or implement a kNN-sparsified variant ourselves (more
  effort, drops the `SNFtool` dep).
- **Recommendation.** **Low priority.** WNN already occupies the per-cell-weighted graph-fusion niche and
  scales; SNF adds per-modality (not per-cell) weighting and an O(n²) cost. Worth it only if a user
  specifically wants the SNF fusion semantics on a small dataset.

## LIGER / iNMF (integrative NMF) — reduction-level

- **Entry point.** `runReduction(facets = c("RNA","ADT"), method = "liger")` (or `"inmf"`) →
  `reductions[["LIGER"]]` (shared `cells × k` factor loadings `H`), per-facet `loadings[["LIGER"]]`
  (the `W` shared + `V` modality-specific metagene loadings).
- **Algorithm.** Integrative NMF: factor each facet's **non-negative** normalized matrix as
  `Eₘ ≈ (W + Vₘ) Hₘ`, sharing `W` (common factors) across facets while `Vₘ` captures dataset/modality-
  specific structure. Then quantile-normalize the `H` factors across facets. `rliger`'s
  `optimizeALS` + `quantile_norm`.
- **Inputs.** **Raw / non-negatively-normalized feature matrices, not the PCA reductions** — LIGER wants
  counts-like inputs it can NMF-factor, with its own gene selection + scaling. This is a different input
  contract than CCA/concat (which take reductions): a facet-level `getMatrixView`/`getRawCounts` feed,
  not `reductions[[...]]`. Worth noting because it breaks the "all reduction-level methods consume
  per-facet reductions" symmetry.
- **Output shape.** Joint scores **and** interpretable per-facet metagene loadings (shared `W` +
  specific `V`) → rich lstar `input_axes` round-trip; arguably the most *interpretable* factor model of
  the five. Also the only one here that natively handles **mosaic/diagonal** (partially-shared features),
  though that's out of the vertical-facet scope.
- **Effort.** **Medium-high.** `rliger` is the heaviest dep (its own S4 object model, gene-selection,
  ALS), and the input contract differs (raw matrices + LIGER's own normalization, so we either feed its
  preprocessing or bypass it and call `optimizeALS` directly on our normalized views). ~3–5 days incl.
  reconciling normalization. Alternative: a native iNMF via `RcppML`/`NMF` to avoid the `rliger` object
  model — more code, fewer deps.
- **Scale.** ALS is iterative but linear in `nnz`; scales better than SNF. `rliger` has on-disk (HDF5)
  paths of its own — but those are *its* storage, not lstar, so disk-backed LIGER would mean either
  round-tripping through rliger's HDF5 or feeding it from our lstar gather (the latter keeps one storage
  story).
- **Recommendation.** **Medium priority** — high interpretability payoff, but the dep weight + divergent
  input contract make it the second-most-expensive after Schema. Good Phase-3 candidate once CCA proves
  the reduction-level integration plumbing.

## Schema — reduction-level (explicit per-modality weights)

- **Entry point.** `runReduction(facets = c("RNA","ADT"), method = "schema", weights = c(RNA=1, ADT=0.5))`
  → `reductions[["Schema"]]`. The **only** method with an explicit user-set per-modality weight knob.
- **Algorithm.** Metric learning: find a transformation of a *primary* modality that maximizes agreement
  with *secondary* modalities under a hard constraint that the primary's structure is largely preserved
  (a quadratic program). Each secondary modality gets a tunable weight. Python `schema_learn`.
- **Inputs.** Per-facet feature/reduction blocks + a designated primary facet + per-modality weights.
- **Output shape.** A transformed `cells × k` reduction (joint scores). Feature attribution exists (the
  learned metric) but isn't a clean per-facet `features × k` loading the way CCA/LIGER give — so lstar
  round-trip is scores-only unless we derive loadings.
- **Effort.** **Highest of the five.** Python-only via `reticulate`: requires a managed venv,
  `schema_learn` install, numpy↔R matrix marshaling, and version pinning — the same backend burden as
  totalVI/MOFA+ (which is exactly why §5.1 buckets those as "optional-backend, later"). `reticulate` is
  present but the Python package and environment are **not**. ~1 week incl. env tooling, most of which is
  reusable for *any* future Python `method=` (MOFA+/totalVI).
- **Scale.** The QP is the cost; Schema is generally fine to moderate `n` but the marshaling + Python
  boundary dominate operationally.
- **Recommendation.** **Lowest priority of the five** as a standalone, **but** it's the natural pilot for
  the Python-backend `method=` machinery. If/when we build the reticulate backend for MOFA+/totalVI,
  Schema comes nearly for free and brings the unique explicit-weight knob. Don't build the Python bridge
  *for* Schema alone.

---

## Summary & recommended order

| method | level | dep | loadings? | scale | effort | priority |
|---|---|---|---|---|---|---|
| **CCA** | reduction | irlba (have) | yes (dense) | large `n` | **lowest** | **✅ implemented (2.1)** |
| **sparse-CCA** | reduction | PMA (Suggests) | yes (sparse) | large `n` | low (+CCA) | **✅ implemented (`sparse=`)** |
| **LIGER/iNMF** | reduction | rliger (heavy) | yes (W+V, rich) | medium | med-high | 3 — Phase 3 |
| **SNF** | graph | SNFtool (light) | no (graph only) | **O(n²) ≤~20k** | low-med | 4 — niche/small-n |
| **Schema** | reduction | reticulate+py | scores only | moderate | **highest** | 5 — with py-backend |

**Recommended sequencing:**

1. **CCA + sparse-CCA as one `method="cca"` (with `sparse=`/`penalty=`).** ✅ **Done (2.1).** Implemented in
   `R/workflow.R::.pagoda2_r6_run_cca`, dispatched from `runReduction(facets=, method="cca"|"scca")` (or
   `method="cca", sparse=TRUE`). Vertical, two-block: each facet's scaled feature block restricted to the
   shared cells, centered cross-covariance `t(X1)X2` SVD (dense via `irlba` on a kept-sparse cross-product;
   sparse via `PMA::CCA`, PMA a *Suggests* behind a `requireNamespace` guard). Emits `reductions[["CCA"]]`
   (averaged canonical variates), per-facet feature loadings `facet$loadings[["CCA"]]` (`genes×k`,
   `proteins×k`), and provenance `{facets, input_axes, method=joint:cca/scca, cancor}`. Tested in
   `tests/testthat/test_facet_cca.R` (recovers a shared latent, loadings on the right axes, provenance,
   no-shadow + two-block guards, correctness vs a direct base-R `svd(crossprod)`, downstream
   `runGraph`+`runClustering`; sparse path skip-gated on PMA). Lowest effort, no new hard dep, reuses the
   concat-PCA feature-axis plumbing, scales, and closes the loop with conos (shared CCA semantics,
   transposed to the vertical orientation).
2. **LIGER** when we want interpretable shared/specific metagene factors — accept the `rliger` dep weight
   and the raw-matrix input contract, or build native iNMF on `RcppML` to avoid the object model.
3. **SNF** only if a user wants its fusion semantics on small data; gate on a cell-count guard. Otherwise
   WNN already owns scalable graph fusion.
4. **Schema last**, folded into a general Python-backend `method=` effort alongside MOFA+/totalVI — its
   value is the explicit per-modality weight knob, not enough to justify the venv burden on its own.

**No structural changes required.** All five fit the existing `runReduction(facets=, method=)` /
`runGraph(facets=, method=)` entry points and the §4.5 named-product storage. The only contract wrinkle
worth flagging: LIGER (and the generative/Python methods) consume **raw/normalized feature matrices**
rather than per-facet **reductions** — a per-method input declaration the dispatcher should carry, so
`runReduction` knows whether to hand a method the facet's reduction or its view/raw counts.

**Status:** CCA + sparse-CCA (item 1) are **implemented and tested** in 2.1. LIGER, SNF, and Schema
remain scoped-but-deferred per the order above; each lands behind the same `runReduction`/`runGraph`
entry points with no schema change when picked up.
