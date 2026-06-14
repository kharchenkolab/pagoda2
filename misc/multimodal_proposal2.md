# Pagoda2.1 Multimodal Proposal 2

*An opinionated, implementable data model for multiple molecular facets, aligned to the lstar
axes+fields interchange.*

This supersedes `multimodal_proposal1.md` as the design of record. It is grounded in (a) the *actual*
pagoda2.1 code on `devel` — `R/Pagoda2-class.R`, `R/matrix-storage.R`, `R/markers.R`, `R/workflow.R`,
`src/misc2.cpp`; (b) the lstar model (`~/p21/lstar/docs/model.md`) and its format profiles
(`profile_pagoda2.R`, `profile_seurat.R`, `mudata.py`, `SUPPORT.md`); and (c) a survey of how every
major multimodal framework formulates the problem.

> **Revision note (keystone flipped).** An earlier draft of this document recommended *generalizing the
> existing `type` token into a facet-aware matrix-context token* rather than introducing a `facet`
> namespace. That recommendation is **withdrawn.** The directive driving this revision:
>
> > *"facet should be distinguished from type. In fact, type is something we've carried since the
> > inception of pagoda2 and it has not been useful (e.g. it's very rare that someone deviates from
> > PCA)."*
>
> So **`facet` is now a first-class, explicit, orthogonal concept** with its own argument and its own
> primary registry key, and **`type` is demoted to a near-vestigial, rarely-used reduction selector**
> (or deprecated outright). Proposal1's instinct — keep `facets` separate from any reduction token — was
> actually closer to right than the prior draft; it merely under-specified the details. This revision
> keeps proposal1's separation and supplies the load-bearing details against the real code.

---

## 0. TL;DR recommendation

### 0.1 The pivot, in one line

**Make `facet` a real, orthogonal dimension** (its own `facet=` argument, its own primary registry key,
RNA as the default facet) and **demote `type`** — the reduction-space token pagoda2 has carried since
inception — to a rarely-used optional qualifier, because in pagoda2's whole history users essentially
never deviate from PCA. The meaningful axis of variation in real multimodal use is the **facet**
(RNA vs ADT vs ATAC), *not* the reduction **type**. Let each facet own its **default reduction** (RNA→PCA,
ATAC→LSI) so the reduction is *implied by the facet*, not surfaced as a user-facing knob.

### 0.2 The recommendations

1. **A facet is a first-class *count backend + view recipe*, not a loose bag of slots.** A facet is
   exactly the existing `rawCounts` + `matrixViews` + `featureMeta` + `depth`/`batch` + `varinfo`/`odgenes`
   bundle, factored into a reusable unit. The current single-RNA object *is* `facets[["RNA"]]`, and the
   top-level `rawCounts`/`matrixViews`/`geneMeta`/`depth` become **delegating active bindings** to the
   default facet. These bindings exist as a *clean single-facet ergonomic surface*, **not** as a
   back-compat obligation: see §0.3 and §10 — we are taking a clean break from pagoda2 ≤1.x, so they are
   kept because they read well, not because an old object or an old call site depends on them. *(proposal1
   was right about the registry; we make it literally true via delegation.)*

2. **`facet` is its own explicit argument and primary registry key — NOT sugar over `type`.** Introduce
   a genuine `facet=` parameter (default = `defaultFacet` = `"RNA"`) and key **count/feature-space**
   registries by **facet** (`markerResults[[facet]][[grouping]]`, `diffgenes[[facet]][[grouping]]`), while
   **cell-space** registries (`reductions`, `graphs`, `embeddings`) stay **name-keyed with facet(s) in
   provenance** — because a graph/reduction may *integrate* several facets and so has no single owning
   facet (§4.5, §5). The `DefaultAssay`-style ergonomics are preserved — facet-free calls hit RNA — but
   `facet` is a real dimension, not a token half.

3. **`type` is demoted to a near-vestigial *reduction selector* and is a candidate for deprecation.**
   Today `type` is incoherently overloaded: it means `"counts"` for markers/DE but a *reduction name*
   (`"PCA"`) for graphs/embeddings (see §3 — this is verifiable in the current code, and is itself
   evidence the token never earned its keep). We stop loading multimodal meaning onto it. Each facet
   carries a `defaultReduction` (RNA→`"PCA"`, ATAC→`"LSI"`); `type=`/`reduction=` survives only as a
   rarely-passed override when a user genuinely has >1 reduction in a facet. **We do NOT smuggle facet
   back into `type`.**

4. **One canonical, namespaced `cells` axis shared by all facets; partial overlap is a per-facet
   *membership mask* (boolean over the canonical axis), never an NA-padded count matrix.** Each facet's
   `rawCounts` stays on its own observed cell labels; a facet's cells must be a **subset** of the
   canonical axis. This honors the house "linked collection, not aligned tensor" stance; the aligned
   case short-circuits to today's code path.

5. **Cell metadata is shared and top-level (`cellMeta`); feature metadata is per-facet
   (`facets$<F>$featureMeta`); the global `geneMeta` becomes an alias for the default facet.** Per-facet
   QC columns land in `cellMeta` with a `<metric>_<facet>` suffix (proposal1 got this right).

6. **Facet integration is a generic *step with pluggable methods*; joints (WNN / totalVI / MOFA+) are its
   byproducts, NOT functions or a `type`.** `runGraph` builds the cell graph and integrates **all
   available facets by default** to weight edges; the integration algorithm is a `method=` (WNN now;
   totalVI/MOFA+/concat later), exactly as `leiden` is a `method=` of `runClustering`. There is no
   `runWNN`/`runLeiden`. The byproducts (an integrated graph, optionally a joint reduction, per-cell
   modality weights, per-facet loadings) are stored as name-keyed **named products** (`graphs[["WNN"]]`,
   `reductions[["WNN"]]`) with the facet set in provenance (§5). pagoda2.1 ships one integration method
   (§0.4.4); the named-product storage is reserved for the rest.

7. **The C++ view kernels (`colMeanVarView`, `colSumByFacView`, `viewKernelValue`) generalize to N
   facets for free** — they consume a `(raw, view-args)` pair and a `rowSel` mask and know nothing about
   RNA or about `type`. ADT/ATAC need *new view models* (CLR for ADT, TF-IDF for ATAC) as new
   `view$model` branches; §6 specifies the numerical gating test proving these preserve the kernels'
   thread-count-invariant float64-accumulation-over-float32-storage contract. LSI's "drop component 1" is
   a *reduction* post-step, not a view transform.

8. **The whole model is a 1:1 image of lstar's `(shared cells axis) + (per-facet feature axes) +
   (measures over (cells, feature-axis))`.** Round-trip is lossless *by construction* because the
   in-memory shape and the on-disk shape are the same. `write_pagoda2` already emits this structure for
   the single-RNA case (single `genes` axis + single-grouping markers); the multi-facet version iterates
   `p2$facets`. **A facet membership mask is exactly an lstar partial-coverage `index`** — and that
   mechanism is now *implemented* in lstar (Python/C++/R; `SUPPORT.md:55,111`, round-trip asserted in
   `lstar/python/tests/test_partial.py`): a partial measure carries an integer `index` into the shared
   `cells` axis, **not** a `cells.<mod>` axis and **not** zero/NA-padded. So pagoda2.1 emits typed `index`
   partial coverage directly — there is no interim fallback and no switch-over (this *simplifies* the
   earlier draft). **The real remaining cross-repo dependency** is narrower and is a *corpus/profile* gap,
   not a mechanism gap: the lstar pagoda2 profile must (a) gain facet iteration and (b) be exercised on a
   **real, multi-facet pagoda2 object** — today it is validated only on a *mock* pagoda2 (`SUPPORT.md`
   Conos/pagoda2 table, gap #5), and no real `.h5mu` multiome is in the corpus yet (gap #2). Track those
   (§7, §11).

9. **The coupling surface to downstream consumers is a versioned *accessor contract*, not the field
   layout.** The active sister packages — `conos` (collections of pagoda2 samples) and `cacoa`
   (case-control analysis, `dev_lm`) — must bind to a small, explicit, **facet-aware accessor API**
   (`getRawCounts`, `getExpression`, `getReduction`, `getEmbedding`, `getClustering`, `getOdGenes`,
   `getVarInfo`, `getDepth`, `listFacets`, `getFacet`), **never** to internal fields. cacoa is purely
   *collection-level* and reaches pagoda2 only transitively through `conos`'s `access_wrappers.R`
   generics, so porting conos's wrappers onto the new accessors carries cacoa along. Once the contract is
   the seam, the facet refactor (facet-keyed registries, membership masks, view models) is internal and
   free to change. This is the design that lasts — and it is the direct antidote to old pagoda2's pain,
   where every consumer welded itself to `p$counts` / `p$misc$rawCounts` (§8.5, §10).

### 0.3 Back-compat stance: a clean break, by design

pagoda2 ≤1.x exposed its internals as a public surface — `p$counts`, `p$misc$rawCounts`, `p$misc$varinfo`,
`p$reductions$PCA`, `p$embeddings$PCA[[type]]` were read and written *directly* by users and by sister
packages. Preserving that surface byte-for-byte through a facet refactor is both expensive and, worse,
**anti-durable**: it re-cements the exact direct-field coupling that makes the current object hard to
evolve. So this revision makes an explicit decision:

> **We do not maintain backwards compatibility with pagoda2 ≤1.x object layouts or direct field
> access.** We take a clean break, bump `apiVersion`, and define the coupling surface as the §8.5
> accessor contract. Legacy `.rds` objects are handled (if at all) by a single one-shot
> `convertLegacy()` migrator or simply re-running the pipeline — *not* by load-time shims woven through
> every registry.

Two consequences for the rest of this document: (a) the delegating active bindings of §4.2 are kept
purely as a clean single-facet ergonomic, not as a compatibility layer; (b) the elaborate `type="counts"`
double-duty sentinel, the per-registry legacy-read shims, and the `type=`-as-deprecated-alias machinery
described in earlier drafts of §10 are **removed** — with no old objects to honor, registries are simply
facet-keyed from the start, and `type=` is dropped (or kept only as an unadvertised reduction override).
This deletes the proposal's single most fragile mechanism (former risk #2).

### 0.4 Decisions of record (2026-06-13)

Four scope/architecture questions were resolved by the maintainer; they are now binding and the rest of
this document is read in their light:

1. **Modality scope & order: ADT first, ATAC next.** pagoda2.1 ships CITE-seq/ADT (CLR view) end-to-end;
   ATAC (TF-IDF/LSI, genomic ranges, multiome barcode translation + partial overlap) is a *follow-up*, not
   part of the first multimodal release. The ATAC-specific material in §6.3 and §11.8 is therefore
   **Phase 2b** (§10.1).
2. **Import is lstar-mediated.** Multimodal import (`.h5mu`, 10x multiome H5) routes through lstar's R
   reader to inherit the canonical feature-axis vocabulary and partial-overlap (`index`) handling (§7).
   *Consequence:* **lstar's R package becomes a required dependency for the multimodal import path** —
   loaded **on demand** so RNA-only installs stay lean (the single-RNA `readCounts`/`Pagoda2$from` path
   keeps its native hdf5r implementation and does not pull lstar).
3. **Canonical cell axis = union, with a complete-cases helper.** The canonical `cells` axis is the union
   of facet cell sets (MuData default); per-facet membership masks track coverage; a `requireFacets=` /
   complete-cases filter selects cells measured in a given set of facets. `filterData()` evaluates QC on
   the default facet over the canonical axis, then intersects every facet's mask (§4.3). This settles
   former open risk #3.
4. **pagoda2.1 implements one joint method (not shape-only).** A working joint integration ships in 2.1,
   not merely the reserved storage shape (§5). Target: **WNN on RNA+ADT** (field-standard for CITE-seq;
   the §5 named-product storage already targets it), with concatenated/weighted-PCA as the fallback if WNN
   proves too heavy. *Open sub-decision:* WNN vs concatenated-PCA as the shipped method.
5. **`runPCA`/`runLSI` are removed; `runReduction` is the only reduction step.** PCA is `runReduction`'s
   default method on RNA (LSI on ATAC) — there is no first-class `runPCA`, by exact parity with the removed
   `runLeiden`/`runWNN`/`runUMAP` (the algorithm is always a `method=`; §5, §8). **TODO:** delete the legacy
   `runPCA` wrapper when the `runReduction` generic lands (Phase 1). *Open sub-decision:* whether to keep
   `runPCA` as a thin deprecated alias for familiarity (leaning no, for consistency).

**Testing.** The rollout is gated phase-by-phase against the lstar multi-omic corpus (CITE-seq + multiome),
imported via the lstar-mediated path; see **`misc/multimodal_testing_plan.md`** for the OODA'd per-phase
test sets, fixtures, and the round-trip/invariance gates.

---

## 1. What proposal1 got right (keep)

- **A named `facets` registry** (`p2$facets$RNA`, `$ADT`, `$ATAC`) with per-facet matrix + feature
  state. Correct, and it matches lstar's per-feature-axis decomposition and Seurat's `Assays`,
  MuData's `.mod`, SCE's `altExps`, SOMA's `ms`. This is the universal shape; we keep it.
- **Keeping `facet` separate from any reduction/`type` token.** proposal1 named a `facet=` argument and
  a `defaultFacet` field and *never* tried to express the modality via the reduction token. The prior
  draft of *this* document overruled that and folded facet into `type`; we now affirm proposal1 was
  right on the structure. (proposal1's only fault here was under-specifying the registry keying and the
  resolver — §3, §4 supply that.)
- **Shared cell-level state at the top level** (`cellMeta`, `embeddings`, `graphs`, `clusterings`,
  `markers`, `defaultGroup`). Correct: every surveyed framework keeps the observation axis global.
- **The current object as the special case `facets$RNA` with `defaultFacet="RNA"`.** Correct and
  essential for back-compat. We make this *literally true* via delegation rather than aspirational.
- **RNA-first defaults**: methods default to the default facet; users only name a facet when they mean
  something other than RNA. Correct — this is exactly Seurat's `DefaultAssay`.
- **`<metric>_<facet>` naming for facet-origin QC in shared `cellMeta`.** Correct; this is MuData's
  `rna:n_genes` prefixing convention, and it's the right call for plotting and resolution.
- **Provenance on shared products** (`graphs$WNN$facets <- c("RNA","ADT")`). Correct and necessary;
  lstar makes provenance first-class for the same reason.
- **Naming-collision resolution order** (cellMeta → default-facet feature → explicit facet). Correct;
  keep it, with refinements in §6 (qualified `facet:feature` syntax) and the resolver precedence in §8.
- **Export schema with `facets/<F>/{var,layers,norm}` and `obs/obsm/graphs/...` at top level.** Correct
  in spirit — but it should be *expressed as the lstar Zarr store*, not a parallel ad-hoc schema (§7).

## 2. What proposal1 got wrong or left dangerously vague (change)

**(a) "Defer partial cell overlap entirely / assume shared cells."** This is the biggest mistake. The
real targets *force* partial overlap on day one:
- 10x multiome uses **different barcode whitelists for GEX and ATAC** and requires barcode translation;
  the cells are "the same" only after a mapping — they are not row-name-identical, and droplet calling
  differs per modality so the sets genuinely differ.
- CITE-seq routinely has cells with RNA but failed/empty ADT and vice versa.
- Every framework that matters bakes partial overlap into its core (MuData `obsmap`, Seurat v5
  `Assay5` `cells` slot, MAE `sampleMap`, SOMA per-measurement obs filtering). lstar's coverage tracker
  lists partial-overlap as a known, designed-for case (`cells.<mod>` axis) — see §7, §11 for its
  current implementation status.

Deferring it means the cell-axis abstraction will be wrong, and retrofitting it later touches every
resolver. The cost of *designing for it now* is small if we do it the memory-lean way (a boolean
membership mask, §4) and keep the common aligned case as the fast path. **Design the axis for partial
overlap; optimize for the aligned case.**

**(b) A facet modeled as a loose list of slots (`$counts`, `$featureMeta`, `$normalization`,
`$variance`).** This re-derives, badly, the structure pagoda2.1 already has as `rawCounts` +
`matrixViews$analysis` + `geneMeta` + `misc$varinfo`. A facet should be that exact bundle, so the view
kernels, materializer, and validators work on a facet **unchanged**. Don't fork the matrix-storage
logic per facet; parameterize it.

**(c) Under-specified registry keying — *but the fix is `facet`, not `type`.*** proposal1 left the
`facets` registry and the existing `type`-keyed result registries unreconciled. The prior draft "fixed"
this by folding facet into `type`; that was the wrong fix (§3). The right fix is to make `facet` the
**primary** key of every result registry (`markerResults[[facet]][[grouping]]`) and stop using `type`
as a multimodal discriminator. proposal1's `facet=` argument was the correct instinct; we adopt it as a
genuine, non-sugar parameter (§8).

**(d) "Do not generalize the export schema's feature axis" stated as a *constraint* but the schema
sketch is a bespoke `facets/...` tree.** We already have the canonical multi-feature-axis target: the
lstar store. Re-deriving a second multimodal schema is wasted work and a lossiness risk. Target lstar.

**(e) Markers/DE keyed only by `(type, name)` with `type="counts"`.** For multimodal this collides:
`markerResults[["counts"]][["leiden"]]` would clash when you run markers on RNA vs ADT for the same
clustering. proposal1 did not catch this. The fix is to key by **facet** —
`markerResults[["RNA"]][["leiden"]]` vs `markerResults[["ADT"]][["leiden"]]` — *not* to widen `type`
into a facet token (§3, §4, §10).

---

## 3. The central idea: `facet` is first-class; `type` is vestigial — demote it

### 3.1 The empirical case against `type`

`type` has been carried since pagoda2's inception as the "which reduction space" token. Two facts kill
the case for building the multimodal architecture on top of it.

**Fact 1 — nobody deviates from PCA.** In pagoda2's entire history, real users essentially never set a
non-PCA reduction. The reduction-space `type` token therefore models a degree of freedom that is almost
never exercised. A token whose value is "PCA" >99% of the time has not earned a place as a load-bearing
seam, let alone as the seam onto which we bolt all of multimodality.

**Fact 2 — `type` is not even coherent today.** Read the actual code, not the docstrings:

| registry | how it is *actually* keyed today | source |
|---|---|---|
| `reductions[[name]]` | reduction name, `"PCA"` | `runPCA` → `self$reductions[[name]] <- pcas` (`Pagoda2-class.R:2151`) |
| `graphs[[type]]` | **reduction name**, `"PCA"` — `runGraph(reduction)` calls `makeKnnGraph(type=reduction)` | `workflow.R:299`, `Pagoda2-class.R:1005` |
| `embeddings[[type]][[name]]` | **reduction name**, `["PCA"]["UMAP"]` — `runEmbedding(reduction)` → `getEmbedding(type=reduction)` | `workflow.R:384`, `Pagoda2-class.R:2605` |
| `diffgenes[[type]][[name]]` | **`"counts"`** — `runMarkers` defaults `type="counts"` | `markers.R:246,300` |
| `markerResults[[type]][[name]]` | **`"counts"`** — same default | `markers.R:267,301-304` |

So the *same* token `type` means a **reduction name** (`"PCA"`) in the graph/embedding registries but a
**raw analysis view** (`"counts"`) in the marker/DE registries. It is two unrelated concepts wearing one
name. Generalizing *that* into the multimodal discriminator (as the prior draft proposed) would have
cemented an already-incoherent overload and made `"PCA"` vs `"ADT"` vs `"counts"` share a namespace where
collisions ("is `PCA` a reduction or a facet?") are inevitable. **We reject that.**

### 3.2 The design: facet primary, reduction implied, `type` demoted

1. **`facet` is a genuine, orthogonal argument and the primary registry key.** It is *not* sugar that
   sets half of a `type` token. `p2$defaultFacet` (default `"RNA"`) plays the `DefaultAssay` role:
   facet-free calls hit RNA.

2. **Each facet owns its default reduction.** `facets$RNA$defaultReduction = "PCA"`,
   `facets$ATAC$defaultReduction = "LSI"`. The reduction is *implied by the facet*. ATAC genuinely uses
   LSI not PCA — but that variation is **driven by the facet**, which is exactly Peter's point: the facet
   is the meaningful axis, so let it carry the reduction default instead of surfacing `type` as a knob.

3. **`type`/`reduction` survives only as a rarely-passed optional override.** A user who has computed two
   reductions inside one facet (e.g. `PCA` and `CCA` on RNA) can still pass `reduction="CCA"`. That is
   the *entire* remaining job of the old `type` token, and it is a power-user escape hatch, not a
   first-class dimension. **Recommendation: rename the surviving argument `reduction=` for clarity and
   keep `type=` as a deprecated alias** (it currently appears on `makeKnnGraph`, `getEmbedding`,
   `getDifferentialGenes`, `testPathwayOverdispersion`, `resolveMarkers`, `getMarkerResult`,
   `getTopMarkers`). See §8 for the per-method disposition and §10 for migration.

4. **Result registries are re-keyed by facet** (full scheme in §4). The reduction space becomes at most
   an *optional secondary qualifier*, present only where a facet legitimately holds >1 reduction — which,
   per Fact 1, is almost never.

### 3.3 Why this is also the framework consensus (forward-pointer to §9)

None of MuData / Seurat-v5 / SCE / MAE / SOMA keys its core result registries by *reduction type*. They
all key by **modality / assay / measurement** (`.mod`, `Assays`, `altExps`/`ms`, `ExperimentList`).
"Reduction type" is universally a *secondary* attribute (a named `DimReduc`, a `reducedDims` slot), never
the primary discriminator. The field's convergent design *supports* making facet primary and `type`
vestigial; the prior draft's "generalize `type`" recommendation was the one choice no surveyed framework
makes.

---

## 4. In-memory data model

### 4.1 The facet object

A facet is the existing matrix bundle, factored out. Define a lightweight R6 (or plain environment) so
the same code paths run per facet:

```r
Facet <- R6::R6Class("Pagoda2Facet",
  public = list(
    name            = NULL,        # "RNA", "ADT", "ATAC"
    rawCounts       = NULL,        # dgCMatrix, cells x features  (this facet's OWN cell labels)
    matrixViews     = list(),      # named view recipes; $analysis is the canonical one
    featureMeta     = data.frame(),# per-facet feature table (genes / proteins / peaks)
    depth           = NULL,        # per-cell library size for THIS facet
    batch           = NULL,        # per-cell batch (usually shared, may differ)
    modelType       = "plain",     # "plain"/"raw" for RNA; "clr" for ADT; "tfidf" for ATAC
    defaultReduction = "PCA",      # reduction implied BY this facet (RNA->"PCA", ATAC->"LSI")
    varinfo         = NULL,        # overdispersion fit (was misc$varinfo)
    odgenes         = NULL,        # overdispersed features (was misc$odgenes)
    featureType     = "gene",      # "gene" | "protein" | "peak" | ...  (lstar feature-axis hint)
    loadings        = list(),      # per-facet (FEATURE-space) loadings, e.g. loadings[["PCA"]], [["MOFA"]]
    palettes        = list()       # per-facet feature palettes
    # NOTE: reduction *scores* are NOT stored here — they are cell-space and live in the top-level
    #       `reductions` registry (§4.5). The facet owns feature-space state only; loadings stay here.
  ))
```

`defaultReduction` is what lets us demote `type`: a method that needs a reduction for this facet uses
`facets[[f]]$defaultReduction` unless the caller explicitly overrides it. RNA defaults to `"PCA"`
(matching the existing `defaults$reduction`), ATAC to `"LSI"`. The reduction is **implied by the facet**,
not surfaced as a user-facing `type` knob.

Crucially `matrixViews`, `rawCounts`, `depth`, `batch` have **identical structure** to today's
top-level fields, so `.pagoda2_materialize_view`, `.pagoda2_view_kernel_args`,
`.pagoda2_r6_view_col_mean_var`, and `colMeanVarView`/`colSumByFacView` operate on a facet with **zero
changes** — they already take `(raw, view)`; we just hand them a facet's pair.

### 4.2 The object, top level

```r
Pagoda2  (R6, unchanged class name; bumped apiVersion — clean break, §0.3)
  ## --- canonical cell axis (NEW, but defaulted) ---
  cells         : character        # the canonical, ordered cell-id axis (union over facets)
  facets        : list<Facet>      # named registry; facets[["RNA"]] etc.
  defaultFacet  : "RNA"

  ## --- shared cell-level state (top-level, as today) ---
  cellMeta      : data.frame       # shared; per-facet QC as <metric>_<facet>
  reductions    : list             # ALL reduction scores (cell-space), name-keyed: "PCA","ADT:PCA","WNN",...
                                    #   facet-qualified only to disambiguate; provenance{facet(s),method}; §4.5
  graphs        : list             # name-keyed by source reduction/product: "PCA","ADT:PCA","WNN"  (§4.5)
  embeddings    : list             # [source-key][name], e.g. ["PCA"]["UMAP"], ["WNN"]["UMAP"]  (§4.5)
  clusterings   : list             # provenance per grouping (records the graph/reduction it used)
  diffgenes     : list             # [facet][grouping]   <-- facet-keyed (was [type][name])
  markerResults : list             # [facet][grouping]   <-- facet-keyed (was [type][name])
  defaultGrouping, palettes$cellMeta, history, threadPolicy, ...   # unchanged

  ## --- back-compat aliases (active bindings) ---
  rawCounts     -> facets[[defaultFacet]]$rawCounts
  matrixViews   -> facets[[defaultFacet]]$matrixViews
  geneMeta      -> facets[[defaultFacet]]$featureMeta
  depth         -> facets[[defaultFacet]]$depth
```

The **active-binding aliases** are what make migration free: existing code touching `p2$rawCounts`,
`p2$matrixViews$analysis`, `p2$geneMeta`, `p2$depth` keeps working, transparently reading/writing the
default facet. (The class is already `lock_objects = FALSE` and already uses an active binding for the
removed `counts` slot — `Pagoda2-class.R:3037` — so this pattern is established.)

Note the registries are now **facet-keyed**, not token-keyed. `markerResults[["RNA"]][["leiden"]]` and
`markerResults[["ADT"]][["leiden"]]` are distinct results for the same clustering on different facets —
the collision §2(e) flagged is structurally impossible. §4.5 specifies the keying and the back-compat
flattening; §10 specifies how existing `type="counts"`-keyed stores migrate.

### 4.3 Partial cell overlap — the membership mask

The canonical axis `p2$cells` is the **union** of facet cell sets (MuData uses union by default;
intersection is a filtering choice, not a storage one). Each facet stores its counts on its *own*
observed cells (a subset of `cells`). Resolution is a label join, never a dense NA matrix:

- A facet exposes membership as a logical/integer mask over `cells`:
  `facetMembership(facet) -> logical(length(cells))`. This is exactly MuData's `obsmap` (0 = absent)
  and the lstar partial-coverage semantics. It costs one bit-ish vector per facet, not a widened matrix.
  **On export the mask *is* an lstar partial-coverage `index`** (`which(mask)` into the shared `cells`
  axis) — that mechanism is implemented in lstar (§7), so there is no `cells.<facet>` fallback.
- The C++ kernels **already accept a `rowSel` mask** (`colMeanVarView`, `colSumByFacView` take
  `rowSel`; `colMeanVarS` too). Per-facet operations pass that facet's membership as `rowSel`. **No
  kernel change.**
- "Get expression of feature X" gathers from whichever facet owns X, returning values for the cells
  that facet covers and *absent* (not 0, not NA-by-default) elsewhere; plotting renders uncovered cells
  as a distinct "not measured" color, the way it must for any honest multimodal viewer.
- The **fast path**: when a facet's cells equal `p2$cells` in order (the aligned CITE-seq case), the
  mask is `all-TRUE` and we skip it entirely — identical performance to today.

This honors the house principle directly: facets within a sample are a **linked collection of parts
joined by the cell axis**, aligned where they overlap, *absent* where they don't — not a single
NA-padded tensor. We refuse to over-normalize into a dense `cells × allfeatures` block the way a naive
AnnData-concatenation would.

**Union axis, complete-cases helper, and `filterData` (decision §0.4.3).** The canonical axis is the
**union** of facet cell sets; intersection is a *selection*, not a storage choice. Two ergonomics make
the common CITE-seq workflow clean:

- `requireFacets = c("RNA","ADT")` — a complete-cases selector that restricts the canonical axis (and
  re-derives masks) to cells covered by all named facets. Available on analysis entry points and as a
  standalone subset. The aligned CITE-seq case (all masks all-TRUE) is the default and costs nothing.
- `filterData()` over the union axis: QC is evaluated on the **default facet** over the canonical axis,
  then **every facet's membership mask is intersected** with the surviving cells, atomically. Per-facet QC
  columns (`pct_mito_RNA`, `n_molecules_ADT`) are available for explicit per-facet thresholds, but the
  default keeps the single, predictable canonical-axis behavior single-facet users already expect. A cell
  dropped on RNA QC leaves the canonical axis entirely (it is not kept "RNA-absent, ADT-present").

### 4.4 Shared vs per-facet, decided

| thing | where | rationale |
|---|---|---|
| cell ids (canonical axis) | `p2$cells` (shared) | one observation axis; every framework agrees |
| cell metadata, annotations, clusters | `p2$cellMeta` (shared) | groupings are cell-level, facet-agnostic |
| per-facet QC (`n_molecules_ADT`, `pct_mito_RNA`) | `p2$cellMeta`, suffixed | MuData `rna:` convention; resolvable in plots |
| facet membership mask | `facets$<F>` | partial overlap, memory-lean |
| raw counts, view recipe, depth, batch | `facets$<F>` | the matrix bundle; kernels run on it unchanged |
| feature metadata | `facets$<F>$featureMeta` | genes ≠ proteins ≠ peaks; never one global `geneMeta` |
| overdispersion fit / odgenes / varinfo | `facets$<F>` | per-feature-space; was `misc$varinfo` |
| reductions | top-level, **name-keyed** (`reductions[["PCA"]]`) | shared cell-space results; each records its source facet(s) in provenance |
| graphs / embeddings | top-level, **name-keyed** (`graphs[["RNA"]]`, `graphs[["WNN"]]`) | shared cell-space results; facet(s)+method+reduction in provenance — a graph may integrate several facets, so it has no single owning facet (§4.5) |
| markers / DE | top-level, **facet-keyed** (`markerResults[[facet]][[grouping]]`) | result of (facet × grouping); reduction is irrelevant for count-space markers |
| joint products (WNN/MOFA) | byproduct of the facet-integration step, stored under their **own name** (`reductions[["WNN"]]`, `graphs[["WNN"]]`) | shared latent/graph over cells; a *named product* produced by `runGraph(method=…)`/`runReduction(method=…)`, not a `type`; §5 |

### 4.5 Registry keying, precisely

Two keying regimes, by what the result is *about*. **Count/feature-space results are facet-keyed**
(markers/DE genuinely belong to one feature space). **Cell-space results are name-keyed with provenance**
(reductions, graphs, embeddings) — because once `runGraph`/`runReduction` can *integrate several facets*
(§5), a result has no single owning facet; the contributing facet(s) + method + source reduction live in
provenance, exactly as `reductions` already does.

**Count/feature-space results are facet-keyed:**
- **`markerResults` / `diffgenes`** — `[[facet]][[grouping]]`. Count-space markers do not depend on a
  reduction at all, so the reduction never enters the key. RNA markers on `leiden` →
  `markerResults[["RNA"]][["leiden"]]`; ADT markers on the same clustering →
  `markerResults[["ADT"]][["leiden"]]`. No collision.

**Cell-space results (`reductions`, `graphs`, `embeddings`) share ONE flat keying rule:** a `:`-separated
key ending in the thing's **own name**, with leading qualifiers (facet, then source) **dropped when
unambiguous**; the default facet is unqualified, joint products are bare, and provenance always carries the
full `{facet(s), method, source, params}`. No nesting, no facet-primary key — because an integrating
product (WNN) has no single owning facet.
- **`reductions`** — keyed by reduction name: `reductions[["PCA"]]` (RNA's, default facet, bare —
  single-RNA and conos unchanged), `reductions[["ADT:PCA"]]` (qualified only because `PCA` would collide),
  `reductions[["RNA:CCA"]]` (non-default within a facet), `reductions[["WNN"]]`/`reductions[["MOFA"]]`
  (joints, bare). Scores live here; per-facet **loadings** stay in `facets$<F>$loadings`.
- **`graphs`** — keyed by the reduction/product the graph is built **on** (same namespace as `reductions`):
  `graphs[["PCA"]]` (kNN on RNA's PCA), `graphs[["ADT:PCA"]]`, `graphs[["WNN"]]` (the integration graph),
  `graphs[["MOFA"]]` (kNN on the MOFA latent).
- **`embeddings`** — keyed by `<source>:<embedding>` (flat, not nested): `embeddings[["UMAP"]]` (default
  source, bare), `embeddings[["WNN:UMAP"]]`, `embeddings[["ADT:PCA:UMAP"]]`, `embeddings[["PCA:tSNE"]]`.

The single key threads through unchanged: `PCA` → `graphs[["PCA"]]` → `embeddings[["PCA:UMAP"]]`; and the
qualifier syntax is the same `facet:name` used for features (`ADT:CD3`) — one disambiguation rule across
the whole API. `getReduction(facet="ADT")` resolves to `reductions[["ADT:PCA"]]`; bare `getReduction()` →
`reductions[["PCA"]]`.

#### 4.5.1 The default/resolution strategy (single-facet stays bare; joints can't shadow)

The two failure modes to avoid: (a) single-facet users forced to type a facet they never needed, and
(b) a bare name silently resolving to the wrong thing when a per-facet `PCA` *and* a joint product coexist.
Both are closed by a small, deterministic rule split into **storage** (the canonical key) and **resolution**
(what a bare user name means):

**Storage — canonical keys are unambiguous and stable:**
1. A per-facet reduction is stored **facet-qualified**: `RNA:PCA`, `ADT:PCA`. (Canonically qualified even
   for the default facet, so keys don't move if `defaultFacet` changes — bareness is a *display/access*
   convenience, not the stored key.)
2. A joint/integration product is stored under its **own distinct product name**: `WNN`, `MOFA`, and for the
   concat-PCA fallback (§0.4.4) something like `jointPCA` — **never** a bare per-facet method name. A
   validator **forbids a joint product name from equalling a per-facet reduction method name** (`PCA`,
   `LSI`, …); on collision it errors (or auto-suffixes). This is what makes "a per-facet PCA *and* a joint
   one" safe — the joint simply cannot be called `PCA`.

**Resolution — a bare name is sugar over the default facet:**
3. An explicit qualified key (`ADT:PCA`, `WNN`) is taken verbatim — always unambiguous.
4. A bare name `X` resolves, deterministically, to **`<defaultFacet>:X` if that facet has it, else the joint
   product named `X`**. Because rule 2 guarantees a joint never shares a bare per-facet method name, these
   two can never both match — **no silent mistake is possible**. `getReduction("PCA")` → `RNA:PCA`;
   `getReduction("WNN")` → the joint `WNN`.
5. Bare `getReduction()` / `runGraph()` / `runEmbedding()` (no name, no facet) → the **default facet's
   default reduction** and the products built on it.

**Net ergonomics:** a single-facet (RNA) user types nothing extra — `PCA`, `graphs`, `UMAP` are all bare
and resolve to RNA. A multi-facet user qualifies only the non-default facet (`ADT:PCA`) and names joints
distinctly (`WNN`); the qualifier is the same `facet:name` syntax as features (`ADT:CD3`). Plots and report
headers surface the **resolved** key + provenance (mandatory, §6.4), so the user always sees which
representation was used. (Because we take a clean break, §0.3, all of this is keyed canonically from the
first commit — no legacy `type="counts"` layout, no flattening shim; old objects are normalized once by
`convertLegacy()`, §10.2.)

---

## 5. Facet integration is a *step with pluggable methods* (WNN is one); joints are its named-product byproduct

**Facet integration is not a special object you build with a dedicated function — it is the default
behavior of the graph (and, one level up, the reduction) step when more than one facet is present.**
`runGraph()` builds the cell-cell graph; by default it uses **all available facets** to weight edges and
set up the neighborhood structure. Single-facet is the degenerate case (plain kNN on that facet's
reduction). The *integration algorithm* — WNN today; totalVI / MOFA+ / concatenation / CCA-style later —
is a `method=` of that step, exactly as `leiden` is a `method=` of `runClustering` and `UMAP` a method of
`runEmbedding`. There is **no `runWNN`**; there is `runGraph(method = "wnn")`.

Every such method produces the **same byproducts** over the shared cell axis: optionally a joint
low-dimensional reduction, the integrated graph, and (for WNN) per-cell modality weights;
MOFA+/totalVI add **per-facet loadings** onto the shared factors. These byproducts are stored as ordinary
**named products** (§4.5) — name-keyed cell-space results with the contributing facets recorded as
provenance — *not* as a value of `type` and *not* as a facet. (This matches Seurat: WNN is a named
graph + `wnn.umap`, not an assay.)

```r
p2$runGraph(facets = c("RNA","ADT"), method = "wnn")   # the integration step; facets default to "all"
#   byproducts, all name-keyed with provenance{facets, method, params, seed}:
p2$reductions[["WNN"]]             # cells x k joint scores (if the method yields a reduction)
p2$graphs[["WNN"]]                 # the integrated kNN/SNN graph (name-keyed; §4.5)
p2$cellMeta$wnn_weight_RNA         # per-cell modality weight (WNN), a shared cell measure
p2$cellMeta$wnn_weight_ADT
p2$runClustering(method = "leiden")      # on that graph; defaultGrouping <- "leiden"
p2$runEmbedding(graph = "WNN", name = "UMAP")          # -> embeddings[["WNN:UMAP"]]
p2$clusterings$leiden$reduction          # records "WNN" so plots/markers know the joint provenance
```

Integration can equally happen one level earlier: `runReduction(facets = c("RNA","ADT"), method = "mofa")`
produces a joint latent `reductions[["MOFA"]]` that `runGraph` then consumes like any single reduction.
Reduction-level and graph-level integration are both facet-aware, pluggable steps; WNN is the graph-level
default for the ADT-first release (§0.4.4).

For factor models with loadings (MOFA+/totalVI), the loadings split across facets and live with each
facet's feature space (lstar's coordinate-axis induction: the `k` factors are *one* coordinate axis
shared by a `cells × factors` embedding and per-facet `features × factors` loadings):

```r
p2$reductions[["MOFA"]]                       # cells x k  (shared scores)
p2$facets$RNA$loadings[["MOFA"]]              # genes    x k
p2$facets$ADT$loadings[["MOFA"]]              # proteins x k
```

This is precisely lstar's worked example (`model.md` induction rules): one factor axis, an embedding
over `(cells, factor)`, loadings over `(feature, factor)` per facet. No special slot, no new object —
which is exactly why it round-trips.

**Decision (§0.4.4): pagoda2.1 ships *one* joint method, not just the shape.** The named-product storage
shape is free (a named reduction + a couple of cell measures + per-facet loadings + provenance) and is
reserved for all joint methods; on top of it, 2.1 implements **WNN on RNA+ADT** as the shipped joint
integration (the ADT-first scope, §0.4.1, makes CITE-seq WNN the natural first target, and Seurat treats
WNN exactly as this named-product shape). *Open sub-decision:* WNN vs a simpler concatenated/weighted-PCA
joint as the v2.1 method — WNN is the field expectation, concatenated-PCA is the lower-risk fallback.
Other joint algorithms (MOFA+/totalVI/MultiVI) remain deferred; their storage shape is reserved now so
they land later without a schema change. Reserving this as a *named product* (not a `type` value) is what
keeps it orthogonal to the facet axis and lossless on round-trip.

### 5.1 Candidate integration methods (the `method=` space)

These are *vertical* integration methods — same cells, ≥2 modalities — which is exactly what a facet is
(facets share the cell axis by construction). *Horizontal* integration (same modality, many samples) is
conos's job (the collection level, §8.6); *diagonal/mosaic* (no shared cells/features) is flagged where a
method also handles it. The methods sort by which step they plug into and by **where the weighting lives**
(per-cell, per-modality-global, or learned):

| level (`method=` of) | method | weighting | notes / status for 2.1 |
|---|---|---|---|
| graph (`runGraph`) | **WNN** (Seurat v4) | **per-cell** | per-cell modality weights from cross-modality prediction; yields weighted SNN + weights. **The 2.1 default candidate (§0.4.4).** |
| graph | **SNF** (similarity network fusion) | per-modality | iterative cross-diffusion of per-modality similarity nets into one fused graph |
| graph | naive merge (union/intersection/weighted-sum of per-modality kNN/SNN) | per-modality | crude but near-free |
| reduction (`runReduction`) | **weighted concatenation** (scale + concat reductions, optional re-PCA) | per-modality-global | simplest baseline; the **low-risk fallback** for the shipped joint method |
| reduction | **MOFA+ / MEFISTO** | learned (factor variances) | shared+specific factors, per-modality sparse loadings; maps onto lstar shared-factor-axis + per-facet loadings. Natural next R `method=` |
| reduction | **CCA / sparse-CCA / PMA** *(implemented, 2.1)*; **DIABLO** (supervised) | per-modality | `runReduction(facets=, method="cca"/"scca")`; centered cross-covariance SVD (dense `irlba`) or `PMA::CCA` (sparse, Suggests); per-facet feature loadings. conos shares the machinery (transposed to vertical) |
| reduction | **MCIA**, **JIVE**, **LIGER/iNMF** (also mosaic), **scAI** | learned | co-inertia / joint+individual / integrative-NMF families |
| reduction | **Schema** | **explicit per-modality** | metric learning with a tunable weight per modality — the cleanest weighting knob |
| generative (optional backend) | **totalVI** (RNA+prot), **MultiVI** (RNA+ATAC±prot, mosaic), **MIRA**, **Cobolt**, **BABEL** | learned | model the counts, emit a joint latent; need a Python/Torch backend → "later, optional-backend" `method=` |
| diagonal (out of facet scope) | **GLUE**, **MultiMAP**, **bindSC**, Seurat anchors | — | for unpaired data; relevant only if a facet ever lacks shared cells |

The design absorbs all of these without new structure: `runGraph(method=)` covers graph-fusion (WNN, SNF,
merge), `runReduction(method=)` covers joint-factor methods (concat, MOFA+, CCA, LIGER, Schema), and
generative methods slot in as optional-backend `method=` later — each producing the same name-keyed
named-product byproducts (§4.5).

**Per-method scoping** (entry point, dependency status, input contract, output/loading shape, effort,
scale, and a recommended build order) for **CCA / sparse-CCA / SNF / LIGER / Schema** is worked out in
`integration_methods_scoping.md`. **CCA + sparse-CCA are now implemented** (reduction-level,
`runReduction(facets=, method="cca"/"scca")`: centered cross-covariance SVD over the shared cells —
dense `irlba` on a kept-sparse cross-product, or `PMA::CCA` for L1-sparse loadings, PMA a *Suggests*;
emits `reductions[["CCA"]]` + per-facet feature loadings + `{facets, input_axes, method, cancor}`
provenance; shares semantics with conos's `quickCCA` transposed to the vertical orientation). Still
deferred: LIGER (interpretable but heavy dep + raw-matrix input), SNF (graph-only, `O(n²)`, small-`n`
niche), and Schema (Python backend, fold into the MOFA+/totalVI reticulate effort). One contract
wrinkle for those: LIGER and the generative/Python methods consume **raw/normalized feature matrices**,
not per-facet **reductions** — a per-method input declaration the dispatcher should carry.

---

## 6. View kernels, normalization, and feature resolution across N facets

### 6.1 Kernels generalize for free; new *view models* are the work

`viewKernelValue` (`misc2.cpp:94`) and its callers are facet-agnostic: they consume
`(raw, depth, depthScale, normalize, logScale, batch, batchFactors, winsorCaps, preWinsorDepth,
postWinsorDepth)` and a `rowSel`. Running them on ADT or ATAC is "pass a different facet's `(raw,
view)`." What differs per modality is the **normalization recipe** (`view$model`):

- **RNA**: `"plain"`/`"raw"` — exists today (depth-normalize, optional winsorize, log).
- **ADT (CITE-seq)**: `"clr"` — centered log-ratio across proteins per cell.
- **ATAC (peaks)**: `"tfidf"` — term-frequency × inverse-document-frequency reweighting, feeding LSI as
  a *reduction* (not a view) — see §6.3.

The recipe-based design is the right substrate: a facet's normalization is *data describing a
transform*, computed on the fly, never a stored second matrix — honoring "memory-lean, low-precision
storage + high-precision accumulation, don't widen dtypes." Each facet keeps only its raw counts + a
small recipe, regardless of how many facets there are.

### 6.2 The kernel's invariance contract, and why CLR/TF-IDF must preserve it

This is the part the prior draft hand-waved. The existing kernels' *value* is not "they normalize" — it
is that they produce a **thread-count-invariant, bit-reproducible** result via **float64 accumulation
over float32-ish sparse storage**, with parallelism over **columns**. From `colMeanVarView`
(`misc2.cpp:192`): the `#pragma omp parallel for` is over genes `g`; for a fixed `g` the inner loop
walks that column's nonzeros `[p[g], p[g+1])` **in CSC index order**, accumulating `sumV`/`sumSqV` in
`double`. Different threads own disjoint columns, so there is no cross-thread reduction, no atomics, and
the per-column accumulation order is **independent of thread count** — the result is byte-identical for
1 core or 64 (`colSumByFacView` documents the same property at `misc2.cpp:346-349`, and the
`if(ncores>1)` guard keeps the serial path fork-safe under `mclapply`).

`viewKernelValue` is the *only* place a value is transformed, and it is a **pure per-entry function**:
its inputs are the single nonzero `value`, its `row` (cell), its `col` (gene), and **precomputed
per-axis scalars** — `depth[row]`, `batchFactors(col, batch[row])`, `winsorCaps[col]`. It performs no
reduction across entries. **That purity is precisely what makes the accumulation thread-invariant.** Any
new normalization preserves invariance **iff it is expressible as a per-entry function of precomputed
per-row and/or per-column scalars.** Concretely:

- **CLR** maps a nonzero `x_{cell,prot}` to `log(x / g_cell)` where `g_cell` is the per-cell geometric
  mean over that cell's nonzero proteins. `g_cell` is a **per-row scalar** — structurally identical to
  `depth[row]`. Precompute `g = exp(rowMeans_of_log_nonzeros)` once (an O(nnz) pass, itself a
  per-row reduction done in R/CSC order *before* the kernel), store it as `view$clrDivisor` (named on
  cells), and have `viewKernelValue` divide by `g[row]` instead of `depth[row]/depthScale`. The kernel's
  per-entry contract and column-parallel accumulation are **unchanged** → thread-invariance holds.
- **TF-IDF** maps `x_{cell,peak}` to `tf(x_{cell}) · idf_peak`, where `idf_peak = log(1 + N_cells /
  n_cells_with_peak)` is a **per-column scalar** — structurally identical to `winsorCaps[col]` /
  `gsf[col]`. Precompute `idf` once (an O(ncols) pass over `diff(p)` and per-column nonzero counts),
  store as `view$idf` (named on features); the per-cell TF normalization (e.g. `x/depth[cell]`) reuses
  the existing `depth[row]` machinery. Again a per-entry function of precomputed scalars → invariance
  holds.

These precomputed scalars (`view$clrDivisor` per-cell, `view$idf` per-feature) are the same `r[]`/`c[]`
the generalized lstar kernel will accept (§8.6.4 S4b), and on lstar export they persist as arity-1
**`recipe_scalar`** fields over `cells`/`genes` — *not* in the provenance dict (§7, lstar S1). One
representation serves the kernel, the round-trip, and byte-exact reproduction.

**Concrete numerical gating test (must ship with the CLR/TF-IDF branches).** For each new model, on a
real CITE-seq (ADT) and a real multiome (ATAC) fixture:

```r
# thread-invariance + precision gate for a new view model
v1 <- p2$viewColMeanVar(facet = "ADT", n.cores = 1)     # CLR view
v8 <- p2$viewColMeanVar(facet = "ADT", n.cores = 8)
stopifnot(identical(v1$m, v8$m), identical(v1$v, v8$v))  # BIT-identical across thread counts
# accumulation-precision gate: float64 kernel vs a float64 dense reference on the materialized view
ref <- .clr_dense_reference(p2$facets$ADT)               # double-precision, single-threaded
stopifnot(max(abs(v1$m - colMeans(ref))) < 1e-10)
stopifnot(max(abs(v1$v - colVars(ref)))  < 1e-8)
```

The `identical()` (not `all.equal`) check is the real gate: it fails the instant a transform introduces
a cross-entry dependency (e.g. computing the geometric mean *inside* the kernel, which would make the
per-column accumulation order-dependent). The reference check guards the precision contract: low-precision
storage, float64 accumulation. **Both must be in the test suite before either branch is considered
done.** This is the bar the prior draft's "it's just a new `view$model` branch" never stated.

Storage-seam changes (minimal, localized):
- `.pagoda2_materialize_view(raw, view)` (`matrix-storage.R:41`) gains `clr` and `tfidf` branches.
- `.pagoda2_view_kernel_args(raw, view)` (`matrix-storage.R:95`) learns the new models' arg packs
  (`clrDivisor` per-cell; `idf` per-feature) and relaxes its `view$model %in% c("plain","raw")` guard.
- `viewKernelValue` (C++) gains a `model`-dispatched divisor: `plain` → `depth[row]`, `clr` →
  `clrDivisor[row]`, `tfidf` → `depth[row]` with a per-column `idf[col]` multiplier. A small enum, not a
  rewrite; the accumulation loop is untouched.
- `.pagoda2_r6_set_count_matrix` (`matrix-storage.R:430`) is refactored to
  `.pagoda2_facet_set_count_matrix(facet, …)` and called per facet; the top-level method delegates to the
  default facet (back-compat).

### 6.3 LSI's "drop component 1" is a reduction post-step, not a view

LSI = TF-IDF view → SVD → **drop the first component** (it correlates with sequencing depth). The
drop-first step operates on *reduction coordinates*, not on count entries, so it belongs to the
reduction, not the view. Keep the `tfidf` view **pure** (a per-entry recipe, as above) and make LSI
`calculatePcaReduction` on that view with a `drop.first = TRUE` flag, stored as `reductions[["LSI"]]`
with `facets$ATAC$defaultReduction = "LSI"`. This keeps the view-recipe layer free of reduction
semantics and keeps the invariance argument in §6.2 intact (the view never does an SVD).

### 6.4 Feature value resolution and name collisions

Resolution order, anchored on **facet** (not on a `type` token):

1. If `facet=` is explicit → resolve the feature in that facet only. (A qualified `facet:feature` name,
   e.g. `"ADT:CD3"`, is equivalent and sets the facet for that one lookup — §8.)
2. Else if the name is a `cellMeta` column → cell metadata (groupings, QC).
3. Else search the **default facet** first, then other facets; **error on cross-facet ambiguity**
   (e.g. `CD3` present as both an ADT protein and a gene alias) and demand `facet=` or the qualified
   name.
4. Feature names are *not* required globally unique across facets (proteins and genes legitimately
   collide, e.g. ADT `CD3` vs gene `CD3D`/`CD3E`); uniqueness is enforced *within* a facet only — same
   rule MuData uses (`var_names` unique per modality; collisions disambiguated by modality).

Plot subtitles and report headers must surface the resolved facet whenever a feature/marker is
involved (proposal1's "default facet drift" mitigation — keep it, make it mandatory in plotting).

---

## 7. Lossless mapping to/from lstar (the interchange)

This is where the design pays off: **the in-memory model is a relabeling of the lstar model**, so the
Zarr round-trip is structural, not a translation.

| pagoda2.1 in-memory | lstar axes+fields | profile evidence |
|---|---|---|
| `p2$cells` (canonical axis) | `cells` axis, `role=observation` | every profile |
| `facets$RNA$featureMeta` rows | `genes` axis, `role=feature` | `profile_pagoda2.R`, `mudata.py` |
| `facets$ADT` → proteins | `proteins` feature axis | `mudata.py` `_MODALITY` map; `profile_seurat.R` other-assay loop |
| `facets$ATAC` → peaks | `peaks` feature axis | `mudata.py`; `profile_seurat.R` |
| `facets$<F>$rawCounts` (view=raw) | `measure` over `(cells, <fax>)`, `state=raw` | `profile_pagoda2.R:43` `counts`; `mudata.py` `_add_measure` |
| analysis view (NOT stored) | recipe **params** (`model`,`depthScale`,`log_base`,`winsor_caps`) in raw measure's `provenance`; large precomputed CLR-divisor/IDF as arity-1 **`recipe_scalar`** fields over `cells`/`genes` | `lstar/misc/response.md` S1; `conformance/provenance.sh` |
| facet membership mask | **typed partial-coverage `index`** into shared `cells` axis (implemented; works over a *derived union* axis too) | `SUPPORT.md:55,111`; `test_partial.py`; `conformance/partial.sh` (S3) |
| `cellMeta` columns | arity-1 fields over `cells` (categorical→factor axis) | all profiles |
| `featureMeta` columns | arity-1 fields over `<fax>` | `mudata.py` var loop; `profile_sce.R` rowData |
| `reductions[["PCA"]]` (RNA) | `embedding` over `(cells, pca)` + `loading` over `(genes, pca)` | `profile_seurat.R` reductions |
| `reductions[["LSI"]]` (ATAC) | `embedding` over `(cells, lsi)` + `loading` over `(peaks, lsi)` | `profile_seurat.R` reductions |
| `reductions[["WNN"]]` (named product) | `embedding` over `(cells, wnn)`; contributing **feature-axis** names in `provenance["input_axes"]` (e.g. `c("genes","proteins")`, not facet labels) | `lstar/misc/response.md` S5; `mudata.py` auto-populates by inference |
| `cellMeta$wnn_weight_RNA` | `measure` over `cells` | global obs |
| `graphs[["RNA"]]` / `graphs[["WNN"]]` (name-keyed) | `relation` over `(cells, cells)`, weighted; facets in provenance | model.md `knn` |
| `markerResults[[facet]][[g]]` | `measure` over `(<grouping-factor>, <fax>)` + uncertainty | `profile_pagoda2.R:87-91` markers_*; viewer@0.1 |

Note reductions land on lstar by their **own name** (`PCA`, `LSI`, `WNN`) with the source facet recorded
as provenance — there is no `type` token to map, and no facet-prefixed reduction key to invent. The
facet is the primary axis (one feature axis per facet); the reduction is a named coordinate axis. This is
exactly the shape `profile_seurat.R`'s reduction loop and other-assay loop already emit.

Three consequences:

1. **`write_pagoda2` (lstar `profile_pagoda2.R:32`) extends by iteration, not redesign.** It already
   emits `cells`/`genes` axes and `counts` over `(cells, genes)` (`profile_pagoda2.R:39-43`); the
   multimodal version loops `p2$facets`, adds one feature axis (`_modality_axis`: RNA→`genes`,
   ADT→`proteins`, ATAC→`peaks`) + measures per facet — the same shape the Python `mudata.py` modality
   loop (`profiles/mudata.py:43-104`) and `profile_seurat.R`'s other-assay loop (~`:287-305`) already
   produce — and emits per-facet membership as partial coverage. The viewer profile's per-cluster stats
   (`stats_<g>_*`, `markers_<g>_*` over the induced `(grouping-factor, <fax>)` axis pair,
   `profile_pagoda2.R:86-91`) are computed **per facet**. CITE-seq (RNA+ADT) already round-trips
   bit-identically through lstar — verified in `lstar/python/tests/test_mudata.py` — so a pagoda2.1 facet
   object lands on the *same* canonical shape. (Caveat from §0.2.8 / risk #7: the *mechanism* is done, but
   the **R** `profile_pagoda2.R` still needs the facet-loop wired in, and the pagoda2 profile has only
   been exercised on a *mock* pagoda2 object, not a real one — a corpus/profile gap, not a format gap.)

2. **Partial coverage is *implemented*, so it is on the happy path, not lossy-by-omission.** lstar's
   typed partial-coverage `index` arrays — an integer index into a span axis, `coverage="partial"`, no
   `cells.<mod>` axis and no zero/NA-padding — are implemented across Python/C++/R (`SUPPORT.md:55`), and
   `lstar/python/tests/test_partial.py` asserts a 60-of-100-cells × 8-proteins measure round-trips with
   its `index` and validates. The MuData profile already routes cell-subset overlap through it
   (`profiles/mudata.py:62-69`, `index=midx, index_axis="cells"`), keeping the `cells.<mod>` axis only for
   genuinely out-of-union labels. So the multiome case is straightforward:
   - **`write_pagoda2` per facet:** when a facet's membership mask is not all-TRUE, emit its measures over
     the shared `cells` axis with `index = which(mask)` (the covered positions) and `index_axis="cells"`.
     The membership mask of §4.3 *is* that index — no auxiliary axis, no fallback, no switch-over.
   - **The fast path is unchanged:** an all-TRUE mask emits full coverage, byte-identical to the
     single-RNA store today.
   The remaining lstar work is therefore **profile + corpus**, not format: wire facet iteration into the R
   `profile_pagoda2.R` (which already emits the single-facet shape and the viewer DE/markers over a factor
   axis), and validate against a **real** multi-facet pagoda2 object — the profile has only seen a *mock*
   one (`SUPPORT.md` Conos/pagoda2 table, gaps #2 and #5). Track that; the format mechanism is done.

3. **Import (`pagoda2FromAnnData`, `pagoda2From10x`, future `pagoda2FromLstar`) gains a facet-mapping
   arg**, not a new code path. Route through lstar's reader so the canonical feature-axis vocabulary
   (`genes`/`proteins`/`peaks`) and the typed `index` partial-overlap handling come for free:

```r
pagoda2FromLstar(path)                                   # all feature axes -> facets, cells -> canonical
Pagoda2$from(path, facets = c("RNA","ADT"))              # select a subset of modalities
pagoda2FromAnnData(path, facet.map = list(RNA="X", ADT="obsm/protein"))   # proposal1's mapping, kept
```

The 10x-multiome barcode-translation problem (GEX vs ATAC whitelists) is handled at import: build the
canonical axis from the translated barcodes and set each facet's membership against it — which is exactly
why the membership mask must exist rather than assuming identical row names, and why it maps to a typed
partial-coverage `index` on export. (This is **Phase 2b** per §0.4.1 — multiome/ATAC follows the
ADT-first release.)

---

## 8. Method signatures (delta from today)

`facet=` is a **real, defaulted argument** (default = `defaultFacet` = `"RNA"`) — not sugar over `type`.
`type=` is **demoted** to a rarely-used optional reduction override and renamed `reduction=` going
forward, with `type=` retained as a deprecated alias (§10). Almost everything is additive and defaulted.

```r
# unchanged single-RNA call -> unchanged behavior. Steps are GENERIC with method= defaults:
#   runGraph (method="knn"), runClustering (method="leiden"), runEmbedding (method="UMAP").
#   There is no runLeiden / runUMAP / runWNN — the algorithm is a method=, never its own function.
p2$runReduction(); p2$runGraph(); p2$runClustering(); p2$findMarkers(grouping="leiden")

# facet-explicit: facet= is a genuine argument, resolved against p2$facets
p2$runVariance(facet="ADT")                          # operates on facets$ADT analysis view (CLR)
p2$runReduction(facet="ADT")                         # ADT's defaultReduction=pca -> reductions[["ADT:PCA"]]; loadings -> facets$ADT$loadings[["PCA"]]
p2$runReduction(facet="ATAC")                        # uses ATAC's defaultReduction = "LSI" -> reductions[["ATAC:LSI"]]; no `type`
# (there is no runPCA/runLSI: PCA is just runReduction's default method on RNA, same as leiden/umap/knn)
p2$findMarkers(grouping="cell_type", facet="ADT")    # markerResults[["ADT"]][["cell_type"]]
p2$getExpression("CD3", facet="ADT")
p2$getExpression("ADT:CD3")                          # qualified name == facet="ADT" for this call
p2$plotEmbedding(color="MS4A1", facet="RNA")

# runGraph IS the facet-integration step; facets= defaults to ALL available facets; method= picks the
# integration algorithm. Single-facet is the degenerate case. WNN/totalVI/MOFA are methods, not functions.
p2$runGraph()                                        # multi-facet object: integrate all facets (default method)
p2$runGraph(facets=c("RNA","ADT"), method="wnn")     # -> graphs[["WNN"]] (+ reductions[["WNN"]], weights)
p2$runGraph(facets="RNA")                            # restrict to one facet: plain kNN on RNA's reduction
p2$runReduction(facets=c("RNA","ADT"), method="mofa")# reduction-level integration -> reductions[["MOFA"]]
p2$runClustering(method="leiden", graph="WNN")       # generic clustering; leiden default; on the named graph
p2$runEmbedding(graph="WNN", name="UMAP")            # -> embeddings[["WNN:UMAP"]]
attr(p2$graphs$WNN, "facets")   # c("RNA","ADT")  -- provenance, not a key

# the demoted reduction override (power user, rare): only when a facet has >1 reduction
p2$runGraph(facets="RNA", reduction="CCA")           # else reduction defaults to facet's defaultReduction

# facet management
p2$addFacet("ADT", countMatrix, modelType="clr", featureType="protein", defaultReduction="PCA")
p2$listFacets(); p2$defaultFacet <- "RNA"
```

**Resolver precedence (deterministic; the pivot removes the worst ambiguity).** Because facet is no
longer encoded in `type`, the "is `PCA` a reduction or a facet?" question simply *cannot arise* — `PCA`
is only ever a reduction name; a facet is whatever is in `p2$facets`. The remaining resolution is the
feature/grouping/facet name lookup, with this order:

1. Explicit `facet=` (or a qualified `facet:feature` name) → that facet.
2. A name matching a `p2$facets` key, *only* where a facet is syntactically expected (e.g. the first key
   of a legacy `markerResults[[...]]` access) → that facet.
3. A `cellMeta` column name → cell metadata (grouping/QC).
4. A feature in the default facet, then other facets (error on cross-facet ambiguity, demand `facet=`).

The qualified one-off syntax `facet:feature` (`"ADT:CD3"`) sets the facet for a single plot/lookup
without mutating `defaultFacet` — cheap, recommended, and the clean answer to default-facet drift.

New/changed internals (small, localized):
- `resolveFacet(facet)` → a `Facet`; `resolveReduction(facet, reduction)` defaults to the facet's
  `defaultReduction`. *Two small resolvers, each owning one concept* — replacing the prior draft's single
  overloaded `resolveMatrixContext(type)`.
- `.pagoda2_facet_set_count_matrix(facet, …)`; top-level `setCountMatrix` delegates to the default facet.
- active bindings `rawCounts`/`matrixViews`/`geneMeta`/`depth`/`varinfo`/`odgenes` → default facet.
- the §8.5 accessor contract (`getRawCounts`/`getExpression`/`getReduction`/`getEmbedding`/
  `getClustering`/`getOdGenes`/`getVarInfo`/`getDepth`/`listFacets`/`getFacet`), facet-aware.
- `clr`/`tfidf` branches in the materializer, kernel-arg builder, and `viewKernelValue` (§6.2).
- markers/DE/graph/embedding storage keyed by **facet** from the start (with reduction as an optional
  secondary key for graphs/embeddings); no legacy-read shims (§0.3, §10).

Everything else — kernels, marker selection (`markers.R`), plotting selection logic — is untouched
because it consumes `(raw, view)` pairs and now resolves facet-keyed registries internally.

---

## 8.5 The accessor contract and downstream consumers (conos, cacoa)

The clean break (§0.3) is only safe if the *coupling surface* is small, explicit, and stable. pagoda2's
two active downstream consumers are `conos` and `cacoa`, and the way they couple to pagoda2 dictates the
contract.

### 8.5.1 How the consumers actually couple (audited)

**conos** (`kharchenkolab/conos`) wires a *collection* of per-sample objects. It already routes most access
through an adapter layer, `conos/R/access_wrappers.R`, of `setGeneric`/`setMethod` accessors —
`getCountMatrix`, `getRawCountMatrix`, `getPca`, `getOverdispersedGenes`, `getCellNames`, `getGenes`,
`getEmbedding`, `getClustering`, `getGeneExpression`, `edgeMat<-/edgeMat`. Critically, several of these
prefer a *method* on the sample and fall back to a *field* only if the method is absent:

- `getCountMatrix → getExpressionBlock()` else `sample$counts` (`access_wrappers.R:29-33`)
- `getRawCountMatrix → getRawCounts()` else `sample$misc$rawCounts` (`:44-48`)
- `getOverdispersedGenes → sample$getOdGenes(n.odgenes)` (`:85`)

But three accessors still reach into fields *directly*, with no method indirection:

- variance scaling reads `x$misc$varinfo[od.genes,]$qv` and `$v` directly (`conos.R:22,38`)
- `getPca → sample$reductions$PCA` directly (`:9`)
- `getEmbedding → sample$embeddings$PCA[[type]]`; `getClustering → sample$clusters$PCA[[type]]`
  (`:358`, `:393`)
- `edgeMat` reads/**writes** `sample$misc$edgeMat` (`:157,183`)

**cacoa** (`kharchenkolab/cacoa`, `dev_lm`) is *collection-level only*. It never touches a pagoda2 object
directly; it talks to the Conos object and to conos's generics: `lapply(object$samples,
conos::getRawCountMatrix, ...)` (`access_wrappers.R:83`), `conos::getGeneExpression(object, gene)`
(`:281`), and `self$getJointCountMatrix(raw=)` (`cacoa.R:564,566,4063,4283`). Its pagoda2 dependency is
therefore **entirely transitive through conos's accessor layer**.

**The consequence is decisive:** if pagoda2.1 supplies the full accessor *method* set (so conos never hits
a field fallback) and conos's `access_wrappers.R` is updated to call those methods (including for `varinfo`,
`reductions`, `embeddings`, `clusters`, `edgeMat`), then the facet refactor is invisible to both packages.
cacoa needs no pagoda2-facing change at all — only conos's generics must learn `facet=`.

### 8.5.2 The contract pagoda2.1 must expose

A facet-aware accessor for every field a consumer reads, `facet = defaultFacet` throughout:

```r
p2$getRawCounts(cells=NULL, genes=NULL, orientation=c("cell_by_gene","gene_by_cell"), facet=NULL)
p2$getExpression(genes=NULL, orientation=..., facet=NULL)   # materialized analysis view
p2$getReduction(name=NULL, facet=NULL)                       # default = facet's defaultReduction
p2$getEmbedding(name=NULL, reduction=NULL, facet=NULL)
p2$getClustering(grouping=NULL)                              # cell-level, facet-agnostic
p2$getOdGenes(n=NULL, facet=NULL)
p2$getVarInfo(facet=NULL)                                    # cols: gsf, qv, v  (conos depends on qv, v)
p2$getDepth(facet=NULL)
p2$listFacets(); p2$getFacet(name)
```

Two contract points worth stating explicitly:

- **`getVarInfo()` is part of the contract, not an internal.** conos reads `varinfo$qv`/`$v` directly today;
  under facets, varinfo moves to `facets$<F>$varinfo`. Rather than a field alias, expose `getVarInfo(facet)`
  and migrate `conos.R:22,38` onto it. The `qv`/`v`/`gsf` column names are the stable part of the contract.
- **`edgeMat` is consumer-owned scratch.** conos *writes* `misc$edgeMat` into the sample. Keep a
  general-purpose per-object scratch store (`p2$misc` or an explicit `p2$scratch`) so conos can stash the
  per-sample edge matrix without pagoda2 knowing its shape.

### 8.5.3 Facets compose under collections — the two-level hierarchy

This is the piece the proposal otherwise leaves implicit, and it is exactly the lstar stance: **alignment is
legitimate only *within* a sample (facets share the cell axis); *across* samples you keep a graph-joined
collection, never a concatenated tensor** (`lstar/docs/principles.md:47-68`, `model.md:204-238`). lstar even
uses the word "facet" for within-sample modalities (`model.md:232`), so the vocabulary already lines up:

| layer | unit | lstar `kind` |
|---|---|---|
| within-sample modalities | `facet` (one feature axis: genes/proteins/peaks) | feature axes of a `sample` |
| one biological sample | a `Pagoda2` object | `sample` |
| cross-sample integration | a `Conos` object | `collection` |

The full composition is therefore **Conos collection → multi-facet Pagoda2 samples → facets**. Two design
questions this raises, both of which we *scope now and defer the algorithms*:

1. **Joint count matrix is facet-ambiguous.** cacoa's `getJointCountMatrix()` and conos's gene-intersection
   must take a `facet=` (default RNA), since "the counts" of a multi-facet sample is no longer unique.
   conos's cross-sample gene intersection (`commonOverdispersedGenes`, `conos.R:107`) is inherently
   per-feature-axis: you intersect genes across the RNA facets, peaks across the ATAC facets — never across
   facets.
2. **Cross-sample multimodal integration is the collection-level analogue of WNN.** Within a sample, a joint
   reduction (WNN/MOFA) is a §5 *named product*. Across samples, the natural shape is integrate-per-facet
   (RNA across samples, ADT across samples) then a joint — conos's job, not pagoda2's. We **draw the
   boundary** here rather than assume it: pagoda2.1 owns within-sample facets and within-sample named
   products; conos owns cross-sample integration, which becomes facet-parameterized. cacoa's analyses then
   split cleanly — *compositional* analysis is facet-agnostic (it is over cell groups), *expression-shift /
   cluster-free DE* is per-facet. (cacoa already carries a latent "which assay" notion — `object@misc$assay.name`,
   `access_wrappers.R:89` — which maps directly onto `facet`.)

---

## 8.6 Recommendations for conos: storage-backed collections at scale

**We do not propose changing conos now** (per the standing instruction). This section records the *target*
shape and the implications a future conos update must satisfy, derived by playing one concrete scenario all
the way through. The scenario stress-tests the §8.5 accessor contract against the hardest realistic case.

**Scenario.** An atlas-scale collection — say 500 samples, ~50M cells, multi-facet (RNA + ADT, or RNA +
ATAC multiome) — that we **never** want to hold in memory. Each sample is a storage-backed pagoda2.1 object
(its facets' raw counts live in a Zarr/lstar store, normalization is a view recipe applied on the
fly — exactly the disk-backed backends already benchmarked on `devel`). conos integrates the collection;
cacoa runs case-control on top. Walk it end to end.

### 8.6.1 OODA pass 1 — the loading & accessor layer

**Observe.** conos holds `con$samples` as a list of per-sample objects and, through `access_wrappers.R`,
pulls from each: an od-gene expression *block* (`scaledMatricesP2`, `conos.R:18-45` —
`getExpression(r, genes = od.genes, transposed = TRUE)`), per-gene variance (`varinfo$qv`/`$v`,
`conos.R:22,38`), a per-sample reduction (`reductions[[data.type]]`), and — only for DE — raw counts
stacked across samples (`rawMatricesWithCommonGenes`, `de_functions.R:78-100`, ending in
`t(do.call(rbind, mats))`). cacoa adds `getJointCountMatrix(raw=)` (`access_wrappers.R:345`).

**Orient.** The decisive observation: conos's per-sample working set is *already bounded* — it slices the
**od-gene block** (`ncells_s × |od.genes|`, ~3k genes) and reduces each sample to a covariance / PCA
(`conos.R:162-300`), one or two samples live at a time. It does **not** need a sample's full matrix in
memory. There are exactly **two** genuine full-collection materializations: (a) `rawMatricesWithCommonGenes`
stacking raw counts for pseudobulk DE, and (b) cacoa's `getJointCountMatrix`. Both are *aggregations*
(grouped sums over a clustering), not algorithms that need the dense joint matrix — so both can be streamed.

**Decide.** The accessor contract must let conos get (i) small per-sample reductions (already fine),
(ii) **bounded od-gene blocks via a streaming block reader**, and (iii) **streaming grouped aggregations**
(`viewColSumByFac(grouping, facet)`) — and must *not* force any whole-matrix return. `getCountMatrix` /
`getJointCountMatrix` that return a full materialized matrix become **small-data-only** conveniences; the
scale path is block + streaming-aggregate. The membership-mask/`index` machinery (§4.3, §7) means a
storage-backed facet reads its od-gene block by `(cells ∩ membership, od.genes)` without densifying.

**Act (accessor implications).** pagoda2.1 exposes, backend-agnostically and **lazily**:
- `getExpressionBlock(genes, cells, orientation, facet)` — a streaming block read from the store; bounded
  to the requested submatrix, normalization recipe applied per block. This is conos's `scaledMatricesP2`
  input and the one materialization unit at scale (one sample's od-gene block, transient).
- `viewColSumByFac(grouping, facet)` / `viewColMeanVar(facet)` — streaming grouped/columnar reducers that
  return `(group × gene)` or `(gene)` summaries without ever stacking matrices. These replace
  `rawMatricesWithCommonGenes` for pseudobulk.
- `getVarInfo(facet)`, `getReduction(facet)`, `getOdGenes(facet)` — small, in-memory, fine.
- `getCountHandle(facet)` — an opaque lazy handle (Zarr/lstar) supporting block extraction +
  streaming reduction, for callers that want to drive their own out-of-core loop. `getCountMatrix` that
  materializes is explicitly the small-data fallback.

### 8.6.2 OODA pass 2 — the algorithm methods and the memory ceiling

**Observe.** Past loading, conos: computes per-sample PCA/CPCA/CCA on od-gene blocks/covariances; builds a
joint kNN graph (N2R) over the **stacked reduced space** (cells × n.pcs); runs Leiden on the graph; lays
out a joint embedding (largeVis/UMAP) on the graph; and computes DE/pseudobulk per joint cluster. cacoa
then computes compositional shifts (over cell groups — counts of cells per (sample, cluster)), expression
distances, and cluster-free DE.

**Orient.** Classify every step by its memory behaviour:
- **Reduction-based (already bounded):** per-sample PCA/CPCA/CCA (od-gene block or covariance, 1–2 samples
  at a time); Leiden (sparse graph); these are fine out-of-core with no change beyond lazy block reads.
- **The one large persistent structure:** the stacked reduced space `cells × n.pcs` (50M × 100 ≈ 20 GB)
  that N2R indexes, and the joint kNN graph itself (50M × k edges, sparse). Recommendation: the reduced
  space should itself be **storage-backed / memory-mapped** (it is a derived `embedding` over
  `(cells, pca)` — an lstar field), and the graph spill-able. N2R consuming a memory-mapped reduced matrix
  is the key enabler; the graph is sparse and disk-friendly.
- **Streaming-aggregation (must be refactored from materialization):** pseudobulk DE
  (`rawMatricesWithCommonGenes` → per-sample `viewColSumByFac` accumulated into `(cluster × gene)`),
  cacoa compositional analysis (cell counts per `(sample, cluster)` — a streaming `table()`, never the
  matrix), cacoa expression distances and cluster-free DE (per-cluster / per-sample aggregates). None of
  these needs the dense joint matrix.
- **Full-materialization to forbid at scale:** `getJointCountMatrix()` with no grouping, and any
  `do.call(rbind, raw matrices)`. These stay as small-data conveniences with a guard that errors (or warns
  and falls back to streaming) above a cell-count threshold.

**Decide.** The lasting principle, which the contract should *enforce*: **at collection scale every method
is either (a) an operation on small per-sample/per-cluster reductions, or (b) a streaming grouped
aggregation over storage-backed counts — never a full-matrix materialization.** This is lstar's
"lazy & streaming, bounded memory, float64-accumulation-over-float32-storage" principle and pagoda2.1's
view-kernel design, lifted to the collection level. Because the accessor is the only way in, a consumer
*cannot* accidentally densify 50M × 20k.

**Act (algorithm implications + the backing store).** The whole storage-backed collection **is** an lstar
`kind="collection"` store (principles.md §2; lstar already ingests a real Conos object as a collection,
`SUPPORT.md:166`): per-sample `cells.{s}`/`genes.{s}` axes + lazy `counts.{s}` measures, a union `cells`
axis, a `sample` design label, the joint graph as a `relation` over `(cells, cells)`, joint clusters as a
`label`, the joint embedding as an `embedding`. conos then holds only the **integration layer** (per-sample
reductions, joint graph, clusters, embedding) plus **lazy handles** to per-sample storage-backed counts —
not a single dense matrix. Per-facet integration (RNA across samples, ADT across samples) parameterizes the
whole pipeline by `facet`; a cross-sample joint is the collection-level analogue of the §5 named product.

### 8.6.3 Concrete recommendations for conos (for the future update, not now)

1. **Migrate `access_wrappers.R` to the §8.5 accessor methods** (kill direct `misc$varinfo`/`reductions`/
   `embeddings`/`clusters` reads), and thread `facet=` through every generic and through
   `getJointCountMatrix`/`commonOverdispersedGenes` (gene intersection is per-feature-axis — never across
   facets).
2. **Make the count accessors lazy-capable**: `getExpressionBlock` streams; `getCountMatrix`/
   `getJointCountMatrix` whole-matrix returns gain a size guard and a streaming alternative.
3. **Refactor the two full-materialization sites** (`rawMatricesWithCommonGenes`, `getJointCountMatrix`)
   into streaming grouped aggregations — `viewColSumByFac` within a sample, and lstar's
   `collection_pseudobulk(ds, factor, field, lognorm)` (§8.6.4 S2) across the collection — so DE/pseudobulk
   and cacoa aggregates never stack matrices.
4. **Back the collection with an lstar collection store** and keep only the integration layer + lazy
   handles in memory; store the reduced space memory-mapped.
5. **cacoa rides along**: its analyses are aggregations over cell groups (compositional = facet-agnostic;
   expression-shift / cluster-free DE = per-facet streaming aggregates), so once conos's generics are lazy
   and facet-aware, cacoa needs only `facet=` plumbing and no full-matrix assumption.

### 8.6.4 lstar response & status — S1–S5 mostly implemented (see `lstar/misc/response.md`)

These started as asks (L1–L5); lstar implemented most of them. **Build the Conos work against the
concrete forms below**, not against the original asks.

- **(S1 — recipe params, DONE; it was *broken*, now fixed).** Persist the recipe **parameters** as
  provenance on the **raw** measure — lstar does *not* compute the view (the "virtual recipe field" idea
  stays retracted). Critical catch lstar found: the R cpp11 binding **silently dropped field provenance**,
  so `pagoda2 → lstar(R) → pagoda2` returned `NULL` — the recipe would have been lost on exactly our path.
  Now carried across R as an **opaque JSON string** (`jsonlite::fromJSON`/`toJSON(auto_unbox=TRUE)`),
  guarded by `conformance/provenance.sh`. **Correction we must honor:** only the *small* scalars
  (`model`, `depthScale`, `log_base`, `winsor_caps`) go in provenance; the *large* precomputed per-axis
  vectors (CLR per-row divisor, length n_cells; per-column IDF, length n_genes) must **not** bloat the
  metadata dict — store them as **arity-1 `recipe_scalar` fields** (a `measure` over `cells`/`genes`,
  `subtype="recipe_scalar"`) so they stream/chunk/compress and feed the kernel directly as the `r[]`/`c[]`
  vectors (S4b). So pagoda2 export = a few scalars in provenance + (for byte-exact repro) two `recipe_scalar`
  fields. *(S1′ typed vocabulary: not done; opaque provenance is lossless; revisit with S4b.)*
- **(S2 — collection grouped reducer, DONE, Python).** `lstar.collection_pseudobulk(ds, factor,
  field="counts", lognorm=FALSE)` is the form to build the storage-backed Conos path against (§8.6.2):
  `factor` is the joint clustering `label` over the **union** `cells` axis; it **walks per-sample measures
  one at a time** (bounded by the largest sample, not the union), maps each sample's cells to union
  positions, accumulates in **float64**, and **label-aligns genes** across samples (absent gene → 0).
  Output `pb.<factor>.{mean,frac}` over `(factor, genes)`. Verified streamed == materialized reference for
  shared *and differing* per-sample gene sets (`test_collection_reduce.py`). This replaces conos's
  `rawMatricesWithCommonGenes` stacking. *Coordinated next step:* a truly **block-streaming** variant (each
  sample itself read in column blocks from a backed source) slots onto the same signature — lstar builds it
  *with* us against a concrete on-disk Conos store + benchmark.
- **(S3 — `index` over a derived union axis, DONE; it already worked).** A partial measure whose
  `index_axis` is a **derived** collection union axis validates and round-trips (`index` is keyed by axis
  name, no observed-vs-derived assumption); a facet membership mask maps 1:1. Locked by
  `conformance/partial.sh`. Nothing to work around.
- **(S4a — determinism contract, DONE, taken).** `mean`/`var`/`nnz` are **bit-identical across 1/2/4/8
  threads** (tested `== 0`, not tolerance), float64 accumulation, column-parallel, no cross-thread
  reduction (`principles.md §5`; `conformance/stream_reduce.sh`, `test_determinism.py`). A facet's
  summaries are identical whichever library computes them.
- **(S4b — one shared core, DECISION: don't merge; generalize the signature).** lstar agrees the
  scalars+enum boundary is right but will **not** merge codebases (a merge couples pagoda2's hot path to
  libstar's release cycle for low immediate gain; `misc2.cpp` works today). Instead libstar will generalize
  its reducer toward the `(raw block, r[] per-row, c[] per-col, transform enum t)` signature (the fused
  `depth+log1p` reducer is already half there); pagoda2 can call it **optionally, header-only**, feeding the
  `r[]`/`c[]` it computed (CLR divisor / IDF, stored as S1 `recipe_scalar` fields), and the core never
  learns "CLR." **Repos stay separate.** Not built yet — it's gated on a real consumer (our Conos work) + a
  benchmark, to be **sequenced with us**. On the DE caution: lstar half-agrees — its DE is interchange
  *typing* (it represents scanpy's already-computed `rank_genes_groups` as a `(factor,genes)` bundle, it
  does **not** run a test) and `pseudobulk` is a general primitive; the line it will hold is **never
  implement specific statistical tests in the core**.
- **(S5 — facet-set provenance, DONE, was gated on S1).** Convention: a joint product's embedding records
  its contributing **feature-axis names** in `provenance["input_axes"]` (the MuData profile auto-populates
  by inference — the feature axes carrying a `loading` over the embedding's factor axis). **Our side:** when
  writing `reductions[["WNN"]]` (facets `RNA`,`ADT`), set `provenance$input_axes <- c("genes","proteins")`
  — note it is keyed by **feature-axis** names (genes/proteins), not facet labels — and it round-trips.

**Net for the Conos work:** use today — `collection_pseudobulk` (S2), `index` over the union axis (S3),
recipe-as-provenance + `recipe_scalar` fields (S1), `input_axes` on joint reductions (S5), the determinism
contract (S4a). **Coordinate before building** — the block-streaming reducer (S2 next step) and the
generalized `(r[],c[],enum)` kernel (S4b), both lstar-side, gated on our concrete store + a benchmark.
**Not happening** — merging codebases, computing normalization in lstar, or statistical tests in the core.

---

## 9. Framework survey (how the field formulates it)

| framework | keys cells across modalities | partial overlap | shared vs per-modality cell meta | joint reductions | per-modality features | verdict |
|---|---|---|---|---|---|---|
| **MuData / muon** | global `obs_names`; identical name = same cell | `obsmap` int vector, **0 = absent**; modality gets aligned slice | global `.obs` mirrors per-mod with `mod:col` prefix; merged if same name | container-level `.obsm` (WNN UMAP, MOFA) | per-mod `.var`, names unique *within* modality | Cleanest mental model; the `.update()` sync between global and per-mod obs is clumsy and error-prone |
| **Seurat v5** | one object; `Assay5` has its own `cells`/`features` membership map; `colnames()` = union | native: a layer covers a subset of the assay's cell map | `meta.data` global; `DefaultAssay` selects context | named `DimReduc` per assay + WNN graph/`wnn.umap`; `Idents` active | one `Assay`/`Assay5` per modality; `[[ ]]` registry | Most flexible cell membership; WNN is first-class. Verbose; assay-vs-object cell scoping is a known footgun |
| **SCE + altExps** | `altExp` shares the *same columns* (cells) as main SCE | altExps must align to main cells (strict) | `colData` global; `rowData` per (alt)exp | `reducedDims` (+`rotation` loadings) on main | main `rowData` + per-`altExp` features | Simple, but altExps are second-class (strict cell alignment; no partial overlap) |
| **MultiAssayExperiment** | `sampleMap` (assay, primary, colname) relation | native: a `primary` maps to 0/1/many colnames per assay | `colData` over *biological units*, not assay columns | not its job (harmonization layer, not analysis) | per-`ExperimentList` element | The gold standard for *partial overlap/replicates*; sampleMap = lstar `relation`. Heavyweight for the aligned common case |
| **TileDB-SOMA** | shared `obs` keyed by `soma_joinid`; `ms` = measurements | per-measurement obs filtering via the join id | `obs` global; per-`ms` `var`/`obsm`/`obsp` | per-measurement `obsm`/`obsp` | per-measurement `var` | Storage-grade, scales; one shared obs + N measurements = exactly this proposal's shape |
| **Signac/ArchR (ATAC)** | ChromatinAssay sits beside RNA assay (Seurat) | inherits Seurat's | Seurat `meta.data` | LSI reduction; WNN with RNA | peaks/bins + fragments + ranges | Peaks-as-features works today; genomic ranges/fragments are extra per-feature metadata (lstar records, not yet typed) |
| **lstar** (interchange) | one shared `cells` axis; namespaced per sample | `cells.<mod>` axis today; typed `index` later (spec'd, unimpl.) | cell fields over `cells`; feature fields per feature axis | embedding over `(cells, factor)` + per-facet loadings; induction rules | one feature axis per modality (`genes`/`proteins`/`peaks`) | The target. Everything above is a profile onto it |

**Convergent finding #1 — the shape.** Every serious framework is *the same shape*: **one shared
observation axis + N feature/measurement axes, measures over `(obs, feature-axis)`**. They differ only in
(a) partial-overlap encoding (obsmap-0 vs Assay5 cell-map vs sampleMap-relation vs strict-align),
(b) whether per-modality cell metadata is mirrored globally (MuData's `.update()` is the clumsiest part
of any of them), and (c) where joint reductions sit.

**Convergent finding #2 — nobody keys results by reduction type (this is the pivot's external
warrant).** Look at the "keys cells across modalities" and "per-modality features" columns: every
framework keys its core registries by **modality / assay / measurement** — MuData `.mod`, Seurat
`Assays`/`DefaultAssay`, SCE `altExps`, MAE `ExperimentList`, SOMA `ms`. The reduction type is
*universally* a secondary attribute (a named `DimReduc`, a `reducedDims` slot, an `obsm` key), **never**
the primary discriminator. None of them has anything resembling a "key everything by which reduction you
ran" axis. That consensus is exactly why this revision makes **facet primary and `type` vestigial**: the
prior draft's "generalize the `type` token" was the one design no surveyed framework adopts.

**Therefore pagoda2.1 should adopt the lstar shape (the SOMA/MuData/Seurat-v5 consensus) and pick the
best of each: SOMA's single shared obs (no `.update()` mirror mess), MuData's `obsmap`-style membership
for partial overlap, Seurat's `DefaultAssay` ergonomics — with the modality (facet) as the first-class,
primary key, and the reduction implied by the facet.**

---

## 10. Clean break, rollout, and the accessor contract (no pagoda2 ≤1.x compatibility)

Per §0.3 we **do not** preserve pagoda2 ≤1.x object layouts or direct field access. This section is
therefore about *sequencing the build* and *stabilizing the §8.5 contract* — not about load-time shims.
There is no `"counts"` sentinel, no per-registry legacy-read shim, and no `type=`-deprecated-alias
machinery (all removed; former risk #2 is gone). Registries are facet-keyed from the first commit.

### 10.1 Rollout

**Each phase is gated by a named test set** in `misc/multimodal_testing_plan.md` (OODA'd per phase against
the lstar multi-omic corpus, imported via the lstar-mediated path; round-trip + `identical()`-across-threads
invariance are the recurring gates). Phase 0's gate is that the **existing single-RNA suite passes
unmodified** — the proof the facet refactor is invisible to single-RNA users.

- **Phase 0 — structure.** Introduce `Facet` (with `defaultReduction`), `facets`, `defaultFacet`,
  `p2$cells`, and the delegating active bindings (`rawCounts`/`matrixViews`/`geneMeta`/`depth`/`varinfo`/
  `odgenes` → default facet) *as ergonomics*. The constructor builds `facets[["RNA"]]` and points
  `defaultFacet` at it. Bump `apiVersion`; rewrite the package's own tests to the new layout (we own them).
- **Phase 1 — facet API.** `resolveFacet`/`resolveReduction`, `addFacet`, the genuine `facet=` argument,
  the `facet:feature` qualified syntax, `<metric>_<facet>` QC, and the **facet-keyed result registries**
  (count-space facet-keyed `markerResults[[facet]][[grouping]]`; cell-space name-keyed `graphs[["RNA"]]`/
  `graphs[["WNN"]]` with facets in provenance — §4.5; keyed this way from the start, no flattening shim),
  and the generic `runGraph`/`runClustering`/`runEmbedding` steps with `method=` defaults. Single-RNA
  users never see a facet argument.
- **Phase 1b — accessor contract + consumer port.** Land the full §8.5.2 accessor set
  (`getRawCounts`/`getExpression`/`getReduction`/`getEmbedding`/`getClustering`/`getOdGenes`/`getVarInfo`/
  `getDepth`/`listFacets`/`getFacet`), each facet-aware. Update `conos/R/access_wrappers.R` to call these
  methods (eliminating its direct `misc$varinfo`/`reductions$PCA`/`embeddings$PCA`/`clusters$PCA` reads)
  and to thread `facet=` into the collection-level generics; cacoa needs no pagoda2-facing change. **This
  phase is the contract freeze** — once conos/cacoa bind to the methods, internal layout is free to move.
- **Phase 2a — ADT / CITE-seq (the first multimodal release, §0.4.1).** The `clr` view model with the §6.2
  invariance gating tests; ADT end-to-end; union cell axis + complete-cases helper + `filterData` semantics
  (§0.4.3); **lstar-mediated** multimodal import (§0.4.2, lstar R loaded on demand) and export per facet,
  emitting typed `index` partial coverage from each facet's membership mask (mechanism implemented in lstar;
  the R `profile_pagoda2.R` needs facet iteration + real-object validation — §7); and the shipped **joint
  method, WNN on RNA+ADT** (named-product storage + algorithm, §0.4.4/§5). This is a coherent CITE-seq
  release on its own.
- **Phase 2b — ATAC (follow-up, §0.4.1).** `tfidf` view model + `LSI` reduction (`drop.first`, §6.3),
  genomic peak ranges in `featureMeta` + fragments-as-external-reference (§11.8), and 10x-multiome barcode
  translation feeding the membership mask / partial-coverage `index`.
- **Phase 3 — further joint methods.** MOFA+/totalVI/MultiVI on the reserved named-product shape, as
  demand warrants — storage already in place from Phase 2a, so these add algorithms without a schema change.

### 10.2 Legacy objects (optional, isolated)

Old `.rds` objects are *not* a constraint on the live design. If we choose to read them at all, it is via
a single, isolated, well-tested one-shot converter:

```r
p2 <- convertLegacy(old)   # wraps the legacy top-level matrix as facets[["RNA"]];
                           # remaps any stored type="counts"/reduction-keyed registries to facet-keyed.
```

`convertLegacy()` is the *only* place that knows the old grammar; it is not on any hot path and not woven
into accessors or constructors. The realistic default for most users is simpler still — re-run the
(fast) pipeline on raw counts. Either way, the messy double-duty sentinel that earlier drafts carried
through every registry does not exist.

---

## 11. Top risks & open questions

1. **Membership mask vs full alignment in the hot path.** Threading a `rowSel` everywhere risks
   slowing the 95%-aligned common case. *Mitigation:* a per-facet `aligned` flag short-circuits to the
   no-mask path (byte-identical to today). Must be enforced centrally in `resolveFacet`, not per call
   site.
2. **Consumer port is the real coupling risk (replaces the old migration risk).** The clean break (§0.3)
   removes the legacy-store migration tension entirely — registries are facet-keyed from the start. The
   risk that remains is the *one-time port* of `conos/R/access_wrappers.R` onto the §8.5 accessor contract:
   conos's direct field reads (`misc$varinfo`, `reductions$PCA`, `embeddings$PCA[[type]]`,
   `clusters$PCA[[type]]`, `conos.R:22,38`) must all become method calls, and the collection-level
   generics must thread `facet=`. *Mitigation:* land the full accessor set first (Phase 1b), keep the
   `varinfo` `qv`/`v`/`gsf` column contract stable, and port conos in lockstep; cacoa rides along because
   it only touches conos's generics. Until the port lands, conos against a facet object will break — so
   the contract freeze (Phase 1b) gates everything downstream.
3. **`filterData()` / QC over a union cell axis — RESOLVED (§0.4.3).** Canonical axis = union; `filterData`
   evaluates QC on the default facet over the canonical axis and intersects every facet's mask atomically;
   a cell dropped on RNA QC leaves the canonical axis entirely; `requireFacets=` provides complete-cases
   selection. Specified in §4.3. *Residual implementation note:* the mask re-derivation must be the single
   owner of axis mutation so a facet's `aligned` fast-path flag stays correct (risk #1).
4. **Cross-facet feature name collisions** (ADT `CD3` vs gene `CD3`). Erroring on ambiguity is correct
   but will annoy; needs a clear message + `facet=`. *Resolved:* the `facet:feature` qualified-name syntax
   (`"ADT:CD3"`) for one-off plotting without setting state — recommended, cheap, specified in §6.4/§8.
5. **CLR/TF-IDF thread-invariance under the new per-entry transforms.** The new view models must preserve
   the `misc2.cpp` float64-accumulation, column-parallel, thread-count-invariant contract. *Mitigation:*
   §6.2 proves both are pure per-entry functions of precomputed per-row (CLR divisor) / per-column (IDF)
   scalars, and ships the `identical()`-across-thread-counts + float64-reference gating tests. *Resolved:*
   LSI's "drop component 1" is a **reduction** post-step (`reductions[["LSI"]]`, `drop.first=TRUE`), not a
   view transform — keep the `tfidf` view pure (§6.3).
6. **Per-facet `batch`/`depth` divergence.** Usually shared, but a facet may have its own batch
   structure. Storing them per facet (as proposed) is correct; the open question is whether `cellMeta`
   should also surface a canonical `batch` — yes, with per-facet overrides resolved by facet.
7. **The lstar dependency is now a *profile + corpus* gap, not a format gap — and the format asks (S1–S5)
   are implemented (`lstar/misc/response.md`).** Partial-coverage `index` (incl. over a derived union axis),
   provenance round-trip across R (was broken, now fixed), the determinism contract, the collection grouped
   reducer (`collection_pseudobulk`), and `input_axes` provenance are all done and conformance-guarded
   (§8.6.4). The dependency that **remains** is narrow: the R `profile_pagoda2.R` must gain facet iteration,
   and the pagoda2 profile must be validated on a **real, multi-facet pagoda2 object** (today: mock only)
   plus a **real `.h5mu` multiome** in the corpus (`SUPPORT.md` gaps #2, #5); and two pieces are
   *coordinated, not speculative* — the block-streaming collection reducer and the generalized
   `(r[],c[],enum)` kernel, both gated on our concrete Conos store + a benchmark (§8.6.4 S2/S4b). *Mitigation:*
   the membership mask maps 1:1 to an `index`, so the R profile change is mechanical. (Note
   `lstar/docs/model.md:50` still reads "specified but not yet implemented" — that doc is **stale** vs
   `SUPPORT.md`/code/`response.md`; cite those.)
8. **Genomic ranges/fragments for ATAC** (Signac `ChromatinAssay`). lstar **already types** peak ranges
   (seqnames/start/end) and round-trips them on a real object (real `pbmcMultiome`, 108k peaks;
   `SUPPORT.md:128`), and **records** the external fragment file rather than inlining it. So the proposal's
   plan is corroborated, not speculative: pagoda2.1 holds peak ranges in `facets$ATAC$featureMeta` (typed)
   and treats fragments as an external resource reference — matching what lstar's Seurat profile does.

---

## 12. One-paragraph synthesis

Make a facet the existing `rawCounts`+`matrixViews`+`featureMeta`+`varinfo` bundle, register facets by
name with an RNA default, and share one canonical cell axis with per-facet boolean membership for partial
overlap. **Make `facet` a first-class, orthogonal argument and the primary key of every result registry,
and demote the `type` token** — the reduction-space selector pagoda2 has carried since inception and that
users essentially never deviate from PCA on — to a rarely-used `reduction=` override, with each facet
owning its own default reduction (RNA→PCA, ATAC→LSI) so the reduction is implied by the facet, not
surfaced as a knob. The C++ view kernels and the marker/plot machinery come along almost unchanged because
they consume `(raw, view)` pairs and a `rowSel`; the real new work is per-modality *normalization recipes*
(CLR, TF-IDF) as new `view$model` branches that provably preserve the kernels' thread-invariant float64
accumulation, plus a *named-product* convention (not a `type`) for WNN/MOFA storage (algorithms deferred,
shape reserved). The in-memory model is a one-to-one relabeling of lstar's shared-`cells`-axis +
per-facet-feature-axis shape, so the Zarr interchange round-trips losslessly by construction — with
partial overlap emitted as typed partial-coverage `index` (implemented in lstar Python/C++/R; a facet's
membership mask *is* the index), the only remaining lstar work being a facet loop in the R pagoda2 profile
and validation on a real object. We take a **clean break** from pagoda2 ≤1.x rather than preserving
direct field access (§0.3): the durable seam to the sister packages — `conos` (collections) and `cacoa`
(case-control, transitive through conos) — is a small, versioned, facet-aware **accessor contract**
(§8.5), not the object's field layout, so the facet refactor stays invisible to consumers and the design
is free to evolve.
