library(pagoda2)

# RNA with two clusters (structured) + extra noise facets; per-facet reductions at DIFFERENT dims
# (the faithful, bandwidth-normalized WNN must be dimensionality-robust).
build_wnn_p2 <- function(extra.noise = FALSE) {
  set.seed(12)
  ng <- 40
  nc <- 80
  grp <- rep(1:2, each = nc / 2)
  base <- matrix(rpois(ng * nc, 2), ng, nc)
  base[1:20, grp == 2] <- base[1:20, grp == 2] + rpois(20 * (nc / 2), 12)
  dimnames(base) <- list(paste0("g", seq_len(ng)), paste0("c", seq_len(nc)))
  p2 <- Pagoda2$new(as(Matrix::Matrix(base, sparse = TRUE), "dgCMatrix"), verbose = FALSE, n.cores = 1,
    min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  np <- 12
  adt <- matrix(rpois(nc * np, 4) + 1L, nc, np, dimnames = list(paste0("c", seq_len(nc)), paste0("P", seq_len(np))))
  p2$addFacet("ADT", as(Matrix::Matrix(adt, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "protein")
  suppressWarnings({
    p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(nPcs = 10, var.scale = FALSE, verbose = FALSE) # RNA: 10 dims
    p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(facet = "ADT", nPcs = 5, var.scale = FALSE, verbose = FALSE) # ADT: 5 dims (mismatched)
  })
  if (extra.noise) {
    nh <- 8
    hto <- matrix(rpois(nc * nh, 3) + 1L, nc, nh, dimnames = list(paste0("c", seq_len(nc)), paste0("H", seq_len(nh))))
    p2$addFacet("HTO", as(Matrix::Matrix(hto, sparse = TRUE), "dgCMatrix"), modelType = "plain", featureType = "feature")
    suppressWarnings({
      p2$runVariance(facet = "HTO", use.raw.variance = TRUE, verbose = FALSE)
      p2$runReduction(facet = "HTO", nPcs = 4, var.scale = FALSE, verbose = FALSE)
    })
  }
  p2
}

test_that("A: faithful WNN down-weights the noise modality even with mismatched reduction dims", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2()
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE))
  wr <- p2$cellMeta$wnn_weight_RNA
  wa <- p2$cellMeta$wnn_weight_ADT
  expect_equal(wr + wa, rep(1, length(wr)), tolerance = 1e-9) # weights sum to 1 per cell
  expect_gt(mean(wr), mean(wa)) # structured RNA up-weighted over noise ADT, despite 10 vs 5 dims
})

test_that("B+F: WNN builds a weighted SNN graph (igraph) with provenance + a joint reduction", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2()
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE))
  g <- p2$graphs[["WNN"]]
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 80)
  expect_true(igraph::is_weighted(g))
  expect_identical(attr(g, "facets"), c("RNA", "ADT"))
  expect_identical(attr(g, "method"), "wnn")
  expect_true("WNN" %in% names(p2$reductions))
  expect_identical(attr(p2$reductions[["WNN"]], "input_axes"), c("genes", "proteins"))
})

test_that("listReductions(long=TRUE) surfaces joint-reduction provenance; plain reductions report NA", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2()
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE))

  short <- p2$listReductions()
  expect_identical(colnames(short), c("name", "n.cells", "n.dims")) # default is unchanged (back-compat)

  long <- p2$listReductions(long = TRUE)
  expect_identical(colnames(long), c("name", "n.cells", "n.dims", "facets", "method", "input_axes"))
  expect_true(all(c("PCA", "ADT:PCA", "WNN") %in% long$name))

  w <- long[long$name == "WNN", ] # the joint reduction self-describes via its attrs
  expect_identical(w$facets, "RNA,ADT")
  expect_identical(w$method, "wnn")
  expect_identical(w$input_axes, "genes,proteins")

  pca <- long[long$name == "PCA", ] # a plain per-facet reduction has no provenance attrs -> NA
  expect_true(is.na(pca$facets) && is.na(pca$method) && is.na(pca$input_axes))
})

## capture only the clobber-guard warning, ignoring unrelated/benign warnings + messages
clobber_warned <- function(expr) {
  w <- character()
  withCallingHandlers(suppressMessages(expr),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  any(grepl("overwriting reduction", w))
}

test_that("getModalityWeights() returns name-scoped per-cell weights; survives multiple WNN joints (ask #1)", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2(extra.noise = TRUE) # RNA + ADT + HTO
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), name = "WNNra", verbose = FALSE))
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "HTO"), name = "WNNrh", verbose = FALSE))

  w1 <- p2$getModalityWeights("WNNra")
  expect_identical(colnames(w1), c("RNA", "ADT"))               # name-scoped: NOT clobbered by the WNNrh run
  expect_equal(unname(rowSums(w1)), rep(1, nrow(w1)), tolerance = 1e-9)
  w2 <- p2$getModalityWeights("WNNrh")
  expect_identical(colnames(w2), c("RNA", "HTO"))               # the second joint keeps its own weights

  expect_null(p2$getModalityWeights("PCA"))                     # a plain reduction has no weights -> NULL
  expect_null(p2$getModalityWeights("does_not_exist"))          # unknown name -> NULL (probe-friendly)

  p3 <- build_wnn_p2()                                          # reductions present, but no WNN run
  expect_error(p3$getModalityWeights(), "run runGraph")
})

test_that("subset-facet joints coexist; clobber-guard warns only on name reuse with different provenance (ask #3)", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2(extra.noise = TRUE)
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), name = "WNNra", verbose = FALSE))
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "HTO"), name = "WNNrh", verbose = FALSE))

  expect_true(all(c("WNNra", "WNNrh") %in% names(p2$reductions))) # two subset joints coexist
  expect_true(all(c("WNNra", "WNNrh") %in% names(p2$graphs)))
  expect_identical(attr(p2$reductions[["WNNra"]], "facets"), c("RNA", "ADT")) # distinct provenance
  expect_identical(attr(p2$reductions[["WNNrh"]], "facets"), c("RNA", "HTO"))

  # re-running the SAME facets under the SAME name is a legitimate refresh -> no clobber warning
  expect_false(clobber_warned(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), name = "WNNra", verbose = FALSE)))
  # reusing a name for a DIFFERENT facet set is almost always an accident -> warn
  expect_true(clobber_warned(p2$runGraph(method = "wnn", facets = c("RNA", "HTO"), name = "WNNra", verbose = FALSE)))
})

test_that("runGraph() auto-integrates all reduction-ready facets by default (§0.2.6)", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2()
  suppressWarnings(p2$runGraph(verbose = FALSE)) # bare: 2 facets ready -> auto-WNN
  expect_true("WNN" %in% names(p2$graphs))
  expect_true(all(c("wnn_weight_RNA", "wnn_weight_ADT") %in% colnames(p2$cellMeta)))
})

test_that("single-facet runGraph() stays a plain kNN (no auto-WNN); needs N2R", {
  skip_if_not_installed("N2R")
  cm <- Matrix::Matrix(matrix(rpois(15 * 40, 3), 15, 40, dimnames = list(paste0("g", 1:15), paste0("c", 1:40))), sparse = TRUE)
  p2 <- Pagoda2$new(as(cm, "dgCMatrix"), verbose = FALSE, n.cores = 1, min.cells.per.gene = 0, min.transcripts.per.cell = 0, trim = 0, log.scale = TRUE)
  suppressWarnings({
    p2$runVariance(use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(nPcs = 5, var.scale = FALSE, verbose = FALSE)
    p2$runGraph(verbose = FALSE)
  })
  expect_false("WNN" %in% names(p2$graphs)) # single facet -> kNN, not WNN
  expect_true("PCA" %in% names(p2$graphs))
})

test_that("E: WNN generalizes to >= 3 facets (weights still sum to 1; graph built)", {
  skip_if_not_installed("RcppHNSW")
  p2 <- build_wnn_p2(extra.noise = TRUE)
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT", "HTO"), verbose = FALSE))
  w <- cbind(p2$cellMeta$wnn_weight_RNA, p2$cellMeta$wnn_weight_ADT, p2$cellMeta$wnn_weight_HTO)
  expect_equal(rowSums(w), rep(1, nrow(w)), tolerance = 1e-9)
  expect_gt(mean(w[, 1]), mean(w[, 2])) # RNA still beats a noise modality
  expect_equal(igraph::vcount(p2$graphs[["WNN"]]), 80)
})

test_that("E: end-to-end WNN -> clustering on the WSNN graph + embedding on the joint reduction", {
  skip_if_not_installed("RcppHNSW")
  skip_if_not_installed("leidenAlg")
  skip_if_not_installed("uwot")
  p2 <- build_wnn_p2()
  suppressWarnings(p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE))
  p2$runClustering(graph = "WNN", name = "wnn_leiden")
  expect_true("wnn_leiden" %in% colnames(p2$cellMeta))
  expect_gt(length(unique(na.omit(p2$cellMeta$wnn_leiden))), 1L) # the structured data yields >1 cluster
  suppressWarnings(p2$runEmbedding(reduction = "WNN", name = "umap", verbose = FALSE))
  expect_true("WNN" %in% names(p2$embeddings))
})

test_that("vs-Seurat: per-cell WNN weights rank-correlate with Seurat::FindMultiModalNeighbors (real CITE-seq)", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("RcppHNSW")
  h5 <- Sys.getenv("P21_CITESEQ_10X_H5", "/home/pkharchenko/p21/lstar/testdata/citeseq_10x/pbmc_1k_protein_v3.h5")
  skip_if(!file.exists(h5), "real 10x CITE-seq fixture not present")

  # pagoda2 WNN on real RNA+ADT
  p2 <- pagoda2:::.pagoda2_from_10x_h5_multimodal(h5, verbose = FALSE)
  suppressWarnings({
    p2$runVariance(verbose = FALSE)
    p2$runReduction(nPcs = 20, verbose = FALSE)
    p2$runVariance(facet = "ADT", use.raw.variance = TRUE, verbose = FALSE)
    p2$runReduction(facet = "ADT", nPcs = 10, verbose = FALSE)
    p2$runGraph(method = "wnn", facets = c("RNA", "ADT"), verbose = FALSE)
  })
  rna <- p2$reductions[["PCA"]]
  adtr <- p2$reductions[["ADT:PCA"]]
  cells <- rownames(rna)
  colnames(rna) <- paste0("rnapca_", seq_len(ncol(rna)))
  colnames(adtr) <- paste0("adtpca_", seq_len(ncol(adtr)))

  # Seurat WNN on the SAME reductions (fair algorithm comparison, not a preprocessing comparison)
  so <- suppressWarnings(SeuratObject::CreateSeuratObject(counts = Matrix::t(as(p2$getFacet("RNA")$rawCounts, "CsparseMatrix"))))
  so[["ADT"]] <- suppressWarnings(SeuratObject::CreateAssayObject(counts = Matrix::t(as(p2$getFacet("ADT")$rawCounts, "CsparseMatrix"))))
  so[["rnapca"]] <- SeuratObject::CreateDimReducObject(embeddings = rna[colnames(so), ], key = "rnapca_", assay = "RNA")
  so[["adtpca"]] <- SeuratObject::CreateDimReducObject(embeddings = adtr[colnames(so), ], key = "adtpca_", assay = "ADT")
  so <- suppressWarnings(Seurat::FindMultiModalNeighbors(so,
    reduction.list = list("rnapca", "adtpca"),
    dims.list = list(seq_len(ncol(rna)), seq_len(ncol(adtr))), verbose = FALSE))

  sr <- stats::setNames(so@meta.data[["RNA.weight"]], rownames(so@meta.data))
  pr <- stats::setNames(p2$cellMeta[cells, "wnn_weight_RNA"], cells)
  co <- intersect(cells, names(sr))
  rho <- suppressWarnings(stats::cor(pr[co], sr[co], method = "spearman"))
  expect_gt(rho, 0.2) # pagoda2 and Seurat WNN agree on which cells favor RNA (independent algorithms)
})
