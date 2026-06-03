## Thread policy helpers for pagoda2.

.pagoda2_thread_roles <- c("total", "r.workers", "native", "sgd", "blas")

.pagoda2_thread_method_roles <- list(
  run = .pagoda2_thread_roles,
  constructor = c("total", "native"),
  variance = c("total", "native"),
  pca = c("total", "blas"),
  graph = c("total", "native"),
  embedding = c("total", "native", "sgd"),
  markers = c("total", "r.workers"),
  r = c("total", "r.workers"),
  native = c("total", "native"),
  blas = c("total", "blas")
)

.pagoda2_normalize_thread_method <- function(method) {
  if (is.null(method)) {
    return(NULL)
  }
  key <- c(
    run = "run",
    Pagoda2 = "run",
    constructor = "constructor",
    initialize = "constructor",
    runVariance = "variance",
    adjustVariance = "variance",
    viewColMeanVar = "variance",
    variance = "variance",
    runPCA = "pca",
    calculatePcaReduction = "pca",
    pca = "pca",
    runGraph = "graph",
    makeKnnGraph = "graph",
    graph = "graph",
    runEmbedding = "embedding",
    getEmbedding = "embedding",
    embedding = "embedding",
    umap = "embedding",
    UMAP = "embedding",
    runMarkers = "markers",
    getDifferentialGenes = "markers",
    markers = "markers",
    papply = "r",
    r = "r",
    native = "native",
    blas = "blas"
  )
  out <- key[[method]]
  if (is.null(out)) method else out
}

.pagoda2_available_cores <- function() {
  n <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  if (!is.finite(n) || n < 1L) {
    n <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  }
  if (!is.finite(n) || n < 1L) 1L else as.integer(n)
}

.pagoda2_env_integer <- function(name) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  value <- suppressWarnings(as.integer(value))
  if (!is.finite(value) || value < 1L) {
    warning("Ignoring invalid ", name, " value; expected a positive integer.", call. = FALSE)
    return(NULL)
  }
  value
}

.pagoda2_normalize_thread_value <- function(value, name) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 1) {
    stop("Thread setting `", name, "` must be a positive integer")
  }
  as.integer(value)
}

.pagoda2_normalize_threads <- function(threads = NULL) {
  if (is.null(threads)) {
    return(list())
  }
  if (is.numeric(threads) && length(threads) == 1L) {
    threads <- list(total = threads)
  }
  if (!is.list(threads)) {
    stop("`threads` must be a named list")
  }
  if (length(threads) == 0L) {
    return(list())
  }
  if (is.null(names(threads)) || any(!nzchar(names(threads)))) {
    stop("`threads` must be a named list")
  }

  canonical <- c(
    total = "total",
    cores = "total",
    n.cores = "total",
    ncores = "total",
    num.threads = "total",
    r.workers = "r.workers",
    r_workers = "r.workers",
    workers = "r.workers",
    forks = "r.workers",
    native = "native",
    native.threads = "native",
    cpp = "native",
    cpp.threads = "native",
    openmp = "native",
    omp = "native",
    sgd = "sgd",
    sgd.threads = "sgd",
    n.sgd.cores = "sgd",
    n_sgd_threads = "sgd",
    blas = "blas",
    blas.threads = "blas"
  )
  out <- list()
  for (name in names(threads)) {
    key <- canonical[[name]]
    if (is.null(key)) {
      stop("Unknown thread setting `", name, "`")
    }
    if (!is.null(out[[key]])) {
      stop("Thread setting `", key, "` was supplied more than once")
    }
    out[[key]] <- .pagoda2_normalize_thread_value(threads[[name]], key)
  }
  out
}

.pagoda2_global_threads <- function() {
  opt <- getOption("pagoda2.threads", NULL)
  if (!is.null(opt)) {
    opt <- .pagoda2_normalize_threads(opt)
  } else {
    opt <- list()
  }
  n.opt <- getOption("pagoda2.n.cores", NULL)
  if (!is.null(n.opt) && is.null(opt$total)) {
    opt$total <- .pagoda2_normalize_thread_value(n.opt, "pagoda2.n.cores")
  }
  env <- list(
    total = .pagoda2_env_integer("PAGODA2_NUM_THREADS"),
    r.workers = .pagoda2_env_integer("PAGODA2_R_WORKERS"),
    native = .pagoda2_env_integer("PAGODA2_NATIVE_THREADS"),
    sgd = .pagoda2_env_integer("PAGODA2_SGD_THREADS"),
    blas = .pagoda2_env_integer("PAGODA2_BLAS_THREADS")
  )
  for (name in names(env)) {
    if (is.null(opt[[name]]) && !is.null(env[[name]])) {
      opt[[name]] <- env[[name]]
    }
  }
  opt
}

.pagoda2_default_total_threads <- function() {
  global <- .pagoda2_global_threads()
  if (!is.null(global$total)) {
    return(global$total)
  }
  max(1L, min(16L, .pagoda2_available_cores()))
}

.pagoda2_fill_thread_defaults <- function(policy, tasks = NULL) {
  total <- policy$total
  if (is.null(total)) {
    total <- .pagoda2_default_total_threads()
  }
  total <- .pagoda2_normalize_thread_value(total, "total")

  cap <- function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    max(1L, min(as.integer(value), total))
  }
  task.cap <- if (!is.null(tasks) && is.finite(tasks)) max(1L, as.integer(tasks)) else Inf
  list(
    total = total,
    r.workers = cap(if (is.null(policy$r.workers)) min(total, 8L, task.cap) else policy$r.workers),
    native = cap(if (is.null(policy$native)) total else policy$native),
    sgd = cap(if (is.null(policy$sgd)) min(total, 4L) else policy$sgd),
    blas = cap(if (is.null(policy$blas)) total else policy$blas)
  )
}

.pagoda2_validate_thread_roles <- function(threads, method) {
  method <- .pagoda2_normalize_thread_method(method)
  if (is.null(method) || length(threads) == 0L) {
    return(invisible())
  }
  roles <- .pagoda2_thread_method_roles[[method]]
  if (is.null(roles)) {
    return(invisible())
  }
  bad <- setdiff(names(threads), roles)
  if (length(bad) > 0L) {
    stop(
      "Thread setting(s) not supported by ", method, ": ",
      paste(bad, collapse = ", ")
    )
  }
  invisible()
}

.pagoda2_resolve_threads <- function(p2 = NULL, n.cores = NULL, threads = NULL,
                                     method = NULL, tasks = NULL, validate = TRUE) {
  method <- .pagoda2_normalize_thread_method(method)
  if (!is.null(n.cores) && !is.null(threads)) {
    stop("Specify only one of `n.cores` or `threads`; use `threads = list(total = ..., ...)` for advanced control")
  }
  explicit <- .pagoda2_normalize_threads(threads)
  if (!is.null(n.cores)) {
    explicit$total <- .pagoda2_normalize_thread_value(n.cores, "n.cores")
  }
  if (validate) {
    .pagoda2_validate_thread_roles(explicit, method)
  }

  global <- .pagoda2_global_threads()
  object <- list()
  if (!is.null(p2)) {
    object <- p2$threadPolicy
    if (is.null(object)) {
      object <- list()
    }
    object <- .pagoda2_normalize_threads(object)
    if (is.null(object$total) && !is.null(p2$n.cores)) {
      object$total <- p2$n.cores
    }
  }
  policy <- utils::modifyList(global, object)
  policy <- utils::modifyList(policy, explicit)
  .pagoda2_fill_thread_defaults(policy, tasks = tasks)
}

.pagoda2_thread_subset <- function(policy, method) {
  method <- .pagoda2_normalize_thread_method(method)
  roles <- .pagoda2_thread_method_roles[[method]]
  if (is.null(roles)) {
    roles <- "total"
  }
  policy[intersect(names(policy), roles)]
}

.pagoda2_set_threads <- function(p2, total = NULL, r.workers = NULL, native = NULL,
                                 sgd = NULL, blas = NULL, threads = NULL, ...) {
  extras <- list(...)
  supplied <- c(
    list(total = total, r.workers = r.workers, native = native, sgd = sgd, blas = blas),
    extras
  )
  supplied <- supplied[!vapply(supplied, is.null, logical(1))]
  if (!is.null(threads)) {
    if (length(supplied) > 0L) {
      stop("Supply either `threads` or named thread arguments, not both")
    }
    supplied <- threads
  }
  supplied <- .pagoda2_normalize_threads(supplied)
  if (length(supplied) == 0L) {
    supplied <- list(total = .pagoda2_default_total_threads())
  }
  current <- p2$threadPolicy
  if (is.null(current)) {
    current <- list()
  }
  current <- .pagoda2_normalize_threads(current)
  policy <- utils::modifyList(current, supplied)
  resolved <- .pagoda2_fill_thread_defaults(policy)
  p2$threadPolicy <- policy
  p2$n.cores <- resolved$total
  invisible(p2)
}

.pagoda2_get_threads <- function(p2, method = NULL, n.cores = NULL, threads = NULL, tasks = NULL) {
  method <- .pagoda2_normalize_thread_method(method)
  .pagoda2_resolve_threads(p2, n.cores = n.cores, threads = threads, method = method, tasks = tasks)
}

.pagoda2_describe_threads <- function(p2, method = NULL, n.cores = NULL, threads = NULL, tasks = NULL) {
  display.method <- method
  method <- .pagoda2_normalize_thread_method(method)
  policy <- .pagoda2_get_threads(p2, method = method, n.cores = n.cores, threads = threads, tasks = tasks)
  lines <- c(
    paste0("total: ", policy$total),
    paste0("R workers: ", policy$r.workers),
    paste0("native/OpenMP: ", policy$native),
    paste0("UMAP SGD: ", policy$sgd),
    paste0("BLAS: ", policy$blas)
  )
  if (!is.null(display.method)) {
    lines <- c(paste0(display.method, ":"), paste0("  ", lines))
  }
  message(paste(lines, collapse = "\n"))
  invisible(policy)
}

.pagoda2_with_blas_threads <- function(n, expr) {
  if (!is.null(n) && requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    old <- tryCatch(RhpcBLASctl::blas_get_num_procs(), error = function(e) NULL)
    tryCatch(RhpcBLASctl::blas_set_num_threads(n), error = function(e) NULL)
    if (!is.null(old)) {
      on.exit(tryCatch(RhpcBLASctl::blas_set_num_threads(old), error = function(e) NULL), add = TRUE)
    }
  }
  force(expr)
}
