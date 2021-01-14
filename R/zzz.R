.pkgenv <- new.env(parent=emptyenv())

.onLoad  <- function(libname, pkgname) {
    has_data <- requireNamespace("p2data", quietly = TRUE)
    .pkgenv[["has_data"]] <- has_data
}

.onAttach <- function(libname, pkgname) {
    if (!.pkgenv$has_data) {
        msg <- paste("To use this package, you must install the",
                     "p2data package. To install that ",
                     "package, run `install.packages('p2data',",
                     "repos='https://kharchenkolab.github.io/drat/', type='source')`.",
                     "See the `pagoda2` vignette and https://github.com/kharchenkolab/pagoda2 for more details.")
        msg <- paste(strwrap(msg), collapse="\n")
        packageStartupMessage(msg)
    }
}

hasData <- function(has_data = .pkgenv$has_data) {
    if (!has_data) {
        msg <- paste("To use this function, you must have the",
                     "`p2data` package installed. See the",
                     "See the `pagoda2` vignette and https://github.com/kharchenkolab/pagoda2 for more details.")
        msg <- paste(strwrap(msg), collapse="\n")
        stop(msg)
    }
}