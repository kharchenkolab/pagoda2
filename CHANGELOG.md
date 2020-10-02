## Upcoming

## [0.1.3] - 2020-10-02

### Added
- Makefile.win, Makevars.win
- Made OpenMP headers conditional

### Changed
- Now should (natively) install on Mac OS for all users

## [0.1.2] - 2020-09-24

### Added

- Parameter `gene` in `plotEmbedding` for coloring by normalized counts

### Changed

- Using `sccore` plotting system for embedding plot. 3d plots aren't supported anymore.
- Added LICENSE (July 2020)
- Changed `std::cout` to `Rcpp::Rcout` (July 2020)

## [0.1.1] - 2019-11-26

### Added

- Function `p2.generate.go` now supports arbitrary annotations
- added use.raw.variance
- UMAP and UMAP_graph embedding support
- Specificity metrics to `getDifferentialGenes`

### Changed

- removed negative value check to allow for custom starting values
- Fixed problem with duplicated observations during t-SNE estimation
- Added option `var.scale` to `calculatePcaReduction`, which allow to disable variance scaling on counts

## [0.1.0] - 2019-04-20

### Added

- Parameter `min.transcripts.per.cell`

### Removed

- Removed dependencies on boost, GSL and Cairo

### Fixed

- Moved GO.db to Suggests
- Added BioConductor dependencies to DESCRIPTION
