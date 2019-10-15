## [Unreleased]

### Added

- Function `p2.generate.go` now supports arbitrary annotations
- added use.raw.variance
- UMAP embedding support

### Changed

- removed negative value check to allow for custom starting values
- Fixed problem with duplicated observations during t-SNE estimation

## [0.1.0] - 2019-04-20

### Added

- Parameter `min.transcripts.per.cell`

### Removed

- Removed dependencies on boost, GSL and Cairo

### Fixed

- Moved GO.db to Suggests
- Added BioConductor dependencies to DESCRIPTION
