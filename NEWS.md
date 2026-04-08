# fcnet 0.2.0

## New features

* `plot_scatter()` gains `colors`, `show_points`, `show_line`, `point_args`,
  `line_args`, and `band_args` parameters for flexible layer customization.
* `plot_scatter()` now supports toggling points and regression lines on/off
  independently, enabling line-only or point-only scatter plots.
* `plot_scatter()` accepts a single color string for ungrouped plots
  (e.g., `colors = "steelblue"`) or a named vector for grouped plots.
* Per-layer appearance (size, shape, linetype, confidence level, etc.) can be
  customized through `point_args`, `line_args`, and `band_args` list parameters.

## Improvements

* `plot_scatter()` warns when band parameters (`se`, `level`, `alpha`) are
  mistakenly placed in `line_args` instead of `band_args`.
* `plot_scatter()` no longer produces unexpected messages when using
  `show_line = FALSE` with grouped plots.
* Automated pkgdown site deployment via GitHub Actions.

# fcnet 0.1.0

* Initial release.
* Data loading: `load_matrices()` for importing connectivity matrices from
  CONN toolbox output.
* ROI organization: `get_indices()` for extracting network and ROI groupings
  from Schaefer atlas naming conventions.
* Connectivity computation: `calc_within()`, `calc_between()`, and `calc_conn()`
  for within-network, between-network, and user-defined connectivity metrics.
* Visualization: `plot_heatmap()`, `plot_compare()`, and `plot_scatter()` for
  connectivity matrix heatmaps, group comparison bar plots, and
  connectivity-behavior scatter plots.
* Example data: `ex_conn_array`, `ex_roi_names`, and `ex_indices`.