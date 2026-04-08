# Changelog

## fcnet 0.2.0

### New features

- [`plot_scatter()`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
  gains `colors`, `show_points`, `show_line`, `point_args`, `line_args`,
  and `band_args` parameters for flexible layer customization.
- [`plot_scatter()`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
  now supports toggling points and regression lines on/off
  independently, enabling line-only or point-only scatter plots.
- [`plot_scatter()`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
  accepts a single color string for ungrouped plots (e.g.,
  `colors = "steelblue"`) or a named vector for grouped plots.
- Per-layer appearance (size, shape, linetype, confidence level, etc.)
  can be customized through `point_args`, `line_args`, and `band_args`
  list parameters.

### Improvements

- [`plot_scatter()`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
  warns when band parameters (`se`, `level`, `alpha`) are mistakenly
  placed in `line_args` instead of `band_args`.
- [`plot_scatter()`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
  no longer produces unexpected messages when using `show_line = FALSE`
  with grouped plots.
- Automated pkgdown site deployment via GitHub Actions.

## fcnet 0.1.0

- Initial release.
- Data loading:
  [`load_matrices()`](https://shijing-z.github.io/fcnet/reference/load_matrices.md)
  for importing connectivity matrices from CONN toolbox output.
- ROI organization:
  [`get_indices()`](https://shijing-z.github.io/fcnet/reference/get_indices.md)
  for extracting network and ROI groupings from Schaefer atlas naming
  conventions.
- Connectivity computation:
  [`calc_within()`](https://shijing-z.github.io/fcnet/reference/calc_within.md),
  [`calc_between()`](https://shijing-z.github.io/fcnet/reference/calc_between.md),
  and
  [`calc_conn()`](https://shijing-z.github.io/fcnet/reference/calc_conn.md)
  for within-network, between-network, and user-defined connectivity
  metrics.
- Visualization:
  [`plot_heatmap()`](https://shijing-z.github.io/fcnet/reference/plot_heatmap.md),
  [`plot_compare()`](https://shijing-z.github.io/fcnet/reference/plot_compare.md),
  and
  [`plot_scatter()`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
  for connectivity matrix heatmaps, group comparison bar plots, and
  connectivity-behavior scatter plots.
- Example data: `ex_conn_array`, `ex_roi_names`, and `ex_indices`.
