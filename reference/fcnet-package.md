# fcnet: Functional Connectivity Network Analysis

Tools for analyzing network-level functional connectivity from fMRI
data. Provides functions for loading connectivity matrices, organizing
ROIs by network, computing within-network, between-network, and
user-defined connectivity, and generating visualizations.

## Data loading

[`load_matrices`](https://shijing-z.github.io/fcnet/reference/load_matrices.md)
loads correlation or z-transformed matrices from CONN toolbox .mat
files.

## ROI organization

[`get_indices`](https://shijing-z.github.io/fcnet/reference/get_indices.md)
extracts and organizes ROI positions by functional network. Supports
Schaefer atlases (100-1000 parcels, 7 or 17 networks) and custom ROIs.

## Connectivity calculation

[`calc_within`](https://shijing-z.github.io/fcnet/reference/calc_within.md)
computes within-network connectivity.
[`calc_between`](https://shijing-z.github.io/fcnet/reference/calc_between.md)
computes between-network connectivity.
[`calc_conn`](https://shijing-z.github.io/fcnet/reference/calc_conn.md)
computes user-defined ROI or network connectivity.

## Visualization

[`plot_heatmap`](https://shijing-z.github.io/fcnet/reference/plot_heatmap.md)
displays connectivity matrices as heatmaps with network boundaries.
[`plot_compare`](https://shijing-z.github.io/fcnet/reference/plot_compare.md)
creates group comparison bar plots.
[`plot_scatter`](https://shijing-z.github.io/fcnet/reference/plot_scatter.md)
creates connectivity-behavior scatter plots.

All visualization functions return `ggplot` objects for further
customization.

## Atlas compatibility

All functions work with any Schaefer atlas version and support
non-Schaefer ROIs such as hippocampal subregions.

## See also

Useful links:

- <https://shijing-z.github.io/fcnet/>

## Author

**Maintainer**: Shijing Zhou <shijingz@uoregon.edu>
([ORCID](https://orcid.org/0000-0001-7053-0953))
