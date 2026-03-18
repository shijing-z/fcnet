# fcnet: Functional Connectivity Network Analysis

Tools for analyzing network-level functional connectivity from fMRI
data. Provides functions for loading connectivity matrices, organizing
ROIs by network, computing within-network, between-network, and
user-defined connectivity, and generating visualizations.

## Data loading

[`load_matrices`](load_matrices.md) loads correlation or z-transformed
matrices from CONN toolbox .mat files.

## ROI organization

[`get_indices`](get_indices.md) extracts and organizes ROI positions by
functional network. Supports Schaefer atlases (100-1000 parcels, 7 or 17
networks) and custom ROIs.

## Connectivity calculation

[`calc_within`](calc_within.md) computes within-network connectivity.
[`calc_between`](calc_between.md) computes between-network connectivity.
[`calc_conn`](calc_conn.md) computes user-defined ROI or network
connectivity.

## Visualization

[`plot_heatmap`](plot_heatmap.md) displays connectivity matrices as
heatmaps with network boundaries. [`plot_compare`](plot_compare.md)
creates group comparison bar plots. [`plot_scatter`](plot_scatter.md)
creates connectivity-behavior scatter plots.

All visualization functions return `ggplot` objects for further
customization.

## Atlas compatibility

All functions work with any Schaefer atlas version and support
non-Schaefer ROIs such as hippocampal subregions.

## Author

**Maintainer**: Shijing Zhou <shijingz@uoregon.edu>
([ORCID](https://orcid.org/0000-0001-7053-0953))
