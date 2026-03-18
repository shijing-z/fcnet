# Get ROI indices organized by network

Extracts ROI index positions and organizes them by network or region
name. Schaefer ROIs are automatically grouped by functional network.
Other ROIs are preserved individually unless grouped via
manual_assignments.

## Usage

``` r
get_indices(
  x,
  roi_include = c("all", "schaefer"),
  reorder_networks = TRUE,
  manual_assignments = NULL
)
```

## Arguments

- x:

  Input source for ROI names. Can be:

  - A file path to a CONN toolbox .mat file (requires NetworkToolbox)

  - A 3D connectivity array with ROI names in dimnames

  - A character vector of ROI names

- roi_include:

  Character. "all" (default) processes all ROIs; "schaefer" processes
  only Schaefer-labeled ROIs.

- reorder_networks:

  Logical. If TRUE (default), reorder Schaefer networks as: default,
  cont, limbic, salventattn, dorsattn, sommot, vis. Non-Schaefer ROIs
  always appear at the end.

- manual_assignments:

  Named list to override groupings. Format: list(roi_name =
  "group_name"). Matching is case-insensitive.

## Value

A named list where names are network/ROI labels and values are integer
vectors of ROI indices.

## Details

Schaefer ROIs are grouped by network (e.g., all visual ROIs become
"vis"). Non-Schaefer ROIs are preserved individually with lowercase
names (e.g., "AHIP" becomes "ahip"). Attention network naming variations
across atlas versions (ventatt, salventattn, dorsatt, etc.) are
automatically normalized for consistency.

## See also

[`load_matrices`](load_matrices.md) for loading connectivity data.
[`calc_within`](calc_within.md), [`calc_between`](calc_between.md),
[`calc_conn`](calc_conn.md) for connectivity calculations using indices.
[`plot_heatmap`](plot_heatmap.md) for visualizing connectivity matrices
using indices.

## Examples

``` r
# From character vector of ROI names
indices <- get_indices(ex_roi_names)
names(indices)
#> [1] "default"     "cont"        "limbic"      "salventattn" "dorsattn"   
#> [6] "sommot"      "vis"         "ahip"        "phip"       

# Access specific network indices
indices$vis
#> [1] 1 2 3 4 5
indices$default
#> [1] 25 26 27 28
indices$ahip
#> [1] 29

# From connectivity array with dimnames
indices <- get_indices(ex_conn_array)
names(indices)
#> [1] "default"     "cont"        "limbic"      "salventattn" "dorsattn"   
#> [6] "sommot"      "vis"         "ahip"        "phip"       

# Extract Schaefer ROIs only
indices_schaefer <- get_indices(ex_roi_names, roi_include = "schaefer")
names(indices_schaefer)
#> [1] "default"     "cont"        "limbic"      "salventattn" "dorsattn"   
#> [6] "sommot"      "vis"        

# Manual grouping of ROIs
indices_grouped <- get_indices(
  ex_roi_names,
  manual_assignments = list(ahip = "hippocampus", phip = "hippocampus")
)
indices_grouped$hippocampus
#> [1] 29 30

# Preserve original matrix order
indices_original <- get_indices(ex_roi_names, reorder_networks = FALSE)

if (FALSE) { # \dontrun{
# From CONN .mat file
indices <- get_indices("path/to/conn.mat")

# From connectivity array loaded with NetworkToolbox
conn_mats <- NetworkToolbox::convertConnBrainMat("path/to/conn.mat")
indices <- get_indices(conn_mats$rmat)
} # }
```
