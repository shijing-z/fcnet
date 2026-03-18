# Calculate user-defined connectivity

General-purpose connectivity calculator for user-defined ROI or network
connections. Computes mean connectivity between a source (`from`) and
one or more targets (`to`) using raw cell average.

## Usage

``` r
calc_conn(conn_array, indices, from, to)
```

## Arguments

- conn_array:

  3D numeric array of connectivity values with dimensions (ROI x ROI x
  subjects)

- indices:

  Named list of integer vectors mapping network/ROI names to index
  positions

- from:

  Character. Name of the source ROI or network. Must match a name in
  `indices`

- to:

  Character vector. One or more target names. Each must match a name in
  `indices`. Connectivity is calculated from `from` to each target
  separately

## Value

A data frame with one row per subject and one column per target, named
`{from}_{to}`.

## Details

Connectivity is computed using raw cell average: for each subject, the
function extracts `conn_array[from_idx, to_idx, subj]` and takes the
mean of all values. When both `from` and `to` resolve to single ROIs,
the direct connection value is returned (no averaging needed).

Supports any combination of ROIs and networks:

- ROI-to-network: e.g., `from = "ahip", to = "default"`

- ROI-to-ROI: e.g., `from = "ahip", to = "phip"`

- Network-to-network: e.g., `from = "default", to = "cont"`

Names in `from` and `to` reference entries in `indices`, including any
manual groupings.

## See also

[`calc_within`](calc_within.md) for within-network connectivity.
[`calc_between`](calc_between.md) for between-network connectivity.
[`get_indices`](get_indices.md) for generating the `indices` input.

## Examples

``` r
# ROI-to-network
indices <- get_indices(ex_conn_array)
ahip_default <- calc_conn(ex_conn_array, indices,
                          from = "ahip", to = "default")

# ROI-to-multiple-networks
ahip_nets <- calc_conn(ex_conn_array, indices,
                       from = "ahip",
                       to = c("default", "cont", "vis"))

# ROI-to-ROI
ahip_phip <- calc_conn(ex_conn_array, indices,
                       from = "ahip", to = "phip")

if (FALSE) { # \dontrun{
# Full workflow with manual grouping
z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(46, 57))
indices <- get_indices(z_mat,
  manual_assignments = list(ahip = "hippocampus", phip = "hippocampus"))

hip_nets <- calc_conn(z_mat, indices,
                      from = "hippocampus",
                      to = c("default", "cont", "limbic",
                             "salventattn", "dorsattn", "sommot", "vis"))
} # }
```
