# Example Network Indices

Pre-computed network indices for the ex_conn_array dataset. Shows which
ROI positions belong to each functional network.

## Usage

``` r
ex_indices
```

## Format

A named list where each element contains integer indices:

- vis:

  Indices for visual network ROIs

- sommot:

  Indices for somatomotor network ROIs

- dorsattn:

  Indices for dorsal attention ROIs

- salventattn:

  Indices for salience/ventral attention ROIs

- limbic:

  Indices for limbic network ROIs

- cont:

  Indices for control network ROIs

- default:

  Indices for default mode network ROIs

- ahip:

  Index for anterior hippocampus

- phip:

  Index for posterior hippocampus

## Examples

``` r
names(ex_indices)
#> [1] "default"     "cont"        "limbic"      "salventattn" "dorsattn"   
#> [6] "sommot"      "vis"         "ahip"        "phip"       
ex_indices$vis
#> [1] 1 2 3 4 5
```
