# Example ROI Names from Schaefer Atlas

A character vector containing 30 example ROI names from the Schaefer 100
parcellation (7 networks) plus anterior and posterior hippocampus.
Includes examples from all 7 functional networks.

## Usage

``` r
ex_roi_names
```

## Format

A character vector with 30 ROI names:

- Visual:

  5 ROIs from visual network

- Somatomotor:

  4 ROIs from somatomotor network

- Dorsal Attention:

  4 ROIs from dorsal attention network

- Ventral Attention:

  4 ROIs from ventral attention network

- Limbic:

  3 ROIs from limbic network

- Control:

  4 ROIs from control network

- Default:

  4 ROIs from default mode network

- Hippocampus:

  2 ROIs (AHIP, PHIP)

## Examples

``` r
head(ex_roi_names)
#> [1] "Schaefer100.l_vis_1"    "Schaefer100.l_vis_2"    "Schaefer100.l_vis_3"   
#> [4] "Schaefer100.r_vis_1"    "Schaefer100.r_vis_2"    "Schaefer100.l_sommot_1"
length(ex_roi_names)
#> [1] 30
```
