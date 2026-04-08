# Calculate within-network connectivity

Calculate mean within-network connectivity for each network across all
subjects. For each subject, extracts the upper triangle of each
network's submatrix (excluding diagonal) and averages those values.

## Usage

``` r
calc_within(conn_array, indices)
```

## Arguments

- conn_array:

  3D numeric array of connectivity values with dimensions (ROI x ROI x
  subjects)

- indices:

  Named list of integer vectors mapping network names to ROI index
  positions

## Value

A data frame with one row per subject. Contains one column per network
(named `within_{network}`) and a `within_network` column with the
average across all networks.

## Details

Entries in `indices` with fewer than 2 ROIs are automatically dropped
with a warning, as within-network connectivity requires at least 2 ROIs
to compute an upper triangle. Use
[`calc_conn`](https://shijing-z.github.io/fcnet/reference/calc_conn.md)
for single-ROI connectivity.

## See also

[`calc_between`](https://shijing-z.github.io/fcnet/reference/calc_between.md)
for between-network connectivity.
[`calc_conn`](https://shijing-z.github.io/fcnet/reference/calc_conn.md)
for user-defined ROI-to-ROI or ROI-to-network connectivity.
[`get_indices`](https://shijing-z.github.io/fcnet/reference/get_indices.md)
for generating the `indices` input.

## Examples

``` r
# Calculate within-network connectivity
indices <- get_indices(ex_conn_array, roi_include = "schaefer")
within <- calc_within(ex_conn_array, indices)
head(within)
#>   within_default  within_cont within_limbic within_salventattn within_dorsattn
#> 1     0.02295872 -0.007883598   -0.07526051       -0.169091813      0.07551777
#> 2    -0.01224774  0.042284519    0.18129910        0.009067142      0.09597176
#> 3     0.10757508  0.028470898    0.06799083        0.069625032     -0.21766960
#> 4     0.02361150 -0.004251002    0.27014029       -0.082327840      0.07662707
#> 5     0.02912008  0.002857445    0.11234488        0.002264887      0.03978844
#> 6    -0.03824059 -0.049946358   -0.02696672       -0.164610445     -0.03405838
#>   within_sommot  within_vis within_network
#> 1    0.05622365  0.05852082   -0.005573563
#> 2    0.03350536  0.11636414    0.066606327
#> 3   -0.01332180 -0.01834732    0.003474730
#> 4    0.02377714 -0.05731943    0.035751103
#> 5    0.02295433 -0.02772012    0.025944278
#> 6    0.07027714 -0.05276221   -0.042329651

# Select specific network columns
within[, c("within_default", "within_vis")]
#>    within_default  within_vis
#> 1      0.02295872  0.05852082
#> 2     -0.01224774  0.11636414
#> 3      0.10757508 -0.01834732
#> 4      0.02361150 -0.05731943
#> 5      0.02912008 -0.02772012
#> 6     -0.03824059 -0.05276221
#> 7      0.02228933  0.02120909
#> 8      0.03197366  0.03970775
#> 9     -0.04298190  0.06780827
#> 10    -0.10358461 -0.01328397

if (FALSE) { # \dontrun{
# Full workflow
z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(3, 5))
indices <- get_indices(z_mat, roi_include = "schaefer")
within <- calc_within(z_mat, indices)
} # }
```
