# Calculate between-network connectivity

Calculate mean between-network connectivity across all subjects.
Supports two modes: per-network averages (each network vs. all others)
and pairwise combinations (all unique network pairs).

## Usage

``` r
calc_between(conn_array, indices, pairwise = FALSE)
```

## Arguments

- conn_array:

  3D numeric array of connectivity values with dimensions (ROI x ROI x
  subjects)

- indices:

  Named list of integer vectors mapping network names to ROI index
  positions

- pairwise:

  Logical. If FALSE (default), returns per-network averages (each
  network vs. all other networks). If TRUE, returns all unique pairwise
  combinations between networks

## Value

A data frame with one row per subject.

When `pairwise = FALSE`: one column per network (named
`between_{network}`) and a `between_network` column with the overall
between-network average (raw cell average across all between-network
connections).

When `pairwise = TRUE`: one column per unique network pair (named
`{net1}_{net2}`).

## Details

Entries in `indices` with fewer than 2 ROIs are automatically dropped
with a warning, as between-network calculations at the network level are
intended for multi-ROI networks. Use
[`calc_conn`](https://shijing-z.github.io/fcnet/reference/calc_conn.md)
for single-ROI-to-network connectivity.

The overall `between_network` average (when `pairwise = FALSE`) uses raw
cell average: all between-network connections are pooled and averaged
with equal weight per cell. This means larger network pairs contribute
proportionally more than smaller pairs.

## See also

[`calc_within`](https://shijing-z.github.io/fcnet/reference/calc_within.md)
for within-network connectivity.
[`calc_conn`](https://shijing-z.github.io/fcnet/reference/calc_conn.md)
for user-defined ROI-to-ROI or ROI-to-network connectivity.
[`get_indices`](https://shijing-z.github.io/fcnet/reference/get_indices.md)
for generating the `indices` input.

## Examples

``` r
# Per-network between-network averages
indices <- get_indices(ex_conn_array, roi_include = "schaefer")
between <- calc_between(ex_conn_array, indices)
head(between)
#>   between_default  between_cont between_limbic between_salventattn
#> 1   -0.0048067546 -4.501994e-03   -0.012165856        0.0245853073
#> 2   -0.0317770660  1.249933e-02    0.021459344        0.0001011974
#> 3    0.0119209649  2.319006e-02   -0.006982458       -0.0231126025
#> 4   -0.0311574064 -6.465594e-05    0.007286081       -0.0021131003
#> 5   -0.0191066686  1.066845e-03   -0.013689533       -0.0009127735
#> 6   -0.0006063092 -1.369784e-02    0.031205969        0.0029093385
#>   between_dorsattn between_sommot   between_vis between_network
#> 1    -0.0109401352    0.009869065 -0.0122839832   -0.0014348811
#> 2    -0.0161195017   -0.014143417  0.0179967485   -0.0015927036
#> 3    -0.0356410021    0.021905312  0.0006591845   -0.0009173964
#> 4    -0.0138275248    0.025063289  0.0039501928   -0.0016728568
#> 5     0.0001009788   -0.022886468 -0.0114889798   -0.0094847820
#> 6    -0.0173695954    0.008840942  0.0121442119    0.0027229538

# All pairwise combinations
pairwise <- calc_between(ex_conn_array, indices, pairwise = TRUE)
head(pairwise)
#>   default_cont default_limbic default_salventattn default_dorsattn
#> 1  0.077246094   -0.008229589        -0.007322148       0.05144760
#> 2 -0.027958401   -0.006317766        -0.066033833      -0.07204074
#> 3 -0.007908106    0.050089057         0.047961214      -0.03102491
#> 4 -0.021371198   -0.066578785        -0.053668765      -0.04970355
#> 5  0.000060043   -0.023202936         0.004246479      -0.06483325
#> 6 -0.024355598    0.046329851        -0.014032574      -0.02142706
#>   default_sommot  default_vis   cont_limbic cont_salventattn cont_dorsattn
#> 1   -0.068090333 -0.060759638 -0.0574796149     -0.040522090  -0.037782868
#> 2   -0.070414657  0.040418850  0.0579357732      0.002066906   0.026452868
#> 3   -0.060167271  0.068078458  0.0761081544     -0.021051908   0.013882137
#> 4   -0.015293104  0.002421011  0.0223891686     -0.039393185   0.030242240
#> 5   -0.048823905  0.009690258 -0.0009276706      0.063969335   0.006978702
#> 6    0.003472619  0.014365891 -0.0077215131     -0.048818085  -0.013331401
#>    cont_sommot    cont_vis limbic_salventattn limbic_dorsattn limbic_sommot
#> 1  0.073281640 -0.04490002         0.08959628    -0.018007559   -0.03453961
#> 2  0.008697156  0.01782848         0.05074842     0.064761513   -0.07255699
#> 3  0.070581836  0.02124424         0.01912144    -0.168101744    0.06026022
#> 4 -0.005831806  0.01533931         0.05451919    -0.014985782    0.03680396
#> 5 -0.048347317 -0.01245115        -0.05409786     0.002135495    0.01722483
#> 6  0.074016301 -0.05112572         0.01482934    -0.072962175    0.09259772
#>    limbic_vis salventattn_dorsattn salventattn_sommot salventattn_vis
#> 1 -0.03790121          -0.02130564        0.106676068     0.034230753
#> 2  0.03163996          -0.04247247        0.026746556     0.033790966
#> 3 -0.06489400          -0.08589537        0.015151595    -0.087345785
#> 4  0.01071220           0.02333720        0.019264055    -0.002485847
#> 5 -0.02135315           0.05580094       -0.026608142    -0.049849483
#> 6  0.09757127           0.03361418       -0.009120638     0.035752917
#>   dorsattn_sommot dorsattn_vis   sommot_vis
#> 1    -0.053884950  0.007512572  0.021709337
#> 2    -0.018670456 -0.030845876  0.018558916
#> 3     0.006417055  0.007081106  0.043402792
#> 4     0.024889836 -0.080393237  0.079798224
#> 5    -0.014302355  0.012288174 -0.009724568
#> 6    -0.065097340  0.013396540 -0.015738865

if (FALSE) { # \dontrun{
# Full workflow
z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(3, 5))
indices <- get_indices(z_mat, roi_include = "schaefer")
between <- calc_between(z_mat, indices)
pairwise <- calc_between(z_mat, indices, pairwise = TRUE)
} # }
```
