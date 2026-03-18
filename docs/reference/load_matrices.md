# Load connectivity matrices from MATLAB file

Load connectivity matrices from CONN toolbox output. Extracts either
correlation-based (rmat) or Fisher z-transformed (zmat) connectivity
matrices.

## Usage

``` r
load_matrices(mat_file, type, exclude = NULL)
```

## Arguments

- mat_file:

  Character. Path to MATLAB .mat file containing connectivity data.

- type:

  Character. Type of matrix: "rmat" (correlation) or "zmat" (Fisher
  z-transformed). Must be specified.

- exclude:

  Integer vector. Subject indices to exclude, or NULL for no exclusions.
  Default is NULL.

## Value

A 3D numeric array of connectivity values with dimensions (ROI x ROI x
subjects). First two dimensions represent pairwise ROI connectivity,
third dimension indexes subjects.

## See also

[`get_indices`](get_indices.md) for extracting network indices from the
loaded matrices. [`ex_conn_array`](ex_conn_array.md) for example data
included in the package.

## Examples

``` r
# Example connectivity array
dim(ex_conn_array)
#> [1] 30 30 10
dimnames(ex_conn_array)[[1]]
#>  [1] "Schaefer100.l_vis_1"                   
#>  [2] "Schaefer100.l_vis_2"                   
#>  [3] "Schaefer100.l_vis_3"                   
#>  [4] "Schaefer100.r_vis_1"                   
#>  [5] "Schaefer100.r_vis_2"                   
#>  [6] "Schaefer100.l_sommot_1"                
#>  [7] "Schaefer100.l_sommot_2"                
#>  [8] "Schaefer100.r_sommot_1"                
#>  [9] "Schaefer100.r_sommot_2"                
#> [10] "Schaefer100.l_dorsattn_post_1"         
#> [11] "Schaefer100.l_dorsattn_post_2"         
#> [12] "Schaefer100.r_dorsattn_post_1"         
#> [13] "Schaefer100.r_dorsattn_post_2"         
#> [14] "Schaefer100.l_salventattn_paroper_1"   
#> [15] "Schaefer100.l_salventattn_froperins_1" 
#> [16] "Schaefer100.r_salventattn_tempoccpar_1"
#> [17] "Schaefer100.r_salventattn_froperins_1" 
#> [18] "Schaefer100.l_limbic_ofc_1"            
#> [19] "Schaefer100.l_limbic_temppole_1"       
#> [20] "Schaefer100.r_limbic_ofc_1"            
#> [21] "Schaefer100.l_cont_par_1"              
#> [22] "Schaefer100.l_cont_pfcl_1"             
#> [23] "Schaefer100.r_cont_par_1"              
#> [24] "Schaefer100.r_cont_pfcl_1"             
#> [25] "Schaefer100.l_default_temp_1"          
#> [26] "Schaefer100.l_default_par_1"           
#> [27] "Schaefer100.r_default_temp_1"          
#> [28] "Schaefer100.r_default_par_1"           
#> [29] "AHIP"                                  
#> [30] "PHIP"                                  

# Use with get_indices()
indices <- get_indices(ex_conn_array)
names(indices)
#> [1] "default"     "cont"        "limbic"      "salventattn" "dorsattn"   
#> [6] "sommot"      "vis"         "ahip"        "phip"       

# Extract specific network connectivity
vis_connectivity <- ex_conn_array[indices$vis, indices$vis, ]
dim(vis_connectivity)
#> [1]  5  5 10

if (FALSE) { # \dontrun{
# Load z-transformed matrices with subject exclusions
z_mat <- load_matrices(
  mat_file = "data/conn.mat",
  type = "zmat",
  exclude = c(46, 57)
)

# Load correlation matrices
r_mat <- load_matrices(
  mat_file = "data/conn.mat",
  type = "rmat"
)

# Check dimensions
dim(z_mat)

# Combine with get_indices()
indices <- get_indices(r_mat)
vis_connectivity <- r_mat[indices$vis, indices$vis, ]
} # }
```
