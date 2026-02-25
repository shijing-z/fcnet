## Create Example Data for fcnet Package
## This script creates small example datasets for documentation and testing

library(usethis)

# ==============================================================================
# 1. Example ROI Names (Character Vector)
# ==============================================================================
# Based on actual Schaefer 100 + 2 hippocampal ROIs

ex_roi_names <- c(
  # Visual network (first 9 ROIs)
  "Schaefer100.l_vis_1",
  "Schaefer100.l_vis_2",
  "Schaefer100.l_vis_3",
  "Schaefer100.r_vis_1",
  "Schaefer100.r_vis_2",

  # Somatomotor network (5 ROIs)
  "Schaefer100.l_sommot_1",
  "Schaefer100.l_sommot_2",
  "Schaefer100.r_sommot_1",
  "Schaefer100.r_sommot_2",

  # Dorsal attention network (5 ROIs)
  "Schaefer100.l_dorsattn_post_1",
  "Schaefer100.l_dorsattn_post_2",
  "Schaefer100.r_dorsattn_post_1",
  "Schaefer100.r_dorsattn_post_2",

  # Ventral attention network (5 ROIs)
  "Schaefer100.l_salventattn_paroper_1",
  "Schaefer100.l_salventattn_froperins_1",
  "Schaefer100.r_salventattn_tempoccpar_1",
  "Schaefer100.r_salventattn_froperins_1",

  # Limbic network (3 ROIs)
  "Schaefer100.l_limbic_ofc_1",
  "Schaefer100.l_limbic_temppole_1",
  "Schaefer100.r_limbic_ofc_1",

  # Control network (5 ROIs)
  "Schaefer100.l_cont_par_1",
  "Schaefer100.l_cont_pfcl_1",
  "Schaefer100.r_cont_par_1",
  "Schaefer100.r_cont_pfcl_1",

  # Default network (5 ROIs)
  "Schaefer100.l_default_temp_1",
  "Schaefer100.l_default_par_1",
  "Schaefer100.r_default_temp_1",
  "Schaefer100.r_default_par_1",

  # Hippocampal ROIs
  "AHIP",
  "PHIP"
)

# Save as internal data
usethis::use_data(ex_roi_names, overwrite = TRUE)


# ==============================================================================
# 2. Example Small Connectivity Array (R Object)
# ==============================================================================
# Create a small 30x30x10 array (30 ROIs, 10 subjects)
# This is for quick examples without needing NetworkToolbox

set.seed(123)
n_rois <- 30
n_subjects <- 10

# Create realistic connectivity values
ex_conn_array <- array(
  dim = c(n_rois, n_rois, n_subjects),
  dimnames = list(
    ex_roi_names[1:n_rois],
    ex_roi_names[1:n_rois],
    paste0("subj", 1:n_subjects)
  )
)

# Fill with realistic connectivity values
for (subj in 1:n_subjects) {
  # Create a correlation matrix
  base_mat <- matrix(rnorm(n_rois * n_rois, 0, 0.3), n_rois, n_rois)
  base_mat <- (base_mat + t(base_mat)) / 2 # Make symmetric
  diag(base_mat) <- 1 # Perfect self-correlation

  # Ensure valid correlation range [-1, 1]
  base_mat[base_mat > 1] <- 1
  base_mat[base_mat < -1] <- -1

  ex_conn_array[,, subj] <- base_mat
}

# Save as internal data
usethis::use_data(ex_conn_array, overwrite = TRUE)


# ==============================================================================
# 3. Example Network Indices (Pre-computed)
# ==============================================================================
# Save the result of get_indices() for quick examples

# Generate using get_indices() for consistency
# Make sure get_indices is available
devtools::load_all()

# Use get_indices to create the indices
ex_indices <- get_indices(ex_roi_names)

# Verify it
print("Generated indices:")
print(names(ex_indices))
print("Types:")
print(sapply(ex_indices, class))

# Save
usethis::use_data(ex_indices, overwrite = TRUE)


# ==============================================================================
# 4. Documentation for Example Data
# ==============================================================================

# Create R/data.R file for documentation
cat(
  '
#\' Example ROI Names from Schaefer Atlas
#\'
#\' A character vector containing 30 example ROI names from the Schaefer 100
#\' parcellation (7 networks) plus anterior and posterior hippocampus.
#\' Includes examples from all 7 functional networks.
#\'
#\' @format A character vector with 30 ROI names:
#\' \\describe{
#\'   \\item{Visual}{5 ROIs from visual network}
#\'   \\item{Somatomotor}{4 ROIs from somatomotor network}
#\'   \\item{Dorsal Attention}{4 ROIs from dorsal attention network}
#\'   \\item{Ventral Attention}{4 ROIs from ventral attention network}
#\'   \\item{Limbic}{3 ROIs from limbic network}
#\'   \\item{Control}{4 ROIs from control network}
#\'   \\item{Default}{4 ROIs from default mode network}
#\'   \\item{Hippocampus}{2 ROIs (AHIP, PHIP)}
#\' }
#\'
#\' @examples
#\' head(ex_roi_names)
#\' length(ex_roi_names)
"ex_roi_names"

#\' Example Connectivity Array
#\'
#\' A small 3D connectivity array (30 ROIs × 30 ROIs × 10 subjects) with
#\' realistic correlation values for demonstration purposes.
#\'
#\' @format A 3D array with dimensions (30, 30, 10):
#\' \\describe{
#\'   \\item{Dimension 1}{ROI (rows)}
#\'   \\item{Dimension 2}{ROI (columns)}
#\'   \\item{Dimension 3}{Subject}
#\' }
#\'
#\' Dimnames include ROI names (from ex_roi_names) and subject IDs.
#\'
#\' @examples
#\' dim(ex_conn_array)
#\' dimnames(ex_conn_array)[[1]]  # ROI names
#\'
#\' # Access connectivity for first subject
#\' ex_conn_array[, , 1]
"ex_conn_array"

#\' Example Network Indices
#\'
#\' Pre-computed network indices for the ex_conn_array dataset.
#\' Shows which ROI positions belong to each functional network.
#\'
#\' @format A named list where each element contains integer indices:
#\' \\describe{
#\'   \\item{vis}{Indices for visual network ROIs}
#\'   \\item{sommot}{Indices for somatomotor network ROIs}
#\'   \\item{dorsattn}{Indices for dorsal attention ROIs}
#\'   \\item{salventattn}{Indices for ventral attention ROIs}
#\'   \\item{limbic}{Indices for limbic network ROIs}
#\'   \\item{cont}{Indices for control network ROIs}
#\'   \\item{default}{Indices for default mode network ROIs}
#\'   \\item{ahip}{Index for anterior hippocampus}
#\'   \\item{phip}{Index for posterior hippocampus}
#\' }
#\'
#\' @examples
#\' names(ex_indices)
#\' ex_indices$vis
"ex_indices"
',
  file = "R/data.R"
)

message("Example data created! Now run:")
message("  1. Document with: devtools::document()")
message("  2. Check with: devtools::check()")
