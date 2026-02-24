#' Example ROI Names from Schaefer Atlas
#'
#' A character vector containing 30 example ROI names from the Schaefer 100
#' parcellation (7 networks) plus anterior and posterior hippocampus.
#' Includes examples from all 7 functional networks.
#'
#' @format A character vector with 30 ROI names:
#' \describe{
#'   \item{Visual}{5 ROIs from visual network}
#'   \item{Somatomotor}{4 ROIs from somatomotor network}
#'   \item{Dorsal Attention}{4 ROIs from dorsal attention network}
#'   \item{Ventral Attention}{4 ROIs from ventral attention network}
#'   \item{Limbic}{3 ROIs from limbic network}
#'   \item{Control}{4 ROIs from control network}
#'   \item{Default}{4 ROIs from default mode network}
#'   \item{Hippocampus}{2 ROIs (AHIP, PHIP)}
#' }
#'
#' @examples
#' head(example_roi_names)
#' length(example_roi_names)
"example_roi_names"

#' Example Connectivity Array
#'
#' A small 3D connectivity array (30 ROIs × 30 ROIs × 10 subjects) with
#' realistic correlation values for demonstration purposes.
#'
#' @format A 3D array with dimensions (30, 30, 10):
#' \describe{
#'   \item{Dimension 1}{ROI (rows)}
#'   \item{Dimension 2}{ROI (columns)}
#'   \item{Dimension 3}{Subject}
#' }
#'
#' Dimnames include ROI names (from example_roi_names) and subject IDs.
#'
#' @examples
#' dim(example_conn_array)
#' dimnames(example_conn_array)[[1]]  # ROI names
#'
#' # Access connectivity for first subject
#' example_conn_array[, , 1]
"example_conn_array"

#' Example Network Indices
#'
#' Pre-computed network indices for the example_conn_array dataset.
#' Shows which ROI positions belong to each functional network.
#'
#' @format A named list where each element contains integer indices:
#' \describe{
#'   \item{vis}{Indices for visual network ROIs}
#'   \item{sommot}{Indices for somatomotor network ROIs}
#'   \item{dorsattn}{Indices for dorsal attention ROIs}
#'   \item{salventattn}{Indices for ventral attention ROIs}
#'   \item{limbic}{Indices for limbic network ROIs}
#'   \item{cont}{Indices for control network ROIs}
#'   \item{default}{Indices for default mode network ROIs}
#'   \item{ahip}{Index for anterior hippocampus}
#'   \item{phip}{Index for posterior hippocampus}
#' }
#'
#' @examples
#' names(example_network_indices)
#' example_network_indices$vis
"example_network_indices"
