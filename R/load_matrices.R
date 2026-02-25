#' Load connectivity matrices from MATLAB file
#'
#' Load connectivity matrices from CONN toolbox output. Extracts either
#' correlation-based (rmat) or Fisher z-transformed (zmat) connectivity matrices.
#'
#' @param mat_file Character. Path to MATLAB .mat file containing connectivity data.
#' @param type Character. Type of matrix: "rmat" (correlation) or "zmat"
#'   (Fisher z-transformed). Must be specified.
#' @param exclude Integer vector. Subject indices to exclude, or NULL for no exclusions.
#'   Default is NULL.
#'
#' @return A 3D numeric array of connectivity values with dimensions
#'   (ROI x ROI x subjects). First two dimensions represent pairwise ROI
#'   connectivity, third dimension indexes subjects.
#'
#' @examples
#' # Example connectivity array
#' dim(ex_conn_array)
#' dimnames(ex_conn_array)[[1]]
#'
#' # Use with get_indices()
#' indices <- get_indices(ex_conn_array)
#' names(indices)
#'
#' # Extract specific network connectivity
#' vis_connectivity <- ex_conn_array[indices$vis, indices$vis, ]
#' dim(vis_connectivity)
#'
#' \dontrun{
#' # Load z-transformed matrices with subject exclusions
#' z_mat <- load_matrices(
#'   mat_file = "data/conn.mat",
#'   type = "zmat",
#'   exclude = c(46, 57)
#' )
#'
#' # Load correlation matrices
#' r_mat <- load_matrices(
#'   mat_file = "data/conn.mat",
#'   type = "rmat"
#' )
#'
#' # Check dimensions
#' dim(z_mat)
#'
#' # Combine with get_indices()
#' indices <- get_indices(r_mat)
#' vis_connectivity <- r_mat[indices$vis, indices$vis, ]
#' }
#'
#' @seealso
#' \code{\link{get_indices}} for extracting network indices from the loaded matrices.
#' \code{\link{ex_conn_array}} for example data included in the package.
#'
#' @export

load_matrices <- function(mat_file, type, exclude = NULL) {
  # Validate type argument
  type <- match.arg(type, choices = c("rmat", "zmat"))

  conn_mats <- NetworkToolbox::convertConnBrainMat(mat_file)

  if (type == "rmat") {
    result <- conn_mats$rmat
  } else {
    result <- conn_mats$zmat
  }

  # Apply exclusions if specified
  if (!is.null(exclude)) {
    result <- result[,, -exclude, drop = FALSE]
  }

  return(result)
}
