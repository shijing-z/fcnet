#' Calculate within-network connectivity
#'
#' Calculate mean within-network connectivity for each network across all
#' subjects. For each subject, extracts the upper triangle of each network's
#' submatrix (excluding diagonal) and averages those values.
#'
#' @param conn_array 3D numeric array of connectivity values with dimensions
#'   (ROI x ROI x subjects), as returned by \code{\link{load_matrices}}
#' @param indices Named list of integer vectors mapping network names to ROI
#'   index positions, as returned by \code{\link{get_indices}}
#'
#' @return A data frame with one row per subject. Contains one column per
#'   network (named \code{within_{network}}) and a \code{within_network}
#'   column with the average across all networks.
#'
#' @details
#' Entries in \code{indices} with fewer than 2 ROIs are automatically dropped
#' with a warning, as within-network connectivity requires at least 2 ROIs
#' to compute an upper triangle. Use \code{\link{calc_conn}} for single-ROI
#' connectivity.
#'
#' Works with any Schaefer version (100-1000 parcels, 7 or 17 networks).
#'
#' @examples
#' # Calculate within-network connectivity
#' indices <- get_indices(ex_conn_array, roi_include = "schaefer")
#' within <- calc_within(ex_conn_array, indices)
#' head(within)
#'
#' # Select specific network columns
#' within[, c("within_default", "within_vis")]
#'
#' \dontrun{
#' # Full workflow
#' z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(46, 57))
#' indices <- get_indices(z_mat, roi_include = "schaefer")
#' within <- calc_within(z_mat, indices)
#' }
#'
#' @seealso
#' \code{\link{calc_between}} for between-network connectivity.
#' \code{\link{calc_conn}} for user-defined ROI-to-ROI or ROI-to-network
#'   connectivity.
#' \code{\link{get_indices}} for generating the \code{indices} input.
#'
#' @export

calc_within <- function(conn_array, indices) {

  # --- Input validation ---
  if (!is.array(conn_array) || length(dim(conn_array)) != 3) {
    stop(
      "conn_array must be a 3D array (ROI x ROI x subjects). ",
      "Use load_matrices() to load connectivity data.",
      call. = FALSE
    )
  }

  if (!is.list(indices) || is.null(names(indices))) {
    stop(
      "indices must be a named list of ROI index vectors. ",
      "Use get_indices() to generate indices.",
      call. = FALSE
    )
  }

  # --- Safety net: drop entries with fewer than 2 ROIs ---
  roi_counts <- sapply(indices, length)
  small_entries <- names(indices)[roi_counts < 2]

  if (length(small_entries) > 0) {
    warning(
      "Dropped entries with fewer than 2 ROIs: ",
      paste(small_entries, collapse = ", "), ". ",
      "These are not suitable for network-level calculation. ",
      "Use calc_conn() for ROI-level connectivity.",
      call. = FALSE
    )
    indices <- indices[roi_counts >= 2]
  }

  if (length(indices) == 0) {
    stop(
      "No entries with 2 or more ROIs found in indices. ",
      "Within-network connectivity requires multi-ROI networks.",
      call. = FALSE
    )
  }

  # --- Calculate within-network connectivity ---
  n_subjects <- dim(conn_array)[3]
  network_names <- names(indices)

  within_list <- purrr::map(network_names, function(net) {
    net_idx <- indices[[net]]

    purrr::map_dbl(seq_len(n_subjects), function(subj) {
      net_matrix <- conn_array[net_idx, net_idx, subj]
      upper_tri <- net_matrix[upper.tri(net_matrix)]
      mean(upper_tri, na.rm = TRUE)
    })
  })

  names(within_list) <- paste0("within_", network_names)
  within_network <- data.frame(within_list)

  # --- Add overall within-network average ---
  within_network$within_network <- rowMeans(
    within_network[, paste0("within_", network_names)],
    na.rm = TRUE
  )

  return(within_network)
}
