#' Calculate user-defined connectivity
#'
#' General-purpose connectivity calculator for user-defined ROI or network
#' connections. Computes mean connectivity between a source (\code{from}) and
#' one or more targets (\code{to}) using raw cell average.
#'
#' @param conn_array 3D numeric array of connectivity values with dimensions
#'   (ROI x ROI x subjects), as returned by \code{\link{load_matrices}}
#' @param indices Named list of integer vectors mapping network/ROI names to
#'   index positions, as returned by \code{\link{get_indices}}
#' @param from Character. Name of the source ROI or network. Must match a
#'   name in \code{indices}
#' @param to Character vector. One or more target names. Each must match a
#'   name in \code{indices}. Connectivity is calculated from \code{from} to
#'   each target separately
#'
#' @return A data frame with one row per subject and one column per target,
#'   named \code{{from}_{to}}.
#'
#' @details
#' Connectivity is computed using raw cell average: for each subject, the
#' function extracts \code{conn_array[from_idx, to_idx, subj]} and takes the
#' mean of all values. When both \code{from} and \code{to} resolve to single
#' ROIs, the direct connection value is returned (no averaging needed).
#'
#' Supports any combination of ROIs and networks:
#' \itemize{
#'   \item ROI-to-network: e.g., \code{from = "ahip", to = "default"}
#'   \item ROI-to-ROI: e.g., \code{from = "ahip", to = "phip"}
#'   \item Network-to-network: e.g., \code{from = "default", to = "cont"}
#' }
#'
#' Names in \code{from} and \code{to} reference entries in \code{indices},
#' including any manual groupings created via \code{manual_assignments} in
#' \code{\link{get_indices}}.
#'
#' Works with any Schaefer version (100-1000 parcels, 7 or 17 networks).
#'
#' @examples
#' # ROI-to-network
#' indices <- get_indices(example_conn_array)
#' ahip_default <- calc_conn(example_conn_array, indices,
#'                           from = "ahip", to = "default")
#'
#' # ROI-to-multiple-networks
#' ahip_nets <- calc_conn(example_conn_array, indices,
#'                        from = "ahip",
#'                        to = c("default", "cont", "vis"))
#'
#' # ROI-to-ROI
#' ahip_phip <- calc_conn(example_conn_array, indices,
#'                        from = "ahip", to = "phip")
#'
#' \dontrun{
#' # Full workflow with manual grouping
#' z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(46, 57))
#' indices <- get_indices(z_mat,
#'   manual_assignments = list(ahip = "hippocampus", phip = "hippocampus"))
#'
#' hip_nets <- calc_conn(z_mat, indices,
#'                       from = "hippocampus",
#'                       to = c("default", "cont", "limbic",
#'                              "salventattn", "dorsattn", "sommot", "vis"))
#' }
#'
#' @seealso
#' \code{\link{calc_within}} for within-network connectivity.
#' \code{\link{calc_between}} for between-network connectivity.
#' \code{\link{get_indices}} for generating the \code{indices} input.
#'
#' @export

calc_conn <- function(conn_array, indices, from, to) {

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

  if (!is.character(from) || length(from) != 1) {
    stop("from must be a single character string.", call. = FALSE)
  }

  if (!is.character(to) || length(to) == 0) {
    stop("to must be a character vector with at least one name.", call. = FALSE)
  }

  # Validate from name
  available_names <- names(indices)

  if (!from %in% available_names) {
    stop(
      "'from' name '", from, "' not found in indices. ",
      "Available names: ", paste(available_names, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate to names
  invalid_to <- to[!to %in% available_names]
  if (length(invalid_to) > 0) {
    stop(
      "'to' name(s) not found in indices: ",
      paste(invalid_to, collapse = ", "), ". ",
      "Available names: ", paste(available_names, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Calculate connectivity ---
  n_subjects <- dim(conn_array)[3]
  from_idx <- indices[[from]]

  result <- purrr::map_dfc(to, function(target) {
    to_idx <- indices[[target]]

    connectivity <- purrr::map_dbl(seq_len(n_subjects), function(subj) {
      connections <- conn_array[from_idx, to_idx, subj]
      mean(connections, na.rm = TRUE)
    })

    return(connectivity)
  })

  colnames(result) <- paste(from, to, sep = "_")

  return(result)
}
