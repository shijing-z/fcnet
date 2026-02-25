#' Calculate between-network connectivity
#'
#' Calculate mean between-network connectivity across all subjects. Supports
#' two modes: per-network averages (each network vs. all others) and pairwise
#' combinations (all unique network pairs).
#'
#' @param conn_array 3D numeric array of connectivity values with dimensions
#'   (ROI x ROI x subjects), as returned by \code{\link{load_matrices}}
#' @param indices Named list of integer vectors mapping network names to ROI
#'   index positions, as returned by \code{\link{get_indices}}
#' @param pairwise Logical. If FALSE (default), returns per-network averages
#'   (each network vs. all other networks). If TRUE, returns all unique
#'   pairwise combinations between networks
#'
#' @return A data frame with one row per subject.
#'
#'   When \code{pairwise = FALSE}: one column per network
#'   (named \code{between_{network}}) and a \code{between_network} column with
#'   the overall between-network average (raw cell average across all
#'   between-network connections).
#'
#'   When \code{pairwise = TRUE}: one column per unique network pair
#'   (named \code{{net1}_{net2}}).
#'
#' @details
#' Entries in \code{indices} with fewer than 2 ROIs are automatically dropped
#' with a warning, as between-network calculations at the network level are
#' intended for multi-ROI networks. Use \code{\link{calc_conn}} for
#' single-ROI-to-network connectivity.
#'
#' The overall \code{between_network} average (when \code{pairwise = FALSE})
#' uses raw cell average: all between-network connections are pooled and
#' averaged with equal weight per cell. This means larger network pairs
#' contribute proportionally more than smaller pairs.
#'
#' Works with any Schaefer version (100-1000 parcels, 7 or 17 networks).
#'
#' @examples
#' # Per-network between-network averages
#' indices <- get_indices(ex_conn_array, roi_include = "schaefer")
#' between <- calc_between(ex_conn_array, indices)
#' head(between)
#'
#' # All pairwise combinations
#' pairwise <- calc_between(ex_conn_array, indices, pairwise = TRUE)
#' head(pairwise)
#'
#' \dontrun{
#' # Full workflow
#' z_mat <- load_matrices("data/conn.mat", type = "zmat", exclude = c(46, 57))
#' indices <- get_indices(z_mat, roi_include = "schaefer")
#' between <- calc_between(z_mat, indices)
#' pairwise <- calc_between(z_mat, indices, pairwise = TRUE)
#' }
#'
#' @seealso
#' \code{\link{calc_within}} for within-network connectivity.
#' \code{\link{calc_conn}} for user-defined ROI-to-ROI or ROI-to-network
#'   connectivity.
#' \code{\link{get_indices}} for generating the \code{indices} input.
#'
#' @export

calc_between <- function(conn_array, indices, pairwise = FALSE) {
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

  if (!is.logical(pairwise) || length(pairwise) != 1) {
    stop("pairwise must be TRUE or FALSE.", call. = FALSE)
  }

  # --- Safety net: drop entries with fewer than 2 ROIs ---
  roi_counts <- sapply(indices, length)
  small_entries <- names(indices)[roi_counts < 2]

  if (length(small_entries) > 0) {
    warning(
      "Dropped entries with fewer than 2 ROIs: ",
      paste(small_entries, collapse = ", "),
      ". ",
      "These are not suitable for network-level calculation. ",
      "Use calc_conn() for ROI-level connectivity.",
      call. = FALSE
    )
    indices <- indices[roi_counts >= 2]
  }

  if (length(indices) < 2) {
    stop(
      "At least 2 networks with 2 or more ROIs are required for ",
      "between-network connectivity.",
      call. = FALSE
    )
  }

  # --- Calculate ---
  n_subjects <- dim(conn_array)[3]
  network_names <- names(indices)

  if (pairwise) {
    # --- Pairwise mode: all unique network pairs ---
    pairwise_conn <- data.frame(row.names = seq_len(n_subjects))

    for (i in seq_len(length(network_names) - 1)) {
      for (j in (i + 1):length(network_names)) {
        net1 <- network_names[i]
        net2 <- network_names[j]
        net1_idx <- indices[[net1]]
        net2_idx <- indices[[net2]]

        pair_connectivity <- purrr::map_dbl(
          seq_len(n_subjects),
          function(subj) {
            between_connections <- conn_array[net1_idx, net2_idx, subj]
            mean(between_connections, na.rm = TRUE)
          }
        )

        col_name <- paste(net1, net2, sep = "_")
        pairwise_conn[[col_name]] <- pair_connectivity
      }
    }

    return(pairwise_conn)
  } else {
    # --- Per-network mode: each network vs. all others ---
    between_list <- purrr::map(network_names, function(target_net) {
      target_idx <- indices[[target_net]]
      other_idx <- unlist(indices[network_names != target_net])

      purrr::map_dbl(seq_len(n_subjects), function(subj) {
        between_connections <- conn_array[target_idx, other_idx, subj]
        mean(between_connections, na.rm = TRUE)
      })
    })

    names(between_list) <- paste0("between_", network_names)
    between_network <- data.frame(between_list)

    # --- Overall between-network average (raw cell average) ---
    between_avg <- purrr::map_dbl(seq_len(n_subjects), function(subj) {
      all_between <- c()
      for (net in network_names) {
        target_idx <- indices[[net]]
        other_idx <- unlist(indices[network_names != net])
        net_between <- conn_array[target_idx, other_idx, subj]
        all_between <- c(all_between, as.vector(net_between))
      }
      mean(all_between, na.rm = TRUE)
    })

    between_network$between_network <- between_avg

    return(between_network)
  }
}
