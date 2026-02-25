# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("calc_between exists and is callable", {
  expect_true(is.function(calc_between))
})

test_that("calc_between has required parameters", {
  args <- formals(calc_between)

  expect_true("conn_array" %in% names(args))
  expect_true("indices" %in% names(args))
  expect_true("pairwise" %in% names(args))
})

test_that("calc_between has correct default for pairwise", {
  args <- formals(calc_between)

  expect_false(args$pairwise)
})

# ==============================================================================
# Input Validation Tests
# ==============================================================================

test_that("calc_between errors with non-3D array", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")

  expect_error(
    calc_between(matrix(1:9, 3, 3), indices),
    "3D array"
  )

  expect_error(
    calc_between(1:10, indices),
    "3D array"
  )
})

test_that("calc_between errors with invalid indices", {
  # Unnamed list
  expect_error(
    calc_between(ex_conn_array, list(1:5, 6:10)),
    "named list"
  )

  # Not a list
  expect_error(
    calc_between(ex_conn_array, c(1, 2, 3)),
    "named list"
  )
})

test_that("calc_between errors with invalid pairwise parameter", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")

  expect_error(
    calc_between(ex_conn_array, indices, pairwise = "yes"),
    "TRUE or FALSE"
  )

  expect_error(
    calc_between(ex_conn_array, indices, pairwise = c(TRUE, FALSE)),
    "TRUE or FALSE"
  )
})

# ==============================================================================
# Safety Net Tests
# ==============================================================================

test_that("calc_between warns and drops entries with fewer than 2 ROIs", {
  indices <- get_indices(ex_conn_array)

  expect_warning(
    result <- calc_between(ex_conn_array, indices),
    "Dropped entries with fewer than 2 ROIs"
  )

  expect_false("between_ahip" %in% colnames(result))
  expect_false("between_phip" %in% colnames(result))
})

test_that("calc_between warning suggests calc_conn", {
  indices <- get_indices(ex_conn_array)

  expect_warning(
    calc_between(ex_conn_array, indices),
    "calc_conn"
  )
})

test_that("calc_between runs without warning when all entries have 2+ ROIs", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")

  expect_no_warning(
    calc_between(ex_conn_array, indices)
  )
})

test_that("calc_between errors when fewer than 2 networks remain", {
  one_net_indices <- list(vis = 1:5L)

  expect_error(
    calc_between(ex_conn_array, one_net_indices),
    "At least 2 networks"
  )
})

# ==============================================================================
# Per-Network Mode (pairwise = FALSE) Tests
# ==============================================================================

test_that("calc_between per-network mode returns correct structure", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), dim(ex_conn_array)[3])

  # One column per network + between_network average
  network_names <- names(indices)
  expected_cols <- c(paste0("between_", network_names), "between_network")
  expect_equal(colnames(result), expected_cols)
})

test_that("calc_between per-network column names have between_ prefix", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices)

  network_cols <- setdiff(colnames(result), "between_network")
  expect_true(all(grepl("^between_", network_cols)))
})

test_that("calc_between per-network returns numeric values", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices)

  expect_true(all(sapply(result, is.numeric)))
})

test_that("calc_between per-network computes correctly", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices)

  # Manual calculation for visual network vs all others, subject 1
  vis_idx <- indices$vis
  other_idx <- unlist(indices[names(indices) != "vis"])
  expected_vis <- mean(ex_conn_array[vis_idx, other_idx, 1])

  expect_equal(result$between_vis[1], expected_vis)
})

# ==============================================================================
# Pairwise Mode (pairwise = TRUE) Tests
# ==============================================================================

test_that("calc_between pairwise mode returns correct structure", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices, pairwise = TRUE)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), dim(ex_conn_array)[3])

  # Number of unique pairs: n*(n-1)/2
  n_nets <- length(indices)
  expected_pairs <- n_nets * (n_nets - 1) / 2
  expect_equal(ncol(result), expected_pairs)
})

test_that("calc_between pairwise with 7 networks gives 21 pairs", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices, pairwise = TRUE)

  # 7 networks → 7*6/2 = 21 pairs
  expect_equal(ncol(result), 21)
})

test_that("calc_between pairwise column names use net1_net2 format", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices, pairwise = TRUE)

  # First pair should be first two networks
  network_names <- names(indices)
  first_pair <- paste(network_names[1], network_names[2], sep = "_")
  expect_true(first_pair %in% colnames(result))
})

test_that("calc_between pairwise computes correctly", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices, pairwise = TRUE)

  # Manual calculation for default_cont pair, subject 1
  def_idx <- indices$default
  cont_idx <- indices$cont
  expected_pair <- mean(ex_conn_array[def_idx, cont_idx, 1])

  expect_equal(result$default_cont[1], expected_pair)
})

test_that("calc_between pairwise returns numeric values", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices, pairwise = TRUE)

  expect_true(all(sapply(result, is.numeric)))
})

# ==============================================================================
# Between-Network Average Tests
# ==============================================================================

test_that("calc_between between_network uses raw cell average", {
  indices <- get_indices(ex_conn_array, roi_include = "schaefer")
  result <- calc_between(ex_conn_array, indices)

  # Manual raw cell average for subject 1
  network_names <- names(indices)
  all_between <- c()
  for (net in network_names) {
    target_idx <- indices[[net]]
    other_idx <- unlist(indices[network_names != net])
    net_between <- ex_conn_array[target_idx, other_idx, 1]
    all_between <- c(all_between, as.vector(net_between))
  }
  expected_avg <- mean(all_between)

  expect_equal(result$between_network[1], expected_avg)
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("calc_between works with single subject", {
  single_subj <- ex_conn_array[, , 1, drop = FALSE]
  indices <- get_indices(single_subj, roi_include = "schaefer")

  result <- calc_between(single_subj, indices)

  expect_equal(nrow(result), 1)
  expect_true(is.data.frame(result))
})

test_that("calc_between works with two networks", {
  two_net_indices <- list(
    vis = get_indices(ex_conn_array)$vis,
    default = get_indices(ex_conn_array)$default
  )

  # Per-network mode
  result <- calc_between(ex_conn_array, two_net_indices)
  expect_equal(ncol(result), 3) # between_vis, between_default, between_network

  # Pairwise mode: 2 networks → 1 pair
  result_pw <- calc_between(ex_conn_array, two_net_indices, pairwise = TRUE)
  expect_equal(ncol(result_pw), 1)
})

test_that("calc_between pairwise safety net also works", {
  indices <- get_indices(ex_conn_array)

  expect_warning(
    result <- calc_between(ex_conn_array, indices, pairwise = TRUE),
    "Dropped entries with fewer than 2 ROIs"
  )

  # No column should contain ahip or phip
  expect_false(any(grepl("ahip|phip", colnames(result))))
})
