# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("calc_conn exists and is callable", {
  expect_true(is.function(calc_conn))
})

test_that("calc_conn has required parameters", {
  args <- formals(calc_conn)
  expect_true("conn_array" %in% names(args))
  expect_true("indices" %in% names(args))
  expect_true("from" %in% names(args))
  expect_true("to" %in% names(args))
})

test_that("calc_conn returns correct structure", {
  indices <- get_indices(example_conn_array)
  result <- calc_conn(example_conn_array, indices,
                      from = "ahip", to = "default")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), dim(example_conn_array)[3])
  expect_equal(ncol(result), 1)
})

test_that("calc_conn returns correct column names for single target", {
  indices <- get_indices(example_conn_array)
  result <- calc_conn(example_conn_array, indices,
                      from = "ahip", to = "default")

  expect_equal(colnames(result), "ahip_default")
})

test_that("calc_conn returns correct column names for multiple targets", {
  indices <- get_indices(example_conn_array)
  result <- calc_conn(example_conn_array, indices,
                      from = "ahip",
                      to = c("default", "cont", "vis"))

  expect_equal(colnames(result), c("ahip_default", "ahip_cont", "ahip_vis"))
  expect_equal(ncol(result), 3)
})

# ==============================================================================
# Computed Value Tests
# ==============================================================================

test_that("calc_conn computes correct ROI-to-network value", {
  # Create controlled array: 1 ahip ROI, 2 default ROIs, 1 subject
  roi_names <- c("Schaefer100.l_default_1", "Schaefer100.l_default_2", "AHIP")
  mat <- array(0, dim = c(3, 3, 1),
               dimnames = list(roi_names, roi_names, "subj1"))

  # ahip to default connections: [3,1]=0.5, [3,2]=0.3
  mat[3, 1, 1] <- 0.5; mat[1, 3, 1] <- 0.5
  mat[3, 2, 1] <- 0.3; mat[2, 3, 1] <- 0.3

  indices <- get_indices(roi_names)
  result <- calc_conn(mat, indices, from = "ahip", to = "default")

  # Raw cell average: mean(0.5, 0.3) = 0.4
  expect_equal(result$ahip_default, 0.4)
})

test_that("calc_conn computes correct ROI-to-ROI value", {
  indices <- get_indices(example_conn_array)
  result <- calc_conn(example_conn_array, indices,
                      from = "ahip", to = "phip")

  # Should return the direct cell value for each subject
  ahip_idx <- indices$ahip
  phip_idx <- indices$phip
  expected <- example_conn_array[ahip_idx, phip_idx, ]
  expect_equal(result$ahip_phip, unname(expected))
})

test_that("calc_conn computes correct network-to-network value", {
  roi_names <- c("Schaefer100.l_vis_1", "Schaefer100.l_vis_2",
                 "Schaefer100.l_default_1", "Schaefer100.l_default_2")
  mat <- array(0, dim = c(4, 4, 1),
               dimnames = list(roi_names, roi_names, "subj1"))

  # vis→default block: [1,3]=0.1, [1,4]=0.2, [2,3]=0.3, [2,4]=0.4
  mat[1, 3, 1] <- 0.1; mat[3, 1, 1] <- 0.1
  mat[1, 4, 1] <- 0.2; mat[4, 1, 1] <- 0.2
  mat[2, 3, 1] <- 0.3; mat[3, 2, 1] <- 0.3
  mat[2, 4, 1] <- 0.4; mat[4, 2, 1] <- 0.4

  indices <- get_indices(roi_names, roi_include = "schaefer")
  result <- calc_conn(mat, indices, from = "vis", to = "default")

  # Raw cell average: mean(0.1, 0.2, 0.3, 0.4) = 0.25
  expect_equal(result$vis_default, 0.25)
})

test_that("calc_conn batch mode computes each target separately", {
  indices <- get_indices(example_conn_array)

  # Batch call
  result_batch <- calc_conn(example_conn_array, indices,
                            from = "ahip",
                            to = c("default", "vis"))

  # Individual calls
  result_default <- calc_conn(example_conn_array, indices,
                              from = "ahip", to = "default")
  result_vis <- calc_conn(example_conn_array, indices,
                          from = "ahip", to = "vis")

  expect_equal(result_batch$ahip_default, result_default$ahip_default)
  expect_equal(result_batch$ahip_vis, result_vis$ahip_vis)
})

# ==============================================================================
# Use Case Tests
# ==============================================================================

test_that("calc_conn works with manual_assignments grouping", {
  indices <- get_indices(
    example_conn_array,
    manual_assignments = list(ahip = "hippocampus", phip = "hippocampus")
  )

  result <- calc_conn(example_conn_array, indices,
                      from = "hippocampus", to = "default")

  expect_true(is.data.frame(result))
  expect_equal(colnames(result), "hippocampus_default")
  expect_equal(nrow(result), 10)
})

test_that("calc_conn works with all 7 networks as targets", {
  indices <- get_indices(example_conn_array)

  targets <- c("default", "cont", "limbic", "salventattn",
               "dorsattn", "sommot", "vis")

  result <- calc_conn(example_conn_array, indices,
                      from = "ahip", to = targets)

  expect_equal(ncol(result), 7)
  expect_equal(nrow(result), 10)
})

# ==============================================================================
# Error Handling Tests
# ==============================================================================

test_that("calc_conn errors with non-3D array", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(matrix(1:9, 3, 3), indices, from = "ahip", to = "default"),
    "3D array"
  )
})

test_that("calc_conn errors with unnamed list", {
  expect_error(
    calc_conn(example_conn_array, list(1:5, 6:10),
              from = "ahip", to = "default"),
    "named list"
  )
})

test_that("calc_conn errors with invalid from name", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(example_conn_array, indices,
              from = "nonexistent", to = "default"),
    "not found in indices"
  )
})

test_that("calc_conn error for invalid from lists available names", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(example_conn_array, indices,
              from = "nonexistent", to = "default"),
    "Available names"
  )
})

test_that("calc_conn errors with invalid to name", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(example_conn_array, indices,
              from = "ahip", to = "nonexistent"),
    "not found in indices"
  )
})

test_that("calc_conn errors with multiple invalid to names", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(example_conn_array, indices,
              from = "ahip", to = c("default", "fake1", "fake2")),
    "fake1, fake2"
  )
})

test_that("calc_conn errors when from is not a single string", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(example_conn_array, indices,
              from = c("ahip", "phip"), to = "default"),
    "single character string"
  )
})

test_that("calc_conn errors when to is empty", {
  indices <- get_indices(example_conn_array)

  expect_error(
    calc_conn(example_conn_array, indices,
              from = "ahip", to = character(0)),
    "at least one name"
  )
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("calc_conn works with single subject", {
  single_subj <- example_conn_array[, , 1, drop = FALSE]
  indices <- get_indices(single_subj)

  result <- calc_conn(single_subj, indices,
                      from = "ahip", to = "default")

  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result[[1]]))
})

test_that("calc_conn returns numeric values", {
  indices <- get_indices(example_conn_array)
  result <- calc_conn(example_conn_array, indices,
                      from = "ahip",
                      to = c("default", "vis", "phip"))

  expect_true(all(sapply(result, is.numeric)))
})
