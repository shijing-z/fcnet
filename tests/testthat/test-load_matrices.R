# Tests for load_matrices()
# Save as: tests/testthat/test-load_matrices.R

# ==============================================================================
# Basic Functionality Tests
# ==============================================================================

test_that("load_matrices function exists and is callable", {
  expect_true(is.function(load_matrices))
})

test_that("load_matrices has required parameters", {
  args <- formals(load_matrices)

  expect_true("mat_file" %in% names(args))
  expect_true("type" %in% names(args))
  expect_true("exclude" %in% names(args))
})

# ==============================================================================
# Parameter Validation Tests
# ==============================================================================

test_that("load_matrices validates type parameter", {
  # Test invalid type parameter
  expect_error(
    load_matrices("fake.mat", type = "invalid"),
    "should be one of"
  )
})

test_that("load_matrices has correct default for exclude", {
  args <- formals(load_matrices)

  # Default should be NULL
  expect_null(args$exclude)
})

# ==============================================================================
# Integration Tests (with real .mat file)
# ==============================================================================

# Note: These tests are skipped unless you have a test .mat file
# To enable: create inst/extdata/test_conn.mat

test_that("load_matrices works with rmat type", {
  skip("Requires real .mat file - implement when available")

  # Example of how to test with real file:
  # test_file <- system.file("extdata", "test_conn.mat", package = "fconn")
  # skip_if(test_file == "", "Test .mat file not available")
  #
  # result <- load_matrices(test_file, type = "rmat")
  #
  # expect_true(is.array(result))
  # expect_equal(length(dim(result)), 3)
})

test_that("load_matrices works with zmat type", {
  skip("Requires real .mat file - implement when available")

  # Similar to rmat test above
})

test_that("load_matrices handles exclusions correctly", {
  skip("Requires real .mat file - implement when available")

  # Example:
  # result_full <- load_matrices(test_file, type = "rmat", exclude = NULL)
  # result_excl <- load_matrices(test_file, type = "rmat", exclude = c(1, 2))
  #
  # expect_equal(dim(result_excl)[3], dim(result_full)[3] - 2)
})

# ==============================================================================
# Return Value Tests (when you have test data)
# ==============================================================================

test_that("load_matrices returns array with correct structure", {
  skip("Requires test .mat file")

  # Example structure tests:
  # result <- load_matrices(test_file, type = "rmat")
  #
  # # Should be 3D array
  # expect_true(is.array(result))
  # expect_equal(length(dim(result)), 3)
  #
  # # Should have dimnames
  # expect_false(is.null(dimnames(result)))
  #
  # # ROI names should be in dimnames
  # expect_true(length(dimnames(result)[[1]]) > 0)
})

test_that("load_matrices returns numeric values", {
  skip("Requires test .mat file")

  # Example:
  # result <- load_matrices(test_file, type = "rmat")
  # expect_true(is.numeric(result))
})

test_that("load_matrices rmat values are correlations", {
  skip("Requires test .mat file")

  # Correlation values should be between -1 and 1
  # result <- load_matrices(test_file, type = "rmat")
  # expect_true(all(result >= -1 & result <= 1, na.rm = TRUE))
})

# ==============================================================================
# Error Handling Tests
# ==============================================================================

test_that("load_matrices errors with non-existent file", {
  expect_error(
    load_matrices("definitely_not_a_file.mat", type = "rmat"),
    "cannot open|does not exist"
  )
})

test_that("load_matrices errors with invalid file format", {
  # Create a temporary non-.mat file
  temp_file <- tempfile(fileext = ".txt")
  writeLines("not a mat file", temp_file)

  expect_error(
    load_matrices(temp_file, type = "rmat")
  )

  # Cleanup
  unlink(temp_file)
})

test_that("load_matrices validates exclude parameter", {
  skip("Requires test .mat file")

  # Example tests for exclude parameter:
  # test_file <- system.file("extdata", "test_conn.mat", package = "fconn")
  #
  # # Should error with non-numeric exclude
  # expect_error(
  #   load_matrices(test_file, type = "rmat", exclude = "not_numeric")
  # )
  #
  # # Should error with out-of-range exclude
  # expect_error(
  #   load_matrices(test_file, type = "rmat", exclude = 1000)
  # )
})

# ==============================================================================
# Notes for Future Implementation
# ==============================================================================

# To fully test load_matrices, you need:
# 1. A small test .mat file in inst/extdata/
# 2. That file should have known dimensions and values
# 3. Then you can uncomment and adapt the skipped tests above
#
# Example of creating a minimal test file:
# library(R.matlab)
# test_rmat <- array(rnorm(10*10*3), dim = c(10, 10, 3))
# test_zmat <- array(rnorm(10*10*3), dim = c(10, 10, 3))
# writeMat("inst/extdata/test_conn.mat", rmat = test_rmat, zmat = test_zmat)
