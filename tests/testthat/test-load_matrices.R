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
# Error Handling Tests
# ==============================================================================

test_that("load_matrices errors with non-existent file", {
  expect_error(
    suppressWarnings(load_matrices("definitely_not_a_file.mat", type = "rmat")),
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
