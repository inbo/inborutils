test_that("download_zenodo() works for a single-file record", {
  zenodo_dir <- tempfile()
  withr::local_file(zenodo_dir)
  dir.create(zenodo_dir)
  expect_no_error(
    download_zenodo(doi = "10.5281/zenodo.3784149", path = zenodo_dir) |>
      suppressMessages()
  )
})

test_that("download_zenodo() works for a GitHub code record", {
  zenodo_dir <- tempfile()
  withr::local_file(zenodo_dir)
  dir.create(zenodo_dir)
  expect_no_error(
    download_zenodo(doi = "10.5281/zenodo.7335805", path = zenodo_dir) |>
      suppressMessages()
  )
})

test_that("download_zenodo() works for a multi-file record", {
  zenodo_dir <- tempfile()
  withr::local_file(zenodo_dir)
  dir.create(zenodo_dir)
  expect_no_error(
    download_zenodo(
      doi = "10.5281/zenodo.4420858",
      path = zenodo_dir
    ) |>
      suppressMessages()
  )
})

test_that("download_zenodo() can work sequentially for a multi-file record", {
  zenodo_dir <- tempfile()
  withr::local_file(zenodo_dir)
  dir.create(zenodo_dir)
  expect_no_error(
    download_zenodo(
      doi = "10.5281/zenodo.4420858",
      path = zenodo_dir,
      parallel = FALSE
    ) |>
      suppressMessages()
  )
})
