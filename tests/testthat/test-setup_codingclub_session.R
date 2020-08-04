context("setup_codingclub_session")
library('mockery')

testthat::test_that("check class and format of the input param", {
  expect_error(setup_codingclub_session(session_date = 123456),
               "session_date must be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = TRUE),
               "session_date must be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = "a string"),
               "session_date must be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = "2020-04-02"),
               "session_date must be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
})

testthat::test_that("Check no download starts if user doesn't want", {
  # Take a snapshot of the root directory
  snapshot_before <- fileSnapshot("./", md5sum = TRUE)

  # To do this, we mock Sys.Date and readline
  # 20200528 is a valid coding club session date
  # anything except "Y" or "y" would abort download
  scs_20200528_n <- setup_codingclub_session
  stub(scs_20200528_n, 'Sys.Date', as.Date('2020-05-28'))
  stub(scs_20200528_n, 'readline', "n")

  # Take snapshot after
  snapshot_after <- fileSnapshot("./", md5sum = TRUE)

  # Check that number of unchanged files/directories is equal to all files/dirs
  expect_identical(snapshot_before, snapshot_after)
})

testthat::test_that("Nothing is changed in directory if no session exists", {
  scs_20200802_y <- setup_codingclub_session
  stub(scs_20200802_y, 'Sys.Date', as.Date('2020-08-02'))
  stub(scs_20200802_y, 'readline', "y")

  # Take a snapshot of the root directory
  snapshot_before <- fileSnapshot("./", md5sum = TRUE)

  # 20200802 is an invalid coding club session date
  expect_warning(scs_20200802_y(),
                 paste("No content for this session (20200802) found.",
                       "Is the date correct?"),
                 all = TRUE,
                 fixed = TRUE)

  # Check that number of unchanged files/directories is equal to all files/dirs
  # expect_true(
  #   nrow(snapshot$info) == length(changedFiles(snapshot)$unchanged))
  snapshot_after <- fileSnapshot("./", md5sum = TRUE)
  expect_identical(snapshot_before, snapshot_after)
})

testthat::test_that("Files and directories are created only if not exists", {
  # To do this, we mock Sys.Date and readline
  # 20200528 is a valid coding club session date
  # anything except "Y" or "y" would abort download
  scs_20200528_y <- setup_codingclub_session
  stub(scs_20200528_y, 'Sys.Date', as.Date('2020-05-28'))
  stub(scs_20200528_y, 'readline', "y")
  # Use not default directories for extra checks
  src_rel_path <- "sss"
  data_rel_path <- "ddd"
  root_dir <- "../"
  scs_20200528_y(root_dir = root_dir,
                 src_rel_path = src_rel_path,
                 data_rel_path = data_rel_path)

  src_dir <- sprintf("%s/%s", root_dir, src_rel_path)
  src_session_dir <- sprintf("%s/%s/20200528", root_dir, src_rel_path)
  data_dir <- sprintf("%s/%s", root_dir, data_rel_path)
  data_session_dir <- sprintf("%s/%s/20200528", root_dir, data_rel_path)
  # Directories are created
  expect_true(all(c(src_dir,
                src_session_dir,
                data_dir,
                data_session_dir) %in% list.dirs(path = root_dir)))
  # Files are created
  fns_src <- c("20200528_challenges.R",
               "20200528_challenges_solutions.R")
  expect_true(all(fns_src %in%
                    list.files(src_session_dir)))
  fn_data <- "20200528_births_belgium.txt"
  expect_true(fn_data %in% list.files(data_session_dir))

  # Directories and files are not overwritten
  # Take a snapshot of the directory
  snapshot_before <- fileSnapshot(root_dir)
  # Run scs_20200528_y() again with same args
  scs_20200528_y(root_dir = root_dir,
                 src_rel_path = src_rel_path,
                 data_rel_path = data_rel_path)
  snapshot_after <- fileSnapshot(root_dir)
  expect_identical(snapshot_before, snapshot_after)

  # remove files and directories after test
  unlink(src_dir, recursive = TRUE)
  unlink(data_dir, recursive = TRUE)
})
