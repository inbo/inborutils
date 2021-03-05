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
  snapshot <- fileSnapshot("./", md5sum = TRUE)

  # To do this, we mock Sys.Date and readline
  # 20200528 is a valid coding club session date
  # anything except "Y" or "y" would abort download
  scs_20200528_n <- setup_codingclub_session
  stub(scs_20200528_n, 'Sys.Date', as.Date('2020-05-28'))
  stub(scs_20200528_n, 'readline', "n")

  # Check that number of unchanged files/directories is equal to all files/dirs
  expect_true(all(changedFiles(snapshot)$changes[,"mtime"] == FALSE))
})

testthat::test_that("Nothing is changed in directory if no session exists", {
  scs_20200802_y <- setup_codingclub_session
  stub(scs_20200802_y, 'Sys.Date', as.Date('2020-08-02'))
  stub(scs_20200802_y, 'readline', "y")

  # Take a snapshot of the root directory
  snapshot <- fileSnapshot("./", md5sum = TRUE)

  # 20200802 is an invalid coding club session date, 2 warnings returned
  warnings_scs_20200802_y <- capture_warnings(scs_20200802_y())
  expect_true(length(warnings_scs_20200802_y) == 2)
  expect_match(warnings_scs_20200802_y,
    "No src files found for session 20200802. Is the date correct?",
    all = FALSE,
    fixed = TRUE
  )
  expect_match(warnings_scs_20200802_y,
    "No data files found for session 20200802. Is the date correct?",
    all = FALSE
  )

  # Check that number of unchanged files/directories is equal to all files/dirs
  expect_true(all(changedFiles(snapshot)$changes[,"mtime"] == FALSE))
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
  snapshot <- fileSnapshot(root_dir)
  # Run scs_20200528_y() again with same args
  scs_20200528_y(root_dir = root_dir,
                 src_rel_path = src_rel_path,
                 data_rel_path = data_rel_path)
  # modified time is the same for all files/directories
  expect_true(all(changedFiles(snapshot)$changes[,"mtime"] == FALSE))

  # remove files and directories after test
  unlink(src_dir, recursive = TRUE)
  unlink(data_dir, recursive = TRUE)
})
