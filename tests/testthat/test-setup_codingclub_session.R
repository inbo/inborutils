context("setup_codingclub_session")

testthat::test_that("check class and format of the input param", {
  expect_error(setup_codingclub_session(session_date = 123456),
               "session_date should be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = TRUE),
               "session_date should be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = "a string"),
               "session_date should be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = "2020-04-02"),
               "session_date should be a string representing a date (YYYYMMDD)",
               fixed = TRUE)
  expect_error(setup_codingclub_session(session_date = "20200402"), NA)
})

# TODO: test that /src and /data directories are created, if needed
# TODO: test that nothing is changed in the directory if there's an error (incorrect date format)
# TODO: test that nothing is changed in the directory if there's no session on that date (20180124)
# TODO: test the helper works fine if only data but no src (20180316)
# TODO: test the helper works fine if there are both data and src
# TODO: What to do if the downloaded file already exists? Decide, implement and test behaviour
