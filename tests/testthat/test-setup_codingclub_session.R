context("setup_codingclub_session")
library('mockery')

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
  expect_error(expect_warning(setup_codingclub_session(session_date = "20200402")), NA)
})

testthat::test_that("check that the session_date default to the current date", {
  # To do this, we mock Sys.Date and inspect the warning messages to check the result
  stub(setup_codingclub_session, 'Sys.Date', as.Date('2020-08-02'))
  expect_warning(setup_codingclub_session(), regexp = "No content for this session (20200802) found in", all= TRUE, fixed = TRUE)

})


# TODO: test that /src and /data directories are created, if needed
# TODO: test that nothing is changed in the directory if there's an error (incorrect date format)
# TODO: test that nothing is changed in the directory if there's no session on that date (20180124)
# TODO: test the helper works fine if there are both data and src
# TODO: What to do if the downloaded file already exists? Decide, implement and test behaviour
