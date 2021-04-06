test_that("csv to sqlite works", {
  library(dplyr)
  library(readr)
  library(glue)
  library(odbc)
  library(RSQLite)

  cols_subset <- cols_only(
    mpg = col_double(),
    hp = col_double(),
    gear = col_double()
  )

  mtcars <- read_csv(readr_example("mtcars.csv"))
  temp_dir <- tempdir()
  oldwd <- getwd()
  setwd(temp_dir)
  mtcars_csv <- "mtcars.csv"
  write_csv(mtcars, mtcars_csv)
  mtcars_sqlite_cols_only <- "./mtcars_sqlite_cols_only.sqlite"
  table_name <- "mtcars"

  csv_to_sqlite(csv_file = mtcars_csv,
                sqlite_file = mtcars_sqlite_cols_only,
                table_name = table_name,
                pre_process_size = 10,
                chunk_size = 5,
                delim = ",",
                col_types = cols_subset
  )

  mtcars_sqlite_cols_only <- dbConnect(SQLite(), dbname = mtcars_sqlite_cols_only)

  ## Get values
  query <- glue_sql(
    "SELECT * FROM {table}",
    table = table_name,
    .con = mtcars_sqlite_cols_only
  )

  mtcars_cols_only <- dbGetQuery(mtcars_sqlite_cols_only, query)

  dbDisconnect(mtcars_sqlite_cols_only)
  # check original data
  mtcars_original_cols_only <- mtcars %>%
    select(mpg, hp, gear) %>%
    as.data.frame()

  expect_equal(
    mtcars_cols_only,
    mtcars_original_cols_only
    )

  setwd(oldwd)
})
