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
  mtcars_sqlite <- "./mtcars_sqlite.sqlite"
  table_name_1 <- "mtcars"
  table_name_2 <- "mtcars_selected_columns"

  # all columns
  csv_to_sqlite(csv_file = mtcars_csv,
                sqlite_file = mtcars_sqlite,
                table_name = table_name_1,
                pre_process_size = 10,
                chunk_size = 5,
                delim = ",")
  # selection of columns
  csv_to_sqlite(csv_file = mtcars_csv,
                sqlite_file = mtcars_sqlite,
                table_name = table_name_2,
                pre_process_size = 10,
                chunk_size = 5,
                delim = ",",
                col_types = cols_subset
  )


  mtcars_sqlite <- dbConnect(SQLite(), dbname = mtcars_sqlite)

  ## Get values
  query_all <- glue_sql(
    "SELECT * FROM {table}",
    table = table_name_1,
    .con = mtcars_sqlite
  )
  query_selection <- glue_sql(
    "SELECT * FROM {table}",
    table = table_name_2,
    .con = mtcars_sqlite
  )

  mtcars_1 <- dbGetQuery(mtcars_sqlite, query_all)
  mtcars_2 <- dbGetQuery(mtcars_sqlite, query_selection)

  dbDisconnect(mtcars_sqlite)

  # check original data on selected columns
  mtcars_original_cols_only <- mtcars %>%
    select(mpg, hp, gear) %>%
    as.data.frame()

  testthat::expect_equal(
    mtcars_2,
    mtcars_original_cols_only
  )

  # check original full dataset
  mtcars_df <- mtcars %>% as.data.frame()
  attr(mtcars_df, "spec") <- NULL
  testthat::expect_equal(
    mtcars_1,
    mtcars_df
    )

  setwd(oldwd)
})
