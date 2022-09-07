context("transform_coordinates")
library(sf)
library(purrr)
library(readr)

data_pts <- read_tsv("./data_test_project_coordinate/data_pts_input.tsv")
# EPSG 4269
if (requireNamespace("sp")) {
  sp_crs1 <- sp::CRS(
    "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    )
  sp_crs2 <- sp::CRS(
    paste0(
      "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0",
      " +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")
    )
}
sf_crs1 <- st_crs(4269)
# EPSG 3857
sf_crs2 <- st_crs(3857)

data_out_sp_sp <- transform_coordinates(data_pts,
  col_x = lon, col_y = lat,
  crs_input = sp_crs1,
  crs_output = sp_crs2
)

data_out_sp_sf <- transform_coordinates(data_pts,
  col_x = lon, col_y = lat,
  crs_input = sp_crs1,
  crs_output = sf_crs2
)

data_out_sf_sp <- transform_coordinates(data_pts,
  col_x = lon, col_y = lat,
  crs_input = sf_crs1,
  crs_output = sp_crs2
)

data_out_sf_sf <- transform_coordinates(data_pts,
  col_x = lon, col_y = lat,
  crs_input = sf_crs1,
  crs_output = sf_crs2
)

read_reproj_df_sp <- read_tsv(paste0(
  "./data_test_project_coordinate/",
  "reproject_epsg4269_to_epsg3857_sp.tsv"
))
read_reproj_df_sf <- read_tsv(paste0(
  "./data_test_project_coordinate/",
  "reproject_epsg4269_to_epsg3857_sf.tsv"
))


testthat::test_that("check classes of input params", {
  expect_error(
    transform_coordinates(c(1, 2, 3),
      col_x = x,
      col_y = y,
      crs_input = sp_crs1,
      crs_output = sp_crs2
    )
  )

  expect_error(
    transform_coordinates(data.frame(
      id = c(1, 2, 3),
      x = c(1.23, 2.34, 1.54),
      y = c(34.12, 4.04, 5.09),
      stringsAsFactors = FALSE
    ),
    col_x = x,
    col_y = y,
    crs_input = "this is not a crs class",
    crs_output = sp_crs2
    ),
    "Input CRS should be an object of class \"CRS\" or \"crs\"."
  )

  expect_error(
    transform_coordinates(data.frame(
      id = c(1, 2, 3),
      x = c(1.23, 2.34, 1.54),
      y = c(34.12, 4.04, 5.09),
      stringsAsFactors = FALSE
    ),
    col_x = x,
    col_y = y,
    crs_input = sp_crs1,
    crs_output = "Houston, we\'ve got a problem"
    ),
    "Output CRS should be an object of class \"CRS\" or \"crs\"."
  )
})

testthat::test_that("check support quasiquotation (xy cols)", {
  expect_identical(
    transform_coordinates(data_pts,
      col_x = 3,
      col_y = 2,
      crs_input = sf_crs1,
      crs_output = sf_crs2
    ),
    transform_coordinates(data_pts,
      col_x = "lon",
      col_y = "lat",
      crs_input = sf_crs1,
      crs_output = sf_crs2
    )
  )

  expect_identical(
    transform_coordinates(data_pts,
      col_x = lon,
      col_y = lat,
      crs_input = sf_crs1,
      crs_output = sf_crs2
    ),
    transform_coordinates(data_pts,
      col_x = "lon",
      col_y = "lat",
      crs_input = sf_crs1,
      crs_output = sf_crs2
    )
  )
})

testthat::test_that("check presence of XY coords (at least two numeric cols)", {
  expect_error(
    transform_coordinates(data.frame(
      id = c(1, 2, 3),
      x = c("1.23", "2.34", "1.54"),
      y = c(34.12, 4.04, 5.09),
      stringsAsFactors = FALSE
    ),
    col_x = x,
    col_y = y,
    crs_input = sp_crs1,
    crs_output = sp_crs2
    ),
    "x coordinates (longitude) should be numbers.",
    fixed = TRUE
  )

  expect_error(
    transform_coordinates(data.frame(
      id = c(1, 2, 3),
      x = c(34.12, 4.04, 5.09),
      y = c("1.23", "2.34", "1.54"),
      stringsAsFactors = FALSE
    ),
    col_x = x,
    col_y = y,
    crs_input = sp_crs1,
    crs_output = sp_crs2
    ),
    "y coordinates (latitude) should be numbers.",
    fixed = TRUE
  )
})

testthat::test_that("Same CRS input and output", {
  expect_warning(
    transform_coordinates(data_pts,
      col_x = lon, col_y = lat,
      crs_input = sp_crs1,
      crs_output = sp_crs1
    ),
    paste(
      "Input CRS equal to output CRS",
      "No transform performed."
    )
  )

  expect_warning(
    transform_coordinates(data_pts,
      col_x = lon, col_y = lat,
      crs_input = sf_crs1,
      crs_output = sf_crs1
    ),
    paste(
      "Input CRS equal to output CRS",
      "No transform performed."
    )
  )
})

testthat::test_that(paste(
  "input and output projections are both CRS-class",
  "(sp points)"
), {
  expect_equal(as.data.frame(data_out_sp_sp), as.data.frame(read_reproj_df_sp))
})

testthat::test_that(paste(
  "input projections of CRS-class (sp points),",
  "output of crs-class (sf points)"
), {
  expect_equal(as.data.frame(data_out_sp_sf), as.data.frame(read_reproj_df_sf))
})

testthat::test_that(paste(
  "input projections of crs-class (sf points),",
  "output of CRS-class (sp points)"
), {
  expect_equal(as.data.frame(data_out_sf_sp), as.data.frame(read_reproj_df_sp))
})

testthat::test_that(paste(
  "input and output projections are both crs-class",
  "(sf points)"
), {
  expect_equal(as.data.frame(data_out_sf_sf), as.data.frame(read_reproj_df_sf))
})
