test_that("Get features from a WFS works", {

  testwfs <- "https://geo.api.vlaanderen.be/StatistischeSectoren/wfs"

  expect_identical(
    get_feature_wfs(
      wfs = testwfs,
      version = "2.0.0",
      layername = "StatistischeSectoren:StatSec",
      crs = "EPSG:31370",
      result_type = "hits"),
    "10000")

  first2 <- get_feature_wfs(
    wfs = testwfs,
    version = "2.0.0",
    layername = "StatistischeSectoren:StatSec",
    crs = "EPSG:31370",
    result_type = "results",
    count = 2)

  expect_is(
    first2, "sf"
  )

})
