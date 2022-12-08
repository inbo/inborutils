test_that("Get a coverage from a WCS works", {
  bbox <- sf::st_bbox(
    c(xmin = 150800, xmax = 150850, ymin = 200800, ymax = 200850),
    crs = sf::st_crs(31370))
  expect_is(
    get_coverage_wcs(wcs = "dsm",
                   bbox = bbox,
                   layername = "EL.GridCoverage.DSM",
                   resolution = 1),
    "SpatRaster"
  )
})
