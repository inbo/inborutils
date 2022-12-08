# inborutils 0.3.0

* Implemented `checklist` R package checks
* New function `get_features_wfs` to get features (vector data) from a Web
  Feature Service (`WFS`)
* New function `get_coverage_wcs` to get raster data from a Web Coverage Service
  (`WCS`)
* Fixed a bug in `csv_to_sqlite` that occurred when passing `col_names` to `...`
* Removed `inboveg_*` and `florabank_*` defunct functions (moved to `inbodb`
  package)
* Removed `guess_projection`: use `guess_crs` instead
* Removed `reproject_coordinates`: use `transform_coordinates` instead
* Superseded `gbif_species_name_match` function by
  `rgbif::name_backbone_checklist()`

# inborutils 0.2.1

* Fixed url to download `knmi` data. Thanks to @ViktorHartman

# inborutils 0.2.0

* Functions no longer depend on `sp` package
* Fixed a bug that occurred if a subset of columns was read in `csv_to_sqlite`
* Deprecated `reproject_coordinates`. Use `transform_coordinates` instead.
* Deprecated `guess_projection`. Use `guess_crs` instead.

# inborutils 0.1.5

* Fixes missing links on pkgdown website

# inborutils 0.1.4

* Changed setup_coding_club_session so it works without authentication (#111)
* Added a `NEWS.md` file to track changes to the package.
