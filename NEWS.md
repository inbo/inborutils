# inborutils 0.2.1

* Fixed url to download knmi data. Thanks to @ViktorHartman

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
