context("gbif_species_name_match")

names <- c("Lagenaceae", # family
           "Borzinemataceae", # family
           "Mastigamoebaceae", # family
           "Acipenseriformes" # order
           )
kingdoms <- c("Chromista",
              "Bacteria",
              "Protozoa",
              "Animalia")

df <- data.frame(name = names,
                 kingdom_col = kingdoms,
                 stringsAsFactors = FALSE)

df2 <- read.csv(paste0("https://raw.githubusercontent.com/inbo",
                                "/inbo-pyutils/master/gbif/gbif_name_match",
                                "/sample.csv"),
                strip.white = TRUE)

# column with names is called as one of the returned cols from GBIF
df3 <- df[1:3,]
names(df3) <- c("family", "kingdom_col")

testthat::test_that("check classes of input params", {
  expect_error(gbif_species_name_match(df = names),
               "Error: df. Expected a data.frame. Got an object of class character.")
  expect_error(gbif_species_name_match(df = df, name = 1),
               "name. Expected a character. Got an object of class numeric.")
  expect_error(gbif_species_name_match(df = df, gbif_terms = c(4,3)),
               "gbif_terms. Expected a character. Got an object of class numeric.")
})

testthat::test_that("check validity of parameter values", {
  # wrong name parameter
  expect_error(gbif_species_name_match(df3, gbif_terms = c("family")),
               paste("These columns exist in colnames but not",
                     "in your dataframe: name and these exist in your",
                     "dataframe but not in colnames: family kingdom_col"))
  # wrong GBIF name_backbone parameter
  expect_error(gbif_species_name_match(df, kingdomsss = "kingdom_col"),
               paste("Only optional parameters of GBIF name search",
                     "allowed: rank, kingdom, phylum, class, order, family,",
                     "genus, strict, verbose, start, limit, curlopts.",
                     "Check ?name_backbone for more details."),
               fixed = TRUE
  )
  # wrong GBIF name_backbone parameters
  expect_error(gbif_species_name_match(df,
                                         kingdomsss = "kingdom_col",
                                         strinctsss = TRUE),
               paste("Only optional parameters of GBIF name search",
                     "allowed: rank, kingdom, phylum, class, order, family,",
                     "genus, strict, verbose, start, limit, curlopts.",
                     "Check ?name_backbone for more details."),
               fixed = TRUE
  )
  # name column is equal to one of the returned GIBF terms
  expect_warning(gbif_species_name_match(df3,
                                           name = "family",
                                           gbif_terms = c("familyKey",
                                                          "family")),
                 paste0("Column with names \'family\' is also one of the",
                        " returned gbif_terms. GBIF column name is",
                        " authomatically recalled \'family1\'."))
  # some GBIF terms not returned
  expect_warning(gbif_species_name_match(df, gbif_terms = c("class",
                                                              "classKey",
                                                              "genus",
                                                              "genusKey")),
                 paste("The following terms are not returned by GBIF:",
                       "genus, genusKey. It is possible they refer to ranks",
                       "lower than rank of matched names.")
  )
})

testthat::test_that("Test output", {
  requested_gbif_terms <- c("class", "classKey")
  out <- gbif_species_name_match(df, gbif_terms = requested_gbif_terms)
  out2 <- gbif_species_name_match(df2, gbif_terms = c("species",
                                                        "kingdom"))
  expect_true(all(names(df) %in% names(out)))
  expect_true(all(names(out) %in% c(names(df), names(out))))
  expect_true(all(requested_gbif_terms %in% names(out)))
  expect_true(nrow(out) == nrow(df))
  expect_true(all(is.na(out$genus)))
  expect_true(all(is.na(out$genusKey)))
  expect_true(all(out$name == df$name))
  expect_true(all(out$kingdom_col == df$kingdom_col))
})
