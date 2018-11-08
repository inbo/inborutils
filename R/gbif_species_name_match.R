
#' Add species information provided by the GBIF taxonomic backbone API
#'
#' This functions extends an existing dataframe with additional columns provided
#' by the GBIF taxonomic backbone and matched on the species scientific name,
#' which need to be an available column in the dataframe.
#'
#' This function is essentially a wrapper around the existing rgbif
#' functionality `name_lookup()` and extends the application to a data.frame.
#' For more information on the name matching API of GBIF on which rgbif relies,
#' see https://www.gbif.org/developer/species
#'
#' @param df data.frame with species information
#' @param name_col char column name of the column containing the scientific
#' names used for the name matching with the GBIF taxonomic backbone.
#' @param gbif_terms list of valid GBIF terms to add as additional columns to
#' the data.frame
#'
#' @return df with GBIF information as additional columns
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr %>% rowwise do select bind_cols do_
#' @importFrom rgbif name_backbone
#' @importFrom lazyeval interp
gbif_species_name_match <- function(df, name_col,
                                    gbif_terms = c('usageKey',
                                                   'scientificName',
                                                   'rank',
                                                   'order',
                                                   'matchType',
                                                   'phylum',
                                                   'kingdom',
                                                   'genus',
                                                   'class',
                                                   'confidence',
                                                   'synonym',
                                                   'status',
                                                   'family')){

    API_terms <- c('usageKey', 'scientificName', 'canonicalName', 'rank',
                   'status', 'confidence', 'matchType', 'kingdom', 'phylum',
                   'order', 'family', 'genus', 'species', 'kingdomKey',
                   'phylumKey', 'classKey', 'orderKey', 'familyKey', 'genusKey',
                   'speciesKey', 'synonym', 'class')

    # test incoming arguments
    assert_that(is.data.frame(df))
    assert_colnames(df, name_col, only_colnames = FALSE) # colname exists in df
    gbif_terms <- match.arg(gbif_terms, API_terms, several.ok = TRUE)

    # matching the GBiF matching information to the sample_data
    df %>%
        mutate(
            scientific_name = df[[name_col]],
            gbif = map(scientific_name, name_backbone) %>%
                map(as.tibble)
        ) %>%
        unnest() %>%
        select(c(colnames(df), gbif_terms))
}
