
#' Add species information provided by the GBIF taxonomic backbone API
#'
#' This functions extends an existing dataframe with additional columns provided
#' by the GBIF taxonomic backbone and matched on the species scientific name,
#' which need to be an available column in the dataframe.
#'
#' This function is essentially a wrapper around the existing rgbif
#' `name_backbone` and extends the application to a data.frame. For more
#' information on the name matching API of GBIF on which rgbif relies, see
#' <https://www.gbif.org/developer/species/#searching>.
#'
#' @param df data.frame with species information
#' @param name char column name of the column containing the scientific names
#'   used for the name matching with the GBIF taxonomic backbone. Default:
#'   "name".
#' @param gbif_terms list of valid GBIF terms to add as additional columns to
#' the data.frame. Default: usageKey, scientificName, rank, order, matchType,
#' phylum, kingdom, genus, class, confidence, synonym, status, family.
#' @param ... any parameter to pass to rgbif function `name_bakbone`. One of:
#'   rank, kingdom, phylum, class, order, family, genus, strict, verbose, start,
#'   limit, curlopts. See `?name_backbone` for more details.
#'
#' @return a tibble data.frame with GBIF information as additional columns.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertable assert_colnames
#' @importFrom dplyr %>% bind_cols select
#' @importFrom purrr pmap_dfr map_chr
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' library(readr)
#' library(dplyr)
#' species_list <- read_csv(paste0("https://raw.githubusercontent.com/inbo",
#'                                 "/inbo-pyutils/master/gbif/gbif_name_match",
#'                                 "/sample.csv"),
#'                          trim_ws = TRUE, col_types = cols())
#' # basic usage
#' species_list %>%
#'   gbif_species_name_match()
#' # pass optional parameters to name_backbone
#' species_list %>%
#'   gbif_species_name_match(name = "name", kingdom = "kingdom", strict = TRUE)
#' # select GBIF terms
#' species_list %>%
#'   gbif_species_name_match(gbif_terms = c('scientificName','rank'))
#' }
gbif_species_name_match <- function(df,
                                    name = "name",
                                    gbif_terms = c('usageKey',
                                                   'scientificName','rank',
                                                   'order',
                                                   'matchType',
                                                   'phylum',
                                                   'kingdom',
                                                   'genus',
                                                   'class',
                                                   'confidence',
                                                   'synonym',
                                                   'status',
                                                   'family'),
                                    ...){
  inargs <- list(...)
  API_terms <- c('usageKey', 'scientificName', 'canonicalName', 'rank',
                 'status', 'confidence', 'matchType', 'kingdom', 'phylum',
                 'order', 'family', 'genus', 'species', 'kingdomKey',
                 'phylumKey', 'classKey', 'orderKey', 'familyKey', 'genusKey',
                 'speciesKey', 'synonym', 'class')

  # test incoming arguments
  assert_that(is.data.frame(df))
  if (!is.null(inargs$name_col)) {
    inargs$name <- inargs$name_col
    warning("\'name_col\' is deprecated. Use \'name\' instead.")
    inargs$name_col <- NULL
  }
  # column with names exists in  df
  assert_colnames(df, name, only_colnames = FALSE) # colname exists in df
  # GBIF terms to add as additional columns to df
  gbif_terms <- match.arg(gbif_terms, API_terms, several.ok = TRUE)
  # make df with names only
  name_df <- select(df, eval(name))
  colnames(name_df) <- "name" # rename to "name"
  # optional fields accepted by name_backbone
  name_backbone_fields <- c("rank",
                            "kingdom",
                            "phylum",
                            "class",
                            "order",
                            "family",
                            "genus",
                            "strict",
                            "verbose",
                            "start",
                            "limit",
                            "curlopts")
  # Check optional parameters are all search name parameters
  assert_that(all(names(inargs) %in% name_backbone_fields),
              msg = paste0(
                "Only optional parameters of GBIF name search allowed: ",
                paste(name_backbone_fields, collapse = ", "),
                ". Check ?name_backbone for more details."))
  # optional fields defined by user
  search_terms <- names(inargs)[which(names(inargs) %in% name_backbone_fields)]
  if (length(search_terms) > 0) {
    inargs <- inargs[which(names(inargs) %in% search_terms)]
    # subset with taxonomic related fields accepted by name_backbone
    taxa_terms <- c("rank",
                    "kingdom",
                    "phylum",
                    "class",
                    "order",
                    "family",
                    "genus")
    taxaargs <- inargs[which(names(inargs) %in% taxa_terms)]
    taxa_terms <- names(taxaargs)
    # other not taxonomic related fields
    otherargs <- inargs[which(!names(inargs) %in% taxa_terms)]
    other_terms <- names(otherargs)
    if (length(taxa_terms) > 0) {
      taxa_df <- select(df, eval(map_chr(taxaargs, function(x) x[[1]])))
      names(taxa_df) <- taxa_terms
      name_df <- bind_cols(name_df, taxa_df)
    }
    if (length(other_terms) > 0) {
      name_df[other_terms] <- as.data.frame(otherargs)
    }
  }
  name_df <-
    name_df %>%
    pmap_dfr(get_name_gbif) %>%
    as_tibble()
  bind_cols(df, name_df) %>%
    select(c(colnames(df), gbif_terms))
}

#' Helper function for retrieving informations from GBIF Taxonomy Backbone
#'
#' @importFrom rgbif name_backbone
#' @keywords internal
get_name_gbif <- function(...) {
  args <- list(...)
  # NA are accepted at dev level (devtools::install_github("ropensci/rgbif"),
  # but still not in CRAN version. Only NULL accepted.
  args[which(is.na(args))] <- NULL
  do.call(name_backbone, args)
}
