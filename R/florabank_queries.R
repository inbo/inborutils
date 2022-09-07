globalVariables("%LIKE%")

#' Query the florabank to get taxon trait values for (a) taxon trait(s)
#'
#' `r lifecycle::badge('defunct')`
#' This function takes as input (part of) a taxon trait name, queries the
#' florabank and returns the taxon trait values in a tidy data format
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param trait_name A (part of) a trait name for which you want to get the
#' associated taxon-specific trait values. If this is missing, the function
#' returns an error and prints a message showing all possible trait names.
#'
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A remote tbl object (collect = FALSE) or a tibble dataframe (collect
#' = TRUE) containing the trait values for each species and for all
#' partially matched traits. The dataframe contains the variables TaxonID,
#' TaxonAfkorting, TaxonWetenschappelijk, TaxonNederlands, Kenmerk, Code,
#' Omschrijving en Rekenwaarde. The first four variables identify the taxon,
#' the latter four variables relate to the taxon traits.
#'
#' @family florabank
#' @examples
#' \dontrun{
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # get all Ellenberg values via partial matching, return as lazy query
#' fb_ellenberg <- florabank_traits(db_connectie, "llenberg")
#' # collect the data
#' fb_ellenberg <- fb_ellenberg %>% collect()
#' # the same can be done by using the collect parameter
#' fb_ellenberg <- florabank_traits(db_connectie, "llenberg", collect = TRUE)
#'
#' # get all red lists via partial matching
#' fb_rodelijsten <- florabank_traits(db_connectie, "rode")
#'
#' # get only the red list for vascular plant species
#' fb_rodelijstvaatplanten <- florabank_traits(db_connectie,
#'   "Rode lijst Vaatplanten")
#'
#' # if the trait_name argument is missing, a list of possible names is printed
#' florabank_traits(db_connectie)
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }
#'
#' @name florabank_traits-defunct
#' @usage florabank_traits(connection, trait_name, collect = FALSE)
#' @seealso \code{\link{inborutils-defunct}}
#' @keywords internal
NULL

#' @rdname inborutils-defunct
#' @section florabank_traits:
#' For \code{florabank_traits}, use
#' [inbodb::florabank_traits()](
#' https://inbo.github.io/inbodb/reference/get_florabank_traits.html)
#'
#'
#' @export

florabank_traits <- function(connection, trait_name, collect = FALSE) {
  .Defunct("inbodb::get_florabank_traits()", package = "inborutils")
}

#' Get all validated observations for one or more taxa from the florabank
#' database
#'
#' `r lifecycle::badge('defunct')`
#' This function takes as input a character vector with one or more names of
#' species either as scientific names and/or Dutch names. By default (fixed =
#' FALSE),
#' partial matching will be used (the names are prepended with appended with %).
#' The function queries the florabank, and returns a dataframe with observation
#' level information about the matching taxa.
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param names Default missing. A character vector with scientific names
#' and/or Dutch names. If fixed = TRUE, character strings are matched exactly
#' and scientific names must include authorship in order to match.
#'
#' @param fixed Logical. If TRUE, names is to be matched as is (no partial
#' matching)
#' .
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with the following variables: "NaamNederlands",
#' "NaamWetenschappelijk", "Bron", "BeginDatum", "EindDatum", "hok",
#' "Toponiem", "CommentaarTaxon", "CommentaarHabitat",
#' "WaarnemingID", "X_waarneming", "Y_waarneming", "X_meting", "Y_meting"
#'
#' @family florabank
#' @examples
#' \dontrun{
#' # code can only be run if a connection to the database is possible
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # query and collect the data using scientific name
#' succprat1 <- florabank_observations(db_connectie,
#'   names = "Succisa pratensis Moench", collect = TRUE
#' )
#'
#' # the same species but using Dutch name
#' succprat2 <- florabank_observations(db_connectie,
#'   names = "Blauwe knoop", collect = TRUE
#' )
#'
#' # providing both a Dutch name and scientific name will not duplicate records
#' # if they are the same species
#' succprat3 <- florabank_observations(db_connectie,
#'   names = c("Succisa pratensis Moench", "Blauwe knoop"), collect = TRUE
#' )
#'
#' all.equal(succprat1, succprat2)
#' all.equal(succprat1, succprat3)
#'
#' # passing dutch names and scientific names for different species
#' # is possible (records for each species is returned)
#' myspecies1 <- florabank_observations(db_connectie,
#'   names = c("Succisa pratensis Moench", "Gevlekte orchis"), collect = TRUE
#' )
#'
#' # passing multiple dutch names
#' myspecies2 <- florabank_observations(db_connectie,
#'   names = c("Gevlekte orchis", "Blauwe knoop"),
#'   collect = TRUE
#' )
#'
#' all.equal(myspecies1, myspecies2)
#'
#' # using default for collect will return a lazy query
#' # fixed = TRUE for exact matches only
#' myspecies3 <- florabank_observations(db_connectie,
#'   names = c("Succisa pratensis Moench", "Gevlekte orchis"),
#'   fixed = TRUE
#' )
#'
#' # to collect the data for a lazy query you can also use the collect()
#' # function:
#' myspecies3 <- dplyr::collect(myspecies3)
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }
#'
#' @name florabank_observations-defunct
#' @usage florabank_observations(connection, names, fixed = FALSE,
#' collect = FALSE)
#' @seealso \code{\link{inborutils-defunct}}
#' @keywords internal
NULL

#' @rdname inborutils-defunct
#' @section florabank_observations:
#' For \code{florabank_observations}, use
#' [inbodb::florabank_observations()](
#' https://inbo.github.io/inbodb/reference/get_florabank_observations.html)
#'
#'
#' @export

florabank_observations <- function(connection, names, fixed = FALSE,
                                   collect = FALSE) {
  .Defunct("inbodb::get_florabank_observations()", package = "inborutils")
}



#' Get unique combinations of taxon, IFBL-square and year.
#'
#' `r lifecycle::badge('defunct')`
#' This functions queries all validated observations of the florabank database
#' and returns unique combinations of taxon, IFBL-square and year. Either a 1 km
#' by 1 km or a 4 km x 4 km resolution can be chosen and a begin year can be
#' set.
#' Observations of taxa at genus level or higher are excluded. The taxonomic
#' group can be chosen.
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param starting_year Filter for observations that start from this year
#' onwards.
#' Default is 2010.
#'
#' @param ifbl_resolution The requested spatial resolution can be either
#' 1km-by-1km IFBL squares or 4km-by-4km. Default is 1km-by-1km.
#'
#' @param taxongroup Choose for which taxonomic group you want the unique
#' combinations. One of "Vaatplanten" (the default), "Mossen", "Korstmossen"
#' of "Kranswieren".
#'
#' @param collect If FALSE (the default), a remote tbl object is returned. This
#' is like a reference to the result of the query but the full result of the
#' query is not brought into memory. If TRUE the full result of the query is
#' collected (fetched) from the database and brought into memory of the working
#' environment.
#'
#' @return A dataframe with one line for each combination of taxon, IFBL-square
#' (either at 1 km x 1 km or 4 km x 4 km resolution) and year. In case the
#' resolution is 1 km x 1 km, a variable ifbl_4by4 gives the corresponding
#' ifbl_4by4 identifier within which the ifbl_1by1 square is located.
#' In case the resolution is 4 km x 4 km, the variable ifbl_squares is a
#' concatenation of all nested squares with observations for the taxon in the
#' corresponding year.
#' This can be nested 1 x 1 squares as well as the corresponding 4 x 4 square
#' (the latter is the case if the original resolution of the observation is at
#' 4 x 4 resolution). In addition, the variable ifbl_number_squares gives the
#' number of unique nested squares where the taxon was observed for that year
#' and 4 x 4 square combination.
#'
#' @family florabank
#' @examples
#' \dontrun{
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # get records at 1 km x 1 km resolution for vascular plants from 2010
#' # (default) without collecting all data into memory (default).
#' fb_kwartier <- florabank_taxon_ifbl_year(db_connectie)
#' # to collect the data in memory set collect to TRUE or do
#' fb_kwartier <- collect(fb_kwartier)
#'
#' # get records at 4 km x 4 km resolution starting from 2000
#' fb_uur <- florabank_taxon_ifbl_year(db_connectie,
#'   starting_year = 2000,
#'   ifbl_resolution = "4km-by-4km", taxongroup = "Mossen"
#' )
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }
#'
#' @name florabank_taxon_ifbl_year-defunct
#' @usage florabank_taxon_ifbl_year(connection,
#'   starting_year = 2010, ifbl_resolution = c("1km-by-1km", "4km-by-4km"),
#'   taxongroup =
#'     c("Vaatplanten", "Mossen", "Lichenen (korstmossen)", "Kranswieren"),
#'   collect = FALSE)
#' @seealso \code{\link{inborutils-defunct}}
#' @keywords internal
NULL

#' @rdname inborutils-defunct
#' @section florabank_taxon_ifbl_year:
#' For \code{florabank_taxon_ifbl_year}, use
#' [inbodb::florabank_taxon_ifbl_year()](
#' https://inbo.github.io/inbodb/reference/get_florabank_taxon_ifbl_year.html)
#'
#'
#' @export

florabank_taxon_ifbl_year <- function(connection,
                                      starting_year = 2010,
                                      ifbl_resolution = c(
                                        "1km-by-1km",
                                        "4km-by-4km"
                                      ),
                                      taxongroup = c(
                                        "Vaatplanten",
                                        "Mossen",
                                        "Lichenen (korstmossen)",
                                        "Kranswieren"
                                      ),
                                      collect = FALSE) {
  .Defunct("inbodb::get_florabank_taxon_ifbl_year()", package = "inborutils")
}
