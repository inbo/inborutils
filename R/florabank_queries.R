globalVariables("%LIKE%")

#' Query the florabank to get taxon trait values for (a) taxon trait(s)
#'
#' This function takes as input (part of) a taxon trait name, queries the florabank
#' and returns the taxon trait values in a tidy data format
#'
#' @param connection A connection to the florabank database. See the example section
#' for how to connect and disconnect to the database.
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
#' fb_rodelijstvaatplanten <- florabank_traits(db_connectie, "Rode lijst Vaatplanten")
#'
#' #if the trait_name argument is missing, a list of possible names is printed
#' florabank_traits(db_connectie)
#'
#' #disconnect from florabank
#' dbDisconnect(db_connectie)
#' }
#'
#' @name florabank_traits-deprecated
#' @usage florabank_traits(connection, trait_name, collect = FALSE)
#' @seealso \code{\link{inborutils-deprecated}}
#' @keywords internal
NULL

#' @rdname inborutils-deprecated
#' @section florabank_traits:
#' For \code{florabank_traits}, use [inbodb::florabank_traits()](https://inbo.github.io/inbodb/reference/get_florabank_traits.html)
#'
#' @importFrom dplyr
#' tbl
#' collect
#' distinct
#' pull
#' %>%
#' inner_join
#' left_join
#' filter
#' select
#' rename
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#'
#' @export

florabank_traits <- function(connection, trait_name, collect = FALSE) {

  .Deprecated("inbodb::get_florabank_traits()", package = "inborutils")

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  if (missing(trait_name)) {
    traitnames <- tbl(connection, "tblTaxonKenmerk") %>%
      distinct(.data$Naam) %>%
      collect() %>%
      pull(.data$Naam)
    stop(paste0("Please provide (part of) a trait name from this list: ",
                paste(traitnames, collapse = ", ")))
  }

  trait_name <- tolower(trait_name)

  fb_taxon <- tbl(connection, "tblTaxon")
  fb_taxon_kenmerk <- tbl(connection, "tblTaxonKenmerk")
  fb_taxon_kenmerk_waarde <- tbl(connection, "tblTaxonKenmerkWaarde")
  rel_taxon_taxon_kenmerk_waarde <- tbl(connection, "relTaxonTaxonKenmerkWaarde")

  query_result <- rel_taxon_taxon_kenmerk_waarde %>%
    inner_join(fb_taxon_kenmerk %>%
                 filter(tolower(.data$Naam) %LIKE%
                          paste0("%", trait_name, "%")) %>%
                 select(.data$ID, .data$Naam),
               by = c("TaxonKenmerkID" = "ID")) %>%
    select(-.data$Omschrijving) %>%
    left_join(fb_taxon_kenmerk_waarde %>%
                distinct(.data$ID, .data$Code, .data$TaxonKenmerkID,
                         .data$Omschrijving, .data$Rekenwaarde),
              by = c("TaxonKenmerkID" = "TaxonKenmerkID",
                     "TaxonKenmerkWaardeID" = "ID")) %>%
    left_join(fb_taxon %>%
                rename(NaamAfkorting = .data$Code),
              by = c("TaxonID" = "ID")) %>%
    distinct(.data$TaxonID,
             TaxonAfkorting = .data$NaamAfkorting,
             TaxonWetenschappelijk = .data$NaamWetenschappelijk,
             TaxonNederlands = .data$NaamNederlands,
             Kenmerk = .data$Naam,
             .data$Code,
             .data$Omschrijving,
             .data$Rekenwaarde
    )
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}

#' Get all validated observations for one or more taxa from the florabank database
#'
#' This function takes as input a character vector with one or more names of
#' species either as scientific names and/or Dutch names. By default (fixed = FALSE),
#' partial matching will be used (the names are prepended with appended with %).
#' The function queries the florabank, and returns a dataframe with observation
#' level information about the matching taxa.
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param names Default missing. A character vector with scientific names
#' and/or Dutch names. If fixed = TRUE, character strings are matched exactly and
#' scientific names must include authorship in order to match.
#'
#' @param fixed Logical. If TRUE, names is to be matched as is (no partial matching)
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
#' succprat1 <-	florabank_observations(db_connectie,
#' names = 'Succisa pratensis Moench', collect = TRUE)
#'
#' # the same species but using Dutch name
#' succprat2 <-	florabank_observations(db_connectie,
#' names = 'Blauwe knoop', collect = TRUE)
#'
#' # providing both a Dutch name and scientific name will not duplicate records
#' # if they are the same species
#' succprat3 <- florabank_observations(db_connectie,
#' names = c("Succisa pratensis Moench", "Blauwe knoop"), collect = TRUE)
#'
#' all.equal(succprat1, succprat2)
#' all.equal(succprat1, succprat3)
#'
#' # passing dutch names and scientific names for different species
#' # is possible (records for each species is returned)
#' myspecies1 <- florabank_observations(db_connectie,
#' names = c('Succisa pratensis Moench', 'Gevlekte orchis'), collect = TRUE)
#'
#' # passing multiple dutch names
#' myspecies2 <- florabank_observations(db_connectie,
#' names = c('Gevlekte orchis', 'Blauwe knoop'),
#' collect = TRUE)
#'
#' all.equal(myspecies1, myspecies2)
#'
#' # using default for collect will return a lazy query
#' # fixed = TRUE for exact matches only
#' myspecies3 <-	florabank_observations(db_connectie,
#' names = c('Succisa pratensis Moench', 'Gevlekte orchis'),
#' fixed = TRUE)
#'
#' # to collect the data for a lazy query you can also use the collect() function:
#' myspecies3 <- dplyr::collect(myspecies3)
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }
#'
#' @name florabank_observations-deprecated
#' @usage florabank_observations(connection, names, fixed = FALSE, collect = FALSE)
#' @seealso \code{\link{inborutils-deprecated}}
#' @keywords internal
NULL

#' @rdname inborutils-deprecated
#' @section florabank_observations:
#' For \code{florabank_observations}, use [inbodb::florabank_observations()](https://inbo.github.io/inbodb/reference/get_florabank_observations.html)
#'
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom dplyr
#' tbl
#' collect
#' sql
#'
#' @export

florabank_observations <- function(connection, names, fixed = FALSE,
                                   collect = FALSE) {

  .Deprecated("inbodb::get_florabank_observations()", package = "inborutils")

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  if (missing(names)) {
    stop("Please provide names.")
  }

  sql_statement <- "SELECT DISTINCT
      tblTaxon.NaamNederlands
  , tblTaxon.NaamWetenschappelijk
  , cdeBron.Omschrijving AS Bron
  , tblWaarneming.BeginDatum
  , tblWaarneming.EindDatum
  , tblIFBLHok.Code AS hok
  , tblWaarneming.Opmerking AS Toponiem
  , tblMeting.CommentaarTaxon
  , tblMeting.CommentaarHabitat
  , tblWaarneming.ID AS WaarnemingID
  , tblWaarneming.Cor_X AS X_waarneming
  , tblWaarneming.Cor_Y AS Y_waarneming
  , tblMeting.Cor_X AS X_meting
  , tblMeting.Cor_Y AS Y_meting
  FROM dbo.tblWaarneming
  INNER JOIN dbo.tblMeting ON tblWaarneming.ID = tblMeting.WaarnemingID
  INNER JOIN dbo.relTaxonTaxon ON relTaxonTaxon.TaxonIDChild = tblMeting.TaxonID
  INNER JOIN dbo.tblTaxon ON tblTaxon.ID = relTaxonTaxon.TaxonIDParent
  LEFT JOIN dbo.tblIFBLHok ON tblIFBLHok.ID = tblWaarneming.IFBLHokID
  INNER JOIN dbo.cdeBron ON cdeBron.Code = tblWaarneming.BronCode
  WHERE 1=1
  AND (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
  "

  if (!fixed) {
    like_string <-
      paste0("AND (",
             paste0(
               c(paste0("tblTaxon.NaamNederlands", " LIKE ", "'%", names,"%'"),
                 paste0("tblTaxon.NaamWetenschappelijk", " LIKE ", "'%", names,"%'")),
               collapse = " OR "),
             ")")
    sql_statement <- glue_sql(
      sql_statement,
      like_string,
      .con = connection)
  } else {
    sql_statement <- glue_sql(
      sql_statement,
      "AND (tblTaxon.NaamWetenschappelijk IN ({names*}) OR
             tblTaxon.NaamNederlands IN ({names*}))
             ",
      names = names,
      .con = connection)
  }

  sql_statement <- glue_sql(
    sql_statement,
    "ORDER BY tblWaarneming.BeginDatum DESC OFFSET 0 ROWS",
    .con = connection)

  sql_statement <- iconv(sql_statement, from =  "UTF-8", to = "latin1")

  query_result <- tbl(connection, sql(sql_statement))
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}



#' Get unique combinations of taxon, IFBL-square and year.
#'
#' This functions queries all validated observations of the florabank database
#' and returns unique combinations of taxon, IFBL-square and year. Either a 1 km
#' by 1 km or a 4 km x 4 km resolution can be chosen and a begin year can be set.
#' Observations of taxa at genus level or higher are excluded. The taxonomic
#' group can be chosen.
#'
#' @param connection A connection to the florabank database. See the example section
#' for how to connect and disconnect to the database.
#'
#' @param starting_year Filter for observations that start from this year onwards.
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
#' ifbl_4by4 identifier within which the ifbl_1by1 square is located. In case the
#' resolution is 4 km x 4 km, the variable ifbl_squares is a concatenation of
#' all nested squares with observations for the taxon in the corresponding year.
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
#' fb_uur <- florabank_taxon_ifbl_year(db_connectie, starting_year = 2000,
#'  ifbl_resolution = "4km-by-4km", taxongroup = "Mossen")
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }
#'
#' @name florabank_taxon_ifbl_year-deprecated
#' @usage florabank_taxon_ifbl_year(connection,
#'   starting_year = 2010, ifbl_resolution = c("1km-by-1km", "4km-by-4km"),
#'   taxongroup =
#'     c("Vaatplanten", "Mossen", "Lichenen (korstmossen)", "Kranswieren"),
#'   collect = FALSE)
#' @seealso \code{\link{inborutils-deprecated}}
#' @keywords internal
NULL

#' @rdname inborutils-deprecated
#' @section florabank_taxon_ifbl_year:
#' For \code{florabank_taxon_ifbl_year}, use [inbodb::florabank_taxon_ifbl_year()](https://inbo.github.io/inbodb/reference/get_florabank_taxon_ifbl_year.html)
#'
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% group_by summarize n ungroup sql collect
#' @importFrom rlang .data
#'
#' @export

florabank_taxon_ifbl_year <- function(connection,
                                      starting_year = 2010,
                                      ifbl_resolution = c("1km-by-1km",
                                                          "4km-by-4km"),
                                      taxongroup = c("Vaatplanten",
                                                     "Mossen",
                                                     "Lichenen (korstmossen)",
                                                     "Kranswieren"),
                                      collect = FALSE) {

  .Deprecated("inbodb::get_florabank_taxon_ifbl_year()", package = "inborutils")

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  assert_that(is.numeric(starting_year))
  assert_that(starting_year <= as.numeric(format(Sys.Date(), '%Y')))

  ifbl_resolution = match.arg(ifbl_resolution)
  taxongroup = match.arg(taxongroup)



  if (ifbl_resolution == "4km-by-4km") {
    glue_statement <- glue_sql(
      "SELECT DISTINCT
    tblIFBLHok.Code AS hok
    , SUBSTRING(tblIFBLHok.Code, 1, 5) AS ifbl_4by4
    , Year(tblWaarneming.BeginDatum) AS Jaar
    , relTaxonTaxon.TaxonIDParent
    , tblTaxon.Code AS Taxoncode
    FROM
    (((tblMeting INNER JOIN
    (tblIFBLHok INNER JOIN tblWaarneming ON tblIFBLHok.ID = tblWaarneming.IFBLHokID)
    ON tblMeting.WaarnemingID = tblWaarneming.ID)
    INNER JOIN relTaxonTaxon ON tblMeting.TaxonID = relTaxonTaxon.TaxonIDChild)
    INNER JOIN tblTaxon ON relTaxonTaxon.TaxonIDParent = tblTaxon.ID)
    INNER JOIN relTaxonTaxonGroep ON tblTaxon.ID = relTaxonTaxonGroep.TaxonID
    INNER JOIN tblTaxonGroep ON relTaxonTaxonGroep.TaxonGroepID = tblTaxonGroep.ID
    WHERE
    tblIFBLHok.Code LIKE '%-%' AND
    tblTaxon.Code NOT LIKE '%-sp' AND
    Year([tblWaarneming].[BeginDatum]) >={starting_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC OFFSET 0 ROWS",
      starting_year = starting_year,
      taxongroup = taxongroup,
      .con = connection)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- tbl(connection, sql(glue_statement))


    query_result <- query_result %>%
      group_by(.data$ifbl_4by4, .data$Jaar, .data$TaxonIDParent,
               .data$Taxoncode) %>%
      #paste with collapse does not translate to sql
      #str_flatten() is not available for Microsoft SQL Server
      #sql(STRING_AGG("hok", ",")) also does not work
      #fix this later
      summarize(#ifbl_squares = paste(hok, collapse = '|'),
                ifbl_number_squares = n()) %>%
      ungroup()

    if (!isTRUE(collect)) {
      return(query_result)
    } else {
      query_result <- query_result %>%
        collect()
      return(query_result)
    }
  }

  glue_statement <- glue_sql(
    "SELECT DISTINCT
    tblIFBLHok.Code AS ifbl_1by1
    , SUBSTRING(tblIFBLHok.Code, 1, 5) AS ifbl_4by4
    , Year(tblWaarneming.BeginDatum) AS Jaar
    , relTaxonTaxon.TaxonIDParent
    , tblTaxon.Code AS Taxoncode
    FROM
    (((tblMeting INNER JOIN
    (tblIFBLHok INNER JOIN tblWaarneming ON tblIFBLHok.ID = tblWaarneming.IFBLHokID)
    ON tblMeting.WaarnemingID = tblWaarneming.ID)
    INNER JOIN relTaxonTaxon ON tblMeting.TaxonID = relTaxonTaxon.TaxonIDChild)
    INNER JOIN tblTaxon ON relTaxonTaxon.TaxonIDParent = tblTaxon.ID)
    INNER JOIN relTaxonTaxonGroep ON tblTaxon.ID = relTaxonTaxonGroep.TaxonID
    INNER JOIN tblTaxonGroep ON relTaxonTaxonGroep.TaxonGroepID = tblTaxonGroep.ID
    WHERE
    tblIFBLHok.Code LIKE '%-%-%' AND
    tblTaxon.Code NOT LIKE '%-sp' AND
    Year([tblWaarneming].[BeginDatum]) >={starting_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC OFFSET 0 ROWS",
    starting_year = starting_year,
    taxongroup = taxongroup,
    .con = connection)
  glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
  query_result <- tbl(connection, sql(glue_statement))
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}






