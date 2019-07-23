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

florabank_traits <- function(connection, trait_name, collect = FALSE) {

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
#' This function takes as input either a character vector or a character string
#' of scientific names and/or Dutch names. In case of a character string,
#' partial matching will be used so that part of a scientific name or Dutch name
#' can be given and wildcards can be used. The function queries the florabank,
#' and returns a dataframe with observation level information about the matching taxa.
#' The following wildcards may be used within the input parameters in case a
#' a character string is given:
#' \itemize{
#'   \item `%`: Represents zero or more characters;
#'   \item `_`: Represents a single character;
#'   \item `[]`: Represents any single character within the brackets, e.g. `[sp]`
#'   \item `^`: Represents any character not in the brackets, e.g. `[^sp]`
#'   \item `-`: Represents a range of characters, e.g. `[a-z]`
#' }
#'
#' @param connection A connection to the florabank database. See the example
#' section for how to connect and disconnect to the database.
#'
#' @param scient_name Default missing. Can be either a character vector with
#' exact scientific names (including authorship) or a character string with
#' (part) of a taxon name which may contain wildcards
#'
#' @param dutch_name Default missing. Can be either a character vector with
#' exact Dutch names or a character string with (part) of a taxon name which
#' may contain wildcards
#'
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
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom dplyr
#' tbl
#' collect
#' sql
#'
#' @export
#' @family florabank
#' @examples
#' \dontrun{
#' # code can only be run if a connection to the database is possible
#' # connect to florabank
#' db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
#'
#' # query and collect the data using scientific name
#' Dactmacu1 <-	florabank_observations(db_connectie,
#' scient_name = 'Dactylorhiza maculata (L.) Soó', collect = TRUE)
#'
#' # the same species but using Dutch name
#' Dactmacu2 <-	florabank_observations(db_connectie,
#' dutch_name = 'Gevlekte orchis', collect = TRUE)
#'
#' # providing both a Dutch name and scientific name will not duplicate records
#' # if they are the same species
#' Dactmacu3 <- florabank_observations(db_connectie,
#' scient_name = 'Dactylorhiza maculata (L.) Soó',
#' dutch_name = 'Gevlekte orchis', collect = TRUE)
#'
#' all.equal(Dactmacu1, Dactmacu2)
#' all.equal(Dactmacu1, Dactmacu3)
#'
#' # passing dutch names and scientific names for different species
#' # is possible (records for each species is returned)
#' myspecies1 <- florabank_observations(db_connectie,
#' scient_name = 'Dactylorhiza maculata (L.) Soó',
#' dutch_name = 'Blauwe knoop', collect = TRUE)
#'
#' # passing multiple scientific names
#' myspecies2 <- florabank_observations(db_connectie,
#' scient_name = c('Dactylorhiza maculata (L.) Soó', 'Succisa pratensis Moench'),
#' collect = TRUE)
#'
#' all.equal(myspecies1, myspecies2)
#'
#' # use wildcards to retrieve all partial matches to a name
#' # using default for collect will return a lazy query
#' Dactmacu4 <-	florabank_observations(db_connectie,
#' scient_name = 'Dactylorhiza maculata%')
#'
#' # to collect the data for a lazy query you can also do:
#' Dactmacu4 <- collect(Dactmacu4)
#'
#' # disconnect from florabank
#' dbDisconnect(db_connectie)
#' }

florabank_observations <- function(connection, scient_name,
                                   dutch_name, collect = FALSE) {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")

  if (missing(scient_name) & missing(dutch_name)) {
    stop("Please provide scient_name and / or dutch_name.")
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

  if (!missing(scient_name) & !missing(dutch_name)) {
    sql_statement <- glue_sql(
      sql_statement,
      ifelse(length(dutch_name) == 1 && length(scient_name) == 1,
             "AND (tblTaxon.NaamWetenschappelijk LIKE {scient_name} OR
              tblTaxon.NaamNederlands LIKE {dutch_name})
             ",
             "AND (tblTaxon.NaamWetenschappelijk IN ({scient_name*}) OR
              tblTaxon.NaamNederlands IN ({dutch_name*})
             "),
      scient_name = scient_name,
      dutch_name = dutch_name,
      .con = connection)
  }

  if (!missing(scient_name) & missing(dutch_name)) {
    sql_statement <- glue_sql(
      sql_statement,
      ifelse(length(scient_name) == 1,
      "AND tblTaxon.NaamWetenschappelijk LIKE {scient_name}
      ",
      "AND tblTaxon.NaamWetenschappelijk IN ({scient_name*})
      "),
      scient_name = scient_name,
      .con = connection)
  }

  if (!missing(dutch_name) & missing(scient_name)) {
    sql_statement <- glue_sql(
      sql_statement,
      ifelse(length(dutch_name) == 1,
      "AND tblTaxon.NaamNederlands LIKE {dutch_name}
      ",
      "AND tblTaxon.NaamNederlands IN ({dutch_name*})
      "),
      dutch_name = dutch_name,
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
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% group_by summarize n ungroup sql collect
#' @importFrom rlang .data
#'
#' @export
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

florabank_taxon_ifbl_year <- function(connection,
                                      starting_year = 2010,
                                      ifbl_resolution = c("1km-by-1km",
                                                          "4km-by-4km"),
                                      taxongroup = c("Vaatplanten",
                                                     "Mossen",
                                                     "Lichenen (korstmossen)",
                                                     "Kranswieren"),
                                      collect = FALSE) {

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






