#' Query the florabank to get taxon trait values for (a) taxon trait(s)
#'
#' This function takes as input (part of) a taxon trait, queries the florabank
#' and returns the taxon trait values in a tidy data format
#'
#' @param trait_name A (part of) a trait name for which you want to get the
#' associated taxon-specific trait values. If this is missing, the function
#' returns an error and prints a message showing all possible trait names.
#'
#' @return A dataframe containing the trait values for each species and for all
#' partially matched traits. The dataframe contains the variables TaxonID,
#' TaxonAfkorting, TaxonWetenschappelijk, TaxonNederlands, Kenmerk, Code,
#' Omschrijving en Rekenwaarde. The first four variables identify the taxon,
#' the latter four variables relate to the taxon traits.
#'
#' @importFrom DBI dbDisconnect
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # get all Ellenberg values via partial matching
#' fb_ellenberg <- florabank_traits("llenberg")
#' # get all red lists via partial matching
#' fb_rodelijsten <- florabank_traits("rode")
#' # get only the red list for vascular plant species
#' fb_rodelijstvaatplanten <- florabank_traits("Rode lijst Vaatplanten")
#' #if the trait_name argument is missing, a list of possible names is printed
#' florabank_traits()
#' }

florabank_traits <- function(trait_name) {
  db_connectie <- connect_inbo_dbase("D0021_00_userFlora")

  if (missing(trait_name)) {
    traitnames <- tbl(db_connectie, "tblTaxonKenmerk") %>%
      distinct(Naam) %>%
      collect() %>%
      pull(Naam)
    dbDisconnect(db_connectie)
    message(paste(traitnames, collapse = ", "))
    stop("Please provide (part of) a trait name from the above list.")
  }

  trait_name <- tolower(trait_name)

  fb_taxon <- tbl(db_connectie, "tblTaxon")
  fb_taxon_kenmerk <- tbl(db_connectie, "tblTaxonKenmerk")
  fb_taxon_kenmerk_waarde <- tbl(db_connectie, "tblTaxonKenmerkWaarde")
  rel_taxon_taxon_kenmerk_waarde <- tbl(db_connectie, "relTaxonTaxonKenmerkWaarde")

  query_result <- rel_taxon_taxon_kenmerk_waarde %>%
    inner_join(fb_taxon_kenmerk %>%
                 filter(tolower(Naam) %LIKE% paste0("%", trait_name, "%")) %>%
                 select(ID, Naam),
               by = c("TaxonKenmerkID" = "ID")) %>%
    select(-Omschrijving) %>%
    left_join(fb_taxon_kenmerk_waarde %>%
                distinct(ID, Code, TaxonKenmerkID, Omschrijving, Rekenwaarde),
              by = c("TaxonKenmerkID" = "TaxonKenmerkID",
                     "TaxonKenmerkWaardeID" = "ID")) %>%
    left_join(fb_taxon %>%
                rename(NaamAfkorting = Code),
              by = c("TaxonID" = "ID")) %>%
    distinct(TaxonID,
             TaxonAfkorting = NaamAfkorting,
             TaxonWetenschappelijk = NaamWetenschappelijk,
             TaxonNederlands = NaamNederlands,
             Kenmerk = Naam,
             Code,
             Omschrijving,
             Rekenwaarde
    ) %>%
    collect()
  dbDisconnect(db_connectie)
  return(query_result)
}

#' Get all validated observations for a species from the florabank database
#'
#' This function takes as input either (part of) a scientific name or (part of)
#' the Dutch name for a species, queries the florabank, and returns a dataframe
#' observation level information about the matching species. The following
#' wildcards may be used within the input parameters:
#' %: #' Represents zero or more characters;
#' _: Represents a single character;
#' []: Represents any single character within the brackets, e.g. [sp]
#' ^:Represents any character not in the brackets, e.g. [^sp]
#' -: Represents a range of characters, e.g. [a-z]
#'
#'
#' @param scient_name (Part of) a scientific species name
#' @param dutch_name (Part of) a Dutch species name, may contain wildcards
#'
#' @return A dataframe with the following variables: "NaamNederlands",
#' "NaamWetenschappelijk", "Bron", "BeginDatum", "EindDatum", "hok",
#' "Toponiem", "CommentaarTaxon", "CommentaarHabitat", "Voornaam", "Achternaam",
#' "ID", "X_waarneming", "Y_waarneming", "X_meting", "Y_meting"
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom glue glue_sql
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Dactmacu1 <-	florabank_observations(scient_name = 'Dactylorhiza maculata (L.) Soó')
#' Dactmacu2 <-	florabank_observations(dutch_name = 'Gevlekte orchis')
#' all.equal(Dactmacu1, Dactmacu2)
#' # use wildcards to retrieve all partial matches to a species
#' Dactmacu3 <-	florabank_observations(scient_name = 'Dactylorhiza maculata%')
#' }

florabank_observations <- function(scient_name, dutch_name) {

  if (missing(scient_name) & missing(dutch_name)) {
    stop("Please provide either a scientific name or a Dutch species name.")
  }

  if (!missing(scient_name) & !missing(dutch_name)) {
    stop("Both a scientific name and a Dutch name are provided. Use either,
         but not both.")
  }

  db_connectie <- connect_inbo_dbase("D0021_00_userFlora")
  if (!missing(scient_name)) {
    glue_statement <- glue_sql(
      "SELECT DISTINCT
      tblTaxon.NaamNederlands
      , tblTaxon.NaamWetenschappelijk
      , cdeBron.Omschrijving AS Bron
      , tblWaarneming.BeginDatum
      , tblWaarneming.EindDatum
      , tblIFBLHok.Code AS hok
      , tblWaarneming.Opmerking AS Toponiem
      , tblMeting.CommentaarTaxon
      , tblMeting.CommentaarHabitat
      , tblMedewerker.Voornaam
      , tblMedewerker.Achternaam
      , tblWaarneming.ID
      , tblWaarneming.Cor_X AS X_waarneming
      , tblWaarneming.Cor_Y AS Y_waarneming
      , tblMeting.Cor_X AS X_meting
      , tblMeting.Cor_Y AS Y_meting
      FROM dbo.tblWaarneming
      INNER JOIN dbo.tblMeting ON tblWaarneming.ID = tblMeting.WaarnemingID
      INNER JOIN dbo.relTaxonTaxon ON relTaxonTaxon.TaxonIDChild = tblMeting.TaxonID
      INNER JOIN dbo.tblTaxon ON tblTaxon.ID = relTaxonTaxon.TaxonIDParent
      INNER JOIN dbo.relTaxonTaxonGroep ON relTaxonTaxonGroep.TaxonID = tblTaxon.ID
      INNER JOIN dbo.tblIFBLHok ON tblIFBLHok.ID = tblWaarneming.IFBLHokID
      INNER JOIN dbo.relWaarnemingMedewerker ON relWaarnemingMedewerker.WaarnemingID = tblWaarneming.ID
      INNER JOIN dbo.tblMedewerker ON tblMedewerker.ID = relWaarnemingMedewerker.MedewerkerID
      INNER JOIN dbo.cdeBron ON cdeBron.Code = tblWaarneming.BronCode
      WHERE 1=1
      AND (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
      AND tblTaxon.NaamWetenschappelijk LIKE {scient_name}
      ORDER BY tblWaarneming.BeginDatum DESC;",
      scient_name = scient_name,
      .con = db_connectie)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- dbGetQuery(db_connectie, glue_statement)
    dbDisconnect(db_connectie)
    return(query_result)
  }

  if (!missing(dutch_name)) {
    glue_statement <- glue_sql(
      "SELECT DISTINCT
      tblTaxon.NaamNederlands
      , tblTaxon.NaamWetenschappelijk
      , cdeBron.Omschrijving AS Bron
      , tblWaarneming.BeginDatum
      , tblWaarneming.EindDatum
      , tblIFBLHok.Code AS hok
      , tblWaarneming.Opmerking AS Toponiem
      , tblMeting.CommentaarTaxon
      , tblMeting.CommentaarHabitat
      , tblMedewerker.Voornaam
      , tblMedewerker.Achternaam
      , tblWaarneming.ID
      , tblWaarneming.Cor_X AS X_waarneming
      , tblWaarneming.Cor_Y AS Y_waarneming
      , tblMeting.Cor_X AS X_meting
      , tblMeting.Cor_Y AS Y_meting
      FROM dbo.tblWaarneming
      INNER JOIN dbo.tblMeting ON tblWaarneming.ID = tblMeting.WaarnemingID
      INNER JOIN dbo.relTaxonTaxon ON relTaxonTaxon.TaxonIDChild = tblMeting.TaxonID
      INNER JOIN dbo.tblTaxon ON tblTaxon.ID = relTaxonTaxon.TaxonIDParent
      INNER JOIN dbo.relTaxonTaxonGroep ON relTaxonTaxonGroep.TaxonID = tblTaxon.ID
      INNER JOIN dbo.tblIFBLHok ON tblIFBLHok.ID = tblWaarneming.IFBLHokID
      INNER JOIN dbo.relWaarnemingMedewerker ON relWaarnemingMedewerker.WaarnemingID = tblWaarneming.ID
      INNER JOIN dbo.tblMedewerker ON tblMedewerker.ID = relWaarnemingMedewerker.MedewerkerID
      INNER JOIN dbo.cdeBron ON cdeBron.Code = tblWaarneming.BronCode
      WHERE 1=1
      AND (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
      AND tblTaxon.NaamNederlands LIKE {dutch_name}
      ORDER BY tblWaarneming.BeginDatum DESC;",
      dutch_name = dutch_name,
      .con = db_connectie)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- dbGetQuery(db_connectie, glue_statement)
    dbDisconnect(db_connectie)
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
#'
#' @param begin_year Filter for observations that start from this year onwards.
#' Default is 2010.
#' @param ifbl_resolution The requested spatial resolution can be either
#' 1km-by-1km IFBL squares or 4km-by-4km. Default is 1km-by-1km.
#' @param taxongroup Choose for which taxonomic group you want the unique
#' combinations. One of "Vaatplanten" (the default), "Mossen", "Korstmossen"
#' of "Kranswieren".
#'
#' @return A dataframe with one line for each combination of taxon, IFBL-square
#' (either at 1 km x 1 km or 4 km x 4 km resolution) and year. In case the
#' resolution is 1 km x 1 km, a variable ifbl_4by4 gives the corresponding
#' ifbl_4by4 identifier within which the ifbl_1by1 square is located. In case the
#' resolution is 4 km x 4 km, the variable ifbl_squares is a concatenation of
#' all nested squares with observations for the taxon in the corresponding year.
#' This can be nested 1 x 1 squares as well as the corresponding 4 x 4 square
#' (the latter is the case if the original resolution of the observation is at
#' 4 x 4 resolution). In addition, the variableifbl_number_squares gives the
#' number of unique nested squares where the taxon was observed for that year
#' and 4 x 4 square combination.
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom glue glue_sql
#' @importFrom assertthat assert_that
#' @importFrom stringr str_sub
#' @importFrom dplyr %>% tbl_df group_by summarize n ungroup
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fb_kwartier <- florabank_taxon_ifbl_year()
#' fb_uur <- florabank_taxon_ifbl_year(begin_year = 2000,
#'  ifbl_resolution = "4km-by-4km", taxongroup = "Mossen")
#' }

florabank_taxon_ifbl_year <- function(begin_year = 2010,
                                      ifbl_resolution = c("1km-by-1km",
                                                          "4km-by-4km"),
                                      taxongroup = c("Vaatplanten",
                                                     "Mossen",
                                                     "Lichenen (korstmossen)",
                                                     "Kranswieren")) {

  assert_that(is.numeric(begin_year))
  assert_that(begin_year <= as.numeric(format(Sys.Date(), '%Y')))

  ifbl_resolution = match.arg(ifbl_resolution)
  taxongroup = match.arg(taxongroup)

  db_connectie <- connect_inbo_dbase("D0021_00_userFlora")

  if (ifbl_resolution == "4km-by-4km") {
    glue_statement <- glue_sql(
      "SELECT DISTINCT
             tblIFBLHok.Code AS hok
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
    Year([tblWaarneming].[BeginDatum]) >={begin_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC;",
      begin_year = begin_year,
      taxongroup = taxongroup,
      .con = db_connectie)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- dbGetQuery(db_connectie, glue_statement)
    dbDisconnect(db_connectie)

    query_result <- query_result %>%
      tbl_df %>%
      mutate(ifbl_4by4 = str_sub(hok, start = 1, end = 5))

    query_result <- query_result %>%
      group_by(ifbl_4by4, Jaar, TaxonIDParent, Taxoncode) %>%
      summarize(ifbl_squares = paste(hok, collapse = "|"),
                ifbl_number_squares = n()) %>%
      ungroup

    return(query_result)
  }

  glue_statement <- glue_sql(
    "SELECT DISTINCT
    tblIFBLHok.Code AS ifbl_1by1
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
    Year([tblWaarneming].[BeginDatum]) >={begin_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK')
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC;",
    begin_year = begin_year,
    taxongroup = taxongroup,
    .con = db_connectie)
  glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
  query_result <- dbGetQuery(db_connectie, glue_statement)
  dbDisconnect(db_connectie)

  query_result <- query_result %>%
    tbl_df %>%
    mutate(ifbl_4by4 = str_sub(ifbl_1by1, start = 1, end = 5))

  return(query_result)
}






