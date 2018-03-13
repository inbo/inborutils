#' Get plant overview list from INBOVEG
#'
#' In order to exclude the synonyms from the plant list, this table provides a
#' taxonlist overview of the INBOVEG taxa.
#'
#' @param connection odb
#'
#' @return DBIConnection-class, using DBI-ODBC like connectors
#' @export
#' @importFrom DBI dbGetQuery
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' con <- dbConnect(odbc::odbc(), dsn="Cydonia")
#' plant_overview <- inboveg_get_synonym(con)
#' }
inboveg_get_synonym <- function(connection) {

    query <- "SELECT ftt.TaxonName AS TaxonFullText
                  , COALESCE([qry_B_GetSyn].TaxonName, ftt.TaxonName) AS ScientificName
                  , COALESCE([qry_B_GetSyn].TaxonGIVID, ftt.TaxonGIVID) AS TAXON_LIST_ITEM_KEY
                  , COALESCE([qry_B_GetSyn].TaxonQuickCode, ftt.TaxonQuickCode) AS QuickCode
              FROM dbo.ftTaxon ftt
            	INNER JOIN dbo.ftTaxonListItem tli ON tli.TaxonGIVID = ftt.TaxonGIVID
            	INNER JOIN (SELECT ftTaxonListItem.TaxonListItemGIVID
            						, ftTaxon.TaxonGIVID
            						, ftTaxon.TaxonName
            						, ftTaxon.TaxonQuickCode
            						, ftActionGroupList.ListName
            						, ftTaxonListItem.PreferedListItemGIVID
            				FROM dbo.ftActionGroupList
            					INNER JOIN dbo.ftTaxonListItem ON ftTaxonListItem.TaxonListGIVID = ftActionGroupList.ListGIVID
            					LEFT JOIN dbo.ftTaxon ON ftTaxon.TaxonGIVID = ftTaxonListItem.TaxonGIVID
            				WHERE ftActionGroupList.ListName = 'INBO-2011 Sci'
            	) qry_B_GetSyn ON tli.PreferedListItemGIVID = qry_B_GetSyn.TaxonListItemGIVID
            WHERE tli.TaxonListGIVID = 'TL2011092815101010'
            ORDER BY ftt.TaxonName;
            "
    dbGetQuery(connection, query)
}

