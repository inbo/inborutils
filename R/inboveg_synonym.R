#' Provide accepted names list from INBOVEG database
#'
#' Get all taxon names available from an INBOVEG database taxon list
#' (identified with the `TaxonListGIVID`) and give for each name the corresponding
#' preferred taxon name as available in the specified reference list
#' (identified by `ListName`)
#'
#' @param connection odb
#' @param TaxonListGIVID char
#' @param ListName char
#'
#' @return DBIConnection-class, using DBI-ODBC like connectors
#' @export
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult
#' @importFrom assertthat assert_that is.string
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' con <- dbConnect(odbc::odbc(), dsn="Cydonia-prd")
#' plant_overview <- inboveg_synonym_list(con,
#'                            TaxonListGIVID = 'TL2011092815101010',
#'                            ListName = 'INBO-2011 Sci')
#' }
inboveg_synonym_list <- function(connection,
                                 TaxonListGIVID = 'TL2011092815101010',
                                 ListName = 'INBO-2011 Sci') {

    assert_that(is.string(TaxonListGIVID))
    assert_that(is.string(ListName))

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
            				WHERE ftActionGroupList.ListName = ?
            	) qry_B_GetSyn ON tli.PreferedListItemGIVID = qry_B_GetSyn.TaxonListItemGIVID
            WHERE tli.TaxonListGIVID = ?
            ORDER BY ftt.TaxonName;
            "
    query <- dbSendQuery(connection, query)
    dbBind(query, list(TaxonListGIVID, ListName))
    synonym_list <- dbFetch(query)
    dbClearResult(query)
    synonym_list
}
