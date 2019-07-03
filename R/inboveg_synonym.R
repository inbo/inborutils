#' Provide accepted names list from INBOVEG database
#'
#' Get all taxon names available from an INBOVEG database taxon list
#' (identified with the `TaxonListGIVID`) and give for each name the corresponding
#' preferred taxon name as available in the specified reference list
#' (identified by `ListName`)
#'
#' @param connection odb DBIConnection-class, using DBI-ODBC like connectors
#' @param TaxonListGIVID char taxon list
#' @param ListName char preferred name list
#'
#' @return data.frame
#' @export
#' @family inboveg
#' @importFrom DBI dbSendQuery dbBind dbFetch dbClearResult
#' @importFrom assertthat assert_that is.string
#'
#' @examples
#' \dontrun{
#' con <- connect_inbo_dbase("D0013_00_Futon")
#' plant_overview <- inboveg_synonym_list(con,
#'                            TaxonListGIVID = 'TL2011092815101010',
#'                            ListName = 'INBO-2011 Sci')
#' dbDisconnect(con)
#' }
#'
inboveg_synonym_list <- function(connection,
                                 TaxonListGIVID = 'TL2011092815101010',
                                 ListName = 'INBO-2011 Sci') {

  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(is.string(TaxonListGIVID))
  assert_that(is.string(ListName))

  query <- "
        SELECT ftt.TaxonName AS TaxonFullText,
            COALESCE([qry_B_GetSyn].TaxonName, ftt.TaxonName) AS ScientificName,
            COALESCE([qry_B_GetSyn].TaxonGIVID, ftt.TaxonGIVID) AS
                TAXON_LIST_ITEM_KEY,
            COALESCE([qry_B_GetSyn].TaxonQuickCode, ftt.TaxonQuickCode) AS
                QuickCode
            FROM dbo.ftTaxon ftt
            INNER JOIN dbo.ftTaxonListItem tli ON
                tli.TaxonGIVID = ftt.TaxonGIVID
            INNER JOIN (SELECT ftTaxonListItem.TaxonListItemGIVID,
                            ftTaxon.TaxonGIVID,
                            ftTaxon.TaxonName,
                            ftTaxon.TaxonQuickCode,
                            ftActionGroupList.ListName,
                            ftTaxonListItem.PreferedListItemGIVID
                        FROM dbo.ftActionGroupList
            			INNER JOIN dbo.ftTaxonListItem ON
                            ftTaxonListItem.TaxonListGIVID = ftActionGroupList.ListGIVID
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
