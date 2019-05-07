#' Get data from Aquachem measurement table
#'
#' Get all LIMS data linked to a single sampling. Filter options are the location of 
#' the fieldsampling and the date of the sampling
#' 
#'
#' @param my_connection A valid connection to Aquachem database.
#' @param code_name (string) code name of the sampling
#' @param sampling_date (string) date of the sampling
#' @param reportable (string) 0 or 1, 1 if the output is part of the report
#'
#' @return A tibble (tidyverse data.frame).
#'
#' @export
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr pull %>%
#'
#' @examples
#' \dontrun{
#' my_connection <- DBI::dbConnect(odbc::odbc(), 
#'     .connection_string = "Driver=SQL Server;Server=inbo-sql08-prd.inbo.be,1433;Database=M0003_00_Aquachem;Trusted_Connection=Yes;")
#'
#'# get all results where code = AN_BLA_001 , date = 20140630 , reportable = 1
#'aquachem_measurement(my_connection, "AN_BLA_001" , "20140630", "1")
aquachem_measurement <- function(dbase_connection, code_name, 
                                 sampling_date, reportable) {
  dbGetQuery(dbase_connection, glue_sql(
    "SELECT  p.Project
    , p.ContractNumber
    , w.CODE
    , w.Gemeente
    , w.SBZ
    , w.meetnet
    , w.Abiotiek_j
    , w.Biotiek_ja
    , w.HabtypeSel
    , w.HabtypeVel
    , w.INSIDE_X
    , w.INSIDE_Y
    , r.AnalysisDateKey
    , r.AnalysisDate
    , a.SAPcode
    , r.Component
    , r.ResultNumeric
    , r.ResultFormatted
    , r.ResultFormattedNumeric
    , r.IsBelowLOQ
    , r.IsAboveLOQ
    , u.Unit
    , s.FieldSampleID  
    , s.FieldObserver-- navragen Jo waarom zoveel legen records
    , s.FieldSampleRemark
    , s.FieldSamplingDateKey
    , s.FieldSamplingDate
    FROM dbo.FactResult r
    INNER JOIN [dbo].[DimSample] s ON s.SampleKey = r.SampleKey
    INNER JOIN dbo.DimWaterhabitat w ON w.WaterhabitatKey = r.WaterhabitatKey
    INNER JOIN dbo.DimUnit u ON u.UnitKey = r.UnitKey
    INNER JOIN dbo.DimProject p ON p.ProjectKey = r.ProjectKey
    INNER JOIN dbo.DimAnalysis a ON a.AnalysisKey = r.AnalysisKey
    WHERE 1=1
    AND s.WaterhabitatKey != -1
    AND w.code = {code_name}
    AND FieldSamplingDateKey = {sampling_date}
    --and r.Component LIKE '%ph%'
    AND r.IsReportable = {reportable}",
    code_name = code_name,
    sampling_date = sampling_date,
    reportable = reportable,
    .con = dbase_connection))
}