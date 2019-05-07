#' Get data from FactResult
#'
#' Get all or a subset of FactResult data. Filter options are the location of 
#' the fieldsampling and date.
#' 
#'
#' @param my_connection A valid connection to database.
#' @param code_name (string) code name of the measure.
#' @param sampling_date (string) whole date of the measure.
#'
#' @return A tibble (tidyverse data.frame).
#'
#' @export
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr pull %>%
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#'# get all results where code = AN_BLA_001 , date = 20140630 
#'variabelen <- fact_result(my_connection, "AN_BLA_001" , "20140630")
#'head(variabelen) %>% knitr::kable()
#'
#'
#'install.packages(c("DBI", "glue", "tidyverse"))

library(DBI)
library(glue)
library(tidyverse)

my_connection <- DBI::dbConnect(odbc::odbc(), 
                                .connection_string = "Driver=SQL Server;Server=inbo-sql08-prd.inbo.be,1433;Database=M0003_00_Aquachem;Trusted_Connection=Yes;")
print(my_connection)

rel_taxa <- dbReadTable(my_connection, "FactResult")
head(rel_taxa) %>% knitr::kable()

subset_meting <- dbGetQuery(my_connection, 
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
                            AND w.code = 'AN_BLA_001'
                            AND FieldSamplingDateKey = 20140630
                            --and r.Component LIKE '%ph%'
                            AND r.IsReportable = 1")


head(subset_meting) %>% knitr::kable()

fact_result <- function(dbase_connection, code_name, sampling_date, reportable) {
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
    AND r.IsReportable = 1",
    code_name = code_name,
    sampling_date = sampling_date,
    reportable = reportable,
    .con = dbase_connection))
}


variabelen <- fact_result(my_connection, "AN_BLA_001" , "20140630")

head(variabelen) %>% knitr::kable()
