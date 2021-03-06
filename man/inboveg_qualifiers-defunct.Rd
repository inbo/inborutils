% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inboveg_qualifiers.R
\name{inboveg_qualifiers-defunct}
\alias{inboveg_qualifiers-defunct}
\title{Query qualifier information of recordings (releve) from INBOVEG}
\usage{
inboveg_qualifiers(connection, survey_name, qualifier_type,
  multiple = FALSE)
}
\arguments{
\item{survey_name}{A character string or a character vector, depending on
multiple parameter, giving the name or names of the
survey(s) for which you want to extract recordings information. If missing, all
surveys are returned.}

\item{qualifier_type}{A character vector giving the name of qualifier type for which
you want to extract  information e.g. 'SQ' (site qualifier), 'MQ' (management qualifier).
If missing, all qualifier types are returned.}

\item{connection}{dbconnection with the database 'Cydonia'
on the inbo-sql07-prd server}

\item{multiple}{If TRUE, survey_name can take a character vector with
multiple survey names that must match exactly. If FALSE (the default),
survey_name must be a single character string (one survey name) that can
include wildcards to allow partial matches}
}
\value{
A dataframe with variables RecordingGivid (unique Id), UserReference,
Observer, QualifierType, QualifierCode, Description, 2nd QualifierCode,
2nd Description, 3rd QualifierCode, 3rd Description, Elucidation, in case
qualifier is 'NotSure', ParentID, QualifierResource
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#defunct}{\figure{lifecycle-defunct.svg}{options: alt='[Defunct]'}}}{\strong{[Defunct]}}
This function queries the INBOVEG database for
qualifier information on recordings  for one or more surveys.
}
\examples{
\dontrun{
con <- connect_inbo_dbase("D0010_00_Cydonia")

# get the qualifiers from one survey
qualifiers_heischraal2012 <- inboveg_qualifiers(con, survey_name =
"MILKLIM_Heischraal2012")

# get all site qualifiers (SQ) from MILKLIM surveys (partial matching)
qualifiers_milkim <- inboveg_qualifiers2(con, survey_name = "\%MILKLIM\%",
qualifier_type = "SQ")

# get qualifiers from several specific surveys
qualifiers_severalsurveys <- inboveg_qualifiers(con, survey_name =
c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"), multiple = TRUE)

# get all qualifiers of all surveys
allqualifiers <- inboveg_qualifiers(con)

# Close the connection when done
dbDisconnect(con)
rm(con)
}

}
\seealso{
\code{\link{inborutils-defunct}}

Other inboveg: 
\code{\link{inboveg_classification-defunct}},
\code{\link{inboveg_header-defunct}},
\code{\link{inboveg_recordings-defunct}},
\code{\link{inboveg_survey-defunct}}
}
\concept{inboveg}
\keyword{internal}
