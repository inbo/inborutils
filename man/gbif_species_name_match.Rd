% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbif_species_name_match.R
\name{gbif_species_name_match}
\alias{gbif_species_name_match}
\title{Add species information provided by the GBIF taxonomic backbone API}
\usage{
gbif_species_name_match(
  df,
  name = "name",
  gbif_terms = c("usageKey", "scientificName", "rank", "order", "matchType", "phylum",
    "kingdom", "genus", "class", "confidence", "synonym", "status", "family"),
  ...
)
}
\arguments{
\item{df}{\code{data.frame} with species information}

\item{name}{char column name of the column containing the names used for the
name matching with the GBIF taxonomic backbone. Default: "name".}

\item{gbif_terms}{list of valid GBIF terms to add as additional columns to
the data.frame. Default: \code{usageKey}, \code{scientificName}, \code{rank}, \code{order},
\code{matchType}, \code{phylum}, \code{kingdom}, \code{genus}, \code{class}, \code{confidence},
\code{synonym}, \code{status}, \code{family}.}

\item{...}{any parameter to pass to rgbif function \code{name_bakbone}. One of:
\code{rank}, \code{kingdom}, \code{phylum}, \code{class}, \code{order}, \code{family}, \code{genus}, \code{strict},
\code{verbose}, \code{start}, \code{limit}, \code{curlopts}. See \code{?name_backbone} for more
details.}
}
\value{
a tibble data.frame with GBIF information as additional columns. If
none of the taxa in  \code{df} is matched, only the columns \code{confidence},
\code{matchType} and \code{synonym} are added. This behaviour is inherited by
\code{rgbif::name_backbone}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

Development on \code{gbif_species_name_match()} is complete, and for new code we
strongly recommend switching to \code{rgbif::name_backbone_checklist()}, which is
easier to use, has more features, and still under active development.
This functions extends an existing dataframe with additional columns provided
by the GBIF taxonomic backbone and matched on the species (scientific) name,
which need to be an available column in the dataframe.

This function is essentially a wrapper around the existing rgbif
\code{name_backbone} and extends the application to a data.frame. Such extension
has been added to rgbif via the function
\href{https://docs.ropensci.org/rgbif/reference/name_backbone_checklist.html}{rgbif::name_backbone_checklist}. # nolint
For more information on the name matching API of GBIF on which rgbif relies,
see \url{https://www.gbif.org/developer/species/#searching}.
}
\examples{
\dontrun{
library(readr)
library(dplyr)
species_list <- read_csv(paste0(
  "https://raw.githubusercontent.com/inbo",
  "/inbo-pyutils/master/gbif/gbif_name_match",
  "/sample.csv"
),
trim_ws = TRUE, col_types = cols()
)
# basic usage
species_list \%>\%
  gbif_species_name_match()
# pass optional parameters to name_backbone
species_list \%>\%
  gbif_species_name_match(name = "name", kingdom = "kingdom", strict = TRUE)
# select GBIF terms
species_list \%>\%
  gbif_species_name_match(gbif_terms = c("scientificName", "rank"))
}
}
\concept{GBIF_interactions}
