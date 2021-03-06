% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect_inbo_dbase.R, R/florabank_queries.R,
%   R/inborutils-defunct.R, R/inboveg_classification.R, R/inboveg_header.R,
%   R/inboveg_qualifiers.R, R/inboveg_recordings.R, R/inboveg_survey.R
\name{connect_inbo_dbase}
\alias{connect_inbo_dbase}
\alias{florabank_traits}
\alias{florabank_observations}
\alias{florabank_taxon_ifbl_year}
\alias{inborutils-defunct}
\alias{inboveg_classification}
\alias{inboveg_header}
\alias{inboveg_qualifiers}
\alias{inboveg_recordings}
\alias{inboveg_survey}
\title{Defunct functions in package \pkg{inborutils}}
\usage{
connect_inbo_dbase(database_name)

florabank_traits(connection, trait_name, collect = FALSE)

florabank_observations(connection, names, fixed = FALSE, collect = FALSE)

florabank_taxon_ifbl_year(
  connection,
  starting_year = 2010,
  ifbl_resolution = c("1km-by-1km", "4km-by-4km"),
  taxongroup = c("Vaatplanten", "Mossen", "Lichenen (korstmossen)", "Kranswieren"),
  collect = FALSE
)

inboveg_classification(connection, survey_name, classif, collect = FALSE)

inboveg_header(
  connection,
  survey_name,
  rec_type,
  multiple = FALSE,
  collect = FALSE
)

inboveg_qualifiers(connection, survey_name, qualifier_type, multiple = FALSE)

inboveg_recordings(connection, survey_name, collect = FALSE, multiple = FALSE)

inboveg_survey(connection, survey_name, collect = FALSE)
}
\description{
The functions listed below are moved to package \pkg{inbodb}
and removed from \pkg{inborutils}. Please install package
\pkg{inbodb} to continue using these functions (instructions on
https://inbo.github.io/inbodb/).
}
\section{connect_inbo_dbase}{

For \code{connect_inbo_dbase}, use \href{https://inbo.github.io/inbodb/reference/connect_inbo_dbase.html}{inbodb::connect_inbo_dbase()}
}

\section{florabank_traits}{

For \code{florabank_traits}, use \href{https://inbo.github.io/inbodb/reference/get_florabank_traits.html}{inbodb::florabank_traits()}
}

\section{florabank_observations}{

For \code{florabank_observations}, use \href{https://inbo.github.io/inbodb/reference/get_florabank_observations.html}{inbodb::florabank_observations()}
}

\section{florabank_taxon_ifbl_year}{

For \code{florabank_taxon_ifbl_year}, use \href{https://inbo.github.io/inbodb/reference/get_florabank_taxon_ifbl_year.html}{inbodb::florabank_taxon_ifbl_year()}
}

\section{inboveg_classification}{

For \code{inboveg_classification}, use \href{https://inbo.github.io/inbodb/reference/get_inboveg_classification.html}{inbodb::inboveg_classification()}
}

\section{inboveg_header}{

For \code{inboveg_header}, use \href{https://inbo.github.io/inbodb/reference/get_inboveg_header.html}{inbodb::inboveg_header()}
}

\section{inboveg_qualifiers}{

For \code{inboveg_qualifiers}, use \href{https://inbo.github.io/inbodb/reference/get_inboveg_qualifiers.html}{inbodb::inboveg_qualifiers()}
}

\section{inboveg_recordings}{

For \code{inboveg_recordings}, use \href{https://inbo.github.io/inbodb/reference/get_inboveg_recordings.html}{inbodb::inboveg_recordings()}
}

\section{inboveg_survey}{

For \code{inboveg_survey}, use \href{https://inbo.github.io/inbodb/reference/get_inboveg_survey.html}{inbodb::inboveg_survey()}
}

\keyword{internal}
