% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adb_ontology.R
\name{adb_ontology}
\alias{adb_ontology}
\title{Load the currently active ontology}
\usage{
adb_ontology(..., type = "ontology")
}
\arguments{
\item{...}{combination of column name in the ontology and value to filter
that column by to build a tree of the concepts nested into it; see
\code{\link[ontologics]{make_tree}}.}

\item{type}{[\code{character(1)}]\cr the type of ontology to load,
either \code{"ontology"} to get the thematic concepts, or
\code{"gazetteer"} to get the territories.}
}
\value{
returns a tidy table of an ontology or gazetteer that is used in an
  areal database.
}
\description{
Load the currently active ontology
}
