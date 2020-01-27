#' A table with country level metadata
#'
#' @format The object of class tibble has 8 columns and 248 rows. It lists for
#'   each country the \code{name}, \code{iso_a2} and \code{iso-a3}-code, whether
#'   the country is an \code{un_member}, on which \code{continent} the country
#'   is and of which \code{region} and \code{subregion} that is part. Finally,
#'   it lists the initial ahID for each nation.
"countries"

#' Simple feature geometries of all countries
#'
#' @format An object of class sf (inherits from tbl_df, tbl, data.frame) with
#'   177 rows and 7 columns
"countries_sf"