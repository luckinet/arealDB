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
"geom_countries"

#' Default template of a schema description
#'
#' @format The list of lists describes at which position in a table which
#'   information can be found. It contains the two obligatory lists
#'   \code{clusters} and \code{variables}.
#'
#'   Theoretically, there is no limit to how data can be arranged in a
#'   spreadsheet. However, mostly it is the case that data are gathered in areas
#'   of a spreadsheet, which can be encompassed by a rectangle. Those rectangles,
#'   including more or less coherent tables, are what is considered 'cluster' in
#'   arealDB. Clusters are described by the properties: \itemize{
#'     \item \code{row}:\cr the vertical cell values of the top-left cell of each cluster.
#'     \item \code{col}:\cr the horizontal cell value of the top-left cell of each cluster.
#'     \item \code{width}:\cr the width in cells of each cluster.
#'     \item \code{height}:\cr the height in cells of each cluster.
#'     \item \code{id}:\cr it may be the case that data in a spreadsheet are clustered according to one of the variables. In such cases, this variable needs to be registered here.}
#'
#'   Each element in \code{variables} is itself a list that describes one of the
#'   variables that shall be comprised in the geospatial database. Variables are
#'   either so-called \emph{identifying variables}, which indicate observation
#'   units, or \emph{values variables}, which carry the observed values.
#'   Values variables are described by the properties: \itemize{
#'     \item \code{type}:\cr either \code{"id"} or \code{"values"} for
#'       identifying or values variables.
#'     \item \code{name}:\cr a name that would be assigned as column name in the
#'       database instead of the list name.
#'     \item \code{form}: (only relevant for id-variables)\cr whether the
#'       variable is recorded in \code{"long"} or \code{"wide"} form. Variables
#'       that are recorded in long form are spread along one column and in wide
#'       form they are spread along one row.
#'     \item \code{row}:\cr the row(s) in which the variable values are recorded.
#'     \item \code{col}:\cr the column(s) in which the variable values are
#'       recorded.
#'     \item \code{rel}: (logical)\cr whether or not the values in \code{row}
#'       and \code{col} are relative to the cluster positions or whether they
#'       refer to the overall table.
#'     \item \code{unit}: (only relevant for values-variables)\cr the unit in
#'       which the values shall be recorded in the database.
#'     \item \code{factor}: (only relevant for values-variables)\cr a factor to
#'       transform the values to \code{unit}. For instance, if values are
#'       recorded in acres, but shall be contained in the database in hectare,
#'       the factor would be 0.40468.
#'     \item \code{id}: (only relevant for values-variables)\cr if several
#'       values-variables are treated as if they were an id-variable, i.e., the
#'       values-variable is in long form, this element needs to contain the name
#'       of the column that contains the values-variable's names.
#'     \item \code{level}: (only relevant for values-variables)\cr if
#'       values-variables are in long form, this element needs to contain the
#'       value that stands for the variable, i.e. the label that should be the
#'       variable's name.}
#'
#'   The default schema description contains the two variables
#'   \code{territories} and \code{period}, which are the bare minimum
#'   identifying variables in an areal database. However, further identifying
#'   and values variables would be added when more variables are contained in a table.
"meta_default"