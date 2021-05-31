# define global variables for internal use
globalVariables(c(
  "NAME_0", "ahID", "al1", "al1_alt", "al1_name", "al1_id", "al2_id", "al3_id",
  "al4_id", "al5_id", "al6_id", "countries", "iso_a3", ".", "col_name", "col_type",
  "commodities", "countries", "faoID", "geoID", "geom", "harvested area", "id",
  "idParent", "idUnit", "inCountry", "iso_a3", "level", "myNat", "n", "name",
  "nation", "notes", "origin", "parent", "production", "target", "tempFuzz",
  "terms", "toClean", "unit", "year", "years", "yield", "tabID", "source_area",
  "overlap", "tempID", "key", "val", "rn", "values", "ID", "area", "deviation",
  "target_area", "valid", "running", "sourceFID", "targetFID", "overlap_area",
  "orig", "new", "theTerm", "datID"
))

.onAttach <- function(libname, pkgname){
  options(adb_testing = FALSE)
}