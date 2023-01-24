# define global variables for internal use
globalVariables(c(
  "NAME_0", "ahID", "al1", "al1_alt", "al1_name", "al1_id", "al2_id", "al3_id",
  "al4_id", "al5_id", "al6_id", "iso_a3", ".", "col_name", "col_type",
  "commodities", "faoID", "geoID", "geom", "harvested area", "id",
  "idParent", "idUnit", "inCountry", "iso_a3", "level", "myNat", "n", "name",
  "nation", "notes", "origin", "parent", "production", "target", "tempFuzz",
  "terms", "toClean", "unit", "year", "years", "yield", "tabID", "source_area",
  "overlap", "tempID", "key", "val", "rn", "values", "ID", "area", "deviation",
  "target_area", "valid", "running", "sourceFID", "targetFID", "overlap_area",
  "orig", "new", "theTerm", "datID", "label_en", "code", "broader", "ahName",
  ".data", "nested", "external", "sort_in", "harmonised", "sourceName", "label",
  "description", "narrower", "has_broader", "fid", "has_broader_match",
  "has_close_match", "has_exact_match", "has_narrower_match", "has_exact_match",
  "unitCols", "onto_class", "has_source", "unnamed", "iter", "topCol", "gazID",
  "gazName", "ontoID", "ontoName", "lvl", "gazClass", "intersect_area",
  "target_prop", "source_prop", "target_prop_sum", "source_prop_sum", "nr",
  "stage2_geom", "source_cumsum", ""
))

.onAttach <- function(libname, pkgname){
  options(adb_testing = FALSE)
}