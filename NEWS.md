# arealDB 0.5.0 - automatching update

* spatial matches are now found automatically and inserted into the ontology.
* matching can be sped up by simplifying geometries.
* spatial hierarchy information of the different administrative levels are now streamlined and provided differently/more explicitly.
* various bug-fixes to handling terms (they are now more explicitly cleaned from white-space and dots in names are removed, e.g. 'St. Gallen' turns to 'St Gallen', which is important for the output column 'gazName', where the administrative levels are concatenated via a '.').
* updates on tests, to be more explicit about the explicit outcome of geometries and the ontology. 

# arealDB 0.4.6

* revise the whole geometry normalisation process to incorporate the new ontology system.

# arealDB 0.4.5

* fix how to derive the top-most class.

# arealDB 0.4.4

* fix internals to make the code run faster.

# arealDB 0.4.3

* rename `match_ontology()` to `matchOntology()` and revise it so that it can handle several columns sequentially
* make use of getters of the ontologics package so that (large) gazetteers are not loaded but only the requried info are pulled from them
* remove functions `get_variable()` and `select_path()`, the former is integrated into `matchOntology()` and the latter is not relevant anymore.


# arealDB 0.4.2

* when using `regTable` with `update = FALSE`, the schema now has a more explicit diagnostic message.

# arealDB 0.4.1

* rename `match_gazetteer` to `match_ontology` because it can also handle ontologies of other variables
* various fixes to make matching as convenient as possible.

# arealDB 0.4.0 - ontology update

* instead of using id and translation tables, concepts are from now on handled via the ontologics package.
* functions `setVariables` and `translateTables` are discontinued, instead the function `match_gazetteer` handles matching of concepts.
* the package has been streamlined so that functions `regGeometry` and `regTable` don't require a `nation = ` argument anymore, but this can be any other type of "main" polygon.
* new function `get_variable` to extract concepts from stage2 tables.

# arealDB 0.3.9
 
* bugfix of access-rights management
* adapt to changes in tabshiftr

# arealDB 0.3.8

* all `countries` now stored with capital letters and all geoms are stored as MULTIPOLYGON
* store tables and geometries by default in rds format with csv/gpkg as option
* several minor bug-fixes

# arealDB 0.3.7

* streamline translations further by creating the file "target_terms.csv" that contains the translations that are allowed (taken from the respective translation tables or geometries).
* more strict translations by allowing only provided terms that may come from outside of a translation table (such as geometry names that were not known at the time of creating the translation table)

# arealDB 0.3.6

* streamline translations to reduce the amount of items that have to be translated (specifically, only translate nations in normTable() that have been subsetted, not the whole list of nations)
* repeat translation of terms as long as some are still (accidentally) missing.
* improve updating of tables (concerning the functions reg*(), setVariables(), translateTerms()).

# arealDB 0.3.5

* streamline overwriting of already registered items in the inventory tables.

# arealDB 0.3.4

* include 'makeExampleDB', which allows to build an example database at any stage. This is useful for testing, debugging and examplifying an areal database.
* improved documentation due to this change.

# arealDB 0.3.3

* refine package documentation
* refine workflow to adapt some minor tweaks.

# arealDB 0.3.2

* Added a `NEWS.md` file to track changes to the package.
* initial submission to CRAN
