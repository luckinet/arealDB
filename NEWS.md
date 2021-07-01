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
* repeat translation of terms as long as some are still (accidently) missing.
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
