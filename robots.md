# arealDB - reference for LLMs

This document is the single source of truth for the arealDB package. It
exists so an LLM can write correct arealDB code from this file alone,
without opening source. When in doubt, this document overrides any
other description. Update this file whenever a function's signature,
behaviour, or the package structure changes.

If you are an LLM reading this: **read the "Common mistakes" section
first**. Most failures with arealDB come from a small set of recurring
errors that the section names directly.

---

## Common mistakes

These are the mistakes that recur most often. Read them first.

### 1. arealDB is a workflow, not a query library.

Building a database is a stateful, multi-step procedure: `adb_init`
once per project, then `regDataseries` / `regVocabulary` /
`regGeometry` / `regTable` to declare inputs, then `normVocabulary` /
`normGeometry` / `normTable` to harmonise them. Only after that does
`adb_query` return meaningful results. Do not call `adb_query` against
a freshly initialised database expecting answers.

### 2. The active database is held in a package-internal environment.

`.adb_state$path` carries the root directory. It is set by `adb_init`
(and never afterwards). To switch databases you must call `adb_init`
again with a different `root`. Do NOT assume environment variables,
working directory, or arguments carry the path.

In a console session, internals are reached as `arealDB:::.adb_state`.
For interactive debugging, use `devtools::load_all("path/to/arealDB")`
- that exposes internals at the bare prompt.

### 3. File-name convention matters.

Stage2 file names are parsed. The convention is
`<topUnit>_<level>__<dataseries>.<ext>` for geometries (e.g.
`Guatemala_ADM0__gadm.gpkg`) and `<topUnit>_<level>_<subset>_<begin>_<end>__<dataseries>.<ext>`
for tables. `normGeometry` derives the target stage3 file name from
the first underscore-separated token. Don't rename stage2 files by
hand after registering them.

A guard inside `normGeometry` checks that the file's parsed top-unit
matches what its geometry resolves to. If they disagree it errors -
that almost always means the wrong file is at stage2.

### 4. Gazetteer first, everything else second.

`normGeometry` and `normTable` look up territorial labels against a
gazetteer vocabulary. If the gazetteer is empty or only partly
populated, every territorial label hits `match_builder`. Register and
normalise the gazetteer backbone (typically the UN geoscheme or GADM)
*before* registering any geometries or tables.

### 5. `regGeometry` `label` is a `list`, not a vector.

The `label` argument names which column in the source geometry holds
labels at each administrative level. It is a named list where the
name is the canonical class (`ADM0`, `ADM1`, ...) and the value is
the source column name.

```r
# WRONG - vector
regGeometry(ADM0 = "Guatemala", gSeries = "gadm",
            label = c(ADM0 = "COUNTRY", ADM1 = "NAME_1"), ...)

# CORRECT - list
regGeometry(ADM0 = "Guatemala", gSeries = "gadm",
            label = list(ADM0 = "COUNTRY", ADM1 = "NAME_1"), ...)
```

The keys of `label` must be a contiguous slice of the administrative
levels declared in `adb_init(level = ...)`. Skipping levels (e.g.
ADM0 + ADM2 with no ADM1) breaks the down-stream `filledClasses` slice.

### 6. `regTable`'s schema comes from `tabshiftr`.

The `schema` argument expects a `schema` object built via
`tabshiftr::setIDVar`, `setObsVar`, `setFormat`, `setCluster`,
`setFilter`. arealDB re-exports the four most common ones. Build the
schema first; pass the object in.

### 7. `normVocabulary`/`normGeometry`/`normTable` skip already-normalised files.

Each writes `inventory$<kind>$status = "normalised"` on success and
the next run skips rows with that status. To force re-processing, run
`adb_reset(what = "<kind>")` first - it deletes the stage3 artefacts
AND rolls the status back to `"staged"`.

### 8. `adb_reset(what = "vocabularies")` removes terms+mappings, not registrations.

Resetting "vocabularies" deletes `vocabularies/stage3/*.parquet` and
`vocabularies/mappings/*.csv`, and rolls vocabulary inventory status
back to `"staged"`. The inventory rows themselves and the stage2
source files survive. Re-run `normVocabulary` to rebuild.

Resetting `"inventory"` wipes everything in `inventory.rds` - the
nuclear option. Use it only when you also want to re-register from
scratch.

### 9. Match_builder prompts mean a vocabulary mismatch.

When `match_builder` opens, the harmoniser has unmatched source
labels and needs human input. If you didn't expect this, check:

- Is the relevant vocabulary normalised? (`adb_inventory("vocabularies")`)
- Is the parent unit already in the gazetteer mappings? Children
  can't be resolved before parents.
- Does the source data actually contain the unit you think it does?
  (E.g. wrong file at stage2.)

The match_builder browser tab can be closed; the function detects the
disconnect and returns empty (treated as a cancel).

### 10. Stage1 / Stage2 / Stage3 are pipeline stages, not just folders.

- **Stage1** is the original raw download (`stage1_url`, `stage1_name`
  recorded but no on-disk copy is required).
- **Stage2** is the local working copy after manual extraction /
  pre-cleaning. Live in `geometries/stage2/`, `tables/stage2/`,
  `vocabularies/stage2/`.
- **Stage3** is the harmonised, gazetteer-aligned output the norm*
  functions produce.

The `stage2/processed/` subdirectories are vestigial - nothing moves
files into them anymore. Status tracking happens via the inventory.

### 11. `adb_query` needs at minimum a `territory` filter.

```r
# typical query
adb_query(territory = list(ADM0 = "Guatemala"),
          concept   = list(commodity = "barley"),
          variable  = "harvested",
          year      = 2015)
```

Returns an sf object (geometries joined with values).

---

## Object model

There is no exported S4 class. The package state lives in a few
on-disk files plus one package-internal environment:

| Object | Location | Holds |
|---|---|---|
| `.adb_state$path` | package env | root directory of the active DB |
| `.adb_state$testing` | package env | `TRUE` while `adb_example` runs |
| `db_info.RData` | `<root>/db_info.RData` | version, author, licence, `level` (vector of admin class labels) |
| `inventory.rds` | `<root>/inventory.rds` | three tibbles: `$dataseries`, `$geometries`, `$tables`, `$vocabularies` |
| `<gaz>_terms.parquet` | `<root>/vocabularies/stage3/` | gazetteer/ontology terms (id, label, class, parent_id, ...) |
| `<gaz>_levels.parquet` | `<root>/vocabularies/stage3/` | level definitions (level_id, label, parent_level) |
| `<gaz>_<dataseries>.csv` | `<root>/vocabularies/mappings/` | per-dataseries label-to-canonical-id mappings |
| stage3 geoms | `<root>/geometries/stage3/<topUnit>.gpkg` | harmonised geometries, one file per top-unit, layers per admin level |
| stage3 tables | `<root>/tables/stage3/<topUnit>.rds` | harmonised tabular data, one file per top-unit |
| schemas | `<root>/tables/schemas/` | serialised `tabshiftr` schemas |

### Inventory shape

`inventory.rds` is a named list. The four sub-tables and their key
columns:

- `dataseries`: `datID`, `name`, `description`, `homepage`,
  `version`, `licence_link`, `notes`
- `vocabularies`: `vocID`, `datID`, `name`, `description`, `version`,
  `licence_link`, `stage2_name`, `schema`, `stage1_name`, `stage1_url`,
  `download_date`, `status`, `notes`
- `geometries`: `geoID`, `datID`, `stage2_name`, `layer`, `label`,
  `ancillary`, `stage1_name`, `stage1_url`, `download_date`,
  `update_frequency`, `status`, `notes`
- `tables`: `tabID`, `datID`, `geoID`, `geography`, `level`,
  `start_period`, `end_period`, `stage2_name`, `schema`,
  `stage1_name`, `stage1_url`, `download_date`, `update_frequency`,
  `metadata_url`, `metadata_path`, `status`, `notes`

`status` is `"staged"` after registration, `"normalised"` after a
successful norm* call. Skip-gates in norm* read this field.

### Term hierarchy

Terms have a parent_id chain. `.001.002.001.027` is a sample
gazetteer ID: dots are part of the ID (not separators meant to be
parsed). Walk parents via the `parent_id` column.

---

## Minimum working example

```r
library(arealDB)

# 1. initiate the DB
adb_init(root    = paste0(tempdir(), "/myDB"),
         version = "0.1.0", licence = "CC-BY-4.0",
         author  = list(cre = "Jane Doe", aut = "Jane Doe"),
         level   = c("ADM0", "ADM1", "ADM2"))

# 2. register the gazetteer dataseries + vocabulary
regDataseries(name = "un",
              description = "UN Statistics Division geoscheme",
              homepage = "https://unstats.un.org",
              version = "2024", licence_link = "CC-BY-4.0")
regVocabulary(name = "gazetteer", dSeries = "un",
              description = "UN geoscheme backbone",
              archive = "un_geoscheme.csv",
              archiveLink = "https://...",
              downloadDate = as.Date("2024-01-01"))
normVocabulary(pattern = "gazetteer")

# 3. register a country's geometry
regDataseries(name = "gadm", description = "GADM v4.1",
              homepage = "https://gadm.org",
              version = "4.1", licence_link = "academic-only")
regGeometry(ADM0 = "Guatemala", gSeries = "gadm",
            label = list(ADM0 = "COUNTRY"),
            archive = "gadm41_GTM.gpkg",
            archiveLink = "https://geodata.ucdavis.edu/...",
            downloadDate = as.Date("2026-05-07"))
regGeometry(ADM0 = "Guatemala", gSeries = "gadm",
            label = list(ADM0 = "COUNTRY", ADM1 = "NAME_1"),
            archive = "gadm41_GTM.gpkg",
            archiveLink = "https://geodata.ucdavis.edu/...",
            downloadDate = as.Date("2026-05-07"))
normGeometry(pattern = "Guatemala.*gadm")

# 4. register a table
schema <- tabshiftr::setIDVar(name = "ADM1", columns = 2) |>
          tabshiftr::setIDVar(name = "year", columns = 1) |>
          tabshiftr::setObsVar(name = "harvested", columns = 3)
regDataseries(name = "ine_gt", description = "INE Guatemala",
              homepage = "https://ine.gob.gt", version = "2024",
              licence_link = "open")
regTable(ADM0 = "Guatemala", label = "ADM1", subset = "barley",
         dSeries = "ine_gt", gSeries = "gadm", schema = schema,
         begin = 2010, end = 2020,
         archive = "ine_gt_barley.csv",
         archiveLink = "https://ine.gob.gt/...",
         downloadDate = as.Date("2026-05-07"))
normTable(pattern = "ADM1.*ine_gt")

# 5. query
adb_query(territory = list(ADM0 = "Guatemala"),
          variable  = "harvested",
          year      = 2015)
```

---

## Lifecycle: reg* -> norm*

Every input passes through two functions. `reg*` records metadata in
the inventory and stages the file; `norm*` reads from stage2, applies
the gazetteer/ontology, and writes to stage3.

```
register inputs        normalise inputs              query
+-------------+        +-----------------+      +----------+
| regData...  |---+    | normVocabulary  |      | adb_query|
| regVocab... |   |    | normGeometry    |----->|          |
| regGeometry |   |--->| normTable       |      +----------+
| regTable    |   |    +-----------------+
+-------------+   |
                inventory.rds + stage2/
```

Order constraint within norm*: **vocabularies first** (gazetteer,
then any other ontologies), **then geometries** (top-class first so
sub-national files can resolve parents), **then tables**. Within
geometries, ADM0 must be normalised before ADM1 for a given country.

---

## adb_* - lifecycle and inspection

### `adb_init(root, version, author, licence, level)`

Initiate a database. Creates directory structure under `root`,
writes `db_info.RData` and empty `inventory.rds`. `level` is the
ordered vector of admin class labels (broadest first). Sets
`.adb_state$path` to `root`.

### `adb_reset(what = "all")`

Roll the database back to a "registered but not normalised" state.
`what` is one or more of `"vocabularies"`, `"schemas"`, `"tables"`,
`"geometries"`, `"inventory"`, or `"all"`.

For each component reset: deletes stage3 artefacts (plus mappings
csvs for vocabularies), then flips `inventory$<kind>$status` from
`"normalised"` back to `"staged"`. Registrations and stage2 files are
preserved.

`what = "inventory"` wipes `inventory.rds` entirely - use with care.
When combined with other components, the status-flip is skipped (the
file is going away anyway).

### `adb_inventory(type = NULL)`

Return the entire inventory or one named sub-table. `type` is one of
`"dataseries"`, `"tables"`, `"geometries"`, `"vocabularies"`.

### `adb_ontology(..., vocabulary = NULL)`

Return the terms of a registered vocabulary, optionally filtered
(dplyr `...`). Joins external-label mappings as additional columns.

### `adb_translations(vocabulary, dataseries)`

Return the per-dataseries mapping table for a vocabulary. Columns:
`source_label`, `source`, `canonical_id`, `parent_id`, `note`.

### `adb_schemas(pattern = NULL)`

Return a named list of registered `tabshiftr` schemas, optionally
filtered by regex.

### `adb_metadata()`

Summarise stage3 contents: list of nations covered, variables
present, concepts, years available. Reads from `tables/stage3/*.rds`.

### `adb_diagnose(verbose = FALSE)`

Report counts of registered vs stage2 vs stage3 dataseries, and flag
stage3 tables with high unmatched-territory proportions (NA `gazID`).
Returns invisibly.

### `adb_archive(pattern, variables, compress, outPath)`

Export the database to a portable archive (geopackages + CSVs + a
session-info dump). `compress = TRUE` produces a `.tar.gz`.

### `adb_backup()`

Tag stage3 files, inventory, and vocabulary parquets with
`<version>_<date>` and copy them into `<root>/backup/`.

### `adb_restore(version, date)`

Restore files matching `<version>_<date>` from `<root>/backup/` back
to their working locations. Overwrites by default.

### `adb_query(territory, concept, variable, level, year)`

Query the normalised database. Returns an sf object joining
geometries with values.

- `territory` named list mapping admin class to label, e.g.
  `list(ADM0 = "Guatemala", ADM1 = c("Peten", "Izabal"))`
- `concept` named list mapping ontology to value, e.g.
  `list(commodity = "barley")`
- `variable` observed-variable column name
- `level` admin class to return geometries at; defaults to the
  finest available
- `year` integer or vector

### `adb_example(path, until, verbose)`

Build an example database up to a chosen step. `until` is one of
`"adb_init"`, `"regDataseries"`, `"regVocabulary"`, `"regGeometry"`,
`"regTable"`, `"normVocabulary"`, `"normGeometry"`, `"normTable"`.

---

## reg* - declare inputs

### `regDataseries(name, description, homepage, version, licence_link, reference, notes, overwrite)`

Record a data source (the project/agency producing one or more files).
Assigns a `datID`. Required keyword arguments: `name`, `description`,
`homepage`, `version`, `licence_link`.

### `regVocabulary(name, dSeries, description, schema, archive, archiveLink, downloadDate, version, licence_link, notes, overwrite)`

Register a vocabulary file (gazetteer or any thematic ontology).
- `name` canonical vocabulary name (e.g. `"gazetteer"`,
  `"commodity"`).
- `dSeries` the dataseries name to attach (must already be
  registered via `regDataseries`).
- `schema` optional `tabshiftr` schema if the source file isn't in
  the canonical layout.
- `archive` filename in `vocabularies/stage2/`.
- `archiveLink` original URL.

### `regGeometry(..., subset, gSeries, label, ancillary, layer, archive, archiveLink, downloadDate, updateFrequency, notes, overwrite)`

Register a geometry file.
- `...` named admin-class arguments declaring scope, e.g.
  `ADM0 = "Guatemala"`. Use `!!` to inject values
  (`ADM0 = !!thisNation`).
- `gSeries` the dataseries name providing the geometry.
- `label` named **list** mapping each admin class in the file to its
  source column, e.g. `label = list(ADM0 = "COUNTRY", ADM1 = "NAME_1")`.
- `archive` filename, optionally with `|layer.shp` to specify a
  layer inside a zip.
- `archiveLink` original URL.

### `regTable(..., subset, dSeries, gSeries, label, begin, end, schema, archive, archiveLink, downloadDate, updateFrequency, metadataLink, metadataPath, notes, overwrite)`

Register a tabular data file.
- `...` named admin-class scope (e.g. `ADM0 = "Guatemala"`).
- `label` the **deepest** admin class the table reports at (e.g.
  `"ADM2"`).
- `subset` a label distinguishing this table from sibling tables
  from the same dataseries (e.g. `"bovines"`, `"barley"`).
- `dSeries` the value-providing dataseries.
- `gSeries` the geometry dataseries the table's territorial labels
  refer to (so the harmoniser knows whose names to expect).
- `schema` a `tabshiftr` schema describing the table layout.
- `begin`, `end` year range covered.

---

## norm* - harmonise inputs

### `normVocabulary(input = NULL, pattern = NULL, verbose = FALSE)`

Harmonise registered vocabularies. Reads each stage2 file, applies
its schema, writes `<voc>_terms.parquet` and `<voc>_levels.parquet`
to `vocabularies/stage3/`, and an empty
`<voc>_<dataseries>.csv` mappings file ready to be populated by
downstream norm* calls.

- `input` explicit file path; if NULL, processes every stage2 vocab
  matching `pattern`.
- `pattern` regex over filenames.

Skips files whose inventory `status == "normalised"`. Use
`adb_reset(what = "vocabularies")` to force re-run.

### `normGeometry(input, pattern, query, thresh, beep, simplify, stringdist, strictMatch, verbose)`

Harmonise registered geometries. For each stage2 file:

1. Read with `sf::read_sf` (optionally filtered by SQL `query`).
2. Match top-class labels via `.matchOntology` against the gazetteer.
3. For sub-national geometries: either match labels (if the
   gazetteer already has those terms under the right parent) or
   match spatially against the parent layer.
4. Write per-top-unit `.gpkg` files at `geometries/stage3/`, one
   layer per admin class.

Key args:
- `pattern` regex over filenames
- `thresh` percent-overlap threshold below which geometries are
  considered the same (default 10)
- `simplify` whether to simplify geometries before intersecting
- `stringdist` whether to use string-distance matching (turn off for
  very large vocabularies)
- `strictMatch` require one-to-one matches; reject broader/narrower

Skips files whose inventory `status == "normalised"`. A consistency
guard errors out if a file's parsed top-unit doesn't appear in its
matched top-units (catches "wrong file at stage2").

### `normTable(input, pattern, query, ontoMatch, beep, verbose)`

Harmonise registered tables. For each stage2 file:

1. Read via the schema, producing a long tidy table.
2. Match territorial labels (`ADM0`, `ADM1`, ...) against the
   gazetteer mappings written during `normGeometry`.
3. Optionally match thematic columns (`ontoMatch`) against other
   ontologies registered via `regVocabulary` + `normVocabulary`.
4. Write per-top-unit `.rds` files at `tables/stage3/`.

Key args:
- `pattern` regex over filenames
- `ontoMatch` character vector of column names that should be
  resolved against a non-gazetteer vocabulary

Skips files whose inventory `status == "normalised"`. Use
`adb_reset(what = "tables")` to force re-run.

---

## match_builder

`match_builder(new, topLevel, source, ontology, stringdist)`.

A Shiny gadget that opens (in the browser, `launch.browser = TRUE`)
when norm* encounters unmatched labels. The user assigns each new
label to a canonical concept, marks it `"new"` (mint a new term),
or `"ignore"`.

- Closing the tab without Save & Close is treated as cancel - the
  function unblocks and returns empty.
- Click "Save & Close" to commit decisions to the mappings file and
  return.

You generally don't call `match_builder` directly - the norm* family
calls it on your behalf when needed.

---

## Conventions

- **State is on disk.** Every reg* / norm* call reads
  `inventory.rds`, mutates it, and writes it back. No long-lived
  in-memory state besides `.adb_state$path`.
- **Filenames carry semantics.** The first `_`-separated token of a
  stage2 filename is the top-unit; that drives the stage3 output
  filename. Don't rename stage2 files by hand.
- **Status field gates re-runs.** All three norm* functions skip
  files whose inventory `status == "normalised"`. Resetting flips it
  back to `"staged"`.
- **Mappings are CSV, terms/levels are parquet.** Mappings are
  human-readable because users often hand-edit them; terms/levels
  are derived and machine-only.
- **Dot-IDs are atomic.** Gazetteer IDs like `.001.002.001.027`
  have dots inside the ID. Don't split on `.` to parse them - walk
  the `parent_id` chain instead.
- **Stage2/processed is vestigial.** Old code path; nothing puts
  files there anymore. Ignore.
- **Encoding: UTF-8.** Source files declare `Encoding: UTF-8` in
  DESCRIPTION. Non-ASCII in roxygen docs is allowed; **non-ASCII in
  R code (comments included) breaks CRAN checks** - use ASCII
  hyphens, not em-dashes.

## Internal vs external boundary

- **Public API** (export, document, recommend): the 21 functions
  listed in NAMESPACE: `adb_*`, `reg*`, `norm*`, `match_builder`,
  `.matchOntology` (exported but advanced).
- **Internal helpers** (do not call): functions with leading dot in
  `helpers.R` - `.read_terms`, `.read_levels`, `.read_mappings`,
  `.write_terms`, `.write_mappings`, `.lookup_mappings`,
  `.get_children`, `.get_ancestors`, `.read_all_mappings`,
  `.matchOntology` body. Treat the leading dot as the contract.
- **Package state**: `arealDB:::.adb_state$path` is the active root.
  `arealDB:::.adb_state$testing` is TRUE only while `adb_example`
  runs (skips interactive prompts).

## Dependencies

`Imports`: archive, arrow, beepr, checkmate, dplyr, magrittr,
progress, purrr, readr, rlang, rmapshaper, stringdist, stringr, sf,
tabshiftr, tibble, tidyr, tidyselect. `Suggests`: DT, shiny,
testthat, knitr, rmarkdown, bookdown, covr.

Shiny is suggested because `match_builder` is optional - if you have
no unmatched labels you never need it.
