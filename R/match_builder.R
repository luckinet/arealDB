#' Interactively match unresolved terms to canonical vocabulary concepts
#'
#' Opens a Shiny app that presents unresolved source labels alongside fuzzy
#' candidates and the relevant parent context, allowing the user to confirm,
#' reject, or reassign each match. Confirmed assignments are written to the
#' vocabulary directory via \code{.write_mappings()} when the app closes.
#'
#' @param new [`data.frame(.)`][data.frame]\cr tibble of unmatched terms with
#'   columns \code{source_label}, \code{parent_id}, \code{canonical_id} (NA),
#'   \code{note} (NA).
#' @param topLevel [`logical(1)`][logical]\cr TRUE when matching the top
#'   hierarchy level (no parent context available).
#' @param source [`character(1)`][character]\cr dataseries name, used as the
#'   \code{source} column in the written mappings.
#' @param ontology [`character(1)`][character]\cr name of the vocabulary
#'   to match against (e.g. \code{"gazetteer"}, \code{"commodity"}).
#' @param stringdist [`logical(1)`][logical]\cr whether to pre-fill fuzzy
#'   candidate suggestions.
#' @return tibble with columns \code{source_label}, \code{canonical_id},
#'   \code{parent_id}, \code{note}. Only rows where the user confirmed a
#'   \code{canonical_id} (or set note to "ignore"/"new") are returned.
#' @details Requires the \pkg{shiny} and \pkg{DT} packages.
#' @examples
#' \dontrun{
#' match_builder(
#'   new       = tibble(source_label = c("Ain", "Alpes"),
#'                      parent_id    = c(".001", ".001"),
#'                      canonical_id = NA_character_,
#'                      note         = NA_character_),
#'   topLevel  = FALSE,
#'   source    = "eurostat",
#'   ontology  = "gazetteer"
#' )
#' }
#' @importFrom checkmate assertDataFrame assertNames assertCharacter assertLogical
#' @importFrom dplyr filter select distinct
#' @importFrom tibble tibble
#' @export

match_builder <- function(new, topLevel = FALSE, source = NULL,
                          ontology = NULL, stringdist = TRUE) {

  testing <- .adb_state$testing

  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required: install.packages('shiny')")
  if (!requireNamespace("DT", quietly = TRUE))
    stop("Package 'DT' is required: install.packages('DT')")

  assertDataFrame(x = new)
  assertNames(x = names(new), must.include = c("source_label", "parent_id", "canonical_id"))
  assertLogical(x = topLevel, any.missing = FALSE, len = 1)
  assertCharacter(x = source, len = 1, any.missing = FALSE)
  assertCharacter(x = ontology, len = 1, any.missing = FALSE)

  terms <- .read_terms(ontology)

  # --- look up dataseries homepage from inventory ----------------------------
  ds_homepage <- tryCatch({
    inv <- readRDS(paste0(.adb_state$path, "/inventory.rds"))$dataseries
    hp  <- inv$homepage[inv$name == source]
    if (length(hp) == 1 && nzchar(hp)) hp else NULL
  }, error = function(e) NULL)

  # --- look up db metadata ---------------------------------------------------
  db_meta <- tryCatch({
    e <- new.env(); load(paste0(.adb_state$path, "/db_info.RData"), envir = e); e$db_info
  }, error = function(e) NULL)
  db_version <- if (!is.null(db_meta)) db_meta$version else "unknown"
  db_level   <- if (!is.null(db_meta)) paste(db_meta$level, collapse = ", ") else "unknown"

  # --- build candidate set --------------------------------------------------
  if (topLevel || all(is.na(new$parent_id))) {
    target_class <- if ("class" %in% names(new)) unique(na.omit(new$class)) else character(0)
    if (length(target_class) > 0) {
      candidates <- terms |>
        filter(class %in% target_class) |>
        select(id, label, class, parent_id)
    } else {
      candidates <- terms |>
        filter(is.na(parent_id)) |>
        select(id, label, class, parent_id)
    }
  } else {
    parent_ids <- unique(na.omit(new$parent_id))
    candidates <- terms |>
      filter(parent_id %in% parent_ids) |>
      select(id, label, class, parent_id)
  }
  n_candidates <- nrow(candidates)

  # JSON array of all candidates for jQuery UI autocomplete (built once)
  cand_json <- paste0(
    "[",
    paste(vapply(seq_len(n_candidates), function(i) {
      lbl <- gsub("\\", "\\\\", candidates$label[i], fixed = TRUE)
      lbl <- gsub("\"", "\\\"", lbl, fixed = TRUE)
      sprintf('{"label":"%s (%s)","value":"%s"}', lbl, candidates$id[i], candidates$id[i])
    }, character(1)), collapse = ","),
    "]"
  )

  # --- fuzzy suggestions (Jaro-Winkler) -------------------------------------
  .normalise <- function(x) {
    x <- tolower(x)
    x <- trimws(gsub("\\s+", " ", x))
    x <- gsub("[-\u2013\u2014]+", "-", x)
    x <- gsub("-{2,}", "-", x)
    x
  }

  JW_THRESH <- 0.15   # JW distance 0-1; ~equivalent to LV d<=2 on short strings

  .suggest <- function(src_label, par_id) {
    cands <- if (!is.na(par_id) && n_candidates > 0)
      candidates[!is.na(candidates$parent_id) & candidates$parent_id == par_id, ]
    else
      candidates

    if (nrow(cands) == 0 || !stringdist) return(list(id0 = "", matches = data.frame()))

    query      <- .normalise(src_label)
    cand_norms <- .normalise(cands$label)
    dists      <- stringdist::stringdist(query, cand_norms, method = "jw", p = 0.1)

    matches <- data.frame(id = cands$id, label = cands$label, dist = dists,
                          stringsAsFactors = FALSE)
    matches <- matches[order(matches$dist), ]

    id0 <- if (any(dists <= 0.001)) cands$id[which(dists <= 0.001)[1]] else ""
    list(id0 = id0, matches = matches)
  }

  suggestions <- lapply(seq_len(nrow(new)), function(i)
    .suggest(new$source_label[i], new$parent_id[i]))

  auto_new <- vapply(suggestions, function(s) {
    if (nrow(s$matches) == 0) return(TRUE)
    min(s$matches$dist) > JW_THRESH
  }, logical(1))

  # --- parent context labels ------------------------------------------------
  .parent_label <- function(par_id) {
    if (is.na(par_id)) return("")
    ancestors <- .get_ancestors(par_id, ontology)
    row <- terms[terms$id == par_id, ]
    if (nrow(row) == 0) return(par_id)
    paste(c(ancestors, row$label[1]), collapse = " > ")
  }
  parent_context <- unname(vapply(new$parent_id, .parent_label, character(1)))

  # --- initial decisions ----------------------------------------------------
  # "new?" = auto-suggested, not yet confirmed; shown in table, counts as remaining
  # "new"  = user confirmed; hidden by default
  # ""     = unresolved
  initial_decision <- vapply(seq_len(nrow(new)), function(i) {
    if (auto_new[i]) return("new?")
    suggestions[[i]]$id0
  }, character(1))

  n_rows <- nrow(new)

  if (testing) {
    return(tibble(source_label = "aNation", canonical_id = "a_nation",
                  parent_id = NA_character_, note = NA_character_))
  }

  result_env <- new.env(parent = emptyenv())
  result_env$mappings <- NULL

  # --- JS -------------------------------------------------------------------
  js_handler <- sprintf("
  var mbCandidates = %s;

  function mbInitAC() {
    $('.mb-ac').each(function() {
      if ($(this).data('ui-autocomplete')) return;
      $(this).autocomplete({
        source: mbCandidates,
        minLength: 1,
        select: function(event, ui) {
          var idx = $(this).data('row');
          $(this).val(ui.item.label);
          $('#mb_select_' + idx).val('').trigger('change.silent');
          $('#mb_ignore_' + idx).removeClass('active btn-warning').addClass('btn-outline-warning');
          $('#mb_new_'    + idx).removeClass('active confirmed suggested btn-danger').addClass('btn-outline-secondary');
          $('#mb_skip_'   + idx).removeClass('active btn-secondary').addClass('btn-outline-secondary');
          Shiny.setInputValue('mb_select', {row: idx, val: ui.item.value}, {priority: 'event'});
          return false;
        },
        change: function(event, ui) {
          // only act when user explicitly picked from the list (ui.item set);
          // if they blurred without picking, leave whatever they typed in place
        }
      });
    });
  }

  $(document).on('draw.dt', function() { setTimeout(mbInitAC, 100); });
  $(document).ready(function() { setTimeout(mbInitAC, 500); });

  $(document).on('change', '.mb-select', function() {
    var idx = $(this).data('row');
    var val = $(this).val();
    $('#mb_ac_'     + idx).val('');
    $('#mb_ignore_' + idx).removeClass('active btn-warning').addClass('btn-outline-warning');
    $('#mb_new_'    + idx).removeClass('active confirmed suggested btn-danger').addClass('btn-outline-secondary');
    $('#mb_skip_'   + idx).removeClass('active btn-secondary').addClass('btn-outline-secondary');
    Shiny.setInputValue('mb_select', {row: idx, val: val}, {priority: 'event'});
  });

  $(document).on('click', '.mb-ignore', function() {
    var idx = $(this).data('row');
    var active = !$(this).hasClass('active');
    $(this).toggleClass('active btn-warning btn-outline-warning');
    $('#mb_new_'    + idx).removeClass('active confirmed suggested btn-danger').addClass('btn-outline-secondary');
    $('#mb_skip_'   + idx).removeClass('active btn-secondary').addClass('btn-outline-secondary');
    $('#mb_select_' + idx).val('').trigger('change.silent');
    $('#mb_ac_'     + idx).val('');
    Shiny.setInputValue('mb_special', {row: idx, val: active ? 'ignore' : ''}, {priority: 'event'});
  });

  // new cycles: neutral -> suggested(new?) -> confirmed(new) -> neutral
  // from R side: suggested rendered as .suggested class; confirmed as .confirmed
  $(document).on('click', '.mb-new', function() {
    var idx = $(this).data('row');
    var isSuggested = $(this).hasClass('suggested');
    var isConfirmed = $(this).hasClass('confirmed');
    $('#mb_ignore_' + idx).removeClass('active btn-warning').addClass('btn-outline-warning');
    $('#mb_skip_'   + idx).removeClass('active btn-secondary').addClass('btn-outline-secondary');
    $('#mb_select_' + idx).val('').trigger('change.silent');
    $('#mb_ac_'     + idx).val('');
    var newVal;
    if (isSuggested) {
      $(this).removeClass('suggested btn-outline-danger').addClass('confirmed btn-danger active');
      newVal = 'new';
    } else if (isConfirmed) {
      $(this).removeClass('confirmed active btn-danger').addClass('btn-outline-secondary');
      newVal = '';
    } else {
      $(this).removeClass('btn-outline-secondary').addClass('confirmed btn-danger active');
      newVal = 'new';
    }
    Shiny.setInputValue('mb_special', {row: idx, val: newVal}, {priority: 'event'});
  });

  $(document).on('click', '.mb-skip', function() {
    var idx = $(this).data('row');
    var active = !$(this).hasClass('active');
    $(this).toggleClass('active btn-secondary btn-outline-secondary');
    $('#mb_new_'    + idx).removeClass('active confirmed suggested btn-danger').addClass('btn-outline-secondary');
    $('#mb_ignore_' + idx).removeClass('active btn-warning').addClass('btn-outline-warning');
    $('#mb_select_' + idx).val('').trigger('change.silent');
    $('#mb_ac_'     + idx).val('');
    Shiny.setInputValue('mb_special', {row: idx, val: active ? 'skip' : ''}, {priority: 'event'});
  });

  $(document).on('change.silent', '.mb-select', function(e) { e.stopImmediatePropagation(); });
  ", cand_json)

  # --- per-row HTML builders ------------------------------------------------
  .badge <- function(d) {
    if (d <= 0.001)
      "<span class='mb-badge exact'>exact</span>"
    else if (d <= 0.05)
      "<span class='mb-badge close'>close</span>"
    else
      "<span class='mb-badge fuzzy'>fuzzy</span>"
  }

  .make_select <- function(i, matches, decision) {
    opts <- '<option value=""></option>'
    close_rows <- if (nrow(matches) == 0) integer(0) else which(matches$dist <= JW_THRESH)
    for (j in close_rows) {
      m   <- matches[j, ]
      sel <- if (nzchar(decision) && !decision %in% c("ignore","new","new?") &&
                   decision == m$id) "selected" else ""
      badge   <- .badge(m$dist)
      display <- gsub("&", "&amp;", paste0(m$label, " (", m$id, ")"), fixed = TRUE)
      display <- gsub("<", "&lt;", display, fixed = TRUE)
      display <- gsub(">", "&gt;", display, fixed = TRUE)
      opts <- paste0(opts, sprintf("<option value='%s' %s>%s %s</option>",
                                   m$id, sel, display, badge))
    }
    sprintf("<select id='mb_select_%d' class='mb-select' data-row='%d' style='width:100%%;max-width:240px;'>%s</select>",
            i, i, opts)
  }

  .make_ac <- function(i, decision) {
    val <- if (nzchar(decision) && !decision %in% c("ignore","new","new?")) {
      sugg     <- suggestions[[i]]
      in_close <- nrow(sugg$matches) > 0 &&
        any(sugg$matches$id == decision & sugg$matches$dist <= JW_THRESH)
      if (in_close) "" else decision
    } else ""
    sprintf("<input id='mb_ac_%d' class='mb-ac' data-row='%d' value='%s'
             placeholder='search all...'
             style='width:100%%;max-width:200px;font-size:12px;padding:3px 5px;
                    border:1px solid #ced4da;border-radius:4px;'/>",
            i, i, val)
  }

  .make_buttons <- function(i, decision) {
    ign_cls <- if (decision == "ignore")
      "btn btn-warning btn-sm mb-ignore active"
    else
      "btn btn-outline-warning btn-sm mb-ignore"

    new_cls <- if (decision == "new")
      "btn btn-danger btn-sm mb-new confirmed active"
    else if (decision == "new?")
      "btn btn-outline-danger btn-sm mb-new suggested"
    else
      "btn btn-outline-secondary btn-sm mb-new"

    skp_cls <- if (decision == "skip")
      "btn btn-secondary btn-sm mb-skip active"
    else
      "btn btn-outline-secondary btn-sm mb-skip"

    sprintf("<div style='white-space:nowrap;display:flex;gap:4px;'>
               <button id='mb_new_%d'    class='%s' data-row='%d'>New</button>
               <button id='mb_ignore_%d' class='%s' data-row='%d'>Ignore</button>
               <button id='mb_skip_%d'   class='%s' data-row='%d' title='Skip for now - will reappear next run'>Skip</button>
             </div>",
            i, new_cls, i, i, ign_cls, i, i, skp_cls, i)
  }

  # --- CSS ------------------------------------------------------------------
  app_css <- "
    body { font-family: 'Source Sans Pro', sans-serif; font-size: 13px;
           background: #f4f6f9; }

    /* header bar */
    .mb-header { background: #2c3e50; color: white; padding: 14px 20px 10px 20px;
                 margin: -15px -15px 16px -15px; border-radius: 6px 6px 0 0; }
    .mb-header h4 { margin: 0; font-size: 16px; font-weight: 600; }
    .mb-header p  { margin: 3px 0 0 0; font-size: 11px; opacity: 0.7; }
    .mb-header a  { color: #aed6f1; font-size: 11px; }

    /* sidebar card */
    .mb-sidebar { background: white; border-radius: 8px;
                  box-shadow: 0 1px 4px rgba(0,0,0,0.10); padding: 16px; }
    .section-header { font-weight: 700; color: #7f8c8d; margin-top: 14px;
                      margin-bottom: 5px; font-size: 10px; text-transform: uppercase;
                      letter-spacing: 0.08em; }

    /* progress counters */
    .mb-stats { display: grid; grid-template-columns: 1fr 1fr; gap: 6px; margin-top: 4px; }
    .mb-stat  { border-radius: 6px; padding: 6px 8px; text-align: center; }
    .mb-stat .num  { font-size: 20px; font-weight: 700; line-height: 1.1; display: block; }
    .mb-stat .lbl  { font-size: 10px; text-transform: uppercase; letter-spacing: 0.05em; }
    .mb-stat.matched  { background: #eafaf1; color: #1e8449; }
    .mb-stat.ignored  { background: #fef9e7; color: #b7950b; }
    .mb-stat.new-cnt  { background: #fdf2e9; color: #ca6f1e; }
    .mb-stat.remaining{ background: #eaf2fb; color: #1a5276; }

    /* instructions */
    .mb-steps { background: #f8f9fa; border-radius: 6px; padding: 10px 12px;
                margin-top: 6px; border: 1px solid #e9ecef; }
    .mb-step  { display: flex; gap: 8px; margin-bottom: 6px; font-size: 11px;
                color: #555; align-items: flex-start; }
    .mb-step:last-child { margin-bottom: 0; }
    .mb-step-num { background: #2c3e50; color: white; border-radius: 50%;
                   width: 16px; height: 16px; font-size: 9px; font-weight: 700;
                   display: flex; align-items: center; justify-content: center;
                   flex-shrink: 0; margin-top: 1px; }
    .mb-or    { color: #aaa; font-size: 10px; text-align: center; margin: 2px 0; }

    /* save button */
    .btn-save { background: #27ae60; color: white; border: none;
                padding: 9px 0; border-radius: 6px; font-weight: 700;
                width: 100%; margin-top: 16px; font-size: 13px;
                box-shadow: 0 2px 4px rgba(39,174,96,0.3); }
    .btn-save:hover { background: #1e8449; color: white; }

    /* table panel */
    .tab-content { background: white; border-radius: 0 0 8px 8px;
                   box-shadow: 0 1px 4px rgba(0,0,0,0.08); padding: 0 12px 12px 12px; }

    /* distance badges */
    .mb-badge { border-radius: 3px; padding: 1px 5px; font-size: 10px;
                margin-left: 3px; font-weight: 600; }
    .mb-badge.exact { background: #27ae60; color: white; }
    .mb-badge.close { background: #f39c12; color: white; }
    .mb-badge.fuzzy { background: #e67e22; color: white; }

    /* new button states */
    .btn.suggested { border: 2px dashed #e67e22 !important; color: #e67e22 !important;
                     background: transparent !important; opacity: 1 !important; }
    .btn.confirmed.btn-danger { background: #c0392b !important;
                                border-color: #c0392b !important; }

    /* autocomplete dropdown */
    .ui-autocomplete { max-height: 220px; overflow-y: auto; overflow-x: hidden;
                       font-size: 12px; z-index: 9999 !important;
                       border-radius: 4px; box-shadow: 0 3px 8px rgba(0,0,0,0.15); }
    .ui-menu-item-wrapper { padding: 5px 10px; }
    .ui-state-active { background: #2c3e50 !important; color: white !important; }

    .help-text { color: #95a5a6; font-size: 11px; margin-top: 4px; }
  "

  # --- Shiny app ------------------------------------------------------------
  app <- shiny::shinyApp(

    ui = shiny::fluidPage(

      shiny::tags$head(
        shiny::tags$link(rel  = "stylesheet",
                         href = "https://code.jquery.com/ui/1.13.2/themes/base/jquery-ui.css"),
        shiny::tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),
        shiny::tags$style(shiny::HTML(app_css)),
        shiny::tags$script(shiny::HTML(js_handler))
      ),

      shiny::sidebarLayout(

        shiny::sidebarPanel(width = 3,
          shiny::div(class = "mb-sidebar",

            # header card
            shiny::div(class = "mb-header",
              shiny::h4("match_builder"),
              shiny::p(
                paste0(n_rows, " unresolved terms  |  vocabulary: ", ontology),
                if (!is.null(ds_homepage))
                  shiny::tagList(" | ", shiny::a(source, href = ds_homepage,
                                                 target = "_blank"))
                else
                  source
              )
            ),

            # db metadata
            shiny::div(class = "section-header", "Database"),
            shiny::p(paste0("version ", db_version, "  |  top level: ", db_level,
                            "  |  ", n_candidates, " candidates"),
                     style = "font-size:11px;color:#7f8c8d;margin:0 0 4px 0;"),

            shiny::hr(style = "margin: 12px 0;"),

            # progress
            shiny::div(class = "section-header", "Progress"),
            shiny::uiOutput("progress_stats"),

            shiny::hr(style = "margin: 12px 0;"),

            # options
            shiny::checkboxInput("show_all", "Show all (including resolved)", value = FALSE),

            shiny::hr(style = "margin: 12px 0;"),

            # instructions
            shiny::div(class = "section-header", "How to match"),
            shiny::div(class = "mb-steps",
              shiny::div(class = "mb-step",
                shiny::div(class = "mb-step-num", "1"),
                shiny::span("Pick from the dropdown - shows the spelling-similar candidates
                             (", shiny::strong("exact"), "= same spelling, ",
                             shiny::strong("close"), "= minor difference, ",
                             shiny::strong("fuzzy"), "= loose match).")),
              shiny::div(class = "mb-or", "- or -"),
              shiny::div(class = "mb-step",
                shiny::div(class = "mb-step-num", "2"),
                shiny::span("Type in the search box to find any candidate by name or ID.")),
              shiny::div(class = "mb-or", "- or -"),
              shiny::div(class = "mb-step",
                shiny::div(class = "mb-step-num", "3"),
                shiny::span(shiny::strong("New"), "- this term doesn't exist in the vocabulary yet
                             (dashed border = suggested, solid red = confirmed).")),
              shiny::div(class = "mb-or", "- or -"),
              shiny::div(class = "mb-step",
                shiny::div(class = "mb-step-num", "4"),
                shiny::span(shiny::strong("Ignore"), "- skip this term entirely."))
            ),
            shiny::p("Resolved rows hide automatically.", class = "help-text"),

            shiny::actionButton("save_close", "Save & Close", class = "btn-save")
          )
        ),

        shiny::mainPanel(width = 9,
          shiny::br(),
          shiny::tabsetPanel(
            shiny::tabPanel("Terms to match",
              shiny::div(class = "tab-content",
                shiny::br(),
                DT::dataTableOutput("edit_table")
              )
            ),
            shiny::tabPanel("All candidates",
              shiny::div(class = "tab-content",
                shiny::br(),
                shiny::p("All valid canonical concepts available for matching.",
                         class = "help-text"),
                DT::dataTableOutput("candidates_table")
              )
            )
          )
        )
      )
    ),

    server = function(input, output, session) {

      rv <- shiny::reactiveValues(decisions = initial_decision)

      shiny::observeEvent(input$mb_select, {
        i   <- input$mb_select$row
        val <- input$mb_select$val
        if (!is.null(i) && i >= 1 && i <= n_rows)
          rv$decisions[i] <- if (is.null(val) || !nzchar(trimws(val))) "" else trimws(val)
      })

      shiny::observeEvent(input$mb_special, {
        i   <- input$mb_special$row
        val <- input$mb_special$val
        if (!is.null(i) && i >= 1 && i <= n_rows)
          rv$decisions[i] <- if (is.null(val)) "" else val
      })

      .is_resolved <- function(i) {
        d <- rv$decisions[i]
        nzchar(d) && d != "new?"
      }

      .build_table <- function(show_all) {
        rows <- seq_len(n_rows)
        if (!show_all) rows <- rows[!vapply(rows, .is_resolved, logical(1))]
        has_class <- "class" %in% names(new)
        df <- data.frame(
          `Your label`    = new$source_label[rows],
          `Parent`        = parent_context[rows],
          check.names      = FALSE,
          stringsAsFactors = FALSE
        )
        if (has_class) df[["Class"]] <- new$class[rows]
        df[["Possible matches"]] <- vapply(rows, function(i)
          .make_select(i, suggestions[[i]]$matches, rv$decisions[i]), character(1))
        df[["Search"]]       <- vapply(rows, function(i)
          .make_ac(i, rv$decisions[i]), character(1))
        df[["New / Ignore"]] <- vapply(rows, function(i)
          .make_buttons(i, rv$decisions[i]), character(1))
        df
      }

      output$edit_table <- DT::renderDataTable({
        force(rv$decisions)
        DT::datatable(
          .build_table(isTRUE(input$show_all)),
          escape    = FALSE,
          selection = "none",
          rownames  = FALSE,
          options   = list(
            pageLength = 25, scrollX = TRUE, ordering = FALSE,
            columnDefs = list(
              list(width = "170px", targets = 0),
              list(width = "130px", targets = 1),
              list(width = "250px", targets = if ("class" %in% names(new)) 3L else 2L),
              list(width = "210px", targets = if ("class" %in% names(new)) 4L else 3L),
              list(width = "150px", targets = if ("class" %in% names(new)) 5L else 4L)
            )
          )
        )
      })

      output$candidates_table <- DT::renderDataTable({
        assigned_ids <- rv$decisions[nzchar(rv$decisions) &
                                     !rv$decisions %in% c("ignore","new","new?","skip")]
        tbl <- candidates |> select(id, label, class, parent_id)
        tbl$assigned <- tbl$id %in% assigned_ids
        # assigned column is hidden (index 5, 0-based = 4); used only for row styling
        dt <- DT::datatable(
          tbl,
          selection = "none", rownames = FALSE, filter = "top",
          options   = list(pageLength = 25, scrollX = TRUE,
                           columnDefs = list(list(visible = FALSE, targets = 4)),
                           order = list(list(2, "asc"), list(3, "asc")))
        )
        DT::formatStyle(dt, "assigned",
          target = "row",
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#eafaf1", "")),
          fontWeight      = DT::styleEqual(c(TRUE, FALSE), c("600", "normal"))
        )
      })

      output$progress_stats <- shiny::renderUI({
        decs   <- rv$decisions
        n_conf <- sum(nzchar(decs) & !decs %in% c("ignore","new","new?","skip"), na.rm = TRUE)
        n_ign  <- sum(decs == "ignore", na.rm = TRUE)
        n_new  <- sum(decs == "new",    na.rm = TRUE)
        n_skip <- sum(decs == "skip",   na.rm = TRUE)
        n_left <- n_rows - n_conf - n_ign - n_new - n_skip
        shiny::div(class = "mb-stats",
          shiny::div(class = "mb-stat matched",
            shiny::span(class = "num", n_conf),
            shiny::span(class = "lbl", "matched")),
          shiny::div(class = "mb-stat remaining",
            shiny::span(class = "num", n_left),
            shiny::span(class = "lbl", "remaining")),
          shiny::div(class = "mb-stat new-cnt",
            shiny::span(class = "num", n_new),
            shiny::span(class = "lbl", "new")),
          shiny::div(class = "mb-stat ignored",
            shiny::span(class = "num", n_ign),
            shiny::span(class = "lbl", "ignored"))
        )
      })

      shiny::observeEvent(input$save_close, {
        result_env$mappings <- rv$decisions
        shiny::stopApp()
      })
    }
  )

  shiny::runApp(app, launch.browser = TRUE)

  # --- process results ------------------------------------------------------
  decisions <- result_env$mappings
  if (is.null(decisions)) {
    return(tibble(source_label = character(), canonical_id = character(),
                  parent_id    = character(), note         = character()))
  }

  tibble(
    source_label = new$source_label,
    canonical_id = ifelse(decisions %in% c("ignore","new","new?","skip",""), NA_character_, decisions),
    parent_id    = new$parent_id,
    note         = ifelse(decisions == "ignore", "ignore",
                   ifelse(decisions == "new",    "new", NA_character_))
  ) |>
    filter(nzchar(trimws(ifelse(is.na(canonical_id), "", canonical_id))) |
             note %in% c("ignore", "new") |
             decisions == "skip") |>
    distinct()
}
