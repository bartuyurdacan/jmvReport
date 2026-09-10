# ---- Shared helpers for the wrapper analyses --------------------------------

fill_table <- function(tbl, df) {
  if (is.null(df) || nrow(df) == 0) return(invisible())
  known <- tryCatch(names(tbl$asDF), error = function(e) character())
  cols <- intersect(names(df), known)
  for (i in seq_len(nrow(df))) {
    vals <- lapply(cols, function(cn) { v <- df[[cn]][i]; if (is.factor(v)) v <- as.character(v); if (is.character(v) && !is.na(v) && v %in% c("\u2014", "-")) v <- NA; v })
    names(vals) <- cols
    vals <- vals[!vapply(vals, function(v) is.null(v) || (length(v) == 1 && is.na(v)), logical(1))]
    nr <- tryCatch(tbl$rowCount, error = function(e) 0)
    if (i <= nr) tbl$setRow(rowNo = i, values = vals) else tbl$addRow(rowKey = i, values = vals)
  }
  invisible()
}

build_report_html <- function(summaries, useLLM, backend, model, endpoint,
                              interpret = TRUE, polish = FALSE, checkpoint = NULL, timeout = 600) {
  if (!is.null(summaries$type)) summaries <- list(summaries)
  lang <- "en"; L <- i18n()
  results <- paste(vapply(summaries, function(s) render_results_text(s), character(1)), collapse = "")
  method <- para(paste(vapply(summaries, function(s) method_sentence(s, lang), character(1)), collapse = " "))
  notes <- character(); used <- FALSE; interp <- NULL; stats <- NULL; connection <- NULL
  if (isTRUE(useLLM)) {
    connection <- llm_resolve_backend(backend, model, endpoint)
    if (!isTRUE(connection$ok)) {
      notes <- c(notes, sprintf(L$llm_unavailable, connection$error))
    } else {
      if (is.function(checkpoint)) checkpoint()
      r <- llm_report(results, connection, interpret = interpret, polish = polish, timeout = timeout)
      stats <- r$stats
      if (r$used) { results <- r$html; used <- TRUE }
      if (!is.null(r$interp)) interp <- r$interp
      if (!is.null(r$note)) notes <- c(notes, sprintf(L$llm_fidelity_fail, r$note))
    }
  }
  provider <- if (!is.null(connection) && isTRUE(connection$ok)) connection$label else "local AI"
  html <- paste0(
    "<h3>", L$method_title, "</h3>", method,
    "<h3>", L$results_title, "</h3>", results,
    if (!is.null(interp)) paste0("<h3>", L$interp_title, "</h3>", interp,
      "<p style='color:#777;font-size:90%'>", sprintf(L$interp_note, html_escape(provider)), "</p>") else "",
    "<p style='color:#777;font-size:90%'>",
    if (used) sprintf(L$layer_llm, html_escape(provider)) else if (!is.null(interp)) sprintf(L$layer_interp, html_escape(provider)) else L$layer_template,
    if (!is.null(stats)) paste0(" ", stats_line(stats)) else "", "</p>"
  )
  list(html = html, notes = notes, used = used, stats = stats, connection = connection)
}

set_warnings <- function(self, notes) {
  notes <- notes[!is.na(notes) & nzchar(notes)]
  if (length(notes)) {
    self$results$warnings$setContent(paste0("<ul>", paste0("<li>", html_escape(notes), "</li>", collapse = ""), "</ul>"))
    self$results$warnings$setVisible(TRUE)
  } else self$results$warnings$setVisible(FALSE)
}
