# ---- Shared helpers for the wrapper analyses --------------------------------

#' Copy rows of a data.frame into a jmvcore Table (only known columns)
fill_table <- function(tbl, df) {
  if (is.null(df) || nrow(df) == 0) return(invisible())
  known <- tryCatch(names(tbl$asDF), error = function(e) character())
  cols <- intersect(names(df), known)
  for (i in seq_len(nrow(df))) {
    vals <- lapply(cols, function(cn) { v <- df[[cn]][i]; if (is.factor(v)) v <- as.character(v); if (is.character(v) && !is.na(v) && v %in% c("—", "-")) v <- NA; v })
    names(vals) <- cols
    vals <- vals[!vapply(vals, function(v) is.null(v) || (length(v) == 1 && is.na(v)), logical(1))]
    nr <- tryCatch(tbl$rowCount, error = function(e) 0)
    if (i <= nr) tbl$setRow(rowNo = i, values = vals) else tbl$addRow(rowKey = i, values = vals)
  }
  invisible()
}

#' Build the report HTML (method sentence + results text) with optional LLM polish
build_report_html <- function(summary, lang, useLLM, model, endpoint, checkpoint = NULL, timeout = 600) {
  L <- i18n(lang)
  results <- render_results_text(summary, lang)
  method <- para(method_sentence(summary, lang))
  notes <- character(); used <- FALSE; interp <- NULL
  if (isTRUE(useLLM)) {
    av <- ollama_available(endpoint)
    if (!isTRUE(av$ok)) notes <- c(notes, sprintf(L$llm_unavailable, av$error))
    else if (length(av$models) && !(model %in% av$models) && !(paste0(model, ":latest") %in% av$models)) notes <- c(notes, tx(lang, paste0("Model '", model, "' Ollama'da yüklü değil (yüklü: ", paste(av$models, collapse = ", "), ")."), paste0("Model '", model, "' is not installed in Ollama (installed: ", paste(av$models, collapse = ", "), ").")))
    else {
      if (is.function(checkpoint)) checkpoint()
      template_results <- results
      pr <- llm_polish(results, "results", lang, model, endpoint, timeout = timeout)
      if (pr$used) { results <- pr$html; used <- TRUE } else notes <- c(notes, sprintf(L$llm_fidelity_fail, pr$note))
      if (is.function(checkpoint)) checkpoint()
      it <- llm_interpret(template_results, lang, model, endpoint, timeout = timeout)
      if (isTRUE(it$ok)) interp <- it$html else notes <- c(notes, paste0(L$interp_title, ": ", it$note))
    }
  }
  html <- paste0("<h3>", L$method_title, "</h3>", method, "<h3>", L$results_title, "</h3>", results,
                 if (!is.null(interp)) paste0("<h3>", L$interp_title, "</h3>", interp, "<p style='color:#777;font-size:90%'>", sprintf(L$interp_note, html_escape(model)), "</p>") else "",
                 "<p style='color:#777;font-size:90%'>", if (used) sprintf(L$layer_llm, html_escape(model)) else L$layer_template, "</p>")
  list(html = html, notes = notes, used = used)
}

report_options_ok <- function(self) list(lang = self$options$lang, useLLM = self$options$useLLM, model = self$options$model, endpoint = self$options$endpoint)

set_warnings <- function(self, notes) {
  notes <- notes[!is.na(notes) & nzchar(notes)]
  if (length(notes)) { self$results$warnings$setContent(paste0("<ul>", paste0("<li>", html_escape(notes), "</li>", collapse = ""), "</ul>")); self$results$warnings$setVisible(TRUE) }
  else self$results$warnings$setVisible(FALSE)
}
