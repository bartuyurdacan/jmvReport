# ---- End-to-end: .omv -> summaries -> text -----------------------------------

#' Build the full report from a saved jamovi file
#' @param file Path to a saved .omv file; NULL enables automatic discovery.
#' @param lang Retained for compatibility; output is always English.
#' @param useLLM Whether to use an AI backend.
#' @param backend One of auto, ollama, builtin, or openai.
#' @param model Model identifier for Ollama or a custom server.
#' @param endpoint Optional Ollama or OpenAI-compatible server URL.
#' @param sections Report sections to generate.
#' @param alpha Statistical significance threshold.
#' @param checkpoint Optional callback used to keep jamovi responsive.
#' @param llm_timeout AI request timeout in seconds.
#' @param polish Whether AI may rewrite Results wording.
#' @export
report_from_omv <- function(file = NULL, lang = "en", useLLM = FALSE, backend = "auto", model = "qwen3.5:4b", endpoint = "",
                            sections = c("method", "results", "interpret"), alpha = 0.05, checkpoint = NULL, llm_timeout = 600, polish = FALSE) {
  t0 <- Sys.time(); lang <- "en"; L <- i18n(); warnings_ <- character()
  rp <- resolve_omv_path(file)
  if (is.na(rp$path)) return(list(ok = FALSE, error = if (!is.null(rp$error)) rp$error else L$no_file))
  omv <- tryCatch(read_omv_all(rp$path), error = function(e) e)
  if (inherits(omv, "error")) return(list(ok = FALSE, error = paste0("read_omv: ", conditionMessage(omv))))
  summaries <- list(); list_rows <- list()
  if (length(omv$syntax) == 0) warnings_ <- c(warnings_, tx(lang, "Dosyada yeniden \u00E7al\u0131\u015Ft\u0131r\u0131labilir analiz s\u00F6zdizimi bulunamad\u0131; kaydedilmi\u015F tablolar kullan\u0131ld\u0131.", "No re-runnable analysis syntax found in the file; stored tables were used."))
  for (i in seq_along(omv$syntax)) {
    if (is.function(checkpoint)) checkpoint()
    rr <- run_syntax(omv$syntax[i], omv$data)
    if (isTRUE(rr$ok)) {
      s <- summarize_results(rr$res); s$syntax <- omv$syntax[i]
      if (!isTRUE(s$supported)) warnings_ <- c(warnings_, paste0(i, ". ", s$title, ": ", L$unsupported, if (!is.null(s$error)) paste0(" \u2014 ", s$error) else ""))
    } else {
      fn <- if (!is.null(rr$parsed) && !is.na(rr$parsed$ns)) rr$parsed$fn else "Rj"
      s <- list(type = fn, title = if (fn == "Rj") "R (Rj)" else fn, supported = FALSE, tables = list(), syntax = omv$syntax[i], opts = list())
      warnings_ <- c(warnings_, paste0(i, ". ", fn, ": ", L$not_rerun, " (", rr$error, ")"))
    }
    summaries[[i]] <- s
  }
  # HTML fallback for analyses that could not be re-run (or when no syntax at all)
  ht <- tryCatch(html_tables(omv$html), error = function(e) list())
  h1 <- tryCatch(html_titles(omv$html), error = function(e) character())
  if (length(h1) == length(summaries)) for (i in seq_along(summaries)) if (!isTRUE(summaries[[i]]$supported) && identical(summaries[[i]]$title, summaries[[i]]$type)) summaries[[i]]$title <- h1[i]
  if (length(omv$syntax) == 0 && length(ht)) {
    an <- unique(vapply(ht, function(x) x$analysis, character(1)))
    for (a in an) { tabs <- Filter(function(x) identical(x$analysis, a), ht)
      summaries[[length(summaries) + 1]] <- list(type = "html", title = a, supported = FALSE, tables = setNames(lapply(tabs, function(x) list(title = x$title, df = x$df)), vapply(tabs, function(x) x$title, character(1))), opts = list()) }
  } else if (length(ht)) {
    for (i in seq_along(summaries)) if (!isTRUE(summaries[[i]]$supported) && length(summaries[[i]]$tables) == 0) {
      # match by position among h1 headings
      an <- unique(vapply(ht, function(x) x$analysis, character(1)))
      if (i <= length(an)) { tabs <- Filter(function(x) identical(x$analysis, an[i]), ht)
        summaries[[i]]$tables <- setNames(lapply(tabs, function(x) list(title = x$title, df = x$df)), vapply(tabs, function(x) x$title, character(1))) }
    }
  }
  results_html <- ""; method_html <- ""
  for (i in seq_along(summaries)) results_html <- paste0(results_html, render_results_text(summaries[[i]], lang, index = i))
  supported <- Filter(function(s) isTRUE(s$supported), summaries)
  method_html <- render_method_text(supported, lang, alpha = alpha, jamovi_version = jamovi_version_guess())
  llm_note <- NULL; llm_used <- FALSE; interp_html <- NULL; stats <- NULL
  connection <- NULL
  if (isTRUE(useLLM) && ("results" %in% sections || "interpret" %in% sections)) {
    connection <- llm_resolve_backend(backend, model, endpoint)
    if (!isTRUE(connection$ok)) {
      llm_note <- sprintf(L$llm_unavailable, connection$error)
    } else {
      pieces <- character(); interps <- character()
      for (i in seq_along(summaries)) {
        piece <- render_results_text(summaries[[i]], lang, index = i)
        if (!isTRUE(summaries[[i]]$supported)) { pieces <- c(pieces, piece); next }
        if (is.function(checkpoint)) checkpoint()
        r <- llm_report(piece, connection, interpret = "interpret" %in% sections, polish = polish, timeout = llm_timeout)
        stats <- stats_add(stats, r$stats)
        if (r$used) { pieces <- c(pieces, r$html); llm_used <- TRUE } else pieces <- c(pieces, piece)
        if (!is.null(r$interp)) interps <- c(interps, paste0("<p><b>", i, ". ", html_escape(summaries[[i]]$title), "</b></p>", r$interp))
        if (!is.null(r$note)) llm_note <- c(llm_note, paste0(i, ". ", summaries[[i]]$title, ": ", sprintf(L$llm_fidelity_fail, r$note)))
      }
      if ("results" %in% sections) results_html <- paste(pieces, collapse = "")
      if (length(interps)) interp_html <- paste(interps, collapse = "")
    }
  }
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  list_df <- analysis_list_df(summaries, lang)
  provider <- if (!is.null(connection) && isTRUE(connection$ok)) connection$label else "local AI"
  info <- paste0("<p><b>", L$file_used, ":</b> ", html_escape(rp$path), if (isTRUE(rp$auto)) " (auto-detected)" else "", "<br><b>", L$n_analyses, ":</b> ", length(summaries), "<br>", if (llm_used) sprintf(L$layer_llm, html_escape(provider)) else if (!is.null(interp_html)) sprintf(L$layer_interp, html_escape(provider)) else L$layer_template, "<br><b>", L$elapsed, ":</b> ", elapsed, " ", L$sec, if (!is.null(stats)) paste0("<br>", stats_line(stats)) else "", "</p>")
  list(ok = TRUE, path = rp$path, summaries = summaries, method = method_html, results = results_html, interp = interp_html, info = info, list_df = list_df,
       warnings = c(warnings_, llm_note), llm_used = llm_used, elapsed = elapsed, stats = stats, provider = provider)
}

analysis_list_df <- function(summaries, lang) {
  rows <- lapply(seq_along(summaries), function(i) {
    s <- summaries[[i]]
    vars <- tryCatch(vars_of(s), error = function(e) "")
    key <- primary_stat(s)
    data.frame(index = i, analysis = s$title, type = s$type, variables = vars, stat = key$stat, p = key$p, es = key$es, stringsAsFactors = FALSE)
  })
  if (length(rows) == 0) return(data.frame(index = integer(), analysis = character(), type = character(), variables = character(), stat = character(), p = numeric(), es = character()))
  do.call(rbind, rows)
}

vars_of <- function(s) {
  ch <- function(x) if (is.character(x)) x else character()
  v <- switch(s$type,
    ttestIS = , ttestOneS = c(vapply(s$rows, function(r) r$var, character(1)), ch(s$group)),
    ttestPS = unlist(lapply(s$rows, function(r) c(r$var1, r$var2))),
    anovaOneW = , anovaNP = c(vapply(s$rows, function(r) r$dep, character(1)), ch(s$group)),
    ANOVA = , ancova = c(ch(s$dep), ch(s$factors), ch(s$covs)),
    anovaRM = c(ch(s$rm_labels), ch(s$bs)), anovaRMNP = ch(s$measures),
    corrMatrix = , reliability = , efa = , pca = ch(s$vars),
    linReg = , logRegBin = , logRegOrd = , logRegMulti = c(ch(s$dep), ch(s$covs), ch(s$factors)),
    contTables = , contTablesPaired = c(ch(s$rows), ch(s$cols), ch(s$layers)),
    propTest2 = , propTestN = ch(s$var), mancova = c(ch(s$deps), ch(s$factors), ch(s$covs)),
    descriptives = c(ch(s$vars), ch(s$splitBy)), character())
  paste(unique(v[!is.na(v) & nzchar(v)]), collapse = ", ")
}

primary_stat <- function(s) {
  g <- function(stat, p, es) list(stat = stat, p = p, es = es)
  tryCatch(switch(s$type,
    ttestIS = { r <- s$rows[[1]]; te <- if (!is.null(r$student)) r$student else if (!is.null(r$welch)) r$welch else NULL; if (!is.null(te)) g(paste0("t(", fmt_df(te$df), ") = ", fmt_num(te$t)), te$p, if (!is.na(te$es)) paste0("d = ", fmt_num(te$es)) else "") else g(paste0("U = ", fmt_num(r$mann$U, 1)), r$mann$p, paste0("r_rb = ", fmt_bounded(r$mann$es))) },
    ttestPS = { r <- s$rows[[1]]; te <- if (!is.null(r$student)) r$student else NULL; if (!is.null(te)) g(paste0("t(", fmt_df(te$df), ") = ", fmt_num(te$t)), te$p, if (!is.na(te$es)) paste0("d = ", fmt_num(te$es)) else "") else g(paste0("W = ", fmt_num(r$wilcoxon$W, 1)), r$wilcoxon$p, "") },
    ttestOneS = { r <- s$rows[[1]]; te <- r$student; g(paste0("t(", fmt_df(te$df), ") = ", fmt_num(te$t)), te$p, if (!is.na(te$es)) paste0("d = ", fmt_num(te$es)) else "") },
    anovaOneW = { r <- s$rows[[1]]; te <- if (!is.null(r$fisher)) r$fisher else r$welch; g(paste0("F(", fmt_df(te$df1), ", ", fmt_df(te$df2), ") = ", fmt_num(te$F)), te$p, "") },
    ANOVA = , ancova = { t <- s$terms[[1]]; g(paste0("F(", fmt_df(t$df), ", ", fmt_df(s$resid_df), ") = ", fmt_num(t$F)), t$p, if (!is.na(t$etaSqP)) paste0("\u03B7\u00B2p = ", fmt_bounded(t$etaSqP)) else "") },
    anovaRM = { w <- s$within[[1]]; g(paste0("F(", fmt_df(w$df), ") = ", fmt_num(w$F)), w$p, if (!is.na(w$partEta)) paste0("\u03B7\u00B2p = ", fmt_bounded(w$partEta)) else "") },
    anovaNP = { r <- s$rows[[1]]; g(paste0("H(", fmt_df(r$df), ") = ", fmt_num(r$H)), r$p, if (!is.na(r$es)) paste0("\u03B5\u00B2 = ", fmt_bounded(r$es)) else "") },
    anovaRMNP = g(paste0("\u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi)), s$p, ""),
    corrMatrix = { p <- s$pairs[[1]]; g(paste0("r = ", fmt_bounded(p$r)), p$rp, "") },
    linReg = { m <- s$models[[length(s$models)]]; g(paste0("F(", fmt_df(m$df1), ", ", fmt_df(m$df2), ") = ", fmt_num(m$F)), m$p, paste0("R\u00B2 = ", fmt_bounded(m$r2))) },
    logRegBin = , logRegOrd = , logRegMulti = { m <- s$models[[length(s$models)]]; g(paste0("\u03C7\u00B2(", fmt_df(m$df), ") = ", fmt_num(m$chi)), m$p, if (!is.na(m$r2n)) paste0("R\u00B2N = ", fmt_bounded(m$r2n)) else if (!is.na(m$r2mf)) paste0("R\u00B2MF = ", fmt_bounded(m$r2mf)) else "") },
    contTables = { t <- s$tests[[1]]; g(paste0("\u03C7\u00B2(", fmt_df(t$df), ") = ", fmt_num(t$chi)), t$p, if (!is.na(t$cramer)) paste0("V = ", fmt_bounded(t$cramer)) else "") },
    contTablesPaired = g(paste0("\u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi)), s$p, ""),
    propTestN = g(paste0("\u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi)), s$p, ""),
    reliability = g(paste0("\u03B1 = ", fmt_bounded(s$alpha)), NA_real_, if (!is.na(s$omega)) paste0("\u03C9 = ", fmt_bounded(s$omega)) else ""),
    efa = , pca = g(paste0("KMO = ", fmt_bounded(s$kmo)), if (!is.null(s$bartlett)) s$bartlett$p else NA_real_, ""),
    cfa = g(paste0("\u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi)), s$p, paste0("CFI = ", fmt_bounded(s$cfi, 3))),
    g("", NA_real_, "")), error = function(e) g("", NA_real_, ""))
}
