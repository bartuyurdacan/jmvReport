# ---- Read .omv, re-run stored analyses -------------------------------------

read_omv_all <- function(path) {
  x <- jmvReadWrite::read_omv(path, getSyn = TRUE, getHTM = TRUE, sveAtt = TRUE, rmMsVl = FALSE)
  syn <- attr(x, "syntax"); html <- attr(x, "HTML")
  html <- if (is.null(html)) "" else paste(html, collapse = "\n")
  syn <- if (is.null(syn)) character() else unlist(syn)
  syn <- syn[nzchar(trimws(syn))]
  attr(x, "syntax") <- NULL; attr(x, "HTML") <- NULL
  list(data = as.data.frame(x), syntax = syn, html = html)
}

parse_syntax <- function(syntax) {
  expr <- tryCatch(parse(text = syntax, keep.source = FALSE)[[1]], error = function(e) NULL)
  if (is.null(expr)) return(NULL)
  fn <- expr[[1]]
  fname <- if (is.call(fn) && identical(fn[[1]], as.name("::"))) as.character(fn[[3]]) else as.character(fn)
  ns <- if (is.call(fn) && identical(fn[[1]], as.name("::"))) as.character(fn[[2]]) else NA_character_
  args <- as.list(expr)[-1]
  list(fn = fname, ns = ns, args = args, call = expr)
}

#' Re-run a jmv syntax string on a data frame; returns the results object or NULL
run_syntax <- function(syntax, data) {
  ps <- parse_syntax(syntax)
  if (is.null(ps)) return(list(ok = FALSE, error = "parse error", parsed = NULL))
  if (is.na(ps$ns)) return(list(ok = FALSE, error = "not a jamovi analysis (R code)", parsed = ps))
  if (ps$ns != "jmv") return(list(ok = FALSE, error = paste0("module '", ps$ns, "' not supported"), parsed = ps))
  if (!ensure_jmv()) return(list(ok = FALSE, error = "jmv not available", parsed = ps))
  fn <- tryCatch(get(ps$fn, envir = asNamespace("jmv")), error = function(e) NULL)
  if (!is.function(fn)) return(list(ok = FALSE, error = paste0("jmv::", ps$fn, " not found"), parsed = ps))
  # drop arguments the installed jmv version does not know (e.g. duplicate=, *OV=)
  call <- ps$call
  fm <- names(formals(fn))
  if (!("..." %in% fm)) {
    keep <- c(TRUE, vapply(seq_along(call)[-1], function(i) { nm <- names(call)[i]; is.null(nm) || !nzchar(nm) || nm %in% fm }, logical(1)))
    dropped <- names(call)[!keep]
    call <- call[keep]
  } else dropped <- character()
  env <- new.env(parent = globalenv()); env$data <- data
  res <- tryCatch(suppressWarnings(suppressMessages(eval(call, envir = env))), error = function(e) e)
  if (inherits(res, "error")) return(list(ok = FALSE, error = conditionMessage(res), parsed = ps))
  if (!inherits(res, "R6") || is.null(tryCatch(res$options, error = function(e) NULL))) return(list(ok = FALSE, error = "not a jmv results object", parsed = ps))
  list(ok = TRUE, res = res, parsed = ps, dropped = dropped)
}
