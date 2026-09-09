# ---- APA 7 number formatting helpers ----------------------------------------
# All report numbers pass through these functions so that the template engine,
# the LLM prompt and the fidelity check share exactly the same strings.

#' Round and format a number with fixed decimals (period as decimal separator)
fmt_num <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
  sub("^-", "\u2212", out)
}

#' Format a value bounded in [-1, 1] without leading zero (r, p, eta, alpha)
fmt_bounded <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- suppressWarnings(as.numeric(x))
  s <- formatC(x, format = "f", digits = digits)
  s <- sub("^(-?)0\\.", "\\1.", s)
  s <- sub("^-", "\u2212", s)
  s[is.na(x)] <- NA_character_
  s
}

#' Format p value APA style: "&lt; .001" or "= .024" (HTML-escaped comparison signs)
fmt_p <- function(p, digits = 3) {
  if (is.null(p) || length(p) == 0) return(NA_character_)
  p <- suppressWarnings(as.numeric(p))
  out <- character(length(p))
  for (i in seq_along(p)) {
    if (is.na(p[i])) { out[i] <- NA_character_; next }
    if (p[i] < 0.001) { out[i] <- "&lt; .001"; next }
    s <- formatC(p[i], format = "f", digits = digits)
    if (s == "1.000") s <- "&gt; .999"
    else s <- paste0("= ", sub("^0\\.", ".", s))
    out[i] <- s
  }
  out
}

#' Format degrees of freedom: integer when whole, else 2 decimals (Welch)
fmt_df <- function(df) {
  if (is.null(df) || length(df) == 0) return(NA_character_)
  df <- suppressWarnings(as.numeric(df))
  ifelse(is.na(df), NA_character_,
         ifelse(abs(df - round(df)) < 1e-8, as.character(round(df)),
                formatC(df, format = "f", digits = 2)))
}

#' Format a count
fmt_n <- function(n) {
  if (is.null(n) || length(n) == 0) return(NA_character_)
  n <- suppressWarnings(as.numeric(n))
  ifelse(is.na(n), NA_character_, as.character(round(n)))
}

#' Percentage with one decimal
fmt_pct <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_character_, paste0(formatC(x, format = "f", digits = digits), "%"))
}

#' Effect size formatting by type
fmt_es <- function(value, type = "d", digits = 2) {
  type <- tolower(type)
  bounded <- c("r", "rho", "rrb", "rb", "eta", "etap", "omega", "eps", "epsilon",
               "phi", "v", "cramer", "alpha", "r2", "r2adj", "tau", "cc", "beta")
  if (type %in% bounded) fmt_bounded(value, digits) else fmt_num(value, digits)
}

#' Interpret Cohen's-style thresholds (returns "small"/"medium"/"large"/"negligible")
es_magnitude <- function(value, type = "d") {
  value <- abs(suppressWarnings(as.numeric(value)))
  if (is.na(value)) return(NA_character_)
  type <- tolower(type)
  th <- switch(type,
               d = c(0.2, 0.5, 0.8), g = c(0.2, 0.5, 0.8),
               r = c(0.1, 0.3, 0.5), rho = c(0.1, 0.3, 0.5), rrb = c(0.1, 0.3, 0.5),
               rb = c(0.1, 0.3, 0.5), phi = c(0.1, 0.3, 0.5), v = c(0.1, 0.3, 0.5),
               cramer = c(0.1, 0.3, 0.5),
               eta = c(0.01, 0.06, 0.14), etap = c(0.01, 0.06, 0.14),
               omega = c(0.01, 0.06, 0.14), eps = c(0.01, 0.06, 0.14),
               epsilon = c(0.01, 0.06, 0.14),
               f2 = c(0.02, 0.15, 0.35), r2 = c(0.02, 0.13, 0.26),
               c(0.2, 0.5, 0.8))
  if (value < th[1]) "negligible" else if (value < th[2]) "small" else if (value < th[3]) "medium" else "large"
}

#' Cramér's V thresholds depend on min(df) — simplified per Cohen (1988)
es_magnitude_v <- function(v, min_dim) {
  v <- abs(suppressWarnings(as.numeric(v)))
  if (is.na(v)) return(NA_character_)
  k <- max(1, min_dim - 1)
  th <- c(0.1, 0.3, 0.5) / sqrt(k)
  if (v < th[1]) "negligible" else if (v < th[2]) "small" else if (v < th[3]) "medium" else "large"
}

#' Significance stars
p_stars <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

#' Escape HTML
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Italic statistical symbol in HTML
sym <- function(s) paste0("<i>", s, "</i>")

#' Join a character vector in natural language
join_words <- function(x, lang = "tr") {
  x <- x[!is.na(x) & nzchar(x)]
  n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  conj <- if (lang == "tr") " ve " else " and "
  if (n == 2) return(paste0(x[1], conj, x[2]))
  paste0(paste(x[-n], collapse = ", "), conj, x[n])
}

#' Extract all numeric tokens from a text (for fidelity checking)
num_tokens <- function(text) {
  text <- gsub("<[^>]+>", " ", text)
  text <- gsub("−", "-", text)                # unicode minus
  m <- gregexpr("-?(?:\\d+\\.\\d+|\\.\\d+|\\d+)", text, perl = TRUE)
  toks <- regmatches(text, m)[[1]]
  toks <- gsub("^-", "", toks)
  toks <- sub("^0\\.", ".", toks)
  unique(toks)
}

#' Safe numeric getter from a data frame cell
num_cell <- function(df, col, row = 1) {
  if (is.null(df) || !(col %in% names(df)) || nrow(df) < row) return(NA_real_)
  v <- df[[col]][row]
  if (is.factor(v)) v <- as.character(v)
  v <- suppressWarnings(as.numeric(v))
  if (length(v) == 0) NA_real_ else v
}

#' Safe character getter
chr_cell <- function(df, col, row = 1) {
  if (is.null(df) || !(col %in% names(df)) || nrow(df) < row) return(NA_character_)
  v <- df[[col]][row]
  if (is.null(v)) return(NA_character_)
  as.character(v)
}

#' Convert a jmvcore Table to a data.frame safely
table_df <- function(tbl) {
  if (is.null(tbl)) return(NULL)
  df <- tryCatch(tbl$asDF, error = function(e) NULL)
  if (is.null(df)) return(NULL)
  df
}
