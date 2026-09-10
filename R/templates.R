# ---- Template engine (Layer 0) ---------------------------------------------
# Deterministic APA sentences in English. Every number in the
# report originates here (via format_apa.R). The LLM only rephrases.

tx <- function(lang, tr, en) en
ci_lab <- function(lang, width = 95) paste0(width, "% CI")
ci_lab_en_style <- function(lang, width = 95) paste0(width, "% CI")
mag_word <- function(value, type, lang) {
  m <- es_magnitude(value, type); if (is.na(m)) return(NA_character_)
  i18n(lang)$magnitude[[m]]
}
sig <- function(p) !is.na(p) && p < .05
para <- function(...) paste0("<p>", paste0(..., collapse = ""), "</p>")
h <- function(x, level = 3) paste0("<h", level, ">", html_escape(x), "</h", level, ">")
strip_html <- function(x) { x <- gsub("</p>", "\n\n", x); x <- gsub("<br ?/?>", "\n", x); x <- gsub("<[^>]+>", "", x); x <- gsub("&amp;", "&", x); x <- gsub("&lt;", "<", x); x <- gsub("&gt;", ">", x); trimws(x) }
vn <- function(x) html_escape(as.character(x))   # variable name
pct_l <- function(x, lang, digits = 1) fmt_pct(x, digits)

# statistic strings ------------------------------------------------------------
st_t <- function(t, df, p, es = NULL, esType = "d", ci = NULL, lang = "en", width = 95) {
  s <- paste0(sym("t"), "(", fmt_df(df), ") = ", fmt_num(t), ", ", sym("p"), " ", fmt_p(p))
  if (!is.null(es) && !is.na(es)) s <- paste0(s, ", ", es_label(esType), " = ", fmt_es(es, es_key(esType)))
  if (!is.null(ci) && all(!is.na(ci))) s <- paste0(s, ", ", ci_lab_en_style(lang, width), " [", fmt_num(ci[1]), ", ", fmt_num(ci[2]), "]")
  s
}
es_label <- function(esType) {
  et <- tolower(as.character(esType))
  if (grepl("hedges", et)) return(paste0("Hedges' ", sym("g")))
  if (grepl("glass", et)) return(paste0("Glass' ", sym("\u0394")))
  if (grepl("rank", et) || grepl("biserial", et)) return(paste0(sym("r"), "<sub>rb</sub>"))
  paste0("Cohen's ", sym("d"))
}
es_key <- function(esType) { et <- tolower(as.character(esType)); if (grepl("rank|biserial", et)) "rrb" else "d" }
st_F <- function(F, df1, df2, p, es = NULL, esLab = NULL) {
  s <- paste0(sym("F"), "(", fmt_df(df1), ", ", fmt_df(df2), ") = ", fmt_num(F), ", ", sym("p"), " ", fmt_p(p))
  if (!is.null(es) && !is.na(es) && !is.null(esLab)) s <- paste0(s, ", ", esLab, " = ", fmt_bounded(es))
  s
}
st_chi <- function(chi, df, N, p, V = NULL, Vlab = NULL) {
  s <- paste0("\u03C7\u00B2(", fmt_df(df), if (!is.na(N)) paste0(", ", sym("N"), " = ", fmt_n(N)) else "", ") = ", fmt_num(chi), ", ", sym("p"), " ", fmt_p(p))
  if (!is.null(V) && !is.na(V)) s <- paste0(s, ", ", Vlab, " = ", fmt_bounded(V))
  s
}
st_U <- function(U, p, rrb = NULL) { s <- paste0(sym("U"), " = ", fmt_num(U, 1), ", ", sym("p"), " ", fmt_p(p)); if (!is.null(rrb) && !is.na(rrb)) s <- paste0(s, ", ", sym("r"), "<sub>rb</sub> = ", fmt_bounded(rrb)); s }
st_W <- function(W, p, rrb = NULL) { s <- paste0(sym("W"), " = ", fmt_num(W, 1), ", ", sym("p"), " ", fmt_p(p)); if (!is.null(rrb) && !is.na(rrb)) s <- paste0(s, ", ", sym("r"), "<sub>rb</sub> = ", fmt_bounded(rrb)); s }
st_H <- function(H, df, p, es = NULL) { s <- paste0(sym("H"), "(", fmt_df(df), ") = ", fmt_num(H), ", ", sym("p"), " ", fmt_p(p)); if (!is.null(es) && !is.na(es)) s <- paste0(s, ", \u03B5\u00B2 = ", fmt_bounded(es)); s }
st_r <- function(r, df, p, lab = "r", ci = NULL, lang = "en") {
  s <- paste0(sym(lab), if (!is.na(df)) paste0("(", fmt_df(df), ")") else "", " = ", fmt_bounded(r), ", ", sym("p"), " ", fmt_p(p))
  if (!is.null(ci) && all(!is.na(ci))) s <- paste0(s, ", ", ci_lab_en_style(lang), " [", fmt_bounded(ci[1]), ", ", fmt_bounded(ci[2]), "]")
  s
}
msd <- function(m, sd, lang = "en") paste0("(", sym("M"), " = ", fmt_num(m), ", ", sym("SD"), " = ", fmt_num(sd), ")")
mdn_s <- function(mdn) paste0("(", sym("Mdn"), " = ", fmt_num(mdn), ")")
dir_word <- function(higher, lang) if (higher) tx(lang, "daha y\u00FCksek", "higher") else tx(lang, "daha d\u00FC\u015F\u00FCk", "lower")

# Language-aware sentences ----------------------------------------------------
sent_norm <- function(w, p, var, lang) {
  if (is.na(p)) return("")
  ok <- p > .05
  tx(lang,
     paste0(vn(var), " de\u011Fi\u015Fkeni i\u00E7in Shapiro-Wilk testi normallik varsay\u0131m\u0131n\u0131n ", if (ok) "sa\u011Fland\u0131\u011F\u0131n\u0131" else "sa\u011Flanmad\u0131\u011F\u0131n\u0131", " g\u00F6stermi\u015Ftir (", sym("W"), " = ", fmt_bounded(w, 3), ", ", sym("p"), " ", fmt_p(p), "). "),
     paste0("The Shapiro-Wilk test indicated that the normality assumption was ", if (ok) "met" else "violated", " for ", vn(var), " (", sym("W"), " = ", fmt_bounded(w, 3), ", ", sym("p"), " ", fmt_p(p), "). "))
}
sent_levene <- function(F, df1, df2, p, lang) {
  if (is.na(p)) return("")
  ok <- p > .05
  tx(lang,
     paste0("Levene testi varyanslar\u0131n ", if (ok) "homojen oldu\u011Funu" else "homojen olmad\u0131\u011F\u0131n\u0131", " g\u00F6stermi\u015Ftir (", sym("F"), "(", fmt_df(df1), ", ", fmt_df(df2), ") = ", fmt_num(F), ", ", sym("p"), " ", fmt_p(p), "). "),
     paste0("Levene's test indicated that variances were ", if (ok) "homogeneous" else "not homogeneous", " (", sym("F"), "(", fmt_df(df1), ", ", fmt_df(df2), ") = ", fmt_num(F), ", ", sym("p"), " ", fmt_p(p), "). "))
}
sent_mag <- function(es, type, lang) {
  m <- mag_word(es, type, lang); if (is.na(m)) return("")
  tx(lang, paste0("Etki b\u00FCy\u00FCkl\u00FC\u011F\u00FC ", m, " d\u00FCzeydedir. "), paste0("The effect size was ", m, ". "))
}

# ---- dispatcher ------------------------------------------------------------
#' Render the Results text for one summarised analysis
#' @param s A summarised analysis object.
#' @param lang Retained for compatibility; output is always English.
#' @param index Optional analysis number used in the heading.
#' @export
render_results_text <- function(s, lang = "en", index = NULL) {
  lang <- "en"
  L <- i18n(lang)
  fn <- switch(s$type,
    ttestIS = rt_ttestIS, ttestPS = rt_ttestPS, ttestOneS = rt_ttestOneS,
    anovaOneW = rt_anovaOneW, ANOVA = rt_ANOVA, ancova = rt_ANOVA, anovaRM = rt_anovaRM,
    anovaNP = rt_anovaNP, anovaRMNP = rt_anovaRMNP, corrMatrix = rt_corrMatrix,
    linReg = rt_linReg, logRegBin = rt_logReg, logRegOrd = rt_logReg, logRegMulti = rt_logReg,
    contTables = rt_contTables, contTablesPaired = rt_contTablesPaired,
    propTest2 = rt_propTest2, propTestN = rt_propTestN, reliability = rt_reliability,
    efa = rt_efa, pca = rt_efa, cfa = rt_cfa, mancova = rt_mancova, descriptives = rt_descriptives,
    NULL)
  body <- if (isTRUE(s$supported) && !is.null(fn)) tryCatch(fn(s, lang), error = function(e) rt_generic(s, lang, conditionMessage(e))) else rt_generic(s, lang)
  title <- if (!is.null(index)) paste0(index, ". ", s$title) else s$title
  paste0(h(title), body)
}

rt_generic <- function(s, lang, err = NULL) {
  L <- i18n(lang)
  out <- para(tx(lang, paste0("Bu analiz (", vn(s$type), ") i\u00E7in otomatik metin \u015Fablonu hen\u00FCz bulunmamaktad\u0131r; hesaplanan tablolar a\u015Fa\u011F\u0131da listelenmi\u015Ftir."),
                       paste0("No automatic text template exists yet for this analysis (", vn(s$type), "); the computed tables are listed below.")))
  for (nm in names(s$tables)) out <- paste0(out, "<p><b>", html_escape(s$tables[[nm]]$title), "</b></p>", df_to_html(s$tables[[nm]]$df))
  if (!is.null(err)) out <- paste0(out, para("<i>", html_escape(err), "</i>"))
  out
}

df_to_html <- function(df, digits = 3) {
  if (is.null(df) || nrow(df) == 0) return("")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- df[, !grepl("^\\.name\\[|^\\.stat\\[|^sep$", names(df)), drop = FALSE]
  cells <- lapply(df, function(col) { if (is.numeric(col)) ifelse(is.na(col), "", formatC(col, format = "fg", digits = digits)) else ifelse(is.na(col), "", as.character(col)) })
  hdr <- paste0("<tr>", paste0("<th style='text-align:left;border-bottom:1px solid #333;padding:2px 8px'>", html_escape(names(df)), "</th>", collapse = ""), "</tr>")
  rows <- vapply(seq_len(nrow(df)), function(i) paste0("<tr>", paste0("<td style='padding:2px 8px'>", html_escape(vapply(cells, function(c) c[i], character(1))), "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<table style='border-top:1px solid #333;border-bottom:1px solid #333;border-collapse:collapse;margin:6px 0'>", hdr, paste(rows, collapse = ""), "</table>")
}

# ---- t-tests ----------------------------------------------------------------
rt_ttestIS <- function(s, lang) {
  out <- ""
  for (r in s$rows) {
    txt <- ""
    if (s$opts$norm && !is.null(r$norm)) txt <- paste0(txt, sent_norm(r$norm$w, r$norm$p, r$var, lang))
    if (s$opts$eqv && !is.null(r$levene)) txt <- paste0(txt, sent_levene(r$levene$F, r$levene$df1, r$levene$df2, r$levene$p, lang))
    g1 <- r$g1; g2 <- r$g2
    use_welch <- s$opts$welchs && (!s$opts$students || (!is.null(r$levene) && !is.na(r$levene$p) && r$levene$p < .05))
    use_mann <- s$opts$mann && (!s$opts$students && !s$opts$welchs || (!is.null(r$norm) && !is.na(r$norm$p) && r$norm$p < .05))
    if (s$opts$students || s$opts$welchs) {
      te <- if (use_welch && !is.null(r$welch)) r$welch else r$student
      if (is.null(te)) te <- r$welch
      test_name <- if (use_welch) tx(lang, "Welch t-testi", "Welch's t-test") else tx(lang, "ba\u011F\u0131ms\u0131z \u00F6rneklemler t-testi", "independent samples t-test")
      higher <- !is.null(g1) && !is.na(g1$m) && !is.na(g2$m) && g1$m > g2$m
      ci <- if (s$opts$ci && !is.null(te$cil)) c(te$cil, te$ciu) else NULL
      stat <- st_t(te$t, te$df, te$p, if (s$opts$effectSize) te$es else NULL, if (!is.null(r$student)) r$student$esType else "d", ci, lang, s$opts$ciWidth)
      if (!is.null(g1)) {
        if (sig(te$p)) {
          txt <- paste0(txt, tx(lang,
            paste0(vn(r$var), " puanlar\u0131 ", vn(s$group), " de\u011Fi\u015Fkenine g\u00F6re ", test_name, " ile kar\u015F\u0131la\u015Ft\u0131r\u0131lm\u0131\u015Ft\u0131r. ", vn(g1$name), " grubunun ", msd(g1$m, g1$sd), " ", vn(g2$name), " grubuna ", msd(g2$m, g2$sd), " k\u0131yasla istatistiksel olarak anlaml\u0131 d\u00FCzeyde ", dir_word(higher, lang), " puan ald\u0131\u011F\u0131 belirlenmi\u015Ftir, ", stat, ". "),
            paste0(vn(r$var), " scores were compared across ", vn(s$group), " using an ", test_name, ". The ", vn(g1$name), " group ", msd(g1$m, g1$sd), " scored significantly ", dir_word(higher, lang), " than the ", vn(g2$name), " group ", msd(g2$m, g2$sd), ", ", stat, ". ")))
        } else {
          txt <- paste0(txt, tx(lang,
            paste0(vn(r$var), " puanlar\u0131 ", vn(s$group), " de\u011Fi\u015Fkenine g\u00F6re ", test_name, " ile kar\u015F\u0131la\u015Ft\u0131r\u0131lm\u0131\u015Ft\u0131r. ", vn(g1$name), " ", msd(g1$m, g1$sd), " ve ", vn(g2$name), " ", msd(g2$m, g2$sd), " gruplar\u0131 aras\u0131nda istatistiksel olarak anlaml\u0131 bir fark bulunmam\u0131\u015Ft\u0131r, ", stat, ". "),
            paste0(vn(r$var), " scores were compared across ", vn(s$group), " using an ", test_name, ". There was no statistically significant difference between the ", vn(g1$name), " ", msd(g1$m, g1$sd), " and ", vn(g2$name), " ", msd(g2$m, g2$sd), " groups, ", stat, ". ")))
        }
      } else txt <- paste0(txt, tx(lang, paste0(vn(r$var), " i\u00E7in ", test_name, ": ", stat, ". "), paste0(test_name, " for ", vn(r$var), ": ", stat, ". ")))
      if (s$opts$effectSize && !is.na(te$es)) txt <- paste0(txt, sent_mag(te$es, "d", lang))
    }
    if (s$opts$mann && !is.null(r$mann)) {
      m <- r$mann; stat <- st_U(m$U, m$p, if (s$opts$effectSize) m$es else NULL)
      lead <- if (use_mann && !(s$opts$students || s$opts$welchs)) "" else if (use_mann) tx(lang, "Verilerin normal da\u011F\u0131l\u0131m g\u00F6stermemesi nedeniyle ", "Because the data were not normally distributed, ") else tx(lang, "Ayr\u0131ca ", "In addition, ")
      if (!is.null(g1)) {
        higher <- !is.na(g1$mdn) && !is.na(g2$mdn) && g1$mdn > g2$mdn
        txt <- paste0(txt, if (sig(m$p)) tx(lang,
          paste0(lead, "Mann-Whitney ", sym("U"), " testi uygulanm\u0131\u015Ft\u0131r; ", vn(g1$name), " grubunun ", mdn_s(g1$mdn), " ", vn(g2$name), " grubundan ", mdn_s(g2$mdn), " anlaml\u0131 d\u00FCzeyde ", dir_word(higher, lang), " puan ald\u0131\u011F\u0131 g\u00F6r\u00FClm\u00FC\u015Ft\u00FCr, ", stat, ". "),
          paste0(lead, "a Mann-Whitney ", sym("U"), " test was conducted; the ", vn(g1$name), " group ", mdn_s(g1$mdn), " scored significantly ", dir_word(higher, lang), " than the ", vn(g2$name), " group ", mdn_s(g2$mdn), ", ", stat, ". ")) else tx(lang,
          paste0(lead, "Mann-Whitney ", sym("U"), " testi uygulanm\u0131\u015Ft\u0131r; ", vn(g1$name), " ", mdn_s(g1$mdn), " ile ", vn(g2$name), " ", mdn_s(g2$mdn), " gruplar\u0131 aras\u0131nda anlaml\u0131 bir fark bulunmam\u0131\u015Ft\u0131r, ", stat, ". "),
          paste0(lead, "a Mann-Whitney ", sym("U"), " test was conducted; there was no significant difference between the ", vn(g1$name), " ", mdn_s(g1$mdn), " and ", vn(g2$name), " ", mdn_s(g2$mdn), " groups, ", stat, ". ")))
      } else txt <- paste0(txt, tx(lang, paste0(lead, "Mann-Whitney ", sym("U"), " testi: ", stat, ". "), paste0(lead, "Mann-Whitney ", sym("U"), " test: ", stat, ". ")))
      if (s$opts$effectSize && !is.na(m$es)) txt <- paste0(txt, sent_mag(m$es, "rrb", lang))
    }
    out <- paste0(out, para(txt))
  }
  out
}

rt_ttestPS <- function(s, lang) {
  out <- ""
  for (r in s$rows) {
    txt <- ""
    if (s$opts$norm && !is.null(r$norm)) txt <- paste0(txt, sent_norm(r$norm$w, r$norm$p, paste0(r$var1, " \u2212 ", r$var2), lang))
    use_wilc <- s$opts$wilcoxon && (!s$opts$students || (!is.null(r$norm) && !is.na(r$norm$p) && r$norm$p < .05))
    if (s$opts$students && !is.null(r$student)) {
      te <- r$student; ci <- if (s$opts$ci) c(te$cil, te$ciu) else NULL
      stat <- st_t(te$t, te$df, te$p, if (s$opts$effectSize) te$es else NULL, "d", ci, lang, s$opts$ciWidth)
      d1 <- r$d1; d2 <- r$d2; higher <- !is.null(d1) && !is.na(d1$m) && d1$m > d2$m
      txt <- paste0(txt, if (sig(te$p)) tx(lang,
        paste0("Ba\u011F\u0131ml\u0131 \u00F6rneklemler t-testi sonucunda ", vn(r$var1), " ", msd(d1$m, d1$sd), " ile ", vn(r$var2), " ", msd(d2$m, d2$sd), " \u00F6l\u00E7\u00FCmleri aras\u0131nda istatistiksel olarak anlaml\u0131 bir fark bulunmu\u015Ftur; ", vn(r$var1), " puanlar\u0131 anlaml\u0131 d\u00FCzeyde ", dir_word(higher, lang), "tir, ", stat, ". "),
        paste0("A paired samples t-test showed a statistically significant difference between ", vn(r$var1), " ", msd(d1$m, d1$sd), " and ", vn(r$var2), " ", msd(d2$m, d2$sd), "; ", vn(r$var1), " scores were significantly ", dir_word(higher, lang), ", ", stat, ". ")) else tx(lang,
        paste0("Ba\u011F\u0131ml\u0131 \u00F6rneklemler t-testi sonucunda ", vn(r$var1), " ", msd(d1$m, d1$sd), " ile ", vn(r$var2), " ", msd(d2$m, d2$sd), " \u00F6l\u00E7\u00FCmleri aras\u0131nda istatistiksel olarak anlaml\u0131 bir fark bulunmam\u0131\u015Ft\u0131r, ", stat, ". "),
        paste0("A paired samples t-test showed no statistically significant difference between ", vn(r$var1), " ", msd(d1$m, d1$sd), " and ", vn(r$var2), " ", msd(d2$m, d2$sd), ", ", stat, ". ")))
      if (s$opts$effectSize && !is.na(te$es)) txt <- paste0(txt, sent_mag(te$es, "d", lang))
    }
    if (s$opts$wilcoxon && !is.null(r$wilcoxon)) {
      w <- r$wilcoxon; stat <- st_W(w$W, w$p, if (s$opts$effectSize) w$es else NULL)
      lead <- if (use_wilc && s$opts$students) tx(lang, "Fark puanlar\u0131n\u0131n normal da\u011F\u0131lmamas\u0131 nedeniyle ", "Because the difference scores were not normally distributed, ") else if (s$opts$students) tx(lang, "Ayr\u0131ca ", "In addition, ") else ""
      d1 <- r$d1; d2 <- r$d2
      txt <- paste0(txt, tx(lang,
        paste0(lead, "Wilcoxon i\u015Faretli s\u0131ralar testi uygulanm\u0131\u015Ft\u0131r; ", vn(r$var1), " ", mdn_s(d1$mdn), " ile ", vn(r$var2), " ", mdn_s(d2$mdn), " aras\u0131ndaki fark ", if (sig(w$p)) "istatistiksel olarak anlaml\u0131d\u0131r" else "istatistiksel olarak anlaml\u0131 de\u011Fildir", ", ", stat, ". "),
        paste0(lead, "a Wilcoxon signed-rank test was conducted; the difference between ", vn(r$var1), " ", mdn_s(d1$mdn), " and ", vn(r$var2), " ", mdn_s(d2$mdn), " was ", if (sig(w$p)) "statistically significant" else "not statistically significant", ", ", stat, ". ")))
      if (s$opts$effectSize && !is.na(w$es)) txt <- paste0(txt, sent_mag(w$es, "rrb", lang))
    }
    out <- paste0(out, para(txt))
  }
  out
}

rt_ttestOneS <- function(s, lang) {
  out <- ""
  for (r in s$rows) {
    txt <- ""
    if (s$opts$norm && !is.null(r$norm)) txt <- paste0(txt, sent_norm(r$norm$w, r$norm$p, r$var, lang))
    tv <- fmt_num(s$testValue)
    if (s$opts$students && !is.null(r$student)) {
      te <- r$student; stat <- st_t(te$t, te$df, te$p, if (s$opts$effectSize) te$es else NULL, "d", NULL, lang)
      d <- r$d; higher <- !is.null(d) && !is.na(d$m) && d$m > s$testValue
      txt <- paste0(txt, tx(lang,
        paste0("Tek \u00F6rneklem t-testi sonucunda ", vn(r$var), " ortalamas\u0131n\u0131n ", msd(d$m, d$sd), " test de\u011Feri olan ", tv, "'den ", if (sig(te$p)) paste0("istatistiksel olarak anlaml\u0131 d\u00FCzeyde ", dir_word(higher, lang), " oldu\u011Fu") else "anlaml\u0131 d\u00FCzeyde farkl\u0131 olmad\u0131\u011F\u0131", " belirlenmi\u015Ftir, ", stat, ". "),
        paste0("A one-sample t-test showed that the mean of ", vn(r$var), " ", msd(d$m, d$sd), " was ", if (sig(te$p)) paste0("significantly ", dir_word(higher, lang), " than") else "not significantly different from", " the test value of ", tv, ", ", stat, ". ")))
      if (s$opts$effectSize && !is.na(te$es)) txt <- paste0(txt, sent_mag(te$es, "d", lang))
    }
    if (s$opts$wilcoxon && !is.null(r$wilcoxon)) {
      w <- r$wilcoxon; stat <- st_W(w$W, w$p, if (s$opts$effectSize) w$es else NULL)
      txt <- paste0(txt, tx(lang, paste0("Wilcoxon i\u015Faretli s\u0131ralar testi sonucu: ", vn(r$var), " medyan\u0131 ", mdn_s(r$d$mdn), " ile ", tv, " aras\u0131ndaki fark ", if (sig(w$p)) "anlaml\u0131d\u0131r" else "anlaml\u0131 de\u011Fildir", ", ", stat, ". "),
                                  paste0("Wilcoxon signed-rank test: the difference between the median of ", vn(r$var), " ", mdn_s(r$d$mdn), " and ", tv, " was ", if (sig(w$p)) "significant" else "not significant", ", ", stat, ". ")))
    }
    out <- paste0(out, para(txt))
  }
  out
}

# ---- one-way ANOVA ----------------------------------------------------------
ph_method_name <- function(m, lang) switch(tolower(m), tukey = "Tukey HSD", gameshowell = "Games-Howell", bonf = "Bonferroni", bonferroni = "Bonferroni", holm = "Holm", scheffe = "Scheff\u00E9", none = tx(lang, "d\u00FCzeltmesiz", "uncorrected"), sidak = "\u0160id\u00E1k", m)

sent_pairs <- function(pairs, method, lang, only_sig = FALSE, pkey = NULL) {
  if (length(pairs) == 0) return("")
  items <- character()
  for (p in pairs) {
    pv <- if (is.list(p$p)) { if (!is.null(pkey) && pkey %in% names(p$p)) p$p[[pkey]] else p$p[[1]] } else p$p
    if (only_sig && !sig(pv)) next
    items <- c(items, paste0(vn(p$a), " \u2013 ", vn(p$b), " (", if (!is.na(p$md)) paste0(sym("M"), tx(lang, " fark", " diff"), " = ", fmt_num(p$md), ", ") else "", sym("p"), " ", fmt_p(pv), ")"))
  }
  if (length(items) == 0) return(tx(lang, paste0(method, " ile yap\u0131lan ikili kar\u015F\u0131la\u015Ft\u0131rmalarda anlaml\u0131 fark bulunan grup \u00E7ifti yoktur. "), paste0("Pairwise comparisons with ", method, " revealed no significant differences between any pair of groups. ")))
  tx(lang, paste0(method, " ile yap\u0131lan ikili kar\u015F\u0131la\u015Ft\u0131rmalarda anlaml\u0131 fark: ", join_words(items, lang), ". "),
     paste0("Pairwise comparisons with ", method, " showed significant differences for: ", join_words(items, lang), ". "))
}

rt_anovaOneW <- function(s, lang) {
  out <- ""
  for (r in s$rows) {
    txt <- ""
    if (s$opts$norm && !is.null(r$norm)) txt <- paste0(txt, sent_norm(r$norm$w, r$norm$p, r$dep, lang))
    if (s$opts$eqv && !is.null(r$levene)) txt <- paste0(txt, sent_levene(r$levene$F, r$levene$df1, r$levene$df2, r$levene$p, lang))
    use_welch <- s$opts$welchs && (!s$opts$fishers || (!is.null(r$levene) && !is.na(r$levene$p) && r$levene$p < .05))
    te <- if (use_welch && !is.null(r$welch)) r$welch else if (!is.null(r$fisher)) r$fisher else r$welch
    name <- if (use_welch) tx(lang, "Welch tek y\u00F6nl\u00FC varyans analizi", "Welch's one-way ANOVA") else tx(lang, "tek y\u00F6nl\u00FC varyans analizi (ANOVA)", "one-way ANOVA")
    desc <- if (length(r$groups)) join_words(vapply(r$groups, function(g) paste0(vn(g$name), " ", msd(g$m, g$sd)), character(1)), lang) else ""
    stat <- st_F(te$F, te$df1, te$df2, te$p)
    txt <- paste0(txt, tx(lang,
      paste0(vn(r$dep), " puanlar\u0131n\u0131n ", vn(s$group), " gruplar\u0131na g\u00F6re farkl\u0131la\u015F\u0131p farkl\u0131la\u015Fmad\u0131\u011F\u0131 ", name, " ile incelenmi\u015Ftir", if (nzchar(desc)) paste0(" [", desc, "]") else "", ". Gruplar aras\u0131nda ", if (sig(te$p)) "istatistiksel olarak anlaml\u0131 bir fark bulunmu\u015Ftur" else "istatistiksel olarak anlaml\u0131 bir fark bulunmam\u0131\u015Ft\u0131r", ", ", stat, ". "),
      paste0("A ", name, " was conducted to examine whether ", vn(r$dep), " differed across ", vn(s$group), " groups", if (nzchar(desc)) paste0(" [", desc, "]") else "", ". The difference between groups was ", if (sig(te$p)) "statistically significant" else "not statistically significant", ", ", stat, ". ")))
    if (!is.null(r$posthoc) && length(r$posthoc) && s$opts$phMethod != "none") txt <- paste0(txt, sent_pairs(r$posthoc, ph_method_name(s$opts$phMethod, lang), lang, only_sig = TRUE))
    out <- paste0(out, para(txt))
  }
  out
}

# ---- factorial ANOVA / ANCOVA ---------------------------------------------
rt_ANOVA <- function(s, lang) {
  txt <- ""
  if (!is.null(s$norm)) txt <- paste0(txt, sent_norm(s$norm$w, s$norm$p, tx(lang, "art\u0131klar", "residuals"), lang))
  if (!is.null(s$homo)) txt <- paste0(txt, sent_levene(s$homo$F, s$homo$df1, s$homo$df2, s$homo$p, lang))
  is_ancova <- s$type == "ancova"
  design <- paste(vapply(s$factors, function(f) vn(f), character(1)), collapse = " \u00D7 ")
  name <- if (is_ancova) tx(lang, "kovaryans analizi (ANCOVA)", "analysis of covariance (ANCOVA)") else if (length(s$factors) > 1) tx(lang, paste0(length(s$factors), " fakt\u00F6rl\u00FC varyans analizi (ANOVA)"), paste0(length(s$factors), "-way ANOVA")) else tx(lang, "tek y\u00F6nl\u00FC varyans analizi (ANOVA)", "one-way ANOVA")
  es_key_ <- if ("partEta" %in% s$opts$effectSize) "etaSqP" else if ("eta" %in% s$opts$effectSize) "etaSq" else if ("omega" %in% s$opts$effectSize) "omegaSq" else NULL
  es_lab <- switch(if (is.null(es_key_)) "none" else es_key_, etaSqP = "\u03B7\u00B2<sub>p</sub>", etaSq = "\u03B7\u00B2", omegaSq = "\u03C9\u00B2", NULL)
  intro <- tx(lang,
    paste0(vn(s$dep), " \u00FCzerinde ", design, if (is_ancova && length(s$covs)) paste0(" (kovaryat: ", join_words(vapply(s$covs, vn, character(1)), lang), ")") else "", " etkisi ", name, " ile incelenmi\u015Ftir. "),
    paste0("A ", name, " was conducted to examine the effect of ", design, if (is_ancova && length(s$covs)) paste0(" (covariate: ", join_words(vapply(s$covs, vn, character(1)), lang), ")") else "", " on ", vn(s$dep), ". "))
  txt <- paste0(txt, intro)
  for (t in s$terms) {
    es <- if (!is.null(es_key_)) t[[es_key_]] else NULL
    stat <- st_F(t$F, t$df, s$resid_df, t$p, es, es_lab)
    is_int <- grepl("[:\u273B*]", t$name)
    lab <- if (is_int) tx(lang, paste0(vn(gsub("[:\u273B*]", " \u00D7 ", t$name)), " etkile\u015Fimi"), paste0("The ", vn(gsub("[:\u273B*]", " \u00D7 ", t$name)), " interaction")) else tx(lang, paste0(vn(t$name), " temel etkisi"), paste0("The main effect of ", vn(t$name)))
    if (is_ancova && t$name %in% s$covs) lab <- tx(lang, paste0(vn(t$name), " kovaryat\u0131n\u0131n etkisi"), paste0("The effect of the covariate ", vn(t$name)))
    txt <- paste0(txt, tx(lang, paste0(lab, " ", if (sig(t$p)) "istatistiksel olarak anlaml\u0131d\u0131r" else "istatistiksel olarak anlaml\u0131 de\u011Fildir", ", ", stat, ". "),
                                  paste0(lab, " was ", if (sig(t$p)) "statistically significant" else "not statistically significant", ", ", stat, ". ")))
    if (!is.null(es) && !is.na(es) && sig(t$p)) txt <- paste0(txt, sent_mag(es, "etap", lang))
  }
  for (key in names(s$posthoc)) {
    corr <- if (length(s$opts$postHocCorr)) s$opts$postHocCorr[1] else "tukey"
    pkey <- paste0("p", switch(corr, tukey = "tukey", bonf = "bonferroni", holm = "holm", scheffe = "scheffe", sidak = "sidak", none = "none", corr))
    txt <- paste0(txt, tx(lang, paste0(vn(key), " i\u00E7in "), paste0("For ", vn(key), ", ")), sent_pairs(s$posthoc[[key]], ph_method_name(corr, lang), lang, only_sig = TRUE, pkey = pkey))
  }
  out <- para(txt)
  for (e in s$emm) out <- paste0(out, "<p><b>", html_escape(e$title), "</b></p>", df_to_html(e$df))
  out
}

# ---- repeated measures -----------------------------------------------------
rt_anovaRM <- function(s, lang) {
  txt <- ""
  for (sp in s$spher) if (!is.na(sp$p)) txt <- paste0(txt, tx(lang,
    paste0("Mauchly testi ", vn(sp$name), " i\u00E7in k\u00FCresellik varsay\u0131m\u0131n\u0131n ", if (sp$p > .05) "sa\u011Fland\u0131\u011F\u0131n\u0131" else "sa\u011Flanmad\u0131\u011F\u0131n\u0131", " g\u00F6stermi\u015Ftir (", sym("W"), " = ", fmt_bounded(sp$W, 3), ", ", sym("p"), " ", fmt_p(sp$p), if (sp$p <= .05 && !is.na(sp$gg)) paste0("; Greenhouse-Geisser \u03B5 = ", fmt_bounded(sp$gg, 3)) else "", "). "),
    paste0("Mauchly's test indicated that the assumption of sphericity was ", if (sp$p > .05) "met" else "violated", " for ", vn(sp$name), " (", sym("W"), " = ", fmt_bounded(sp$W, 3), ", ", sym("p"), " ", fmt_p(sp$p), if (sp$p <= .05 && !is.na(sp$gg)) paste0("; Greenhouse-Geisser \u03B5 = ", fmt_bounded(sp$gg, 3)) else "", "). ")))
  corr_sel <- s$opts$spherCorr
  # choose the correction to report: GG if selected and any sphericity violated, else none
  viol <- any(vapply(s$spher, function(sp) !is.na(sp$p) && sp$p <= .05, logical(1)))
  lay <- if (viol && "GG" %in% corr_sel) "GG" else if (viol && "HF" %in% corr_sel) "HF" else if ("none" %in% corr_sel) "none" else corr_sel[1]
  corr_txt <- switch(lay, GG = " (Greenhouse-Geisser)", HF = " (Huynh-Feldt)", "")
  txt <- paste0(txt, tx(lang, paste0("Tekrarl\u0131 \u00F6l\u00E7\u00FCmler varyans analizi", if (length(s$bs)) paste0(" (gruplar aras\u0131 fakt\u00F6r: ", join_words(vapply(s$bs, vn, character(1)), lang), ")") else "", " uygulanm\u0131\u015Ft\u0131r. "),
                        paste0("A repeated measures ANOVA", if (length(s$bs)) paste0(" with ", join_words(vapply(s$bs, vn, character(1)), lang), " as between-subjects factor") else "", " was conducted. ")))
  es_lab <- if ("partEta" %in% s$opts$effectSize) "\u03B7\u00B2<sub>p</sub>" else NULL
  for (w in s$within) { if (w$correction != lay) next
    stat <- st_F(w$F, w$df, s$resid_df[[lay]], w$p, if (!is.null(es_lab)) w$partEta else NULL, es_lab)
    txt <- paste0(txt, tx(lang, paste0(vn(w$name), " denek-i\u00E7i etkisi", corr_txt, " ", if (sig(w$p)) "anlaml\u0131d\u0131r" else "anlaml\u0131 de\u011Fildir", ", ", stat, ". "),
                                paste0("The within-subjects effect of ", vn(w$name), corr_txt, " was ", if (sig(w$p)) "significant" else "not significant", ", ", stat, ". "))) }
  for (b in s$between) { stat <- st_F(b$F, b$df, s$bs_resid_df, b$p, if (!is.null(es_lab)) b$partEta else NULL, es_lab)
    txt <- paste0(txt, tx(lang, paste0(vn(b$name), " gruplar-aras\u0131 etkisi ", if (sig(b$p)) "anlaml\u0131d\u0131r" else "anlaml\u0131 de\u011Fildir", ", ", stat, ". "), paste0("The between-subjects effect of ", vn(b$name), " was ", if (sig(b$p)) "significant" else "not significant", ", ", stat, ". "))) }
  for (key in names(s$posthoc)) { corr <- if (length(s$opts$postHocCorr)) s$opts$postHocCorr[1] else "tukey"; pkey <- paste0("p", switch(corr, tukey = "tukey", bonf = "bonferroni", holm = "holm", scheffe = "scheffe", none = "none", corr))
    txt <- paste0(txt, tx(lang, paste0(vn(key), " i\u00E7in "), paste0("For ", vn(key), ", ")), sent_pairs(s$posthoc[[key]], ph_method_name(corr, lang), lang, only_sig = TRUE, pkey = pkey)) }
  para(txt)
}

rt_anovaNP <- function(s, lang) {
  out <- ""
  for (r in s$rows) {
    stat <- st_H(r$H, r$df, r$p, if (s$opts$es) r$es else NULL)
    txt <- tx(lang, paste0("Kruskal-Wallis ", sym("H"), " testi, ", vn(r$dep), " puanlar\u0131n\u0131n ", vn(s$group), " gruplar\u0131 aras\u0131nda ", if (sig(r$p)) "anlaml\u0131 d\u00FCzeyde farkl\u0131la\u015Ft\u0131\u011F\u0131n\u0131" else "anlaml\u0131 d\u00FCzeyde farkl\u0131la\u015Fmad\u0131\u011F\u0131n\u0131", " g\u00F6stermi\u015Ftir, ", stat, ". "),
                    paste0("A Kruskal-Wallis ", sym("H"), " test showed that ", vn(r$dep), " ", if (sig(r$p)) "differed significantly" else "did not differ significantly", " across ", vn(s$group), " groups, ", stat, ". "))
    if (s$opts$es && !is.na(r$es)) txt <- paste0(txt, sent_mag(r$es, "eps", lang))
    if (!is.null(r$dunn) && length(r$dunn)) { pr <- lapply(r$dunn, function(d) list(a = d$a, b = d$b, md = NA_real_, p = d$padj)); txt <- paste0(txt, sent_pairs(pr, tx(lang, "Dunn testi (Bonferroni d\u00FCzeltmeli)", "Dunn's test (Bonferroni-adjusted)"), lang, only_sig = TRUE)) }
    else if (!is.null(r$dscf) && length(r$dscf)) { pr <- lapply(r$dscf, function(d) list(a = d$a, b = d$b, md = NA_real_, p = d$p)); txt <- paste0(txt, sent_pairs(pr, "Dwass-Steel-Critchlow-Fligner", lang, only_sig = TRUE)) }
    out <- paste0(out, para(txt))
  }
  out
}

rt_anovaRMNP <- function(s, lang) {
  desc <- if (length(s$desc)) join_words(vapply(s$desc, function(d) paste0(vn(d$name), " ", mdn_s(d$mdn)), character(1)), lang) else ""
  stat <- paste0("\u03C7\u00B2<sub>F</sub>(", fmt_df(s$df), ") = ", fmt_num(s$chi), ", ", sym("p"), " ", fmt_p(s$p))
  txt <- tx(lang, paste0("Friedman testi, tekrarl\u0131 \u00F6l\u00E7\u00FCmler aras\u0131nda ", if (sig(s$p)) "anlaml\u0131 bir fark oldu\u011Funu" else "anlaml\u0131 bir fark olmad\u0131\u011F\u0131n\u0131", " g\u00F6stermi\u015Ftir", if (nzchar(desc)) paste0(" [", desc, "]") else "", ", ", stat, ". "),
                  paste0("A Friedman test showed ", if (sig(s$p)) "a significant difference" else "no significant difference", " across the repeated measurements", if (nzchar(desc)) paste0(" [", desc, "]") else "", ", ", stat, ". "))
  if (length(s$pairs)) txt <- paste0(txt, sent_pairs(lapply(s$pairs, function(p) list(a = p$a, b = p$b, md = NA_real_, p = p$p)), "Durbin-Conover", lang, only_sig = TRUE))
  para(txt)
}

# ---- correlation -----------------------------------------------------------
rt_corrMatrix <- function(s, lang) {
  txt <- tx(lang, paste0(join_words(vapply(s$vars, vn, character(1)), lang), " de\u011Fi\u015Fkenleri aras\u0131ndaki ili\u015Fkiler ", join_words(c(if (s$opts$pearson) "Pearson", if (s$opts$spearman) "Spearman", if (s$opts$kendall) "Kendall tau-b"), lang), " korelasyon analizi ile incelenmi\u015Ftir. "),
                  paste0("Associations among ", join_words(vapply(s$vars, vn, character(1)), lang), " were examined with ", join_words(c(if (s$opts$pearson) "Pearson", if (s$opts$spearman) "Spearman", if (s$opts$kendall) "Kendall's tau-b"), lang), " correlation analysis. "))
  items <- character()
  for (p in s$pairs) {
    use <- if (s$opts$pearson && !is.na(p$r)) list(v = p$r, df = p$rdf, p = p$rp, lab = "r", ci = if (s$opts$ci) c(p$rcil, p$rciu) else NULL, key = "r") else if (s$opts$spearman && !is.na(p$rho)) list(v = p$rho, df = p$rhodf, p = p$rhop, lab = "r<sub>s</sub>", ci = NULL, key = "r") else list(v = p$tau, df = NA, p = p$taup, lab = "\u03C4<sub>b</sub>", ci = NULL, key = "tau")
    if (is.na(use$v)) next
    dirw <- if (use$v > 0) tx(lang, "pozitif", "positive") else tx(lang, "negatif", "negative")
    m <- mag_word(use$v, "r", lang)
    items <- c(items, tx(lang, paste0(vn(p$a), " ile ", vn(p$b), " aras\u0131nda ", dirw, " y\u00F6nde, ", m, " d\u00FCzeyde ve ", if (sig(use$p)) "istatistiksel olarak anlaml\u0131" else "istatistiksel olarak anlaml\u0131 olmayan", " bir ili\u015Fki bulunmu\u015Ftur, ", st_r(use$v, use$df, use$p, use$lab, use$ci, lang), ". "),
                            paste0("There was a ", dirw, ", ", m, ", ", if (sig(use$p)) "statistically significant" else "non-significant", " association between ", vn(p$a), " and ", vn(p$b), ", ", st_r(use$v, use$df, use$p, use$lab, use$ci, lang), ". ")))
  }
  para(paste0(txt, paste(items, collapse = "")))
}

# ---- regression ------------------------------------------------------------
rt_linReg <- function(s, lang) {
  out <- ""
  preds <- join_words(vapply(c(s$covs, s$factors), vn, character(1)), lang)
  intro <- tx(lang, paste0(vn(s$dep), " de\u011Fi\u015Fkenini yordamada ", preds, " de\u011Fi\u015Fkenlerinin rol\u00FC ", if (s$opts$nBlocks > 1) "hiyerar\u015Fik " else "", "\u00E7oklu do\u011Frusal regresyon analizi ile incelenmi\u015Ftir. "),
                    paste0("A ", if (s$opts$nBlocks > 1) "hierarchical " else "", "multiple linear regression was conducted to examine whether ", preds, " predicted ", vn(s$dep), ". "))
  txt <- intro
  for (m in s$models) {
    mlab <- if (s$opts$nBlocks > 1) tx(lang, paste0("Model ", m$index), paste0("Model ", m$index)) else tx(lang, "Model", "The model")
    fitpart <- if (!is.na(m$F)) paste0(", ", st_F(m$F, m$df1, m$df2, m$p)) else ""
    txt <- paste0(txt, tx(lang, paste0(mlab, " ", if (!is.na(m$p)) (if (sig(m$p)) "istatistiksel olarak anlaml\u0131d\u0131r" else "istatistiksel olarak anlaml\u0131 de\u011Fildir") else "kurulmu\u015Ftur", fitpart, "; ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2), if (!is.na(m$r2Adj)) paste0(", d\u00FCzeltilmi\u015F ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2Adj)) else "", ". "),
                                  paste0(mlab, " was ", if (!is.na(m$p)) (if (sig(m$p)) "statistically significant" else "not statistically significant") else "estimated", fitpart, "; ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2), if (!is.na(m$r2Adj)) paste0(", adjusted ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2Adj)) else "", ". ")))
    if (!is.na(m$vif_max) && is.finite(m$vif_max)) txt <- paste0(txt, tx(lang, paste0("En y\u00FCksek VIF de\u011Feri ", fmt_num(m$vif_max), " olup \u00E7oklu ba\u011Flant\u0131 sorunu ", if (m$vif_max < 5) "g\u00F6zlenmemi\u015Ftir" else "olabilece\u011Fine i\u015Faret etmektedir", ". "), paste0("The maximum VIF was ", fmt_num(m$vif_max), ", indicating ", if (m$vif_max < 5) "no multicollinearity problem" else "possible multicollinearity", ". ")))
    if (!is.null(m$dw) && !is.na(m$dw$dw)) txt <- paste0(txt, tx(lang, paste0("Durbin-Watson istatisti\u011Fi ", fmt_num(m$dw$dw), " (", sym("p"), " ", fmt_p(m$dw$p), ") bulunmu\u015Ftur. "), paste0("The Durbin-Watson statistic was ", fmt_num(m$dw$dw), " (", sym("p"), " ", fmt_p(m$dw$p), "). ")))
    sig_terms <- character(); ns_terms <- character()
    for (t in m$terms) { if (grepl("Intercept|Sabit", t$term)) next
      piece <- paste0(vn(t$term), " (", sym("B"), " = ", fmt_num(t$est), ", ", sym("SE"), " = ", fmt_num(t$se), if (!is.na(t$stdEst)) paste0(", \u03B2 = ", fmt_bounded(t$stdEst)) else "", if (s$opts$ci && !is.na(t$lower)) paste0(", ", ci_lab_en_style(lang), " [", fmt_num(t$lower), ", ", fmt_num(t$upper), "]") else "", ", ", sym("t"), " = ", fmt_num(t$t), ", ", sym("p"), " ", fmt_p(t$p), ")")
      if (sig(t$p)) sig_terms <- c(sig_terms, piece) else ns_terms <- c(ns_terms, piece) }
    if (length(sig_terms)) txt <- paste0(txt, tx(lang, paste0("Anlaml\u0131 yorday\u0131c\u0131lar: ", join_words(sig_terms, lang), ". "), paste0("Significant predictors were ", join_words(sig_terms, lang), ". ")))
    if (length(ns_terms)) txt <- paste0(txt, tx(lang, paste0("Anlaml\u0131 olmayan yorday\u0131c\u0131lar: ", join_words(ns_terms, lang), ". "), paste0("Non-significant predictors were ", join_words(ns_terms, lang), ". ")))
  }
  for (cmp in s$comps) txt <- paste0(txt, tx(lang, paste0(vn(cmp$m1), " ile ", vn(cmp$m2), " kar\u015F\u0131la\u015Ft\u0131rmas\u0131: \u0394", sym("R"), "\u00B2 = ", fmt_bounded(cmp$dr2), ", ", st_F(cmp$F, cmp$df1, cmp$df2, cmp$p), ". "),
                                             paste0("Comparing ", vn(cmp$m1), " with ", vn(cmp$m2), ": \u0394", sym("R"), "\u00B2 = ", fmt_bounded(cmp$dr2), ", ", st_F(cmp$F, cmp$df1, cmp$df2, cmp$p), ". ")))
  para(txt)
}

rt_logReg <- function(s, lang) {
  preds <- join_words(vapply(c(s$covs, s$factors), vn, character(1)), lang)
  kind <- switch(s$type, logRegBin = tx(lang, "ikili lojistik regresyon", "binomial logistic regression"), logRegOrd = tx(lang, "ordinal lojistik regresyon", "ordinal logistic regression"), tx(lang, "multinomial lojistik regresyon", "multinomial logistic regression"))
  txt <- tx(lang, paste0(vn(s$dep), " de\u011Fi\u015Fkenini yordamada ", preds, " de\u011Fi\u015Fkenlerinin rol\u00FC ", kind, " analizi ile incelenmi\u015Ftir. "), paste0("A ", kind, " was conducted to examine whether ", preds, " predicted ", vn(s$dep), ". "))
  for (m in s$models) {
    mlab <- if (s$opts$nBlocks > 1) paste0("Model ", m$index) else tx(lang, "Model", "The model")
    r2 <- c(if (!is.na(m$r2mf)) paste0("McFadden ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2mf)), if (!is.na(m$r2cs)) paste0("Cox-Snell ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2cs)), if (!is.na(m$r2n)) paste0("Nagelkerke ", sym("R"), "\u00B2 = ", fmt_bounded(m$r2n)))
    txt <- paste0(txt, tx(lang, paste0(mlab, " ", if (!is.na(m$p)) (if (sig(m$p)) "istatistiksel olarak anlaml\u0131d\u0131r" else "istatistiksel olarak anlaml\u0131 de\u011Fildir") else "kurulmu\u015Ftur", if (!is.na(m$chi)) paste0(", \u03C7\u00B2(", fmt_df(m$df), ") = ", fmt_num(m$chi), ", ", sym("p"), " ", fmt_p(m$p)) else "", if (length(r2)) paste0("; ", paste(r2, collapse = ", ")) else "", if (!is.na(m$accuracy)) paste0("; do\u011Fru s\u0131n\u0131fland\u0131rma oran\u0131 ", pct_l(100 * m$accuracy, lang)) else "", ". "),
                                  paste0(mlab, " was ", if (!is.na(m$p)) (if (sig(m$p)) "statistically significant" else "not statistically significant") else "estimated", if (!is.na(m$chi)) paste0(", \u03C7\u00B2(", fmt_df(m$df), ") = ", fmt_num(m$chi), ", ", sym("p"), " ", fmt_p(m$p)) else "", if (length(r2)) paste0("; ", paste(r2, collapse = ", ")) else "", if (!is.na(m$accuracy)) paste0("; classification accuracy ", pct_l(100 * m$accuracy, lang)) else "", ". ")))
    sig_terms <- character(); ns_terms <- character()
    for (t in m$terms) { if (grepl("Intercept|Sabit", t$term)) next
      piece <- paste0(if (!is.na(t$dep) && !is.null(t$dep)) paste0("[", vn(t$dep), "] ") else "", vn(t$term), " (", sym("B"), " = ", fmt_num(t$est), ", ", sym("SE"), " = ", fmt_num(t$se), if (!is.na(t$odds)) paste0(", OR = ", fmt_num(t$odds)) else "", if (!is.na(t$oddsLower)) paste0(", ", ci_lab_en_style(lang), " [", fmt_num(t$oddsLower), ", ", fmt_num(t$oddsUpper), "]") else "", ", ", sym("z"), " = ", fmt_num(t$z), ", ", sym("p"), " ", fmt_p(t$p), ")")
      if (sig(t$p)) sig_terms <- c(sig_terms, piece) else ns_terms <- c(ns_terms, piece) }
    if (length(sig_terms)) txt <- paste0(txt, tx(lang, paste0("Anlaml\u0131 yorday\u0131c\u0131lar: ", join_words(sig_terms, lang), ". "), paste0("Significant predictors were ", join_words(sig_terms, lang), ". ")))
    if (length(ns_terms)) txt <- paste0(txt, tx(lang, paste0("Anlaml\u0131 olmayan yorday\u0131c\u0131lar: ", join_words(ns_terms, lang), ". "), paste0("Non-significant predictors were ", join_words(ns_terms, lang), ". ")))
  }
  para(txt)
}

# ---- contingency -----------------------------------------------------------
rt_contTables <- function(s, lang) {
  out <- ""
  for (t in s$tests) {
    small <- !is.na(t$N) && !is.na(t$df) && t$df == 1 && !is.na(t$fisher) && !is.na(t$fisherp)
    Vlab <- if (!is.na(t$df) && t$df == 1 && !is.na(t$phi)) "\u03C6" else paste0("Cram\u00E9r's ", sym("V"))
    V <- if (!is.na(t$df) && t$df == 1 && !is.na(t$phi)) t$phi else t$cramer
    stat <- st_chi(t$chi, t$df, t$N, t$p, V, Vlab)
    lay <- if (!is.na(t$layer)) tx(lang, paste0(" (", vn(t$layer), " katman\u0131)"), paste0(" (layer ", vn(t$layer), ")")) else ""
    txt <- tx(lang, paste0(vn(s$rows), " ile ", vn(s$cols), " aras\u0131ndaki ili\u015Fki", lay, " ki-kare ba\u011F\u0131ms\u0131zl\u0131k testi ile incelenmi\u015Ftir. De\u011Fi\u015Fkenler aras\u0131nda istatistiksel olarak ", if (sig(t$p)) "anlaml\u0131 bir ili\u015Fki saptanm\u0131\u015Ft\u0131r" else "anlaml\u0131 bir ili\u015Fki saptanmam\u0131\u015Ft\u0131r", ", ", stat, ". "),
                    paste0("The association between ", vn(s$rows), " and ", vn(s$cols), lay, " was examined with a chi-square test of independence. The association was ", if (sig(t$p)) "statistically significant" else "not statistically significant", ", ", stat, ". "))
    if (s$opts$fisher && !is.na(t$fisherp)) txt <- paste0(txt, tx(lang, paste0("Fisher kesin testi de ayn\u0131 sonucu vermi\u015Ftir (", sym("p"), " ", fmt_p(t$fisherp), "). "), paste0("Fisher's exact test gave a consistent result (", sym("p"), " ", fmt_p(t$fisherp), "). ")))
    if (s$opts$chiSqCorr && !is.na(t$pCorr)) txt <- paste0(txt, tx(lang, paste0("S\u00FCreklilik d\u00FCzeltmeli ki-kare: \u03C7\u00B2 = ", fmt_num(t$chiCorr), ", ", sym("p"), " ", fmt_p(t$pCorr), ". "), paste0("Continuity-corrected chi-square: \u03C7\u00B2 = ", fmt_num(t$chiCorr), ", ", sym("p"), " ", fmt_p(t$pCorr), ". ")))
    if (!is.null(V) && !is.na(V)) { m <- if (!is.na(t$df) && t$df == 1) mag_word(V, "phi", lang) else { md <- suppressWarnings(min(s$dims, na.rm = TRUE)); if (is.finite(md)) i18n(lang)$magnitude[[es_magnitude_v(V, md)]] else mag_word(V, "v", lang) }
      if (!is.na(m)) txt <- paste0(txt, tx(lang, paste0("\u0130li\u015Fkinin g\u00FCc\u00FC ", m, " d\u00FCzeydedir. "), paste0("The strength of the association was ", m, ". "))) }
    out <- paste0(out, para(txt))
  }
  out
}

rt_contTablesPaired <- function(s, lang) {
  stat <- st_chi(s$chi, s$df, s$N, s$p)
  txt <- tx(lang, paste0(vn(s$rows), " ile ", vn(s$cols), " aras\u0131ndaki de\u011Fi\u015Fim McNemar testi ile incelenmi\u015Ftir; de\u011Fi\u015Fim ", if (sig(s$p)) "istatistiksel olarak anlaml\u0131d\u0131r" else "istatistiksel olarak anlaml\u0131 de\u011Fildir", ", ", stat, if (!is.na(s$exactp)) paste0(" (kesin ", sym("p"), " ", fmt_p(s$exactp), ")") else "", ". "),
                  paste0("The change between ", vn(s$rows), " and ", vn(s$cols), " was examined with McNemar's test; the change was ", if (sig(s$p)) "statistically significant" else "not statistically significant", ", ", stat, if (!is.na(s$exactp)) paste0(" (exact ", sym("p"), " ", fmt_p(s$exactp), ")") else "", ". "))
  para(txt)
}

rt_propTest2 <- function(s, lang) {
  items <- vapply(s$rows, function(r) paste0(vn(r$level), ": ", fmt_n(r$count), "/", fmt_n(r$total), " (", pct_l(100 * r$prop, lang), if (s$opts$ci && !is.na(r$cil)) paste0(", ", ci_lab_en_style(lang, s$opts$ciWidth), " [", fmt_bounded(r$cil), ", ", fmt_bounded(r$ciu), "]") else "", "), ", sym("p"), " ", fmt_p(r$p)), character(1))
  para(tx(lang, paste0(vn(s$var), " de\u011Fi\u015Fkeninin oranlar\u0131 binom testi ile ", fmt_num(s$testValue), " test de\u011Ferine kar\u015F\u0131 s\u0131nanm\u0131\u015Ft\u0131r: ", join_words(items, lang), ". "), paste0("Proportions of ", vn(s$var), " were tested against ", fmt_num(s$testValue), " with a binomial test: ", join_words(items, lang), ". ")))
}

rt_propTestN <- function(s, lang) {
  items <- vapply(s$levels, function(r) paste0(vn(r$level), " ", fmt_n(r$count), " (", pct_l(100 * r$prop, lang), ")"), character(1))
  para(tx(lang, paste0(vn(s$var), " kategorilerinin da\u011F\u0131l\u0131m\u0131 [", join_words(items, lang), "] ki-kare uyum iyili\u011Fi testi ile s\u0131nanm\u0131\u015Ft\u0131r; kategoriler aras\u0131nda ", if (sig(s$p)) "anlaml\u0131 bir fark vard\u0131r" else "anlaml\u0131 bir fark yoktur", ", \u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi), ", ", sym("p"), " ", fmt_p(s$p), ". "),
                paste0("The distribution of ", vn(s$var), " categories [", join_words(items, lang), "] was tested with a chi-square goodness-of-fit test; the categories ", if (sig(s$p)) "differed significantly" else "did not differ significantly", ", \u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi), ", ", sym("p"), " ", fmt_p(s$p), ". ")))
}

# ---- reliability / FA ------------------------------------------------------
rt_reliability <- function(s, lang) {
  lvl <- function(a) if (is.na(a)) NA_character_ else if (a >= .9) tx(lang, "m\u00FCkemmel", "excellent") else if (a >= .8) tx(lang, "iyi", "good") else if (a >= .7) tx(lang, "kabul edilebilir", "acceptable") else if (a >= .6) tx(lang, "sorgulanabilir", "questionable") else tx(lang, "d\u00FC\u015F\u00FCk", "poor")
  txt <- tx(lang, paste0(fmt_n(s$k), " maddelik \u00F6l\u00E7e\u011Fin i\u00E7 tutarl\u0131l\u0131\u011F\u0131 incelenmi\u015Ftir: ", if (!is.na(s$alpha)) paste0("Cronbach \u03B1 = ", fmt_bounded(s$alpha), " (", lvl(s$alpha), ")") else "", if (!is.na(s$omega)) paste0(", McDonald \u03C9 = ", fmt_bounded(s$omega)) else "", if (!is.na(s$mean)) paste0("; \u00F6l\u00E7ek ortalamas\u0131 ", sym("M"), " = ", fmt_num(s$mean), ", ", sym("SD"), " = ", fmt_num(s$sd)) else "", ". "),
                  paste0("Internal consistency of the ", fmt_n(s$k), "-item scale was examined: ", if (!is.na(s$alpha)) paste0("Cronbach's \u03B1 = ", fmt_bounded(s$alpha), " (", lvl(s$alpha), ")") else "", if (!is.na(s$omega)) paste0(", McDonald's \u03C9 = ", fmt_bounded(s$omega)) else "", if (!is.na(s$mean)) paste0("; scale ", sym("M"), " = ", fmt_num(s$mean), ", ", sym("SD"), " = ", fmt_num(s$sd)) else "", ". "))
  low <- Filter(function(i) !is.na(i$itemRestCor) && i$itemRestCor < .30, s$items)
  if (length(low)) txt <- paste0(txt, tx(lang, paste0("Madde-kalan korelasyonu .30'un alt\u0131nda olan maddeler: ", join_words(vapply(low, function(i) paste0(vn(i$name), " (", fmt_bounded(i$itemRestCor), ")"), character(1)), lang), ". "), paste0("Items with item-rest correlations below .30: ", join_words(vapply(low, function(i) paste0(vn(i$name), " (", fmt_bounded(i$itemRestCor), ")"), character(1)), lang), ". ")))
  para(txt)
}

rt_efa <- function(s, lang) {
  is_pca <- s$type == "pca"
  name <- if (is_pca) tx(lang, "temel bile\u015Fenler analizi (PCA)", "principal component analysis (PCA)") else tx(lang, "a\u00E7\u0131mlay\u0131c\u0131 fakt\u00F6r analizi (AFA)", "exploratory factor analysis (EFA)")
  txt <- tx(lang, paste0(fmt_n(length(s$vars)), " madde ", name, " ile incelenmi\u015Ftir", if (!is_pca) paste0(" (\u00E7\u0131kar\u0131m: ", vn(s$extraction), ", d\u00F6nd\u00FCrme: ", vn(s$rotation), ")") else paste0(" (d\u00F6nd\u00FCrme: ", vn(s$rotation), ")"), ". "),
                  paste0(fmt_n(length(s$vars)), " items were analysed with ", name, if (!is_pca) paste0(" (extraction: ", vn(s$extraction), ", rotation: ", vn(s$rotation), ")") else paste0(" (rotation: ", vn(s$rotation), ")"), ". "))
  if (!is.na(s$kmo)) txt <- paste0(txt, tx(lang, paste0("KMO \u00F6rneklem yeterlili\u011Fi \u00F6l\u00E7\u00FCt\u00FC ", fmt_bounded(s$kmo), " bulunmu\u015Ftur. "), paste0("The KMO measure of sampling adequacy was ", fmt_bounded(s$kmo), ". ")))
  if (!is.null(s$bartlett) && !is.na(s$bartlett$p)) txt <- paste0(txt, tx(lang, paste0("Bartlett k\u00FCresellik testi ", if (sig(s$bartlett$p)) "anlaml\u0131d\u0131r" else "anlaml\u0131 de\u011Fildir", ", \u03C7\u00B2(", fmt_df(s$bartlett$df), ") = ", fmt_num(s$bartlett$chi), ", ", sym("p"), " ", fmt_p(s$bartlett$p), ". "), paste0("Bartlett's test of sphericity was ", if (sig(s$bartlett$p)) "significant" else "not significant", ", \u03C7\u00B2(", fmt_df(s$bartlett$df), ") = ", fmt_num(s$bartlett$chi), ", ", sym("p"), " ", fmt_p(s$bartlett$p), ". ")))
  if (length(s$factors)) { last <- s$factors[[length(s$factors)]]
    txt <- paste0(txt, tx(lang, paste0(fmt_n(length(s$factors)), " ", if (is_pca) "bile\u015Fen" else "fakt\u00F6r", " elde edilmi\u015F olup toplam varyans\u0131n ", pct_l(last$varCum, lang), "'ini a\u00E7\u0131klamaktad\u0131r (", join_words(vapply(s$factors, function(f) paste0(vn(f$comp), ": ", pct_l(f$varProp, lang)), character(1)), lang), "). "),
                                paste0(fmt_n(length(s$factors)), " ", if (is_pca) "component(s)" else "factor(s)", " were retained, explaining ", pct_l(last$varCum, lang), " of the total variance (", join_words(vapply(s$factors, function(f) paste0(vn(f$comp), ": ", pct_l(f$varProp, lang)), character(1)), lang), "). "))) }
  if (!is.null(s$fit) && !is.na(s$fit$rmsea)) txt <- paste0(txt, tx(lang, paste0("Model uyumu: RMSEA = ", fmt_bounded(s$fit$rmsea, 3), if (!is.na(s$fit$tli)) paste0(", TLI = ", fmt_bounded(s$fit$tli, 3)) else "", ". "), paste0("Model fit: RMSEA = ", fmt_bounded(s$fit$rmsea, 3), if (!is.na(s$fit$tli)) paste0(", TLI = ", fmt_bounded(s$fit$tli, 3)) else "", ". ")))
  out <- para(txt)
  if (!is.null(s$loadings)) out <- paste0(out, "<p><b>", tx(lang, "Fakt\u00F6r y\u00FCkleri", "Loadings"), "</b></p>", df_to_html(s$loadings))
  out
}

rt_cfa <- function(s, lang) {
  good <- c(if (!is.na(s$cfi)) s$cfi >= .90, if (!is.na(s$rmsea)) s$rmsea <= .08, if (!is.na(s$srmr)) s$srmr <= .08)
  verdict <- if (length(good) && all(good)) tx(lang, "kabul edilebilir uyum", "acceptable fit") else tx(lang, "yetersiz uyum", "inadequate fit")
  fit <- paste0("\u03C7\u00B2(", fmt_df(s$df), ") = ", fmt_num(s$chi), ", ", sym("p"), " ", fmt_p(s$p), if (!is.na(s$cfi)) paste0(", CFI = ", fmt_bounded(s$cfi, 3)) else "", if (!is.na(s$tli)) paste0(", TLI = ", fmt_bounded(s$tli, 3)) else "", if (!is.na(s$rmsea)) paste0(", RMSEA = ", fmt_bounded(s$rmsea, 3), if (!is.na(s$rmseaLower)) paste0(" [", fmt_bounded(s$rmseaLower, 3), ", ", fmt_bounded(s$rmseaUpper, 3), "]") else "") else "", if (!is.na(s$srmr)) paste0(", SRMR = ", fmt_bounded(s$srmr, 3)) else "")
  para(tx(lang, paste0(fmt_n(s$nFactors), " fakt\u00F6rl\u00FC model do\u011Frulay\u0131c\u0131 fakt\u00F6r analizi (DFA) ile s\u0131nanm\u0131\u015Ft\u0131r. Uyum indeksleri ", verdict, " g\u00F6stermektedir: ", fit, ". "), paste0("The ", fmt_n(s$nFactors), "-factor model was tested with confirmatory factor analysis (CFA). Fit indices indicated ", verdict, ": ", fit, ". ")))
}

rt_mancova <- function(s, lang) {
  txt <- tx(lang, paste0(join_words(vapply(s$deps, vn, character(1)), lang), " ba\u011F\u0131ml\u0131 de\u011Fi\u015Fkenleri \u00FCzerinde ", join_words(vapply(s$factors, vn, character(1)), lang), " etkisi \u00E7ok de\u011Fi\u015Fkenli ", if (length(s$covs)) "kovaryans" else "varyans", " analizi ile incelenmi\u015Ftir. "), paste0("The effect of ", join_words(vapply(s$factors, vn, character(1)), lang), " on ", join_words(vapply(s$deps, vn, character(1)), lang), " was examined with a multivariate analysis of ", if (length(s$covs)) "covariance" else "variance", ". "))
  pref <- Filter(function(t) t$test %in% c("pillai", "wilks"), s$tests)
  for (t in pref) { lab <- if (t$test == "pillai") tx(lang, "Pillai izi", "Pillai's trace") else "Wilks' \u039B"
    txt <- paste0(txt, tx(lang, paste0(vn(t$term), " \u00E7ok de\u011Fi\u015Fkenli etkisi ", if (sig(t$p)) "anlaml\u0131d\u0131r" else "anlaml\u0131 de\u011Fildir", " (", lab, " = ", fmt_bounded(t$stat, 3), ", ", st_F(t$F, t$df1, t$df2, t$p), "). "), paste0("The multivariate effect of ", vn(t$term), " was ", if (sig(t$p)) "significant" else "not significant", " (", lab, " = ", fmt_bounded(t$stat, 3), ", ", st_F(t$F, t$df1, t$df2, t$p), "). "))) }
  uni <- Filter(function(u) !u$term %in% c("Residuals", "Art\u0131klar"), s$univ)
  if (length(uni)) { resid <- Filter(function(u) u$term %in% c("Residuals", "Art\u0131klar"), s$univ)
    items <- vapply(uni, function(u) { rd <- if (length(resid)) { r <- Filter(function(x) x$dep == u$dep, resid); if (length(r)) r[[1]]$df else NA } else NA; paste0(vn(u$dep), " (", vn(u$term), "): ", st_F(u$F, u$df, rd, u$p)) }, character(1))
    txt <- paste0(txt, tx(lang, paste0("Tek de\u011Fi\u015Fkenli izleme testleri: ", join_words(items, lang), ". "), paste0("Univariate follow-up tests: ", join_words(items, lang), ". "))) }
  para(txt)
}

rt_descriptives <- function(s, lang) {
  txt <- ""
  for (v in names(s$stats)) {
    ent <- s$stats[[v]]
    levels <- unique(vapply(ent, function(e) if (is.na(e$level)) "" else e$level, character(1)))
    for (lv in levels) {
      g <- Filter(function(e) identical(if (is.na(e$level)) "" else e$level, lv), ent)
      get <- function(st) { x <- Filter(function(e) e$stat == st, g); if (length(x)) x[[1]]$value else NA_real_ }
      n <- get("n"); m <- get("mean"); sd_ <- get("sd"); md <- get("median"); mn <- get("min"); mx <- get("max"); sk <- get("skew"); ku <- get("kurt"); swp <- get("swp")
      lab <- if (nzchar(lv)) paste0(vn(v), " (", vn(s$splitBy), " = ", vn(lv), ")") else vn(v)
      parts <- c(if (!is.na(n)) paste0(sym("n"), " = ", fmt_n(n)), if (!is.na(m)) paste0(sym("M"), " = ", fmt_num(m)), if (!is.na(sd_)) paste0(sym("SD"), " = ", fmt_num(sd_)), if (!is.na(md)) paste0(sym("Mdn"), " = ", fmt_num(md)), if (!is.na(mn) && !is.na(mx)) paste0(tx(lang, "aral\u0131k", "range"), " ", fmt_num(mn), "\u2013", fmt_num(mx)), if (!is.na(sk)) paste0(tx(lang, "\u00E7arp\u0131kl\u0131k", "skewness"), " = ", fmt_num(sk)), if (!is.na(ku)) paste0(tx(lang, "bas\u0131kl\u0131k", "kurtosis"), " = ", fmt_num(ku)), if (!is.na(swp)) paste0("Shapiro-Wilk ", sym("p"), " ", fmt_p(swp)))
      if (length(parts)) txt <- paste0(txt, lab, ": ", paste(parts, collapse = ", "), ". ")
    }
  }
  for (key in names(s$freqs)) { f <- s$freqs[[key]]; items <- vapply(f, function(x) paste0(vn(x$level), " ", fmt_n(x$count), " (", pct_l(x$pc, lang), ")"), character(1))
    txt <- paste0(txt, tx(lang, paste0(vn(key), " da\u011F\u0131l\u0131m\u0131: ", join_words(items, lang), ". "), paste0("Distribution of ", vn(key), ": ", join_words(items, lang), ". "))) }
  if (!nzchar(txt)) txt <- tx(lang, "Betimsel istatistikler tabloda sunulmu\u015Ftur.", "Descriptive statistics are presented in the table.")
  para(txt)
}
