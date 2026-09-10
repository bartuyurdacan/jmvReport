# ---- Extract structured summaries from jmv result objects --------------------
# Every summariser returns a list with at least:
#   type   : jmv analysis name (e.g. "ttestIS")
#   title  : human readable title
#   opts   : selected options relevant for the Method section
#   tables : named list of data.frames (raw asDF) for generic rendering
# plus analysis-specific fields used by the templates.

analysis_name <- function(res) {
  cls <- class(res$options)[1]
  nm <- sub("Options$", "", cls)
  # jmv uses lower-case internal names for a few analyses
  switch(nm, anova = "ANOVA", nm)
}

opt <- function(res, name, default = NULL) {
  v <- tryCatch(res$options[[name]], error = function(e) NULL)
  if (is.null(v)) default else v
}

item <- function(x, name) {
  if (is.null(x)) return(NULL)
  tryCatch({
    if (name %in% x$itemNames) x$get(name) else NULL
  }, error = function(e) NULL)
}

array_tables <- function(arr) {
  if (is.null(arr)) return(list())
  out <- list()
  its <- tryCatch(arr$items, error = function(e) list())
  for (it in its) {
    if (inherits(it, "Table")) {
      key <- tryCatch(gsub('^"|"$', "", it$name), error = function(e) as.character(length(out) + 1))
      out[[key]] <- list(title = it$title, df = table_df(it))
    } else if (inherits(it, "Group")) {
      key <- tryCatch(gsub('^"|"$', "", it$name), error = function(e) as.character(length(out) + 1))
      for (nm in it$itemNames) {
        sub <- it$get(nm)
        if (inherits(sub, "Table")) out[[paste0(key, "$", nm)]] <- list(title = sub$title, df = table_df(sub))
      }
    }
  }
  out
}

# Collect every table in a results object (used for the generic fallback and
# for "tables" section).
collect_tables <- function(x, path = "") {
  out <- list()
  names_ <- tryCatch(x$itemNames, error = function(e) character())
  for (nm in names_) {
    it <- tryCatch(x$get(nm), error = function(e) NULL)
    if (is.null(it)) next
    p <- if (nzchar(path)) paste0(path, "$", nm) else nm
    if (inherits(it, "Table")) {
      vis <- tryCatch(it$visible, error = function(e) TRUE)
      df <- table_df(it)
      if (isTRUE(vis) && !is.null(df) && nrow(df) > 0 && ncol(df) > 0)
        out[[p]] <- list(title = it$title, df = df)
    } else if (inherits(it, "Group")) {
      out <- c(out, collect_tables(it, p))
    } else if (inherits(it, "Array")) {
      its <- tryCatch(it$items, error = function(e) list())
      for (sub in its) {
        key <- tryCatch(gsub('^"|"$', "", sub$name), error = function(e) "")
        if (inherits(sub, "Table")) {
          df <- table_df(sub)
          if (!is.null(df) && nrow(df) > 0) out[[paste0(p, "[", key, "]")]] <- list(title = sub$title, df = df)
        } else if (inherits(sub, "Group")) {
          out <- c(out, collect_tables(sub, paste0(p, "[", key, "]")))
        }
      }
    }
  }
  out
}

#' Summarise any jmv results object
#' @param res A jmv results object.
#' @export
summarize_results <- function(res) {
  type <- analysis_name(res)
  fn <- switch(type,
    ttestIS = ex_ttestIS, ttestPS = ex_ttestPS, ttestOneS = ex_ttestOneS,
    anovaOneW = ex_anovaOneW, ANOVA = ex_ANOVA, ancova = ex_ANOVA,
    anovaRM = ex_anovaRM, anovaNP = ex_anovaNP, anovaRMNP = ex_anovaRMNP,
    corrMatrix = ex_corrMatrix, linReg = ex_linReg, logRegBin = ex_logReg,
    logRegOrd = ex_logReg, logRegMulti = ex_logReg,
    contTables = ex_contTables, contTablesPaired = ex_contTablesPaired,
    propTest2 = ex_propTest2, propTestN = ex_propTestN,
    reliability = ex_reliability, efa = ex_efa, pca = ex_efa, cfa = ex_cfa,
    mancova = ex_mancova, descriptives = ex_descriptives,
    NULL)
  s <- if (is.null(fn)) list(supported = FALSE) else tryCatch(fn(res), error = function(e) list(supported = FALSE, error = conditionMessage(e)))
  s$type <- type
  s$title <- tryCatch(res$title, error = function(e) type)
  if (is.null(s$supported)) s$supported <- TRUE
  s$tables <- tryCatch(collect_tables(res), error = function(e) list())
  s
}

# ---- helpers for layered tables --------------------------------------------
lcol <- function(df, base, layer, row = 1) num_cell(df, paste0(base, "[", layer, "]"), row)
lchr <- function(df, base, layer, row = 1) chr_cell(df, paste0(base, "[", layer, "]"), row)
has_col <- function(df, col) !is.null(df) && col %in% names(df)

clean_name <- function(x) gsub('^"|"$', "", as.character(x))

# ---- t-tests ---------------------------------------------------------------
ex_ttestIS <- function(res) {
  tt <- table_df(item(res, "ttest")); desc <- table_df(item(res, "desc"))
  assum <- item(res, "assum")
  norm <- table_df(item(assum, "norm")); eqv <- table_df(item(assum, "eqv"))
  vars <- opt(res, "vars"); group <- opt(res, "group")
  rows <- list()
  for (i in seq_len(nrow(tt))) {
    v <- if (has_col(tt, "var[stud]")) chr_cell(tt, "var[stud]", i) else if (has_col(tt, "var[welc]")) chr_cell(tt, "var[welc]", i) else chr_cell(tt, "var[mann]", i)
    r <- list(var = v)
    r$student <- if (has_col(tt, "stat[stud]")) list(t = lcol(tt, "stat", "stud", i), df = lcol(tt, "df", "stud", i), p = lcol(tt, "p", "stud", i), md = lcol(tt, "md", "stud", i), cil = lcol(tt, "cil", "stud", i), ciu = lcol(tt, "ciu", "stud", i), es = lcol(tt, "es", "stud", i), esType = lchr(tt, "esType", "stud", i)) else NULL
    r$welch <- if (has_col(tt, "stat[welc]")) list(t = lcol(tt, "stat", "welc", i), df = lcol(tt, "df", "welc", i), p = lcol(tt, "p", "welc", i), md = lcol(tt, "md", "welc", i), cil = lcol(tt, "cil", "welc", i), ciu = lcol(tt, "ciu", "welc", i), es = lcol(tt, "es", "welc", i)) else NULL
    r$mann <- if (has_col(tt, "stat[mann]")) list(U = lcol(tt, "stat", "mann", i), p = lcol(tt, "p", "mann", i), es = lcol(tt, "es", "mann", i), md = lcol(tt, "md", "mann", i)) else NULL
    if (!is.null(desc) && nrow(desc) >= i) {
      r$g1 <- list(name = lchr(desc, "group", "1", i), n = lcol(desc, "num", "1", i), m = lcol(desc, "mean", "1", i), mdn = lcol(desc, "med", "1", i), sd = lcol(desc, "sd", "1", i))
      r$g2 <- list(name = lchr(desc, "group", "2", i), n = lcol(desc, "num", "2", i), m = lcol(desc, "mean", "2", i), mdn = lcol(desc, "med", "2", i), sd = lcol(desc, "sd", "2", i))
    }
    if (!is.null(norm) && nrow(norm) >= i) r$norm <- list(w = num_cell(norm, "w", i), p = num_cell(norm, "p", i))
    if (!is.null(eqv) && nrow(eqv) >= i) r$levene <- list(F = num_cell(eqv, "f", i), df1 = num_cell(eqv, "df", i), df2 = num_cell(eqv, "df2", i), p = num_cell(eqv, "p", i))
    rows[[length(rows) + 1]] <- r
  }
  list(group = group, vars = vars, rows = rows,
       opts = list(students = isTRUE(opt(res, "students", TRUE)), welchs = isTRUE(opt(res, "welchs")), mann = isTRUE(opt(res, "mann")),
                   effectSize = isTRUE(opt(res, "effectSize")), ci = isTRUE(opt(res, "ci")), ciWidth = opt(res, "ciWidth", 95),
                   norm = isTRUE(opt(res, "norm")), eqv = isTRUE(opt(res, "eqv")), hypothesis = opt(res, "hypothesis", "different")))
}

ex_ttestPS <- function(res) {
  tt <- table_df(item(res, "ttest")); desc <- table_df(item(res, "desc")); norm <- table_df(item(res, "norm"))
  rows <- list()
  for (i in seq_len(nrow(tt))) {
    v1 <- if (has_col(tt, "var1[stud]")) chr_cell(tt, "var1[stud]", i) else chr_cell(tt, "var1[wilc]", i)
    v2 <- if (has_col(tt, "var2[stud]")) chr_cell(tt, "var2[stud]", i) else chr_cell(tt, "var2[wilc]", i)
    r <- list(var1 = v1, var2 = v2)
    r$student <- if (has_col(tt, "stat[stud]")) list(t = lcol(tt, "stat", "stud", i), df = lcol(tt, "df", "stud", i), p = lcol(tt, "p", "stud", i), md = lcol(tt, "md", "stud", i), cil = lcol(tt, "cil", "stud", i), ciu = lcol(tt, "ciu", "stud", i), es = lcol(tt, "es", "stud", i)) else NULL
    r$wilcoxon <- if (has_col(tt, "stat[wilc]")) list(W = lcol(tt, "stat", "wilc", i), p = lcol(tt, "p", "wilc", i), es = lcol(tt, "es", "wilc", i)) else NULL
    if (!is.null(desc)) {
      d1 <- which(desc$name == v1)[1]; d2 <- which(desc$name == v2)[1]
      if (!is.na(d1)) r$d1 <- list(n = num_cell(desc, "num", d1), m = num_cell(desc, "m", d1), mdn = num_cell(desc, "med", d1), sd = num_cell(desc, "sd", d1))
      if (!is.na(d2)) r$d2 <- list(n = num_cell(desc, "num", d2), m = num_cell(desc, "m", d2), mdn = num_cell(desc, "med", d2), sd = num_cell(desc, "sd", d2))
    }
    if (!is.null(norm) && nrow(norm) >= i) r$norm <- list(w = num_cell(norm, "w", i), p = num_cell(norm, "p", i))
    rows[[length(rows) + 1]] <- r
  }
  list(rows = rows, opts = list(students = isTRUE(opt(res, "students", TRUE)), wilcoxon = isTRUE(opt(res, "wilcoxon")), effectSize = isTRUE(opt(res, "effectSize")), ci = isTRUE(opt(res, "ci")), ciWidth = opt(res, "ciWidth", 95), norm = isTRUE(opt(res, "norm"))))
}

ex_ttestOneS <- function(res) {
  tt <- table_df(item(res, "ttest")); desc <- table_df(item(res, "descriptives")); norm <- table_df(item(res, "normality"))
  rows <- list()
  for (i in seq_len(nrow(tt))) {
    v <- if (has_col(tt, "var[stud]")) chr_cell(tt, "var[stud]", i) else chr_cell(tt, "var[wilc]", i)
    r <- list(var = v)
    r$student <- if (has_col(tt, "stat[stud]")) list(t = lcol(tt, "stat", "stud", i), df = lcol(tt, "df", "stud", i), p = lcol(tt, "p", "stud", i), es = lcol(tt, "es", "stud", i), md = lcol(tt, "md", "stud", i), cil = lcol(tt, "cil", "stud", i), ciu = lcol(tt, "ciu", "stud", i)) else NULL
    r$wilcoxon <- if (has_col(tt, "stat[wilc]")) list(W = lcol(tt, "stat", "wilc", i), p = lcol(tt, "p", "wilc", i), es = lcol(tt, "es", "wilc", i)) else NULL
    if (!is.null(desc) && nrow(desc) >= i) r$d <- list(n = num_cell(desc, "num", i), m = num_cell(desc, "mean", i), mdn = num_cell(desc, "median", i), sd = num_cell(desc, "sd", i))
    if (!is.null(norm) && nrow(norm) >= i) r$norm <- list(w = num_cell(norm, "w", i), p = num_cell(norm, "p", i))
    rows[[length(rows) + 1]] <- r
  }
  list(rows = rows, testValue = opt(res, "testValue", 0), opts = list(students = isTRUE(opt(res, "students", TRUE)), wilcoxon = isTRUE(opt(res, "wilcoxon")), effectSize = isTRUE(opt(res, "effectSize")), norm = isTRUE(opt(res, "norm"))))
}

# ---- one-way ANOVA ---------------------------------------------------------
matrix_posthoc_pairs <- function(df) {
  # jmv "matrix" style post-hoc (anovaOneW): columns .name[md], <lvl>[md], <lvl>[p], <lvl>[t], <lvl>[df]
  if (is.null(df)) return(list())
  lv <- sub("\\[md\\]$", "", grep("\\[md\\]$", names(df), value = TRUE))
  lv <- setdiff(lv, ".name")
  rows <- clean_name(df$`.name[md]`)
  pairs <- list()
  for (i in seq_along(rows)) for (b in lv) {
    md <- num_cell(df, paste0(b, "[md]"), i); p <- num_cell(df, paste0(b, "[p]"), i)
    if (is.na(md) || is.na(p)) next
    pairs[[length(pairs) + 1]] <- list(a = rows[i], b = b, md = md, p = p, t = num_cell(df, paste0(b, "[t]"), i), df = num_cell(df, paste0(b, "[df]"), i))
  }
  pairs
}

ex_anovaOneW <- function(res) {
  an <- table_df(item(res, "anova")); desc <- table_df(item(res, "desc"))
  assump <- item(res, "assump"); norm <- table_df(item(assump, "norm")); eqv <- table_df(item(assump, "eqv"))
  ph <- array_tables(item(res, "postHoc"))
  group <- opt(res, "group"); rows <- list()
  for (i in seq_len(nrow(an))) {
    dep <- chr_cell(an, "dep", i); r <- list(dep = dep)
    r$welch <- if (has_col(an, "F[welch]")) list(F = lcol(an, "F", "welch", i), df1 = lcol(an, "df1", "welch", i), df2 = lcol(an, "df2", "welch", i), p = lcol(an, "p", "welch", i)) else NULL
    r$fisher <- if (has_col(an, "F[fisher]")) list(F = lcol(an, "F", "fisher", i), df1 = lcol(an, "df1", "fisher", i), df2 = lcol(an, "df2", "fisher", i), p = lcol(an, "p", "fisher", i)) else NULL
    if (!is.null(desc)) {
      d <- desc[desc$dep == dep, , drop = FALSE]
      r$groups <- lapply(seq_len(nrow(d)), function(k) list(name = chr_cell(d, "group", k), n = num_cell(d, "num", k), m = num_cell(d, "mean", k), sd = num_cell(d, "sd", k)))
    }
    if (!is.null(norm) && nrow(norm) >= i) r$norm <- list(w = num_cell(norm, "w", i), p = num_cell(norm, "p", i))
    if (!is.null(eqv) && nrow(eqv) >= i) r$levene <- list(F = num_cell(eqv, "F", i), df1 = num_cell(eqv, "df1", i), df2 = num_cell(eqv, "df2", i), p = num_cell(eqv, "p", i))
    if (!is.null(ph[[dep]])) r$posthoc <- matrix_posthoc_pairs(ph[[dep]]$df)
    rows[[length(rows) + 1]] <- r
  }
  list(group = group, rows = rows, opts = list(welchs = isTRUE(opt(res, "welchs", TRUE)), fishers = isTRUE(opt(res, "fishers")), phMethod = opt(res, "phMethod", "none"), norm = isTRUE(opt(res, "norm")), eqv = isTRUE(opt(res, "eqv"))))
}

# ---- factorial ANOVA / ANCOVA ---------------------------------------------
ex_ANOVA <- function(res) {
  main <- table_df(item(res, "main"))
  assump <- item(res, "assump"); homo <- table_df(item(assump, "homo")); norm <- table_df(item(assump, "norm"))
  ph <- array_tables(item(res, "postHoc"))
  terms <- list(); resid_df <- NA_real_
  for (i in seq_len(nrow(main))) {
    nm <- chr_cell(main, "name", i)
    if (nm %in% c("Residuals", "Art\u0131klar")) { resid_df <- num_cell(main, "df", i); next }
    if (nm %in% c("Overall model", "Overall Model")) next
    terms[[length(terms) + 1]] <- list(name = nm, ss = num_cell(main, "ss", i), df = num_cell(main, "df", i), F = num_cell(main, "F", i), p = num_cell(main, "p", i),
                                       etaSq = num_cell(main, "etaSq", i), etaSqP = num_cell(main, "etaSqP", i), omegaSq = num_cell(main, "omegaSq", i))
  }
  posthoc <- list()
  for (key in names(ph)) {
    df <- ph[[key]]$df; if (is.null(df) || nrow(df) == 0) next
    pcol <- grep("^p(tukey|scheffe|bonferroni|holm|none|sidak)$", names(df), value = TRUE)
    # factor names are the columns before/after "sep"
    sepi <- which(names(df) == "sep")
    a_col <- names(df)[sepi - 1]; b_col <- names(df)[sepi + 1]
    pairs <- lapply(seq_len(nrow(df)), function(k) {
      pv <- setNames(lapply(pcol, function(pc) num_cell(df, pc, k)), pcol)
      list(a = chr_cell(df, a_col, k), b = chr_cell(df, b_col, k), md = num_cell(df, "md", k), se = num_cell(df, "se", k), t = num_cell(df, "t", k), df = num_cell(df, "df", k), p = pv)
    })
    posthoc[[key]] <- pairs
  }
  emm <- list()
  emmArr <- item(res, "emm")
  if (!is.null(emmArr)) for (g in tryCatch(emmArr$items, error = function(e) list())) {
    tb <- tryCatch(g$get("emmTable"), error = function(e) NULL)
    df <- table_df(tb); if (!is.null(df) && nrow(df) > 0) emm[[length(emm) + 1]] <- list(title = tb$title, df = df)
  }
  list(dep = opt(res, "dep"), factors = opt(res, "factors"), covs = opt(res, "covs"), terms = terms, resid_df = resid_df,
       homo = if (!is.null(homo) && nrow(homo) > 0) list(F = num_cell(homo, "F"), df1 = num_cell(homo, "df1"), df2 = num_cell(homo, "df2"), p = num_cell(homo, "p")) else NULL,
       norm = if (!is.null(norm) && nrow(norm) > 0) list(w = lcol(norm, "s", "sw"), p = lcol(norm, "p", "sw")) else NULL,
       posthoc = posthoc, emm = emm,
       opts = list(effectSize = opt(res, "effectSize", character()), postHocCorr = opt(res, "postHocCorr", character()), homo = isTRUE(opt(res, "homo")), norm = isTRUE(opt(res, "norm")), ss = opt(res, "ss", "3")))
}

# ---- repeated measures ANOVA ----------------------------------------------
ex_anovaRM <- function(res) {
  rm_ <- table_df(item(res, "rmTable")); bs <- table_df(item(res, "bsTable"))
  assump <- item(res, "assump"); sph <- table_df(item(assump, "spherTable"))
  ph <- array_tables(item(res, "postHoc"))
  corr <- opt(res, "spherCorr", "none")
  layers <- unique(sub("^name\\[(.*)\\]$", "\\1", grep("^name\\[", names(rm_), value = TRUE)))
  within <- list()
  for (lay in layers) for (i in seq_len(nrow(rm_))) {
    nm <- lchr(rm_, "name", lay, i); if (is.na(nm) || nm %in% c("Residual", "Residuals")) next
    within[[length(within) + 1]] <- list(correction = lay, name = nm, F = lcol(rm_, "F", lay, i), df = lcol(rm_, "df", lay, i), p = lcol(rm_, "p", lay, i), partEta = lcol(rm_, "partEta", lay, i), eta = lcol(rm_, "eta", lay, i), omega = lcol(rm_, "omega", lay, i))
  }
  resid_rows <- if (!is.null(rm_)) which(rm_[[paste0("name[", layers[1], "]")]] %in% c("Residual", "Residuals")) else integer()
  resid_df <- setNames(lapply(layers, function(lay) if (length(resid_rows)) lcol(rm_, "df", lay, resid_rows[1]) else NA_real_), layers)
  between <- list()
  if (!is.null(bs)) for (i in seq_len(nrow(bs))) { nm <- chr_cell(bs, "name", i); if (nm %in% c("Residual", "Residuals")) next
    between[[length(between) + 1]] <- list(name = nm, F = num_cell(bs, "F", i), df = num_cell(bs, "df", i), p = num_cell(bs, "p", i), partEta = num_cell(bs, "partEta", i)) }
  bs_resid_df <- if (!is.null(bs)) { k <- which(bs$name %in% c("Residual", "Residuals")); if (length(k)) num_cell(bs, "df", k[1]) else NA_real_ } else NA_real_
  spher <- if (!is.null(sph)) lapply(seq_len(nrow(sph)), function(i) list(name = chr_cell(sph, "name", i), W = num_cell(sph, "mauch", i), p = num_cell(sph, "p", i), gg = num_cell(sph, "gg", i), hf = num_cell(sph, "hf", i))) else list()
  posthoc <- list()
  for (key in names(ph)) { df <- ph[[key]]$df; if (is.null(df) || nrow(df) == 0) next
    pcol <- grep("^p(tukey|scheffe|bonferroni|holm|none)$", names(df), value = TRUE); sepi <- which(names(df) == "sep")
    posthoc[[key]] <- lapply(seq_len(nrow(df)), function(k) list(a = chr_cell(df, names(df)[sepi - 1], k), b = chr_cell(df, names(df)[sepi + 1], k), md = num_cell(df, "md", k), t = num_cell(df, "t", k), df = num_cell(df, "df", k), p = setNames(lapply(pcol, function(pc) num_cell(df, pc, k)), pcol))) }
  list(within = within, between = between, resid_df = resid_df, bs_resid_df = bs_resid_df, spher = spher, posthoc = posthoc,
       rm_labels = vapply(opt(res, "rm", list()), function(x) x$label, character(1)), bs = opt(res, "bs"),
       opts = list(spherCorr = corr, effectSize = opt(res, "effectSize", character()), postHocCorr = opt(res, "postHocCorr", character())))
}

# ---- non-parametric ANOVAs -------------------------------------------------
ex_anovaNP <- function(res) {
  tb <- table_df(item(res, "table")); comps <- array_tables(item(res, "comparisons")); dunn <- array_tables(item(res, "comparisonsDunn"))
  rows <- list()
  for (i in seq_len(nrow(tb))) {
    dep <- chr_cell(tb, "name", i)
    r <- list(dep = dep, H = num_cell(tb, "chiSq", i), df = num_cell(tb, "df", i), p = num_cell(tb, "p", i), es = num_cell(tb, "es", i))
    if (!is.null(comps[[dep]])) { d <- comps[[dep]]$df; r$dscf <- lapply(seq_len(nrow(d)), function(k) list(a = chr_cell(d, "p1", k), b = chr_cell(d, "p2", k), W = num_cell(d, "W", k), p = num_cell(d, "p", k))) }
    if (!is.null(dunn[[dep]])) { d <- dunn[[dep]]$df; r$dunn <- lapply(seq_len(nrow(d)), function(k) list(a = chr_cell(d, "p1", k), b = chr_cell(d, "p2", k), z = num_cell(d, "z", k), p = num_cell(d, "p", k), padj = num_cell(d, "padj", k))) }
    rows[[length(rows) + 1]] <- r
  }
  list(group = opt(res, "group"), rows = rows, opts = list(es = isTRUE(opt(res, "es")), pairs = isTRUE(opt(res, "pairs")), pairsDunn = isTRUE(opt(res, "pairsDunn"))))
}

ex_anovaRMNP <- function(res) {
  tb <- table_df(item(res, "table")); comp <- table_df(item(res, "comp")); desc <- table_df(item(res, "desc"))
  list(measures = opt(res, "measures"), chi = num_cell(tb, "stat"), df = num_cell(tb, "df"), p = num_cell(tb, "p"),
       pairs = if (!is.null(comp)) lapply(seq_len(nrow(comp)), function(k) list(a = chr_cell(comp, "i1", k), b = chr_cell(comp, "i2", k), stat = num_cell(comp, "stat", k), p = num_cell(comp, "p", k))) else list(),
       desc = if (!is.null(desc)) lapply(seq_len(nrow(desc)), function(k) list(name = chr_cell(desc, "level", k), m = num_cell(desc, "mean", k), mdn = num_cell(desc, "median", k))) else list(),
       opts = list(pairs = isTRUE(opt(res, "pairs"))))
}

# ---- correlation -----------------------------------------------------------
ex_corrMatrix <- function(res) {
  m <- table_df(item(res, "matrix")); vars <- opt(res, "vars")
  rows <- clean_name(m$`.name[r]`); if (all(is.na(rows))) rows <- clean_name(rownames(m))
  pairs <- list()
  cols <- vars
  for (i in seq_along(rows)) for (j in seq_along(cols)) {
    if (j >= i) next
    a <- cols[j]; b <- rows[i]
    getv <- function(suffix) { cn <- paste0(a, "[", suffix, "]"); if (cn %in% names(m)) num_cell(m, cn, i) else NA_real_ }
    r <- getv("r"); rho <- getv("rho"); tau <- getv("tau")
    if (is.na(r) && is.na(rho) && is.na(tau)) next
    pairs[[length(pairs) + 1]] <- list(a = a, b = b, r = r, rdf = getv("rdf"), rp = getv("rp"), rcil = getv("rcil"), rciu = getv("rciu"),
                                       rho = rho, rhodf = getv("rhodf"), rhop = getv("rhop"), tau = tau, taup = getv("taup"), n = getv("n"))
  }
  list(vars = vars, pairs = pairs, opts = list(pearson = isTRUE(opt(res, "pearson", TRUE)), spearman = isTRUE(opt(res, "spearman")), kendall = isTRUE(opt(res, "kendall")), ci = isTRUE(opt(res, "ci")), hypothesis = opt(res, "hypothesis", "corr")))
}

# ---- regression ------------------------------------------------------------
ex_linReg <- function(res) {
  fit <- table_df(item(res, "modelFit")); comp <- table_df(item(res, "modelComp"))
  models <- list(); arr <- item(res, "models")
  its <- tryCatch(arr$items, error = function(e) list())
  for (k in seq_along(its)) {
    g <- its[[k]]; coef <- table_df(tryCatch(g$get("coef"), error = function(e) NULL))
    terms <- if (!is.null(coef)) lapply(seq_len(nrow(coef)), function(i) list(term = chr_cell(coef, "term", i), est = num_cell(coef, "est", i), se = num_cell(coef, "se", i), lower = num_cell(coef, "lower", i), upper = num_cell(coef, "upper", i), t = num_cell(coef, "t", i), p = num_cell(coef, "p", i), stdEst = num_cell(coef, "stdEst", i))) else list()
    collin <- table_df(tryCatch(g$get("assump")$get("collin"), error = function(e) NULL))
    durbin <- table_df(tryCatch(g$get("assump")$get("durbin"), error = function(e) NULL))
    models[[k]] <- list(index = k, r = num_cell(fit, "r", k), r2 = num_cell(fit, "r2", k), r2Adj = num_cell(fit, "r2Adj", k), F = num_cell(fit, "f", k), df1 = num_cell(fit, "df1", k), df2 = num_cell(fit, "df2", k), p = num_cell(fit, "p", k), aic = num_cell(fit, "aic", k),
                        terms = terms, vif_max = if (!is.null(collin)) suppressWarnings(max(as.numeric(collin$vif), na.rm = TRUE)) else NA_real_, dw = if (!is.null(durbin)) list(dw = num_cell(durbin, "dw"), p = num_cell(durbin, "p")) else NULL)
  }
  comps <- if (!is.null(comp) && nrow(comp) > 0) lapply(seq_len(nrow(comp)), function(i) list(m1 = chr_cell(comp, "model1", i), m2 = chr_cell(comp, "model2", i), dr2 = num_cell(comp, "r2", i), F = num_cell(comp, "f", i), df1 = num_cell(comp, "df1", i), df2 = num_cell(comp, "df2", i), p = num_cell(comp, "p", i))) else list()
  list(dep = opt(res, "dep"), covs = opt(res, "covs"), factors = opt(res, "factors"), models = models, comps = comps,
       opts = list(stdEst = isTRUE(opt(res, "stdEst")), ci = isTRUE(opt(res, "ci")), collin = isTRUE(opt(res, "collin")), durbin = isTRUE(opt(res, "durbin")), norm = isTRUE(opt(res, "norm")), nBlocks = length(its)))
}

ex_logReg <- function(res) {
  fit <- table_df(item(res, "modelFit")); comp <- table_df(item(res, "modelComp"))
  models <- list(); arr <- item(res, "models"); its <- tryCatch(arr$items, error = function(e) list())
  for (k in seq_along(its)) {
    g <- its[[k]]; coef <- table_df(tryCatch(g$get("coef"), error = function(e) NULL))
    terms <- if (!is.null(coef)) lapply(seq_len(nrow(coef)), function(i) list(dep = chr_cell(coef, "dep", i), term = chr_cell(coef, "term", i), est = num_cell(coef, "est", i), se = num_cell(coef, "se", i), z = num_cell(coef, "z", i), p = num_cell(coef, "p", i), odds = num_cell(coef, "odds", i), oddsLower = num_cell(coef, "oddsLower", i), oddsUpper = num_cell(coef, "oddsUpper", i))) else list()
    acc <- tryCatch(num_cell(table_df(g$get("pred")$get("measures")), "accuracy"), error = function(e) NA_real_)
    models[[k]] <- list(index = k, dev = num_cell(fit, "dev", k), aic = num_cell(fit, "aic", k), r2mf = num_cell(fit, "r2mf", k), r2cs = num_cell(fit, "r2cs", k), r2n = num_cell(fit, "r2n", k), chi = num_cell(fit, "chi", k), df = num_cell(fit, "df", k), p = num_cell(fit, "p", k), terms = terms, accuracy = acc)
  }
  comps <- if (!is.null(comp) && nrow(comp) > 0) lapply(seq_len(nrow(comp)), function(i) list(m1 = chr_cell(comp, "model1", i), m2 = chr_cell(comp, "model2", i), chi = num_cell(comp, "chi", i), df = num_cell(comp, "df", i), p = num_cell(comp, "p", i))) else list()
  list(dep = opt(res, "dep"), covs = opt(res, "covs"), factors = opt(res, "factors"), models = models, comps = comps,
       opts = list(OR = isTRUE(opt(res, "OR")), ciOR = isTRUE(opt(res, "ciOR")), pseudoR2 = opt(res, "pseudoR2", character()), nBlocks = length(its)))
}

# ---- contingency tables ----------------------------------------------------
ex_contTables <- function(res) {
  chi <- table_df(item(res, "chiSq")); nom <- table_df(item(res, "nom")); freqs <- table_df(item(res, "freqs"))
  rows <- opt(res, "rows"); cols <- opt(res, "cols"); layers <- opt(res, "layers")
  n_layer_cols <- length(layers)
  out <- list()
  for (i in seq_len(nrow(chi))) {
    layer_lab <- if (n_layer_cols > 0) paste(vapply(seq_len(n_layer_cols), function(k) paste0(names(chi)[k], " = ", chr_cell(chi, names(chi)[k], i)), character(1)), collapse = ", ") else NA_character_
    r <- list(layer = layer_lab, chi = lcol(chi, "value", "chiSq", i), df = lcol(chi, "df", "chiSq", i), p = lcol(chi, "p", "chiSq", i),
              chiCorr = lcol(chi, "value", "chiSqCorr", i), pCorr = lcol(chi, "p", "chiSqCorr", i),
              lr = lcol(chi, "value", "likeRat", i), lrp = lcol(chi, "p", "likeRat", i),
              fisher = lcol(chi, "value", "fisher", i), fisherp = lcol(chi, "p", "fisher", i), N = lcol(chi, "value", "N", i))
    if (!is.null(nom) && nrow(nom) >= i) r$phi <- lcol(nom, "v", "phi", i); if (!is.null(nom) && nrow(nom) >= i) r$cramer <- lcol(nom, "v", "cra", i); if (!is.null(nom) && nrow(nom) >= i) r$cc <- lcol(nom, "v", "cont", i)
    out[[length(out) + 1]] <- r
  }
  # dimensions for Cram\u00E9r thresholds
  n_rows_lv <- if (!is.null(freqs)) length(unique(freqs[[rows]][!freqs[[rows]] %in% c("Total", "Toplam")])) else NA
  n_cols_lv <- if (!is.null(freqs)) length(grep("\\[count\\]$", setdiff(names(freqs), ".total[count]"))) - as.integer("type[count]" %in% names(freqs)) else NA
  list(rows = rows, cols = cols, layers = layers, tests = out, dims = c(n_rows_lv, n_cols_lv), freqs = freqs,
       opts = list(chiSq = isTRUE(opt(res, "chiSq", TRUE)), chiSqCorr = isTRUE(opt(res, "chiSqCorr")), fisher = isTRUE(opt(res, "fisher")), likeRat = isTRUE(opt(res, "likeRat")), phiCra = isTRUE(opt(res, "phiCra")), contCoef = isTRUE(opt(res, "contCoef"))))
}

ex_contTablesPaired <- function(res) {
  t <- table_df(item(res, "test"))
  list(rows = opt(res, "rows"), cols = opt(res, "cols"), chi = lcol(t, "value", "mcn"), df = lcol(t, "df", "mcn"), p = lcol(t, "p", "mcn"), exactp = lcol(t, "p", "exa"), N = lcol(t, "value", "n"),
       opts = list(exact = isTRUE(opt(res, "exact"))))
}

ex_propTest2 <- function(res) {
  t <- table_df(item(res, "table"))
  list(var = opt(res, "var"), testValue = opt(res, "testValue", 0.5), rows = lapply(seq_len(nrow(t)), function(i) list(level = chr_cell(t, "level", i), count = num_cell(t, "count", i), total = num_cell(t, "total", i), prop = num_cell(t, "prop", i), p = num_cell(t, "p", i), cil = num_cell(t, "cil", i), ciu = num_cell(t, "ciu", i))), opts = list(ci = isTRUE(opt(res, "ci")), ciWidth = opt(res, "ciWidth", 95)))
}

ex_propTestN <- function(res) {
  pr <- table_df(item(res, "props")); tt <- table_df(item(res, "tests"))
  list(var = opt(res, "var"), levels = lapply(seq_len(nrow(pr)), function(i) list(level = chr_cell(pr, "level", i), count = lcol(pr, "count", "obs", i), prop = lcol(pr, "prop", "obs", i))), chi = num_cell(tt, "chi"), df = num_cell(tt, "df"), p = num_cell(tt, "p"), opts = list())
}

# ---- reliability / factor analysis ----------------------------------------
ex_reliability <- function(res) {
  sc <- table_df(item(res, "scale")); it <- table_df(item(res, "items"))
  list(vars = opt(res, "vars"), k = length(opt(res, "vars")), alpha = num_cell(sc, "alpha"), omega = num_cell(sc, "omega"), mean = num_cell(sc, "mean"), sd = num_cell(sc, "sd"),
       items = if (!is.null(it)) lapply(seq_len(nrow(it)), function(i) list(name = chr_cell(it, "name", i), itemRestCor = num_cell(it, "itemRestCor", i), alpha = num_cell(it, "alpha", i), omega = num_cell(it, "omega", i))) else list(),
       opts = list(alphaScale = isTRUE(opt(res, "alphaScale", TRUE)), omegaScale = isTRUE(opt(res, "omegaScale")), itemRestCor = isTRUE(opt(res, "itemRestCor"))))
}

ex_efa <- function(res) {
  ld <- table_df(item(res, "loadings")); fs <- item(res, "factorStats"); summ <- table_df(item(fs, "factorSummary"))
  assump <- item(res, "assump"); bart <- table_df(item(assump, "bartlett")); kmo <- table_df(item(assump, "kmo"))
  fit <- table_df(item(item(res, "modelFit"), "fit"))
  kmo_overall <- if (!is.null(kmo)) { k <- which(kmo$name %in% c("Overall", "Genel")); if (length(k)) num_cell(kmo, "msa", k[1]) else num_cell(kmo, "msa", nrow(kmo)) } else NA_real_
  list(vars = opt(res, "vars"), nFactors = if (!is.null(summ)) nrow(summ) else NA, extraction = opt(res, "extraction", "minres"), rotation = opt(res, "rotation", "oblimin"), nFactorMethod = opt(res, "nFactorMethod", "parallel"),
       factors = if (!is.null(summ)) lapply(seq_len(nrow(summ)), function(i) list(comp = chr_cell(summ, "comp", i), varProp = num_cell(summ, "varProp", i), varCum = num_cell(summ, "varCum", i))) else list(),
       bartlett = if (!is.null(bart)) list(chi = num_cell(bart, "chi"), df = num_cell(bart, "df"), p = num_cell(bart, "p")) else NULL, kmo = kmo_overall,
       fit = if (!is.null(fit)) list(rmsea = num_cell(fit, "rmsea"), tli = num_cell(fit, "tli"), chi = num_cell(fit, "chi"), df = num_cell(fit, "df"), p = num_cell(fit, "p")) else NULL,
       loadings = ld, opts = list(kmo = isTRUE(opt(res, "kmo")), bartlett = isTRUE(opt(res, "bartlett"))))
}

ex_cfa <- function(res) {
  mf <- item(res, "modelFit"); test <- table_df(item(mf, "test")); fm <- table_df(item(mf, "fitMeasures"))
  list(chi = num_cell(test, "chi"), df = num_cell(test, "df"), p = num_cell(test, "p"), cfi = num_cell(fm, "cfi"), tli = num_cell(fm, "tli"), srmr = num_cell(fm, "srmr"), rmsea = num_cell(fm, "rmsea"), rmseaLower = num_cell(fm, "rmseaLower"), rmseaUpper = num_cell(fm, "rmseaUpper"),
       nFactors = length(opt(res, "factors", list())), estimator = opt(res, "estTest", "standard"), opts = list())
}

ex_mancova <- function(res) {
  mv <- table_df(item(res, "multivar")); uv <- table_df(item(res, "univar"))
  layers <- unique(sub("^term\\[(.*)\\]$", "\\1", grep("^term\\[", names(mv), value = TRUE)))
  tests <- list()
  for (lay in layers) for (i in seq_len(nrow(mv))) tests[[length(tests) + 1]] <- list(test = lay, term = lchr(mv, "term", lay, i), stat = lcol(mv, "stat", lay, i), F = lcol(mv, "f", lay, i), df1 = lcol(mv, "df1", lay, i), df2 = lcol(mv, "df2", lay, i), p = lcol(mv, "p", lay, i))
  univ <- if (!is.null(uv)) lapply(seq_len(nrow(uv)), function(i) list(term = chr_cell(uv, "term", i), dep = chr_cell(uv, "dep", i), F = num_cell(uv, "F", i), df = num_cell(uv, "df", i), p = num_cell(uv, "p", i))) else list()
  list(deps = opt(res, "deps"), factors = opt(res, "factors"), covs = opt(res, "covs"), tests = tests, univ = univ, opts = list())
}

# ---- descriptives ----------------------------------------------------------
ex_descriptives <- function(res) {
  d <- table_df(item(res, "descriptives")); vars <- opt(res, "vars"); split <- opt(res, "splitBy")
  stats <- c("n", "missing", "mean", "median", "sd", "variance", "min", "max", "iqr", "skew", "kurt", "sw", "swp", "se", "range", "sum", "mode")
  out <- list()
  cn <- names(d)
  for (v in vars) {
    vc <- grep(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", v), "\\["), cn, value = TRUE)
    keys <- sub("^.*\\[(.*)\\]$", "\\1", vc)
    entries <- list()
    for (k in seq_along(keys)) {
      key <- keys[k]
      st <- stats[startsWith(key, stats)]
      if (length(st) == 0) next
      st <- st[which.max(nchar(st))]
      level <- substring(key, nchar(st) + 1)
      val <- num_cell(d, vc[k], 1)
      entries[[length(entries) + 1]] <- list(stat = st, level = if (nzchar(level)) level else NA_character_, value = val)
    }
    out[[v]] <- entries
  }
  freq <- array_tables(item(res, "frequencies"))
  freqs <- list()
  for (key in names(freq)) { df <- freq[[key]]$df; if (is.null(df) || nrow(df) == 0) next
    freqs[[key]] <- lapply(seq_len(nrow(df)), function(i) list(level = chr_cell(df, names(df)[1], i), count = num_cell(df, "counts", i), pc = num_cell(df, "pc", i))) }
  list(vars = vars, splitBy = split, stats = out, freqs = freqs, opts = list())
}
