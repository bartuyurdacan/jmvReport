aiContTablesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aiContTablesClass",
    inherit = aiContTablesBase,
    private = list(
        .run = function() {
            if (is.null(self$options$rows) || is.null(self$options$cols)) return()
            if (!ensure_jmv()) { self$results$report$setContent("<p style='color:#b00'>jmv package not available</p>"); return() }
            o <- self$options
            res <- jmv::contTables(data = self$data, rows = o$rows, cols = o$cols, chiSq = o$chiSq, chiSqCorr = o$chiSqCorr, likeRat = o$likeRat, fisher = o$fisher,
                                   contCoef = o$contCoef, phiCra = o$phiCra, obs = TRUE, exp = o$exp, pcRow = o$pcRow, pcCol = o$pcCol)
            private$.checkpoint()
            s <- summarize_results(res)
            fq <- table_df(res$freqs)
            if (!is.null(fq)) { names(fq) <- gsub("\\[count\\]", "", names(fq)); names(fq) <- gsub("\\[pcRow\\]", " (% row)", names(fq)); names(fq) <- gsub("\\[pcCol\\]", " (% col)", names(fq)); names(fq) <- gsub("\\[expected\\]|\\[exp\\]", " (expected)", names(fq)); names(fq) <- gsub("\\.total", "Total", names(fq)); fq <- fq[, !grepl("^type", names(fq)), drop = FALSE]
                self$results$freqs$setContent(df_to_html(fq, digits = 4)) }
            t <- s$tests[[1]]; tbl <- self$results$tests; k <- 0
            add <- function(name, value, df, p) { k <<- k + 1; vals <- list(test = name, value = value, df = df, p = p); vals <- vals[!vapply(vals, function(v) is.null(v) || is.na(v), logical(1))]; tbl$addRow(rowKey = k, values = vals) }
            if (o$chiSq) add("\u03C7\u00B2", t$chi, t$df, t$p)
            if (o$chiSqCorr) add("\u03C7\u00B2 continuity correction", t$chiCorr, t$df, t$pCorr)
            if (o$likeRat) add("Likelihood ratio", t$lr, t$df, t$lrp)
            if (o$fisher) add("Fisher's exact test", t$fisher, NA, t$fisherp)
            add("N", t$N, NA, NA)
            nomv <- list(cont = t$cc, phi = t$phi, cra = t$cramer); nomv <- nomv[!vapply(nomv, function(v) is.null(v) || is.na(v), logical(1))]
            if (length(nomv)) self$results$nom$setRow(rowNo = 1, values = nomv)
            rep <- build_report_html(s, o$useLLM, o$backend, o$model, o$endpoint, interpret = o$interp, polish = o$polish, checkpoint = function() private$.checkpoint())
            self$results$report$setContent(rep$html)
            set_warnings(self, rep$notes)
        })
)
