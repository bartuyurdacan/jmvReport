aiCorrClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aiCorrClass",
    inherit = aiCorrBase,
    private = list(
        .run = function() {
            if (length(self$options$vars) < 2) return()
            if (!ensure_jmv()) { self$results$report$setContent("<p style='color:#b00'>jmv package not available</p>"); return() }
            o <- self$options
            res <- jmv::corrMatrix(data = self$data, vars = o$vars, pearson = o$pearson, spearman = o$spearman, kendall = o$kendall, n = TRUE, ci = o$ci)
            private$.checkpoint()
            s <- summarize_results(res)
            tbl <- self$results$pairs
            for (i in seq_along(s$pairs)) { p <- s$pairs[[i]]
                vals <- list(a = p$a, b = p$b, n = p$n, r = p$r, rcil = p$rcil, rciu = p$rciu, rp = p$rp, rho = p$rho, rhop = p$rhop, tau = p$tau, taup = p$taup)
                vals <- vals[!vapply(vals, function(v) is.null(v) || is.na(v), logical(1))]
                tbl$addRow(rowKey = i, values = vals) }
            rep <- build_report_html(s, o$useLLM, o$backend, o$model, o$endpoint, interpret = o$interp, polish = o$polish, checkpoint = function() private$.checkpoint())
            self$results$report$setContent(rep$html)
            set_warnings(self, rep$notes)
        })
)
