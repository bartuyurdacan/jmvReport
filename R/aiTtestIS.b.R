aiTtestISClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aiTtestISClass",
    inherit = aiTtestISBase,
    private = list(
        .run = function() {
            if (length(self$options$vars) == 0 || is.null(self$options$group)) return()
            if (!ensure_jmv()) { self$results$report$setContent("<p style='color:#b00'>jmv package not available</p>"); return() }
            o <- self$options
            res <- jmv::ttestIS(data = self$data, vars = o$vars, group = o$group, students = o$students, welchs = o$welchs, mann = o$mann,
                                effectSize = o$effectSize, ci = o$ci, ciWidth = o$ciWidth, meanDiff = TRUE, desc = TRUE, norm = TRUE, eqv = TRUE)
            private$.checkpoint()
            fill_table(self$results$ttest, table_df(res$ttest))
            fill_table(self$results$norm, table_df(res$assum$norm))
            fill_table(self$results$eqv, table_df(res$assum$eqv))
            fill_table(self$results$desc, table_df(res$desc))
            s <- summarize_results(res)
            s$opts$norm <- o$norm; s$opts$eqv <- o$eqv
            rep <- build_report_html(s, o$lang, o$useLLM, o$model, o$endpoint, interpret = o$interp, polish = o$polish, checkpoint = function() private$.checkpoint())
            self$results$report$setContent(rep$html)
            set_warnings(self, rep$notes)
        })
)
