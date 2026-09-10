aiAnovaOneWClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aiAnovaOneWClass",
    inherit = aiAnovaOneWBase,
    private = list(
        .run = function() {
            if (length(self$options$deps) == 0 || is.null(self$options$group)) return()
            if (!ensure_jmv()) { self$results$report$setContent("<p style='color:#b00'>jmv package not available</p>"); return() }
            o <- self$options; lang <- "en"
            res <- jmv::anovaOneW(data = self$data, deps = o$deps, group = o$group, welchs = o$welchs, fishers = o$fishers,
                                  desc = TRUE, norm = TRUE, eqv = TRUE, phMethod = o$phMethod, phMeanDif = TRUE, phTest = TRUE)
            private$.checkpoint()
            fill_table(self$results$anova, table_df(res$anova))
            fill_table(self$results$desc, table_df(res$desc))
            fill_table(self$results$norm, table_df(res$assump$norm))
            fill_table(self$results$eqv, table_df(res$assump$eqv))
            s <- summarize_results(res); s$opts$norm <- o$norm; s$opts$eqv <- o$eqv
            summaries <- list(s)
            ph_html <- ""
            for (r in s$rows) if (length(r$posthoc)) {
                df <- do.call(rbind, lapply(r$posthoc, function(p) data.frame(a = p$a, b = p$b, `Mean diff.` = fmt_num(p$md), t = fmt_num(p$t), df = fmt_df(p$df), p = strip_html(fmt_p(p$p)), check.names = FALSE, stringsAsFactors = FALSE)))
                ph_html <- paste0(ph_html, "<p><b>", html_escape(r$dep), " \u2014 ", ph_method_name(o$phMethod, lang), "</b></p>", df_to_html(df))
            }
            if (isTRUE(o$kruskal)) {
                kw <- if ("pairsDunn" %in% names(formals(jmv::anovaNP))) jmv::anovaNP(data = self$data, deps = o$deps, group = o$group, es = TRUE, pairsDunn = TRUE) else jmv::anovaNP(data = self$data, deps = o$deps, group = o$group, es = TRUE, pairs = TRUE)
                fill_table(self$results$kw, table_df(kw$table))
                sk <- summarize_results(kw); summaries[[2]] <- sk
                for (r in sk$rows) {
                    if (length(r$dunn)) {
                        df <- do.call(rbind, lapply(r$dunn, function(p) data.frame(a = p$a, b = p$b, z = fmt_num(p$z), p = strip_html(fmt_p(p$p)), `p (Bonferroni)` = strip_html(fmt_p(p$padj)), check.names = FALSE, stringsAsFactors = FALSE)))
                        ph_html <- paste0(ph_html, "<p><b>", html_escape(r$dep), " \u2014 Dunn</b></p>", df_to_html(df))
                    } else if (length(r$dscf)) {
                        df <- do.call(rbind, lapply(r$dscf, function(p) data.frame(a = p$a, b = p$b, W = fmt_num(p$W), p = strip_html(fmt_p(p$p)), check.names = FALSE, stringsAsFactors = FALSE)))
                        ph_html <- paste0(ph_html, "<p><b>", html_escape(r$dep), " \u2014 DSCF</b></p>", df_to_html(df))
                    }
                }
            }
            self$results$postHoc$setContent(ph_html)
            rep <- build_report_html(summaries, o$useLLM, o$backend, o$model, o$endpoint, interpret = o$interp, polish = o$polish, checkpoint = function() private$.checkpoint())
            self$results$report$setContent(rep$html)
            set_warnings(self, rep$notes)
        })
)
