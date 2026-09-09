# This file is a generated template, your changes will not be overwritten

reportOmvClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "reportOmvClass",
    inherit = reportOmvBase,
    private = list(
        .init = function() {
            L <- i18n(self$options$lang)
            self$results$info$setTitle(L$info_title)
            self$results$method$setTitle(L$method_title)
            self$results$results$setTitle(L$results_title)
            self$results$warnings$setTitle(L$warnings_title)
            self$results$interp$setTitle(L$interp_title)
            self$results$list$setTitle(if (self$options$lang == "tr") "Dosyadaki analizler" else "Analyses in the file")
            self$results$tables$setTitle(if (self$options$lang == "tr") "Yeniden hesaplanan tablolar" else "Recomputed tables")
            self$results$list$getColumn("analysis")$setTitle(L$analysis)
            self$results$list$getColumn("variables")$setTitle(L$variables)
            self$results$list$getColumn("es")$setTitle(L$effect)
        },
        .run = function() {
            lang <- self$options$lang
            L <- i18n(lang)
            if (!isTRUE(self$options$run)) {
                self$results$info$setContent(paste0("<p>", if (lang == "tr")
                    "Analizlerinizi jamovi'de yapıp dosyayı kaydedin (.omv). Ardından seçenekleri ayarlayıp <b>Generate report</b> kutusunu işaretleyin. Dosya kutusu boşsa Belgeler/Masaüstü/İndirilenler içindeki en son kaydedilen .omv otomatik kullanılır."
                    else "Run your analyses in jamovi and save the file (.omv). Then set the options and tick <b>Generate report</b>. If the file box is empty, the most recently saved .omv in Documents/Desktop/Downloads is used automatically.", "</p>"))
                self$results$warnings$setVisible(FALSE)
                return()
            }
            sections <- c(if (self$options$secMethod) "method", if (self$options$secResults) "results", if (self$options$secInterp) "interpret")
            self_ <- self
            rep <- report_from_omv(
                file = self$options$file, lang = lang, useLLM = self$options$useLLM,
                model = self$options$model, endpoint = self$options$endpoint,
                sections = sections, alpha = self$options$alpha,
                checkpoint = function() private$.checkpoint(), llm_timeout = self$options$llmTimeout)
            if (!isTRUE(rep$ok)) {
                self$results$info$setContent(paste0("<p style='color:#b00'>", html_escape(rep$error), "</p>"))
                self$results$warnings$setVisible(FALSE)
                return()
            }
            self$results$info$setContent(rep$info)
            self$results$method$setContent(rep$method)
            self$results$results$setContent(rep$results)
            if (!is.null(rep$interp)) self$results$interp$setContent(paste0(rep$interp, "<p style='color:#777;font-size:90%'>", sprintf(L$interp_note, html_escape(self$options$model)), "</p>"))
            tbl <- self$results$list
            df <- rep$list_df
            for (i in seq_len(nrow(df))) {
                tbl$addRow(rowKey = i, values = list(index = df$index[i], analysis = df$analysis[i], type = df$type[i], variables = df$variables[i], stat = df$stat[i], p = df$p[i], es = df$es[i]))
            }
            if (self$options$secTables) {
                html <- ""
                for (i in seq_along(rep$summaries)) {
                    s <- rep$summaries[[i]]
                    html <- paste0(html, "<h3>", i, ". ", html_escape(s$title), "</h3>")
                    for (nm in names(s$tables)) html <- paste0(html, "<p><b>", html_escape(s$tables[[nm]]$title), "</b></p>", df_to_html(s$tables[[nm]]$df))
                }
                self$results$tables$setContent(html)
            }
            w <- rep$warnings[!is.na(rep$warnings) & nzchar(rep$warnings)]
            if (length(w)) {
                self$results$warnings$setContent(paste0("<ul>", paste0("<li>", html_escape(w), "</li>", collapse = ""), "</ul>"))
                self$results$warnings$setVisible(TRUE)
            } else self$results$warnings$setVisible(FALSE)
        })
)
