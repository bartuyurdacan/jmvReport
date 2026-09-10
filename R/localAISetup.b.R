# This file is a generated template, your changes will not be overwritten

localAISetupClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "localAISetupClass",
    inherit = localAISetupBase,
    private = list(
        .run = function() {
            status <- runtime_status(verify = FALSE)
            paths <- if (isTRUE(status$supported)) status$paths else NULL

            if (!isTRUE(self$options$install)) {
                colour <- if (isTRUE(status$installed)) "#287a3b" else "#666"
                self$results$status$setContent(paste0(
                    "<p style='color:", colour, "'><b>", html_escape(status$message), "</b></p>",
                    if (!isTRUE(status$installed) && isTRUE(status$supported))
                        "<p>Select <b>Install built-in AI</b> to begin. No files are downloaded before you select it.</p>"
                    else ""
                ))
                if (!is.null(paths)) {
                    self$results$details$setContent(paste0(
                        "<p><b>Platform:</b> ", html_escape(status$platform), "</p>",
                        "<p><b>Storage:</b> ", html_escape(paths$root), "</p>"
                    ))
                }
                return()
            }

            private$.checkpoint()
            self$results$status$setContent("<p><b>Preparing the built-in AI installation\u2026</b></p>")
            result <- tryCatch(
                install_builtin_runtime(
                    force = isTRUE(self$options$repair),
                    progress = function(message) {
                        self$results$status$setContent(paste0("<p><b>", html_escape(message), "\u2026</b></p>"))
                        private$.checkpoint()
                    }
                ),
                error = function(e) e
            )

            if (inherits(result, "error")) {
                self$results$status$setContent(paste0(
                    "<p style='color:#b00'><b>Installation failed:</b> ",
                    html_escape(conditionMessage(result)), "</p>"
                ))
                return()
            }

            self$results$status$setContent(paste0(
                "<p style='color:#287a3b'><b>", html_escape(result$message), "</b></p>",
                "<p>You can now select <b>Built-in llama.cpp</b> or <b>Auto</b> in an AI Report analysis.</p>"
            ))
            self$results$details$setContent(paste0(
                "<p><b>Platform:</b> ", html_escape(result$platform), "</p>",
                "<p><b>Runtime:</b> ", html_escape(result$paths$executable), "</p>",
                "<p><b>Model:</b> ", html_escape(result$paths$model), "</p>"
            ))
        }
    )
)
