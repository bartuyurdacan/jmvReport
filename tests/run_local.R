# Ad-hoc runner used during development inside jamovi's own R
.libPaths(c("/app/lib/jamovi/modules/jmv/R", "/app/lib/R/library"))
setwd(Sys.getenv("JMVREPORT_DIR", "~/jmvReport"))
for (f in list.files("R", full.names = TRUE)) source(f, encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
file <- if (length(args)) args[1] else "/app/lib/R/library/jmvReadWrite/extdata/ToothGrowth.omv"
lang <- if (length(args) > 1) args[2] else "tr"
useLLM <- if (length(args) > 2) as.logical(args[3]) else FALSE
r <- report_from_omv(file, lang = lang, useLLM = useLLM)
if (!isTRUE(r$ok)) { cat("ERROR:", r$error, "\n"); quit(status = 1) }
cat("=== INFO\n", strip_html(r$info), "\n")
cat("=== METHOD\n", strip_html(r$method), "\n")
cat("=== RESULTS\n", strip_html(r$results), "\n")
cat("=== LIST\n"); print(r$list_df)
cat("=== WARNINGS\n", paste(r$warnings, collapse = "\n"), "\n")
cat("=== elapsed", r$elapsed, "\n")
out <- Sys.getenv("JMVREPORT_OUT", "")
if (nzchar(out)) writeLines(paste0("<html><meta charset='utf-8'><body>", r$info, "<h2>Method</h2>", r$method, "<h2>Results</h2>", r$results, "</body></html>"), out)
