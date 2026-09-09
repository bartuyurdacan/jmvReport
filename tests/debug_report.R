.libPaths(c("/home/fbartuyurdacan/jmvReport/build/R4.5.0-x64-linux", "/app/lib/jamovi/modules/jmv/R", "/app/lib/R/library"))
suppressMessages(library(jmvReport))
h <- function(e) { calls <- sys.calls(); for (k in seq_along(calls)) cat(k, ":", substr(paste(deparse(calls[[k]]), collapse = " "), 1, 200), "\n"); cat("MSG:", conditionMessage(e), "\n"); quit(status = 1) }
withCallingHandlers(jmvReport::reportOmv(data = data.frame(x = 1), file = "/app/lib/R/library/jmvReadWrite/extdata/ToothGrowth.omv", run = TRUE, useLLM = FALSE, lang = "en", secTables = TRUE), error = h)
