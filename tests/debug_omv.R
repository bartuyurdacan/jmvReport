.libPaths(c("/app/lib/jamovi/modules/jmv/R", "/app/lib/R/library"))
setwd("~/jmvReport"); for (f in list.files("R", full.names = TRUE)) source(f, encoding = "UTF-8")
for (f in c("bfi_sample2", "bfi_sample", "AlbumSales")) {
  o <- read_omv_all(paste0("/app/lib/R/library/jmvReadWrite/extdata/", f, ".omv"))
  cat("\n#####", f, " nsyn=", length(o$syntax), " html nchar=", nchar(o$html), "\n")
  for (s in o$syntax) cat(" SYN:", substr(s, 1, 160), "\n")
  cat(" H1:", paste(html_titles(o$html), collapse = " | "), "\n")
  ht <- html_tables(o$html); cat(" tables:", length(ht), "\n")
  for (t in head(ht, 4)) { cat("  -", t$analysis, "/", t$title, " dims", nrow(t$df), "x", ncol(t$df), " cols:", paste(names(t$df), collapse = ","), "\n") }
  if (f == "AlbumSales") cat(substr(gsub("\\s+", " ", o$html), 1, 1500), "\n")
}
