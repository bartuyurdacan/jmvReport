# ---- Locate and load the jmv package that ships with jamovi -----------------

jmv_candidate_paths <- function() {
  libdir <- tryCatch(dirname(system.file(package = "jmvcore")), error = function(e) "")
  home <- Sys.getenv("HOME", unset = path.expand("~"))
  cands <- c(
    Sys.getenv("JAMOVI_JMV_LIB", unset = ""),
    if (nzchar(libdir)) c(file.path(dirname(dirname(libdir)), "jamovi", "modules", "jmv", "R"),
                          file.path(dirname(libdir), "modules", "jmv", "R"),
                          file.path(dirname(dirname(dirname(libdir))), "jamovi", "modules", "jmv", "R"),
                          file.path(dirname(dirname(dirname(libdir))), "Resources", "modules", "jmv", "R")),
    "/app/lib/jamovi/modules/jmv/R",
    file.path(home, ".var", "app", "org.jamovi.jamovi", "data", "jamovi", "modules", "jmv", "R"),
    file.path(home, ".jamovi", "modules", "jmv", "R"),
    file.path(home, "Library", "Application Support", "jamovi", "modules", "jmv", "R"),
    file.path(Sys.getenv("APPDATA", unset = ""), "jamovi", "modules", "jmv", "R"),
    "C:/Program Files/jamovi/Resources/modules/jmv/R",
    "/Applications/jamovi.app/Contents/Resources/modules/jmv/R"
  )
  cands <- cands[nzchar(cands)]
  unique(cands[dir.exists(cands)])
}

#' Try to make jmv loadable. Returns TRUE/FALSE.
ensure_jmv <- function() {
  if (isNamespaceLoaded("jmv")) return(TRUE)
  for (p in jmv_candidate_paths()) {
    ok <- tryCatch({ .libPaths(c(p, .libPaths())); suppressWarnings(requireNamespace("jmv", quietly = TRUE)) }, error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
  }
  suppressWarnings(requireNamespace("jmv", quietly = TRUE))
}

jamovi_version_guess <- function() {
  cands <- c("/app/lib/jamovi/version", file.path(dirname(dirname(dirname(system.file(package = "jmvcore")))), "jamovi", "version"))
  for (f in cands) if (file.exists(f)) { v <- tryCatch(trimws(readLines(f, warn = FALSE)[1]), error = function(e) NA); if (!is.na(v) && nzchar(v)) return(v) }
  v <- tryCatch(as.character(utils::packageVersion("jmvcore")), error = function(e) NA)
  if (!is.na(v)) paste0("(jmvcore ", v, ")") else "2.x"
}
