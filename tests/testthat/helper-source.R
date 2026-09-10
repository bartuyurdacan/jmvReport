# Source the pure-R core during development. In R CMD check, copy the installed
# namespace into the test environment because package source files are absent.
pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
core_files <- c(
  "format_apa.R", "i18n.R", "templates.R", "method_writer.R", "fidelity.R",
  "llm_runtime.R", "llm_backend.R", "run_syntax.R", "parse_html.R",
  "find_omv.R", "extract.R", "report.R", "jmv_loader.R"
)

if (file.exists(file.path(pkg_root, "R", core_files[[1]]))) {
  for (f in core_files) source(file.path(pkg_root, "R", f), encoding = "UTF-8", local = FALSE)
} else {
  ns <- asNamespace("jmvReport")
  for (name in ls(ns, all.names = TRUE)) {
    value <- get(name, envir = ns, inherits = FALSE)
    if (is.function(value) && !is.primitive(value)) environment(value) <- globalenv()
    assign(name, value, envir = globalenv())
  }
}
