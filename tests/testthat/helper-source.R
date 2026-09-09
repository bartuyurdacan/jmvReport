# Source the pure-R core (no jmvcore needed) for unit tests
pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
core_files <- c("format_apa.R", "i18n.R", "templates.R", "method_writer.R", "fidelity.R", "llm_backend.R", "run_syntax.R", "parse_html.R", "find_omv.R", "extract.R", "report.R", "jmv_loader.R")
for (f in core_files) source(file.path(pkg_root, "R", f), encoding = "UTF-8", local = FALSE)
