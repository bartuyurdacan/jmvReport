make_ttest_summary <- function() list(
  type = "ttestIS", title = "Independent Samples T-Test", supported = TRUE, group = "supp", vars = "len",
  rows = list(list(var = "len",
    student = list(t = 1.915268, df = 58, p = 0.0603934, md = 3.7, cil = -0.167, ciu = 7.567, es = 0.4945, esType = "Cohen's d"),
    welch = list(t = 1.915268, df = 55.30943, p = 0.0606345, md = 3.7, cil = -0.171, ciu = 7.571, es = 0.4945),
    mann = list(U = 324.5, p = 0.0644907, es = -0.2789, md = NA),
    g1 = list(name = "OJ", n = 30, m = 20.66333, mdn = 22.7, sd = 6.605561),
    g2 = list(name = "VC", n = 30, m = 16.96333, mdn = 16.5, sd = 8.266029),
    norm = list(w = 0.9694896, p = 0.1377619), levene = list(F = 1.097334, df1 = 1, df2 = 58, p = 0.2991977))),
  opts = list(students = TRUE, welchs = TRUE, mann = TRUE, effectSize = TRUE, ci = TRUE, ciWidth = 95, norm = TRUE, eqv = TRUE, hypothesis = "different"),
  tables = list())

test_that("Turkish and English t-test templates contain the key statistics", {
  s <- make_ttest_summary()
  tr <- render_results_text(s, "tr"); en <- render_results_text(s, "en")
  for (txt in list(tr, en)) {
    expect_true(grepl("<i>t</i>\\(58\\) = 1.92", txt))
    expect_true(grepl("<i>p</i> = .060", txt))
    expect_true(grepl("Cohen's <i>d</i> = 0.49", txt))
    expect_true(grepl("<i>U</i> = 324.5", txt))
    expect_true(grepl("<i>M</i> = 20.66, <i>SD</i> = 6.61", txt))
  }
  expect_true(grepl("anlamlı bir fark bulunmamıştır", tr))
  expect_true(grepl("no statistically significant difference", en))
  expect_true(grepl("%95 GA", tr)); expect_true(grepl("95% CI", en))
})

test_that("method paragraph mentions tests and software", {
  s <- make_ttest_summary()
  m <- render_method_text(list(s), "tr", jamovi_version = "2.7")
  expect_true(grepl("Welch t-testi", m)); expect_true(grepl("Levene", m)); expect_true(grepl("jamovi 2.7", m))
  m2 <- render_method_text(list(s), "en", jamovi_version = "2.7")
  expect_true(grepl("Mann-Whitney U test", m2)); expect_true(grepl("&lt; .05", m2))
})

test_that("generic fallback renders tables for unsupported analyses", {
  s <- list(type = "foo", title = "Foo", supported = FALSE, opts = list(), tables = list(a = list(title = "T", df = data.frame(x = 1:2, y = c("a", "b")))))
  out <- render_results_text(s, "en", index = 3)
  expect_true(grepl("<h3>3. Foo</h3>", out)); expect_true(grepl("<table", out))
})

test_that("ollama_available fails gracefully on a dead endpoint", {
  r <- ollama_available("http://127.0.0.1:1", timeout = 1)
  expect_false(r$ok)
})
