test_that("jmv syntax is parsed", {
  ps <- parse_syntax('jmv::ttestIS(formula = len ~ supp, data = data, welchs = TRUE, effectSize = TRUE)')
  expect_equal(ps$fn, "ttestIS"); expect_equal(ps$ns, "jmv")
  expect_true("welchs" %in% names(ps$args))
  ps2 <- parse_syntax("X = cov(data[2:6])\nX")
  expect_true(is.na(ps2$ns))
  expect_null(parse_syntax("this is not R ((("))
})

test_that("HTML table fallback parses jamovi results html", {
  html <- '<html><body><h1>Results</h1><h1>Independent Samples T-Test</h1><table><thead><tr><th colspan="6"><span>Independent Samples T-Test</span></th></tr><tr><th colspan="2"></th><th colspan="2">Statistic</th><th colspan="2">p</th></tr></thead><tbody><tr><td>len</td><td></td><td>1.92</td><td></td><td>0.060</td><td></td></tr></tbody></table><h1>References</h1></body></html>'
  tabs <- html_tables(html)
  expect_equal(length(tabs), 1)
  expect_equal(tabs[[1]]$analysis, "Independent Samples T-Test")
  expect_equal(ncol(tabs[[1]]$df), 3)
  expect_equal(tabs[[1]]$df[[2]][1], "1.92")
  expect_equal(html_titles(html), "Independent Samples T-Test")
})
