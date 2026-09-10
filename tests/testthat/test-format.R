test_that("APA number formatting", {
  expect_equal(fmt_num(3.14159), "3.14")
  expect_equal(fmt_num(-2.5), "−2.50")
  expect_equal(fmt_bounded(0.4945), ".49")
  expect_equal(fmt_bounded(-0.279), "−.28")
  expect_equal(fmt_p(0.0004), "&lt; .001")
  expect_equal(fmt_p(0.0604), "= .060")
  expect_equal(fmt_p(0.5), "= .500")
  expect_equal(fmt_df(58), "58")
  expect_equal(fmt_df(55.30943), "55.31")
  expect_equal(fmt_pct(45.766), "45.8%")
  expect_equal(pct_l(45.766, "en"), "45.8%")
})

test_that("effect size magnitudes", {
  expect_equal(es_magnitude(0.49, "d"), "small")
  expect_equal(es_magnitude(0.85, "d"), "large")
  expect_equal(es_magnitude(0.22, "etap"), "large")
  expect_equal(es_magnitude(0.05, "r"), "negligible")
  expect_equal(es_magnitude_v(0.17, 2), "small")
})

test_that("English joins and numeric token extraction work", {
  expect_equal(join_words(c("a", "b"), "en"), "a and b")
  toks <- num_tokens("<p>t(58) = 1.92, p = .060, d = 0.49, 95% CI [−0.17, 7.57]</p>")
  expect_true(all(c("58", "1.92", ".060", ".49", "95", ".17", "7.57") %in% toks))
})
