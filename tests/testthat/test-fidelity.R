test_that("fidelity checks accept faithful rewrites and reject changed numbers", {
  ref <- "<p>The groups did not differ, t(58) = 1.92, p = .060, d = 0.49.</p>"
  ok <- "<p>The independent-samples t test found no significant group difference, t(58) = 1.92, p = .060, d = 0.49.</p>"
  bad <- "<p>The groups did not differ, t(58) = 1.93, p = .060, d = 0.49.</p>"
  dropped <- "<p>The groups did not differ, t(58) = 1.92.</p>"
  expect_true(check_fidelity(ok, ref)$ok)
  expect_false(check_fidelity(bad, ref)$ok)
  expect_false(check_fidelity(dropped, ref)$ok)
})

test_that("combined AI output is split at the English marker", {
  ref <- "<p>No difference was found, t(58) = 1.92, p = .060.</p>"
  connection <- list(ok = TRUE, type = "openai", endpoint = "http://unused/v1", model = "test")
  fake <- function(out) {
    assign("llm_chat", function(...) list(
      ok = TRUE, text = out,
      stats = completion_stats(10, 5, 1, 1, "Test backend", "test")
    ), envir = globalenv())
  }
  old <- get("llm_chat", envir = globalenv())
  on.exit(assign("llm_chat", old, envir = globalenv()))

  fake("<p>The effect may be small.</p>\n<<<RESULTS>>>\n<p>The independent t test found no difference, t(58) = 1.92, p = .060.</p>")
  r <- llm_polish_interpret(ref, interpret = TRUE, connection = connection)
  expect_true(r$used)
  expect_true(grepl("t\\(58\\) = 1.92", r$html))
  expect_true(grepl("effect may be small", r$interp))
  expect_equal(r$stats$calls, 1)

  fake("<p>The independent t test found no difference, t(58) = 1.92, p = .060.</p>")
  r2 <- llm_polish_interpret(ref, interpret = TRUE, connection = connection)
  expect_false(r2$used)
  expect_null(r2$interp)
  expect_true(grepl("missing", r2$note))

  fake("<p>Interpretation: no difference was detected.</p>")
  r3 <- llm_interpret_only(ref, connection = connection)
  expect_true(grepl("no difference", r3$interp))

  fake("<p>Interpretation: t = 2.50.</p><<<RESULTS>>><p>t(58) = 1.93, p = .060.</p>")
  r4 <- llm_polish_interpret(ref, interpret = TRUE, connection = connection)
  expect_false(r4$used)
  expect_equal(r4$html, ref)
  expect_null(r4$interp)
})
