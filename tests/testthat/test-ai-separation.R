test_that("AI interpretation and Results polish use independent calls", {
  ref <- "<p>The groups differed, F(2, 147) = 119.26, p < .001.</p>"
  connection <- list(ok = TRUE, type = "openai", endpoint = "http://unused/v1", model = "test")
  calls <- 0L
  old <- get("llm_chat", envir = globalenv())
  on.exit(assign("llm_chat", old, envir = globalenv()))
  assign("llm_chat", function(...) {
    calls <<- calls + 1L
    text <- if (calls == 1L) {
      "The groups clearly differed, although the scientific importance should be judged in context."
    } else {
      "<p>A group difference was found, F(2, 147) = 119.26, p < .001.</p>"
    }
    list(ok = TRUE, text = text, stats = completion_stats(10, 5, 1, 1, "Test backend", "test"))
  }, envir = globalenv())

  result <- llm_report(ref, connection, interpret = TRUE, polish = TRUE)

  expect_equal(calls, 2L)
  expect_match(result$interp, "scientific importance")
  expect_true(result$used)
  expect_match(result$html, "F\\(2, 147\\) = 119.26")
  expect_null(result$note)
  expect_equal(result$stats$calls, 2)
})

test_that("AI-only interpretation does not require a response marker", {
  ref <- "<p>No group difference was found, F(2, 147) = 0.30, p = .746.</p>"
  connection <- list(ok = TRUE, type = "openai", endpoint = "http://unused/v1", model = "test")
  old <- get("llm_chat", envir = globalenv())
  on.exit(assign("llm_chat", old, envir = globalenv()))
  assign("llm_chat", function(...) list(
    ok = TRUE,
    text = "The evidence does not support a group difference, but limited power remains possible.",
    stats = completion_stats(10, 5, 1, 1, "Test backend", "test")
  ), envir = globalenv())

  result <- llm_report(ref, connection, interpret = TRUE, polish = FALSE)

  expect_match(result$interp, "does not support")
  expect_identical(result$html, ref)
  expect_false(result$used)
  expect_null(result$note)
})
