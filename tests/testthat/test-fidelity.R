test_that("fidelity check accepts faithful rewrites and rejects new numbers", {
  ref <- "<p>Gruplar arasında fark bulunmamıştır, t(58) = 1.92, p = .060, d = 0.49.</p>"
  ok <- "<p>Bağımsız örneklemler t-testi gruplar arasında anlamlı bir fark göstermemiştir, t(58) = 1.92, p = .060, d = 0.49.</p>"
  bad <- "<p>Gruplar arasında fark bulunmamıştır, t(58) = 1.93, p = .060, d = 0.49.</p>"
  expect_true(check_fidelity(ok, ref)$ok)
  expect_false(check_fidelity(bad, ref)$ok)
  dropped <- "<p>Gruplar arasında fark bulunmamıştır, t(58) = 1.92.</p>"
  expect_false(check_fidelity(dropped, ref)$ok)
})

test_that("combined output is split at the interpretation marker (with fallbacks)", {
  ref <- "<p>Fark bulunmamıştır, t(58) = 1.92, p = .060.</p>"
  fake <- function(out) { assign("ollama_chat", function(...) list(ok = TRUE, text = out, stats = list(calls = 1, prompt_tokens = 10, gen_tokens = 5, gen_seconds = 1, wall_seconds = 1)), envir = globalenv()) }
  old <- get("ollama_chat", envir = globalenv())
  on.exit(assign("ollama_chat", old, envir = globalenv()))
  fake("<p>Fark anlamlı değildir; etki küçük olabilir.</p>\n<<<BULGULAR>>>\n<p>Bağımsız t-testi fark göstermemiştir, t(58) = 1.92, p = .060.</p>")
  r <- llm_polish_interpret(ref, "tr", interpret = TRUE)
  expect_true(r$used); expect_true(grepl("t\\(58\\) = 1.92", r$html)); expect_true(grepl("anlamlı değildir", r$interp)); expect_equal(r$stats$calls, 1)
  fake("<p>Bağımsız t-testi fark göstermemiştir, t(58) = 1.92, p = .060.</p>")
  r2 <- llm_polish_interpret(ref, "tr", interpret = TRUE)
  expect_false(r2$used); expect_null(r2$interp); expect_true(grepl("missing", r2$note))
  fake("<p>Yorum: fark yok.</p>")
  r4 <- llm_interpret_only(ref, "tr")
  expect_true(grepl("fark yok", r4$interp))
  fake("<p>Yorum; t = 2.50.</p><<<BULGULAR>>><p>t(58) = 1.93, p = .060.</p>")
  r3 <- llm_polish_interpret(ref, "tr", interpret = TRUE)
  expect_false(r3$used); expect_equal(r3$html, ref); expect_null(r3$interp)
})
