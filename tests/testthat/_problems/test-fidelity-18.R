# Extracted from test-fidelity.R:18

# test -------------------------------------------------------------------------
ref <- "<p>Fark bulunmamıştır, t(58) = 1.92, p = .060.</p>"
fake <- function(out) { assign("ollama_chat", function(...) list(ok = TRUE, text = out, stats = list(calls = 1, prompt_tokens = 10, gen_tokens = 5, gen_seconds = 1, wall_seconds = 1)), envir = globalenv()) }
old <- get("ollama_chat", envir = globalenv())
on.exit(assign("ollama_chat", old, envir = globalenv()))
fake("<p>Bağımsız t-testi fark göstermemiştir, t(58) = 1.92, p = .060.</p>\n<<<YORUM>>>\n<p>Fark anlamlı değildir; etki küçük olabilir.</p>")
r <- llm_polish_interpret(ref, "tr", interpret = TRUE)
expect_true(r$used)
expect_true(grepl("t\\(58\\) = 1.92", r$html))
expect_true(grepl("anlamlı değildir", r$interp)); 
pect_equal(r$stats$calls, 1)
