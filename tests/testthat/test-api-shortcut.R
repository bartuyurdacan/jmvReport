test_that("OpenAI API shortcut uses the official endpoint and preset model", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old))
  Sys.setenv(OPENAI_API_KEY = "test-key")

  connection <- llm_resolve_backend("openai_api", model = "ignored", endpoint = "http://ignored")

  expect_true(connection$ok)
  expect_equal(connection$endpoint, "https://api.openai.com/v1")
  expect_equal(connection$model, "gpt-4.1-mini")
  expect_equal(connection$api_key, "test-key")
  expect_equal(connection$label, "OpenAI API")
})

test_that("OpenAI API shortcut reports a missing key before making a request", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old))
  Sys.unsetenv("OPENAI_API_KEY")

  connection <- llm_resolve_backend("openai_api")

  expect_false(connection$ok)
  expect_match(connection$error, "OPENAI_API_KEY", fixed = TRUE)
})
