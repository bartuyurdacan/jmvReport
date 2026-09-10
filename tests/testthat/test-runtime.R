test_that("runtime platform mapping is explicit", {
  expect_equal(runtime_platform("Linux", "x86_64"), "linux-x86_64")
  expect_equal(runtime_platform("Darwin", "arm64"), "macos-aarch64")
  expect_equal(runtime_platform("Windows", "AMD64"), "windows-x86_64")
  expect_true(is.na(runtime_platform("Plan9", "x86_64")))
  expect_true(is.na(runtime_platform("Linux", "riscv64")))
})

test_that("managed downloads are pinned", {
  manifest <- runtime_manifest()
  hashes <- c(vapply(manifest$assets, `[[`, character(1), "sha256"), manifest$model$sha256)
  expect_true(all(grepl("^[0-9a-f]{64}$", hashes)))
  expect_equal(manifest$model$bytes, 2385660192)
  expect_match(manifest$model$url, "3e0455024fadf0eee0cec0f910582de99aaae986", fixed = TRUE)
})

test_that("OpenAI-compatible endpoints are normalized", {
  expect_equal(normalise_endpoint("http://127.0.0.1:8080", "openai"), "http://127.0.0.1:8080/v1")
  expect_equal(normalise_endpoint("http://127.0.0.1:8080/v1/", "openai"), "http://127.0.0.1:8080/v1")
  expect_equal(normalise_endpoint("", "ollama"), "http://127.0.0.1:11434")
})

test_that("explicit OpenAI-compatible selection does not perform a network probe", {
  old <- Sys.getenv("JMVREPORT_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("JMVREPORT_API_KEY") else Sys.setenv(JMVREPORT_API_KEY = old))
  Sys.setenv(JMVREPORT_API_KEY = "secret")
  connection <- llm_resolve_backend("openai", model = "test-model", endpoint = "http://localhost:9000")
  expect_true(connection$ok)
  expect_equal(connection$endpoint, "http://localhost:9000/v1")
  expect_equal(connection$api_key, "secret")
})
