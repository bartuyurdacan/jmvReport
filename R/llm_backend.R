# ---- Local LLM providers ---------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

default_endpoint <- function(backend = "ollama") {
  if (identical(backend, "openai")) "http://127.0.0.1:8080/v1" else "http://127.0.0.1:11434"
}

normalise_endpoint <- function(endpoint, backend) {
  endpoint <- trimws(endpoint %||% "")
  if (!nzchar(endpoint)) endpoint <- default_endpoint(backend)
  endpoint <- sub("/+$", "", endpoint)
  if (identical(backend, "openai") && !grepl("/v1$", endpoint)) endpoint <- paste0(endpoint, "/v1")
  endpoint
}

#' Check whether an Ollama server is reachable; returns list(ok, models, error)
#' @param endpoint Ollama server URL.
#' @param timeout Connection timeout in seconds.
#' @export
ollama_available <- function(endpoint = default_endpoint(), timeout = 3) {
  endpoint <- normalise_endpoint(endpoint, "ollama")
  h <- curl::new_handle(timeout = timeout, connecttimeout = timeout)
  r <- tryCatch(curl::curl_fetch_memory(paste0(endpoint, "/api/tags"), handle = h), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, models = character(), error = conditionMessage(r)))
  if (r$status_code != 200) return(list(ok = FALSE, models = character(), error = paste("HTTP", r$status_code)))
  js <- tryCatch(jsonlite::fromJSON(rawToChar(r$content)), error = function(e) NULL)
  models <- if (!is.null(js$models) && length(js$models)) as.character(js$models$name) else character()
  list(ok = TRUE, models = models, error = NULL)
}

model_installed <- function(model, models) length(models) > 0 && model %in% models || paste0(model, ":latest") %in% models

llm_resolve_backend <- function(backend = "auto", model = "qwen3.5:4b", endpoint = "", timeout = 3) {
  backend <- match.arg(tolower(backend), c("auto", "ollama", "builtin", "openai"))
  ollama_error <- NULL

  if (backend %in% c("auto", "ollama")) {
    ollama_endpoint <- normalise_endpoint(endpoint, "ollama")
    available <- ollama_available(ollama_endpoint, timeout)
    if (isTRUE(available$ok) && model_installed(model, available$models)) {
      return(list(ok = TRUE, type = "ollama", endpoint = ollama_endpoint, api_key = "",
                  model = model, label = "Ollama"))
    }
    ollama_error <- if (isTRUE(available$ok)) paste0("Model '", model, "' is not installed in Ollama.") else paste0("Ollama is unavailable: ", available$error)
    if (identical(backend, "ollama")) return(list(ok = FALSE, error = ollama_error))
  }

  if (backend %in% c("auto", "builtin")) {
    built <- start_builtin_server(timeout = max(30, timeout))
    if (isTRUE(built$ok)) return(c(list(type = "openai"), built))
    if (identical(backend, "builtin")) return(built)
    return(list(ok = FALSE, error = paste(c(ollama_error, built$error), collapse = " Built-in fallback: ")))
  }

  endpoint <- normalise_endpoint(endpoint, "openai")
  list(ok = TRUE, type = "openai", endpoint = endpoint, api_key = Sys.getenv("JMVREPORT_API_KEY", ""),
       model = model, label = "OpenAI-compatible server")
}

completion_stats <- function(prompt_tokens = 0, generated_tokens = 0, generated_seconds = 0,
                             wall_seconds = 0, backend = "Local AI", model = "") {
  list(calls = 1, prompt_tokens = as.numeric(prompt_tokens %||% 0), gen_tokens = as.numeric(generated_tokens %||% 0),
       gen_seconds = as.numeric(generated_seconds %||% 0), wall_seconds = as.numeric(wall_seconds %||% 0),
       backend = backend, model = model)
}

ollama_chat <- function(system, user, model = "qwen3.5:4b", endpoint = default_endpoint(),
                        temperature = 0.2, num_predict = 1100, timeout = 600) {
  endpoint <- normalise_endpoint(endpoint, "ollama")
  body <- jsonlite::toJSON(list(
    model = model, stream = FALSE, think = FALSE,
    messages = list(list(role = "system", content = system), list(role = "user", content = user)),
    options = list(temperature = temperature, num_predict = num_predict, num_ctx = 4096), keep_alive = "30m"
  ), auto_unbox = TRUE)
  h <- curl::new_handle(timeout = timeout, connecttimeout = 10)
  curl::handle_setopt(h, customrequest = "POST", postfields = body)
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  t0 <- Sys.time()
  r <- tryCatch(curl::curl_fetch_memory(paste0(endpoint, "/api/chat"), handle = h), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, error = conditionMessage(r)))
  txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"
  if (r$status_code != 200) return(list(ok = FALSE, error = paste("HTTP", r$status_code, substr(txt, 1, 200))))
  js <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  content <- if (!is.null(js)) js$message$content else NULL
  if (is.null(content) || !nzchar(content)) return(list(ok = FALSE, error = "empty response"))
  content <- gsub("<think>.*?</think>", "", content)
  content <- gsub("^```html\\s*|^```\\s*|```\\s*$", "", trimws(content))
  stats <- completion_stats(js$prompt_eval_count, js$eval_count, as.numeric(js$eval_duration %||% 0) / 1e9,
                            as.numeric(difftime(Sys.time(), t0, units = "secs")), "Ollama", model)
  list(ok = TRUE, text = trimws(content), stats = stats)
}

openai_chat <- function(system, user, model, endpoint, api_key = "", label = "OpenAI-compatible server",
                        temperature = 0.2, num_predict = 1100, timeout = 600) {
  endpoint <- normalise_endpoint(endpoint, "openai")
  body <- jsonlite::toJSON(list(
    model = model, stream = FALSE,
    messages = list(list(role = "system", content = system), list(role = "user", content = user)),
    temperature = temperature, max_tokens = num_predict
  ), auto_unbox = TRUE)
  h <- curl::new_handle(timeout = timeout, connecttimeout = 10)
  curl::handle_setopt(h, customrequest = "POST", postfields = body)
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  if (nzchar(api_key)) curl::handle_setheaders(h, Authorization = paste("Bearer", api_key))
  t0 <- Sys.time()
  r <- tryCatch(curl::curl_fetch_memory(paste0(endpoint, "/chat/completions"), handle = h), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, error = conditionMessage(r)))
  txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"
  if (r$status_code != 200) return(list(ok = FALSE, error = paste("HTTP", r$status_code, substr(txt, 1, 200))))
  js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  content <- if (!is.null(js) && length(js$choices)) js$choices[[1]]$message$content else NULL
  if (is.null(content) || !nzchar(content)) return(list(ok = FALSE, error = "empty response"))
  content <- gsub("<think>.*?</think>", "", content)
  content <- gsub("^```html\\s*|^```\\s*|```\\s*$", "", trimws(content))
  usage <- js$usage %||% list()
  stats <- completion_stats(usage$prompt_tokens, usage$completion_tokens, 0,
                            as.numeric(difftime(Sys.time(), t0, units = "secs")), label, model)
  list(ok = TRUE, text = trimws(content), stats = stats)
}

llm_chat <- function(connection, system, user, temperature = 0.2, num_predict = 1100, timeout = 600) {
  if (!isTRUE(connection$ok)) return(list(ok = FALSE, error = connection$error %||% "No AI backend is available."))
  if (identical(connection$type, "ollama")) return(ollama_chat(system, user, connection$model, connection$endpoint, temperature, num_predict, timeout))
  openai_chat(system, user, connection$model, connection$endpoint, connection$api_key %||% "",
              connection$label %||% "OpenAI-compatible server", temperature, num_predict, timeout)
}

stats_add <- function(a, b) {
  if (is.null(a)) return(b); if (is.null(b)) return(a)
  list(calls = a$calls + b$calls, prompt_tokens = a$prompt_tokens + b$prompt_tokens,
       gen_tokens = a$gen_tokens + b$gen_tokens, gen_seconds = a$gen_seconds + b$gen_seconds,
       wall_seconds = a$wall_seconds + b$wall_seconds, backend = b$backend %||% a$backend,
       model = b$model %||% a$model)
}

stats_line <- function(stats) {
  if (is.null(stats) || stats$calls == 0) return("")
  tps <- if (stats$gen_seconds > 0) stats$gen_tokens / stats$gen_seconds else NA
  sprintf("%s (%s): %d call(s), %s prompt + %s generated tokens, %s token/s, %s s.",
          html_escape(stats$backend %||% "Local AI"), html_escape(stats$model %||% ""), stats$calls,
          format(round(stats$prompt_tokens), big.mark = ","), format(round(stats$gen_tokens), big.mark = ","),
          if (is.na(tps)) "n/a" else formatC(tps, format = "f", digits = 1), round(stats$wall_seconds))
}

read_prompt <- function(name, lang = "en") {
  fn <- paste0(name, "_en.md")
  cands <- c(system.file("prompts", fn, package = "jmvReport"), file.path("inst", "prompts", fn), file.path("..", "..", "inst", "prompts", fn))
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands)) paste(readLines(cands[1], encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
}

split_tables <- function(html) {
  m <- regexpr("<p><b>|<table", html)
  if (m > 0) list(text = substr(html, 1, m - 1), rest = substr(html, m, nchar(html))) else list(text = html, rest = "")
}

ensure_p <- function(out) { if (!grepl("<p>", out, fixed = TRUE)) paste0("<p>", gsub("\n{2,}", "</p><p>", out), "</p>") else out }

results_digest <- function(results_html) {
  txt <- strip_html(split_tables(results_html)$text)
  txt <- gsub("[ \t]+", " ", txt); txt <- gsub("\n{3,}", "\n\n", txt)
  trimws(txt)
}

llm_interpret_only <- function(results_html, lang = "en", model = "qwen3.5:4b", endpoint = "",
                               timeout = 600, backend = "auto", connection = NULL) {
  sys_prompt <- read_prompt("system"); task <- read_prompt("interpret")
  if (!nzchar(sys_prompt) || !nzchar(task)) return(list(interp = NULL, note = "prompt file missing", stats = NULL))
  if (is.null(connection)) connection <- llm_resolve_backend(backend, model, endpoint)
  r <- llm_chat(connection, sys_prompt, paste0(task, "\n\n<<<\n", results_digest(results_html), "\n>>>"),
                timeout = timeout, num_predict = 650, temperature = 0.1)
  if (!isTRUE(r$ok)) return(list(interp = NULL, note = r$error, stats = NULL))
  plain <- gsub("<[^>]+>", "", gsub("<<<[^>]*>>>", "", r$text))
  plain <- gsub("&lt;", "<", plain, fixed = TRUE); plain <- gsub("&gt;", ">", plain, fixed = TRUE); plain <- gsub("&amp;", "&", plain, fixed = TRUE)
  cand <- paste0("<p>", paste(html_escape(trimws(strsplit(trimws(plain), "\n{2,}")[[1]])), collapse = "</p><p>"), "</p>")
  fid <- check_fidelity(cand, results_html, allow_missing_ratio = 1)
  if (!fid$ok) return(list(interp = NULL, note = paste0("interpretation fidelity: ", fid$reason), stats = r$stats))
  list(interp = cand, note = NULL, stats = r$stats)
}

llm_polish_interpret <- function(results_html, lang = "en", model = "qwen3.5:4b", endpoint = "",
                                 interpret = TRUE, timeout = 600, backend = "auto", connection = NULL) {
  sp <- split_tables(results_html)
  sys_prompt <- read_prompt("system"); task <- read_prompt(if (interpret) "combined" else "polish")
  if (!nzchar(sys_prompt) || !nzchar(task)) return(list(html = results_html, interp = NULL, used = FALSE, note = "prompt file missing", stats = NULL))
  if (is.null(connection)) connection <- llm_resolve_backend(backend, model, endpoint)
  r <- llm_chat(connection, sys_prompt, paste0(task, "\n\n<<<\n", sp$text, "\n>>>"),
                timeout = timeout, num_predict = if (interpret) 1400 else 1000)
  if (!isTRUE(r$ok)) return(list(html = results_html, interp = NULL, used = FALSE, note = r$error, stats = NULL))
  out <- r$text; notes <- character(); interp <- NULL
  if (interpret) {
    parts <- strsplit(out, "<<<RESULTS>>>", fixed = TRUE)[[1]]
    if (length(parts) == 1) parts <- strsplit(out, "<<<[A-Z]+>>>", perl = TRUE)[[1]]
    if (length(parts) >= 2) {
      cand <- ensure_p(trimws(parts[1])); fid2 <- check_fidelity(cand, sp$text, allow_missing_ratio = 1)
      if (fid2$ok) interp <- cand else notes <- c(notes, paste0("interpretation fidelity: ", fid2$reason))
      polished_raw <- trimws(paste(parts[-1], collapse = " "))
    } else { notes <- c(notes, "marker missing in model output"); polished_raw <- "" }
  } else polished_raw <- out
  used <- FALSE; polished <- results_html
  if (nzchar(polished_raw)) {
    cand <- ensure_p(polished_raw); fid <- check_fidelity(cand, sp$text)
    if (fid$ok) { polished <- paste0(cand, sp$rest); used <- TRUE } else notes <- c(notes, paste0("fidelity: ", fid$reason))
  }
  list(html = polished, interp = interp, used = used, note = if (length(notes)) paste(notes, collapse = "; ") else NULL, stats = r$stats)
}

llm_report <- function(results_html, connection, interpret = TRUE, polish = FALSE, timeout = 600) {
  if (!polish) {
    if (!interpret) return(list(html = results_html, interp = NULL, used = FALSE, note = NULL, stats = NULL))
    r <- llm_interpret_only(results_html, timeout = timeout, connection = connection)
    return(list(html = results_html, interp = r$interp, used = FALSE, note = r$note, stats = r$stats))
  }
  llm_polish_interpret(results_html, interpret = interpret, timeout = timeout, connection = connection)
}

llm_polish <- function(template_html, kind = c("results", "method"), lang = "en", model = "qwen3.5:4b",
                       endpoint = "", timeout = 600, backend = "auto") {
  r <- llm_polish_interpret(template_html, model = model, endpoint = endpoint, interpret = FALSE, timeout = timeout, backend = backend)
  list(html = r$html, used = r$used, note = r$note, stats = r$stats)
}

llm_interpret <- function(results_html, lang = "en", model = "qwen3.5:4b", endpoint = "",
                          timeout = 600, backend = "auto") {
  r <- llm_interpret_only(results_html, model = model, endpoint = endpoint, timeout = timeout, backend = backend)
  if (is.null(r$interp)) list(ok = FALSE, note = r$note, stats = r$stats) else list(ok = TRUE, html = r$interp, stats = r$stats)
}
