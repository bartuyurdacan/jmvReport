# ---- Local LLM backend (Ollama) --------------------------------------------

default_endpoint <- function() "http://localhost:11434"

#' Check whether an Ollama server is reachable; returns list(ok, models, error)
#' @export
ollama_available <- function(endpoint = default_endpoint(), timeout = 3) {
  endpoint <- sub("/+$", "", endpoint)
  h <- curl::new_handle(timeout = timeout, connecttimeout = timeout)
  r <- tryCatch(curl::curl_fetch_memory(paste0(endpoint, "/api/tags"), handle = h), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, models = character(), error = conditionMessage(r)))
  if (r$status_code != 200) return(list(ok = FALSE, models = character(), error = paste("HTTP", r$status_code)))
  js <- tryCatch(jsonlite::fromJSON(rawToChar(r$content)), error = function(e) NULL)
  models <- if (!is.null(js$models) && length(js$models)) as.character(js$models$name) else character()
  list(ok = TRUE, models = models, error = NULL)
}

#' Chat completion via Ollama (/api/chat, non-streaming). Returns text or error.
ollama_chat <- function(system, user, model = "qwen3.5:4b", endpoint = default_endpoint(), temperature = 0.2, num_predict = 1500, timeout = 600) {
  endpoint <- sub("/+$", "", endpoint)
  body <- jsonlite::toJSON(list(
    model = model, stream = FALSE, think = FALSE,
    messages = list(list(role = "system", content = system), list(role = "user", content = user)),
    options = list(temperature = temperature, num_predict = num_predict), keep_alive = "10m"
  ), auto_unbox = TRUE)
  h <- curl::new_handle(timeout = timeout, connecttimeout = 10)
  curl::handle_setopt(h, customrequest = "POST", postfields = body)
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  r <- tryCatch(curl::curl_fetch_memory(paste0(endpoint, "/api/chat"), handle = h), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, error = conditionMessage(r)))
  txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"
  if (r$status_code != 200) return(list(ok = FALSE, error = paste("HTTP", r$status_code, substr(txt, 1, 200))))
  js <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  content <- js$message$content
  if (is.null(content) || !nzchar(content)) return(list(ok = FALSE, error = "empty response"))
  content <- gsub("<think>.*?</think>", "", content)      # safety: strip thinking blocks
  content <- gsub("^```html\\s*|^```\\s*|```\\s*$", "", trimws(content))
  list(ok = TRUE, text = trimws(content), eval_duration = js$eval_duration, eval_count = js$eval_count)
}

read_prompt <- function(name, lang) {
  f <- system.file("prompts", paste0(name, "_", lang, ".md"), package = "jmvReport")
  if (!nzchar(f) || !file.exists(f)) f <- file.path("inst", "prompts", paste0(name, "_", lang, ".md"))
  if (file.exists(f)) paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
}

#' Split rendered HTML into the narrative part (polished) and trailing tables (kept verbatim)
split_tables <- function(html) {
  m <- regexpr("<p><b>|<table", html)
  if (m > 0) list(text = substr(html, 1, m - 1), rest = substr(html, m, nchar(html))) else list(text = html, rest = "")
}

#' Polish template HTML with the LLM; fall back to the template on any failure
llm_polish <- function(template_html, kind = c("results", "method"), lang = "tr", model = "qwen3.5:4b", endpoint = default_endpoint(), timeout = 600) {
  kind <- match.arg(kind)
  sp <- split_tables(template_html)
  if (nzchar(sp$rest)) {
    r <- llm_polish(sp$text, kind, lang, model, endpoint, timeout)
    r$html <- paste0(r$html, sp$rest)
    return(r)
  }
  sys_prompt <- read_prompt("system", lang)
  if (!nzchar(sys_prompt)) return(list(html = template_html, used = FALSE, note = "prompt file missing"))
  task <- read_prompt(kind, lang)
  user <- paste0(task, "\n\n<<<\n", template_html, "\n>>>")
  r <- ollama_chat(sys_prompt, user, model = model, endpoint = endpoint, timeout = timeout)
  if (!isTRUE(r$ok)) return(list(html = template_html, used = FALSE, note = r$error))
  out <- r$text
  if (!grepl("<p>", out, fixed = TRUE)) out <- paste0("<p>", gsub("\n{2,}", "</p><p>", out), "</p>")
  fid <- check_fidelity(out, template_html)
  if (!fid$ok) return(list(html = template_html, used = FALSE, note = paste0("fidelity: ", fid$reason)))
  list(html = out, used = TRUE, note = NULL, eval_count = r$eval_count)
}
