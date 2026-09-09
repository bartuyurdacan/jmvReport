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

model_installed <- function(model, models) length(models) == 0 || model %in% models || paste0(model, ":latest") %in% models

#' Chat completion via Ollama (/api/chat, non-streaming). Returns text + token stats.
ollama_chat <- function(system, user, model = "qwen3.5:4b", endpoint = default_endpoint(), temperature = 0.2, num_predict = 1100, timeout = 600) {
  endpoint <- sub("/+$", "", endpoint)
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
  content <- js$message$content
  if (is.null(content) || !nzchar(content)) return(list(ok = FALSE, error = "empty response"))
  content <- gsub("<think>.*?</think>", "", content)
  content <- gsub("^```html\\s*|^```\\s*|```\\s*$", "", trimws(content))
  num <- function(x) if (is.null(x)) 0 else as.numeric(x)
  stats <- list(calls = 1, prompt_tokens = num(js$prompt_eval_count), gen_tokens = num(js$eval_count),
                gen_seconds = num(js$eval_duration) / 1e9, wall_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  list(ok = TRUE, text = trimws(content), stats = stats)
}

stats_add <- function(a, b) {
  if (is.null(a)) return(b); if (is.null(b)) return(a)
  list(calls = a$calls + b$calls, prompt_tokens = a$prompt_tokens + b$prompt_tokens, gen_tokens = a$gen_tokens + b$gen_tokens,
       gen_seconds = a$gen_seconds + b$gen_seconds, wall_seconds = a$wall_seconds + b$wall_seconds)
}

stats_line <- function(stats, model, lang) {
  if (is.null(stats) || stats$calls == 0) return("")
  tps <- if (stats$gen_seconds > 0) stats$gen_tokens / stats$gen_seconds else NA
  tx(lang,
     sprintf("Ollama (%s): %d çağrı, istem %s + üretim %s token, %s token/s, %s s.", html_escape(model), stats$calls, format(round(stats$prompt_tokens), big.mark = "."), format(round(stats$gen_tokens), big.mark = "."), if (is.na(tps)) "–" else formatC(tps, format = "f", digits = 1), round(stats$wall_seconds)),
     sprintf("Ollama (%s): %d call(s), %s prompt + %s generated tokens, %s tokens/s, %s s.", html_escape(model), stats$calls, format(round(stats$prompt_tokens), big.mark = ","), format(round(stats$gen_tokens), big.mark = ","), if (is.na(tps)) "–" else formatC(tps, format = "f", digits = 1), round(stats$wall_seconds)))
}

read_prompt <- function(name, lang) {
  fn <- paste0(name, "_", lang, ".md")
  cands <- c(system.file("prompts", fn, package = "jmvReport"), file.path("inst", "prompts", fn), file.path("..", "..", "inst", "prompts", fn))
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands)) paste(readLines(cands[1], encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
}

#' Split rendered HTML into the narrative part (polished) and trailing tables (kept verbatim)
split_tables <- function(html) {
  m <- regexpr("<p><b>|<table", html)
  if (m > 0) list(text = substr(html, 1, m - 1), rest = substr(html, m, nchar(html))) else list(text = html, rest = "")
}

ensure_p <- function(out) { if (!grepl("<p>", out, fixed = TRUE)) paste0("<p>", gsub("\n{2,}", "</p><p>", out), "</p>") else out }

interp_marker <- function(lang) if (lang == "en") "<<<RESULTS>>>" else "<<<BULGULAR>>>"

#' Plain-text digest of the results narrative (fewer prompt tokens than HTML)
results_digest <- function(results_html) {
  txt <- strip_html(split_tables(results_html)$text)
  txt <- gsub("[ \t]+", " ", txt); txt <- gsub("\n{3,}", "\n\n", txt)
  trimws(txt)
}

#' Interpretation only (fast): one short call on a plain-text digest.
llm_interpret_only <- function(results_html, lang = "tr", model = "qwen3.5:4b", endpoint = default_endpoint(), timeout = 600) {
  sys_prompt <- read_prompt("system", lang); task <- read_prompt("interpret", lang)
  if (!nzchar(sys_prompt) || !nzchar(task)) return(list(interp = NULL, note = "prompt file missing", stats = NULL))
  r <- ollama_chat(sys_prompt, paste0(task, "\n\n<<<\n", results_digest(results_html), "\n>>>"), model = model, endpoint = endpoint, timeout = timeout, num_predict = 650, temperature = 0.1)
  if (!isTRUE(r$ok)) return(list(interp = NULL, note = r$error, stats = NULL))
  # plain-text in, plain-text out: drop any tags the model added, escape, re-paragraph
  plain <- gsub("<[^>]+>", "", gsub("<<<[^>]*>>>", "", r$text))
  plain <- gsub("&lt;", "<", plain, fixed = TRUE); plain <- gsub("&gt;", ">", plain, fixed = TRUE); plain <- gsub("&amp;", "&", plain, fixed = TRUE)
  cand <- paste0("<p>", paste(html_escape(trimws(strsplit(trimws(plain), "\n{2,}")[[1]])), collapse = "</p><p>"), "</p>")
  fid <- check_fidelity(cand, results_html, allow_missing_ratio = 1)
  if (!fid$ok) return(list(interp = NULL, note = paste0("interpretation fidelity: ", fid$reason), stats = r$stats))
  list(interp = cand, note = NULL, stats = r$stats)
}

#' Polish + interpretation in one call (slower). Interpretation comes first in the
#' model output so that truncation only affects the polished text.
#' Returns list(html = polished or template, interp = html or NULL, used, note, stats)
llm_polish_interpret <- function(results_html, lang = "tr", model = "qwen3.5:4b", endpoint = default_endpoint(), interpret = TRUE, timeout = 600) {
  sp <- split_tables(results_html)
  sys_prompt <- read_prompt("system", lang)
  task <- read_prompt(if (interpret) "combined" else "polish", lang)
  if (!nzchar(sys_prompt) || !nzchar(task)) return(list(html = results_html, interp = NULL, used = FALSE, note = "prompt file missing", stats = NULL))
  r <- ollama_chat(sys_prompt, paste0(task, "\n\n<<<\n", sp$text, "\n>>>"), model = model, endpoint = endpoint, timeout = timeout, num_predict = if (interpret) 1400 else 1000)
  if (!isTRUE(r$ok)) return(list(html = results_html, interp = NULL, used = FALSE, note = r$error, stats = NULL))
  out <- r$text; notes <- character(); interp <- NULL
  if (interpret) {
    parts <- strsplit(out, interp_marker(lang), fixed = TRUE)[[1]]
    if (length(parts) == 1) parts <- strsplit(out, "<<<[A-ZÇĞİÖŞÜ]+>>>", perl = TRUE)[[1]]
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

#' Dispatcher used by the analyses: polish=FALSE -> interpretation only (fast)
llm_report <- function(results_html, lang, model, endpoint, interpret = TRUE, polish = FALSE, timeout = 600) {
  if (!polish) {
    if (!interpret) return(list(html = results_html, interp = NULL, used = FALSE, note = NULL, stats = NULL))
    r <- llm_interpret_only(results_html, lang, model, endpoint, timeout)
    return(list(html = results_html, interp = r$interp, used = FALSE, note = r$note, stats = r$stats))
  }
  llm_polish_interpret(results_html, lang, model, endpoint, interpret = interpret, timeout = timeout)
}

# Backwards-compatible wrappers ---------------------------------------------
llm_polish <- function(template_html, kind = c("results", "method"), lang = "tr", model = "qwen3.5:4b", endpoint = default_endpoint(), timeout = 600) {
  r <- llm_polish_interpret(template_html, lang, model, endpoint, interpret = FALSE, timeout = timeout)
  list(html = r$html, used = r$used, note = r$note, stats = r$stats)
}
llm_interpret <- function(results_html, lang = "tr", model = "qwen3.5:4b", endpoint = default_endpoint(), timeout = 600) {
  r <- llm_interpret_only(results_html, lang, model, endpoint, timeout)
  if (is.null(r$interp)) list(ok = FALSE, note = r$note, stats = r$stats) else list(ok = TRUE, html = r$interp, stats = r$stats)
}
