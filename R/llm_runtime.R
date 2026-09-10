# ---- Managed llama.cpp runtime ---------------------------------------------

.jmvreport_runtime <- new.env(parent = emptyenv())

builtin_model_id <- function() "jmvreport-qwen3.5-4b"

runtime_manifest <- function() {
  release <- "b10516"
  base <- paste0("https://github.com/ggml-org/llama.cpp/releases/download/", release, "/")
  list(
    runtime_version = release,
    assets = list(
      `linux-x86_64` = list(
        file = paste0("llama-", release, "-bin-ubuntu-x64.tar.gz"),
        sha256 = "f263a91280471b4c33c4999d7c76259c0f3a0a53a0b3e692b2c0b84380137a35"
      ),
      `linux-aarch64` = list(
        file = paste0("llama-", release, "-bin-ubuntu-arm64.tar.gz"),
        sha256 = "e7491dca79c9799fc3ae169675a79f5777d3027e31ffb08ae679e5e0a7ae3c97"
      ),
      `windows-x86_64` = list(
        file = paste0("llama-", release, "-bin-win-cpu-x64.zip"),
        sha256 = "fbbbc55e0eb2e1b07f9dcb9488616c98ed47d9003b90e15e7c8c7812c4307cd3"
      ),
      `macos-x86_64` = list(
        file = paste0("llama-", release, "-bin-macos-x64.tar.gz"),
        sha256 = "b7adecf7bd2cde577ddabee8357a72409165d8104f43b4acee9f1b98cc9c447a"
      ),
      `macos-aarch64` = list(
        file = paste0("llama-", release, "-bin-macos-arm64.tar.gz"),
        sha256 = "ee3324327d621026ae80c24031670e65fa62a0b23a3a027dbe2f65f240affd30"
      )
    ),
    asset_base_url = base,
    model = list(
      file = "Qwen3.5-4B-M-TS-Q4_K_M.gguf",
      bytes = 2385660192,
      sha256 = "f8e45572b9cc35161d4772b09bccfd383fe0bb03fc6d69b40a9138731302290b",
      url = paste0(
        "https://huggingface.co/TheStageAI/Qwen3.5-4B-GGUF/resolve/",
        "3e0455024fadf0eee0cec0f910582de99aaae986/",
        "Qwen3.5-4B-M-TS-Q4_K_M.gguf?download=true"
      )
    )
  )
}

runtime_platform <- function(sysname = Sys.info()[["sysname"]], machine = Sys.info()[["machine"]]) {
  os <- switch(tolower(sysname), linux = "linux", windows = "windows", darwin = "macos", NA_character_)
  arch <- tolower(machine)
  arch <- if (arch %in% c("x86_64", "amd64", "x64")) "x86_64" else if (arch %in% c("arm64", "aarch64")) "aarch64" else NA_character_
  if (is.na(os) || is.na(arch)) return(NA_character_)
  paste(os, arch, sep = "-")
}

runtime_dir <- function() {
  root <- tools::R_user_dir("jmvReport", which = "cache")
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

runtime_paths <- function(platform = runtime_platform(), manifest = runtime_manifest()) {
  root <- runtime_dir()
  runtime <- file.path(root, "runtime", manifest$runtime_version, platform)
  model <- file.path(root, "models", manifest$model$file)
  exe_name <- if (identical(substr(platform, 1, 7), "windows")) "llama-server.exe" else "llama-server"
  candidates <- if (dir.exists(runtime)) list.files(runtime, pattern = paste0("^", gsub("\\.", "\\\\.", exe_name), "$"), recursive = TRUE, full.names = TRUE) else character()
  list(
    root = root,
    runtime = runtime,
    executable = if (length(candidates)) candidates[[1]] else file.path(runtime, exe_name),
    model = model,
    downloads = file.path(root, "downloads")
  )
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  paste(format(openssl::sha256(con)), collapse = "")
}

runtime_status <- function(verify = FALSE) {
  manifest <- runtime_manifest()
  platform <- runtime_platform()
  if (is.na(platform) || is.null(manifest$assets[[platform]])) {
    return(list(ok = FALSE, installed = FALSE, supported = FALSE, platform = platform,
                message = "Built-in AI is not available for this operating system or architecture."))
  }
  paths <- runtime_paths(platform, manifest)
  binary_ok <- file.exists(paths$executable)
  model_ok <- file.exists(paths$model) && isTRUE(file.info(paths$model)$size == manifest$model$bytes)
  if (isTRUE(verify) && binary_ok) binary_ok <- identical(sha256_file(file.path(paths$downloads, manifest$assets[[platform]]$file)), manifest$assets[[platform]]$sha256) || file.exists(file.path(paths$runtime, "install.json"))
  if (isTRUE(verify) && model_ok) model_ok <- identical(sha256_file(paths$model), manifest$model$sha256)
  installed <- binary_ok && model_ok
  message <- if (installed) {
    paste0("Built-in AI is installed (llama.cpp ", manifest$runtime_version, "; ", manifest$model$file, ").")
  } else {
    missing <- c(if (!binary_ok) "llama.cpp runtime", if (!model_ok) "language model")
    paste0("Built-in AI is not ready. Missing: ", paste(missing, collapse = " and "), ".")
  }
  list(ok = installed, installed = installed, supported = TRUE, platform = platform,
       binary_ok = binary_ok, model_ok = model_ok, paths = paths, message = message)
}

download_verified <- function(url, destination, sha256, progress = NULL) {
  if (file.exists(destination) && identical(sha256_file(destination), sha256)) return(destination)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  part <- paste0(destination, ".part")
  offset <- if (file.exists(part)) file.info(part)$size else 0
  if (is.function(progress)) progress(if (offset > 0) "Resuming download" else "Starting download")
  handle <- curl::new_handle(connecttimeout = 20, timeout = 0, followlocation = TRUE)
  mode <- "wb"
  if (!is.na(offset) && offset > 0) {
    curl::handle_setopt(handle, resume_from = offset)
    mode <- "ab"
  }
  tryCatch(
    curl::curl_download(url, part, quiet = TRUE, mode = mode, handle = handle),
    error = function(e) stop("Download failed: ", conditionMessage(e), call. = FALSE)
  )
  actual <- sha256_file(part)
  if (!identical(actual, sha256)) {
    unlink(part)
    stop("Downloaded file failed SHA-256 verification.", call. = FALSE)
  }
  if (file.exists(destination)) unlink(destination)
  if (!file.rename(part, destination)) stop("Could not finalise the downloaded file.", call. = FALSE)
  destination
}

install_builtin_runtime <- function(progress = NULL, force = FALSE) {
  manifest <- runtime_manifest()
  platform <- runtime_platform()
  if (is.na(platform)) stop("Built-in AI is not available for this platform.", call. = FALSE)
  asset <- manifest$assets[[platform]]
  if (is.null(asset)) stop("Built-in AI is not available for this platform.", call. = FALSE)
  paths <- runtime_paths(platform, manifest)
  dir.create(paths$downloads, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(paths$downloads, asset$file)

  if (is.function(progress)) progress("Downloading the verified llama.cpp runtime")
  download_verified(paste0(manifest$asset_base_url, asset$file), archive, asset$sha256, progress)

  if (isTRUE(force) || !file.exists(paths$executable)) {
    parent <- dirname(paths$runtime)
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    staging <- file.path(parent, paste0("staging-", Sys.getpid()))
    if (dir.exists(staging)) unlink(staging, recursive = TRUE)
    dir.create(staging, recursive = TRUE, showWarnings = FALSE)
    on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE), add = TRUE)
    if (grepl("\\.zip$", archive, ignore.case = TRUE)) utils::unzip(archive, exdir = staging) else utils::untar(archive, exdir = staging)
    exe_name <- if (grepl("^windows-", platform)) "llama-server.exe" else "llama-server"
    executable <- list.files(staging, pattern = paste0("^", gsub("\\.", "\\\\.", exe_name), "$"), recursive = TRUE, full.names = TRUE)
    if (!length(executable)) stop("The verified runtime archive does not contain llama-server.", call. = FALSE)
    if (!grepl("^windows-", platform)) Sys.chmod(executable, mode = "0755")
    if (dir.exists(paths$runtime)) unlink(paths$runtime, recursive = TRUE)
    if (!file.rename(staging, paths$runtime)) stop("Could not install the llama.cpp runtime.", call. = FALSE)
    jsonlite::write_json(list(version = manifest$runtime_version, platform = platform, installed = format(Sys.time(), tz = "UTC", usetz = TRUE)),
                         file.path(paths$runtime, "install.json"), auto_unbox = TRUE, pretty = TRUE)
  }

  if (is.function(progress)) progress("Downloading the verified 2.39 GB language model")
  download_verified(manifest$model$url, paths$model, manifest$model$sha256, progress)
  notices <- system.file("THIRD_PARTY_NOTICES.md", package = "jmvReport")
  if (nzchar(notices) && file.exists(notices)) file.copy(notices, paths$root, overwrite = TRUE)

  status <- runtime_status(verify = TRUE)
  if (!status$installed) stop(status$message, call. = FALSE)
  status
}

runtime_token <- function() openssl::base64_encode(openssl::rand_bytes(24))

stop_builtin_server <- function() {
  proc <- .jmvreport_runtime$process
  if (!is.null(proc) && inherits(proc, "process") && proc$is_alive()) {
    try(proc$kill_tree(), silent = TRUE)
  }
  rm(list = ls(.jmvreport_runtime, all.names = TRUE), envir = .jmvreport_runtime)
  invisible(TRUE)
}

start_builtin_server <- function(timeout = 120) {
  status <- runtime_status()
  if (!status$installed) return(list(ok = FALSE, error = paste0(status$message, " Open AI Report > Local AI Setup to install it.")))
  proc <- .jmvreport_runtime$process
  if (!is.null(proc) && inherits(proc, "process") && proc$is_alive()) {
    return(list(ok = TRUE, endpoint = .jmvreport_runtime$endpoint, api_key = .jmvreport_runtime$api_key,
                model = builtin_model_id(), label = "Built-in llama.cpp"))
  }

  threads <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (is.na(threads)) threads <- 2L
  threads <- max(1L, as.integer(threads) - 1L)
  token <- runtime_token()
  base_port <- 49152L + (as.integer(Sys.getpid()) %% 12000L)
  log_file <- file.path(status$paths$root, "llama-server.log")

  for (attempt in 0:9) {
    port <- 49152L + ((base_port - 49152L + attempt) %% 16000L)
    args <- c("--model", status$paths$model, "--alias", builtin_model_id(),
              "--host", "127.0.0.1", "--port", as.character(port),
              "--ctx-size", "4096", "--threads", as.character(threads),
              "--api-key", token, "--sleep-idle-seconds", "300")
    proc <- tryCatch(processx::process$new(
      status$paths$executable, args = args, stdout = log_file, stderr = "2>&1",
      cleanup = TRUE, cleanup_tree = TRUE, windows_hide_window = TRUE,
      linux_pdeathsig = TRUE
    ), error = function(e) e)
    if (inherits(proc, "error")) next
    endpoint <- paste0("http://127.0.0.1:", port, "/v1")
    deadline <- Sys.time() + timeout
    repeat {
      if (!proc$is_alive()) break
      h <- curl::new_handle(timeout = 2, connecttimeout = 1)
      health <- tryCatch(curl::curl_fetch_memory(paste0("http://127.0.0.1:", port, "/health"), handle = h), error = function(e) NULL)
      if (!is.null(health) && health$status_code == 200) {
        .jmvreport_runtime$process <- proc
        .jmvreport_runtime$endpoint <- endpoint
        .jmvreport_runtime$api_key <- token
        return(list(ok = TRUE, endpoint = endpoint, api_key = token, model = builtin_model_id(), label = "Built-in llama.cpp"))
      }
      if (Sys.time() >= deadline) {
        try(proc$kill_tree(), silent = TRUE)
        return(list(ok = FALSE, error = paste0("Built-in AI did not become ready. See ", log_file)))
      }
      Sys.sleep(0.2)
    }
  }
  list(ok = FALSE, error = paste0("Could not start built-in AI. See ", log_file))
}
