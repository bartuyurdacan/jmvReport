# ---- Locate saved .omv files -------------------------------------------------

candidate_dirs <- function() {
  home <- path.expand("~")
  dirs <- c(file.path(home, c("Documents", "Desktop", "Downloads", "Belgeler", "Masa\u00FCst\u00FC", "\u0130ndirilenler")),
            file.path(home, "OneDrive", c("Documents", "Desktop", "Belgeler", "Masa\u00FCst\u00FC")),
            Sys.getenv("USERPROFILE", unset = ""), getwd())
  # xdg user dirs (Linux)
  xdg <- file.path(home, ".config", "user-dirs.dirs")
  if (file.exists(xdg)) {
    ln <- readLines(xdg, warn = FALSE)
    for (l in grep("^XDG_(DOCUMENTS|DESKTOP|DOWNLOAD)_DIR", ln, value = TRUE)) {
      v <- sub('^[^=]+="?', "", l); v <- sub('"$', "", v); v <- sub("\\$HOME", home, v)
      dirs <- c(dirs, v)
    }
  }
  dirs <- dirs[nzchar(dirs)]
  unique(dirs[dir.exists(dirs)])
}

#' Find the most recently modified .omv in the usual folders (max depth 2)
find_latest_omv <- function(dirs = candidate_dirs()) {
  files <- character()
  for (d in dirs) {
    f1 <- list.files(d, pattern = "\\.omv$", full.names = TRUE, ignore.case = TRUE)
    subs <- list.dirs(d, recursive = FALSE, full.names = TRUE)
    subs <- subs[!grepl("/\\.", subs)]
    f2 <- unlist(lapply(head(subs, 200), function(sd) list.files(sd, pattern = "\\.omv$", full.names = TRUE, ignore.case = TRUE)))
    files <- c(files, f1, f2)
  }
  files <- unique(files)
  if (length(files) == 0) return(NA_character_)
  info <- file.info(files)
  files[order(info$mtime, decreasing = TRUE)][1]
}

resolve_omv_path <- function(path) {
  if (!is.null(path) && nzchar(trimws(path))) {
    p <- path.expand(trimws(path)); p <- gsub('^"|"$', "", p)
    if (file.exists(p)) return(list(path = p, auto = FALSE))
    return(list(path = NA_character_, auto = FALSE, error = paste("not found:", p)))
  }
  list(path = find_latest_omv(), auto = TRUE)
}
