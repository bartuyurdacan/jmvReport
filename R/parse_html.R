# ---- Fallback: parse tables from the stored results HTML --------------------

html_tables <- function(html) {
  if (is.null(html) || !nzchar(html)) return(list())
  doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
  if (is.null(doc)) return(list())
  out <- list()
  # analysis headings (h1) followed by tables; associate tables with the preceding h1
  nodes <- xml2::xml_find_all(doc, "//body/*")
  current <- ""
  for (nd in nodes) {
    nm <- xml2::xml_name(nd)
    if (nm == "h1") { current <- trimws(xml2::xml_text(nd)); next }
    tabs <- if (nm == "table") list(nd) else xml2::xml_find_all(nd, ".//table")
    for (tb in tabs) {
      tnode <- xml2::xml_find_first(tb, ".//th[@scope='colgroup']")
      if (is.na(tnode)) tnode <- xml2::xml_find_first(tb, ".//thead/tr[1]/th[1]")
      title <- if (is.na(tnode)) "" else trimws(xml2::xml_text(tnode))
      hdr_nodes <- xml2::xml_find_all(tb, ".//thead/tr[last()]/th")
      hdr <- trimws(xml2::xml_text(hdr_nodes))
      rows <- xml2::xml_find_all(tb, ".//tbody/tr")
      body <- lapply(rows, function(r) trimws(xml2::xml_text(xml2::xml_find_all(r, "./th|./td"))))
      body <- Filter(function(b) length(b) > 0, body)
      if (length(body) == 0) next
      # jamovi renders each cell as two <td>s (value + padding); merge pairs when that is the case
      body <- lapply(body, function(b) if (length(hdr) > 0 && length(b) == 2 * length(hdr)) paste0(b[seq(1, length(b), 2)], b[seq(2, length(b), 2)]) else b)
      w <- max(vapply(body, length, integer(1)))
      mat <- do.call(rbind, lapply(body, function(b) c(b, rep("", w - length(b)))))
      df <- as.data.frame(mat, stringsAsFactors = FALSE)
      if (length(hdr) == ncol(df)) names(df) <- ifelse(nzchar(hdr), hdr, paste0("V", seq_along(hdr)))
      out[[length(out) + 1]] <- list(analysis = current, title = if (is.na(title)) current else title, df = df)
    }
  }
  out
}

html_titles <- function(html) {
  doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
  if (is.null(doc)) return(character())
  t <- trimws(xml2::xml_text(xml2::xml_find_all(doc, "//body/h1")))
  t[!t %in% c("Results", "Sonu\u00E7lar", "References", "Kaynaklar")]
}
