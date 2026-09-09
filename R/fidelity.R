# ---- Number fidelity check ---------------------------------------------------
# The LLM must not introduce or drop numbers. Compare numeric tokens of the
# polished text with those of the template text.

fidelity_whitelist <- function() c("95", "99", "90", ".05", ".01", ".001", "0.05", "0.01", "0.001", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "2026", "2020", "0", "30", ".30")

check_fidelity <- function(candidate, reference, allow_missing_ratio = 0.05) {
  ref <- num_tokens(reference); cand <- num_tokens(candidate)
  wl <- fidelity_whitelist()
  extra <- setdiff(cand, c(ref, wl))
  missing <- setdiff(ref, c(cand, wl))
  if (length(extra) > 0) return(list(ok = FALSE, reason = paste0("new numbers: ", paste(head(extra, 8), collapse = ", ")), extra = extra, missing = missing))
  if (length(ref) > 0 && length(missing) / length(ref) > allow_missing_ratio) return(list(ok = FALSE, reason = paste0("dropped numbers: ", paste(head(missing, 8), collapse = ", ")), extra = extra, missing = missing))
  list(ok = TRUE, reason = NULL, extra = extra, missing = missing)
}
