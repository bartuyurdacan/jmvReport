# Generate synthetic (digest -> interpretation) pairs with jmv inside jamovi's R.
.libPaths(c("/app/lib/jamovi/modules/jmv/R", "/app/lib/R/library"))
setwd("~/jmvReport"); for (f in list.files("R", full.names = TRUE)) if (!grepl("\\.b\\.R$", f)) source(f, encoding = "UTF-8")
suppressMessages(library(jmv))
args <- commandArgs(trailingOnly = TRUE)
N <- if (length(args)) as.integer(args[1]) else 3000
out_file <- if (length(args) > 1) args[2] else "finetune/train_raw.jsonl"
set.seed(if (length(args) > 2) as.integer(args[3]) else 20260910)

vn_tr <- c("kaygi", "depresyon", "yasam_kalitesi", "agri", "tukenmislik", "uyku_suresi", "bilgi_puani", "tutum", "motivasyon", "stres", "ozyeterlik", "memnuniyet", "yas", "BKI", "sistolik", "HbA1c", "hemoglobin", "CRP", "VAS", "MMSE", "okuma_hizi", "basari", "empati", "kaygi_sonrasi", "kaygi_oncesi")
vn_en <- c("anxiety", "depression", "quality_of_life", "pain", "burnout", "sleep_hours", "knowledge_score", "attitude", "motivation", "stress", "self_efficacy", "satisfaction", "age", "BMI", "systolic", "HbA1c", "hemoglobin", "CRP", "VAS", "MMSE", "reading_speed", "achievement", "empathy", "post_anxiety", "pre_anxiety")
gn_tr <- list(cinsiyet = c("Kadın", "Erkek"), grup = c("Deney", "Kontrol"), tedavi = c("İlaç", "Plasebo"), sigara = c("Evet", "Hayır"), egitim = c("İlkokul", "Lise", "Üniversite"), bolum = c("Cerrahi", "Dahiliye", "Pediatri"), evre = c("Evre I", "Evre II", "Evre III", "Evre IV"), yas_grubu = c("18-30", "31-45", "46+"), tanı = c("Var", "Yok"), meslek = c("Hemşire", "Hekim", "Teknisyen"))
gn_en <- list(sex = c("Female", "Male"), group = c("Intervention", "Control"), treatment = c("Drug", "Placebo"), smoker = c("Yes", "No"), education = c("Primary", "High school", "University"), department = c("Surgery", "Internal", "Pediatrics"), stage = c("Stage I", "Stage II", "Stage III", "Stage IV"), age_group = c("18-30", "31-45", "46+"), diagnosis = c("Yes", "No"), occupation = c("Nurse", "Physician", "Technician"))

rnum <- function(n, mean = 50, sd = 10, skew = FALSE) { x <- rnorm(n, mean, sd); if (skew) x <- mean + sd * (exp(rnorm(n, 0, 0.6)) - 1.2); round(x, 1) }
pickv <- function(lang, k = 1) sample(if (lang == "tr") vn_tr else vn_en, k)
pickg <- function(lang, levels = NULL) { bank <- if (lang == "tr") gn_tr else gn_en; if (!is.null(levels)) bank <- bank[vapply(bank, length, integer(1)) == levels]; nm <- sample(names(bank), 1); list(name = nm, levels = bank[[nm]]) }

sim_ttestIS <- function(lang) { g <- pickg(lang, 2); n <- sample(20:200, 1); d <- sample(c(0, 0.2, 0.5, 0.8, 1.2), 1) * sample(c(-1, 1), 1); het <- runif(1) < .3; sk <- runif(1) < .3
  df <- data.frame(y = c(rnum(n, 50, 10, sk), rnum(n, 50 + 10 * d, if (het) 18 else 10, sk)), g = factor(rep(g$levels, each = n))); names(df) <- c(pickv(lang), g$name)
  ttestIS(df, vars = names(df)[1], group = names(df)[2], welchs = TRUE, mann = runif(1) < .6, effectSize = TRUE, ci = TRUE, desc = TRUE, norm = TRUE, eqv = TRUE) }
sim_ttestPS <- function(lang) { n <- sample(15:120, 1); d <- sample(c(0, 0.3, 0.6, 1), 1) * sample(c(-1, 1), 1); base <- rnum(n, 40, 8, runif(1) < .3)
  df <- data.frame(a = base, b = base + rnorm(n, 8 * d, 6)); v <- if (lang == "tr") c("on_test", "son_test") else c("pretest", "posttest"); names(df) <- v
  ttestPS(df, pairs = list(list(i1 = v[1], i2 = v[2])), wilcoxon = runif(1) < .5, effectSize = TRUE, desc = TRUE, norm = TRUE, ci = TRUE) }
sim_anovaOneW <- function(lang) { g <- pickg(lang, sample(3:4, 1)); k <- length(g$levels); n <- sample(15:80, 1); eff <- sample(c(0, 4, 8, 14), 1); sk <- runif(1) < .25; het <- runif(1) < .3
  df <- data.frame(y = unlist(lapply(seq_len(k), function(i) rnum(n, 50 + eff * (i - 1) / (k - 1) * sample(c(-1, 1), 1), if (het && i == 1) 20 else 10, sk))), g = factor(rep(g$levels, each = n))); names(df) <- c(pickv(lang), g$name)
  list(anovaOneW(df, deps = names(df)[1], group = names(df)[2], welchs = TRUE, fishers = TRUE, desc = TRUE, norm = TRUE, eqv = TRUE, phMethod = sample(c("tukey", "gamesHowell"), 1), phMeanDif = TRUE, phTest = TRUE),
       if (runif(1) < .5) anovaNP(df, deps = names(df)[1], group = names(df)[2], es = TRUE, pairsDunn = TRUE)) }
sim_corr <- function(lang) { n <- sample(30:250, 1); k <- sample(2:4, 1); v <- pickv(lang, k); base <- rnorm(n)
  df <- as.data.frame(lapply(seq_len(k), function(i) { r <- sample(c(0, .15, .3, .5, .75), 1) * sample(c(-1, 1), 1); round(50 + 10 * (r * base + sqrt(1 - r^2) * rnorm(n)), 1) })); names(df) <- v
  corrMatrix(df, vars = v, pearson = TRUE, spearman = runif(1) < .4, n = TRUE, ci = runif(1) < .5) }
sim_cont <- function(lang) { g1 <- pickg(lang); g2 <- pickg(lang); if (g1$name == g2$name) g2 <- pickg(lang, 2); n <- sample(40:300, 1); assoc <- sample(c(0, .15, .3, .5), 1)
  r <- sample(g1$levels, n, TRUE); p2 <- matrix(runif(length(g1$levels) * length(g2$levels)), length(g1$levels)); p2 <- p2 / rowSums(p2); p2 <- (1 - assoc) * matrix(1 / length(g2$levels), nrow(p2), ncol(p2)) + assoc * p2
  cc <- vapply(r, function(x) sample(g2$levels, 1, prob = p2[match(x, g1$levels), ]), character(1)); df <- data.frame(a = factor(r), b = factor(cc)); names(df) <- c(g1$name, g2$name)
  contTables(df, rows = g1$name, cols = g2$name, chiSq = TRUE, fisher = runif(1) < .5 && length(g1$levels) == 2 && length(g2$levels) == 2, phiCra = TRUE, obs = TRUE) }
sim_linReg <- function(lang) { n <- sample(40:300, 1); k <- sample(2:4, 1); v <- pickv(lang, k + 1); X <- matrix(rnorm(n * k), n); b <- sample(c(0, 0, .2, .4, .6), k, TRUE); y <- X %*% b + rnorm(n, 0, 1)
  df <- as.data.frame(cbind(round(50 + 10 * y, 1), round(50 + 10 * X, 1))); names(df) <- v
  linReg(df, dep = v[1], covs = v[-1], blocks = list(as.list(v[-1])), r2Adj = TRUE, modelTest = TRUE, stdEst = TRUE, ci = TRUE, collin = TRUE, durbin = runif(1) < .5) }
sim_logReg <- function(lang) { n <- sample(60:300, 1); k <- sample(1:3, 1); v <- pickv(lang, k); g <- pickg(lang, 2); X <- matrix(rnorm(n * k), n); b <- sample(c(0, .3, .6, 1), k, TRUE); p <- plogis(X %*% b); y <- factor(ifelse(runif(n) < p, g$levels[2], g$levels[1]))
  df <- as.data.frame(round(50 + 10 * X, 1)); names(df) <- v; df[[g$name]] <- y
  logRegBin(df, dep = g$name, covs = v, blocks = list(as.list(v)), modelTest = TRUE, OR = TRUE, ciOR = TRUE, pseudoR2 = c("r2mf", "r2n"), class = TRUE, acc = TRUE, collin = TRUE) }
sim_ANOVA <- function(lang) { g1 <- pickg(lang, 2); g2 <- pickg(lang, sample(2:3, 1)); if (g2$name == g1$name) g2 <- pickg(lang, 3); n <- sample(10:40, 1); e1 <- sample(c(0, 5, 10), 1); e2 <- sample(c(0, 5, 10), 1); ei <- sample(c(0, 0, 6), 1)
  grid <- expand.grid(a = g1$levels, b = g2$levels); df <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) data.frame(y = rnum(n, 50 + e1 * (grid$a[i] == g1$levels[2]) + e2 * (match(grid$b[i], g2$levels) - 1) + ei * (grid$a[i] == g1$levels[2]) * (match(grid$b[i], g2$levels) - 1), 10), a = grid$a[i], b = grid$b[i])))
  df$a <- factor(df$a); df$b <- factor(df$b); names(df) <- c(pickv(lang), g1$name, g2$name)
  ANOVA(df, dep = names(df)[1], factors = names(df)[2:3], effectSize = "partEta", postHoc = as.formula(paste("~", names(df)[3])), postHocCorr = "tukey", homo = TRUE, norm = TRUE) }

types <- c("ttestIS", "ttestPS", "anovaOneW", "corr", "cont", "linReg", "logReg", "ANOVA")
weights <- c(.20, .10, .18, .14, .14, .10, .07, .07)
con <- file(out_file, open = "w", encoding = "UTF-8"); n_ok <- 0; t0 <- Sys.time()
for (i in seq_len(N)) {
  lang <- Sys.getenv("JMV_LANG", if (runif(1) < .6) "tr" else "en"); ty <- sample(types, 1, prob = weights)
  res <- tryCatch(suppressWarnings(suppressMessages(switch(ty, ttestIS = sim_ttestIS(lang), ttestPS = sim_ttestPS(lang), anovaOneW = sim_anovaOneW(lang), corr = sim_corr(lang), cont = sim_cont(lang), linReg = sim_linReg(lang), logReg = sim_logReg(lang), ANOVA = sim_ANOVA(lang)))), error = function(e) NULL)
  if (is.null(res)) next
  reslist <- if (is.list(res) && !inherits(res, "R6")) Filter(Negate(is.null), res) else list(res)
  summaries <- lapply(reslist, function(r) tryCatch(summarize_results(r), error = function(e) NULL)); summaries <- Filter(Negate(is.null), summaries)
  if (!length(summaries)) next
  results_html <- paste(vapply(summaries, function(s) render_results_text(s, lang), character(1)), collapse = "")
  digest <- results_digest(results_html)
  interp <- paste(vapply(summaries, function(s) strip_html(rule_interpretation(s, lang, seed = sample.int(1e6, 1))), character(1)), collapse = " ")
  interp <- gsub("\\s+", " ", interp)
  if (!nzchar(trimws(interp)) || nchar(digest) < 60) next
  sys_p <- read_prompt("system", lang); task <- read_prompt("interpret", lang)
  rec <- list(lang = lang, type = paste(vapply(summaries, function(s) s$type, character(1)), collapse = "+"), system = sys_p, user = paste0(task, "\n\n<<<\n", digest, "\n>>>"), assistant = interp)
  writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE), con); n_ok <- n_ok + 1
  if (i %% 100 == 0) cat(sprintf("%d/%d ok=%d elapsed=%.0fs\n", i, N, n_ok, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
close(con); cat("DONE", n_ok, "records ->", out_file, "\n")
