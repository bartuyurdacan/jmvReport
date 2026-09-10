# ---- Rule-based interpretation (Layer 0b) -----------------------------------
# Produces a short interpretation paragraph from a summary without any LLM.
# Phrase banks with variants give training-data variety; seed controls choice.

pick <- function(x, rng) x[[1 + (rng() %% length(x))]]
make_rng <- function(seed) { if (is.null(seed)) return(function() sample.int(1e6, 1)); s <- seed; function() { s <<- (s * 1103515245 + 12345) %% 2147483648; s %/% 65536 } }

mag_tr <- c(negligible = "ihmal edilebilir", small = "küçük", medium = "orta", large = "büyük")
mag_en <- c(negligible = "negligible", small = "small", medium = "medium", large = "large")
magw <- function(v, type, lang) { m <- es_magnitude(v, type); if (is.na(m)) return(NA_character_); if (lang == "tr") mag_tr[[m]] else mag_en[[m]] }

# sentence banks -------------------------------------------------------------
B <- list(
  tr = list(
    sig_diff = list("%s açısından %s grupları arasında anlamlı bir fark bulunmuştur", "%s puanları %s gruplarına göre anlamlı biçimde farklılaşmaktadır", "Bulgular, %s değişkeninde %s grupları arasında istatistiksel olarak anlamlı bir farka işaret etmektedir"),
    ns_diff = list("%s açısından %s grupları arasında anlamlı bir fark saptanmamıştır", "%s puanlarının %s gruplarına göre farklılaşmadığı görülmektedir", "Bulgular, %s değişkeninde %s grupları arasında anlamlı bir fark olmadığını göstermektedir"),
    higher = list("%s grubunun puanları %s grubundan daha yüksektir", "%s grubu %s grubuna kıyasla daha yüksek puan almıştır"),
    es = list("Etki büyüklüğü %s düzeydedir; bu, farkın %s", "Gözlenen etki %s düzeydedir ve %s"),
    es_tail = c(negligible = "pratik açıdan önemsiz olabileceğini düşündürmektedir", small = "pratik açıdan sınırlı olabileceğine işaret etmektedir", medium = "pratik açıdan dikkate değer olabileceğini göstermektedir", large = "pratik açıdan belirgin olduğunu düşündürmektedir"),
    ns_es = list("Anlamlı olmayan sonuç, örneklem büyüklüğüne bağlı güç sınırlılığından da kaynaklanmış olabilir", "Fark istatistiksel olarak anlamlı olmasa da, küçük etkiler daha büyük örneklemlerde anlamlı çıkabilir"),
    assump_viol = list("Varyans homojenliği sağlanmadığından %s tercih edilmiştir; sonuçlar bu düzeltmeyle yorumlanmalıdır", "%s kullanılması varyansların homojen olmamasına bağlıdır"),
    norm_viol = list("Normallik varsayımı sağlanmadığından parametrik olmayan sonuçlara ağırlık verilmesi uygundur", "Dağılımın normal olmaması nedeniyle sıra tabanlı testin sonucu daha güvenilir kabul edilebilir"),
    posthoc = list("İkili karşılaştırmalarda %s çiftleri arasında anlamlı fark görülmüştür", "Anlamlı farklar %s karşılaştırmalarında toplanmaktadır"),
    posthoc_none = list("Omnibus test anlamlı olsa da ikili karşılaştırmalarda tek tek çiftler arasında anlamlı fark bulunamamıştır"),
    multi = list("Çoklu karşılaştırma düzeltmesi uygulanmış olsa da, bulgular tek bir örneklemden elde edildiğinden genellemede temkinli olunmalıdır", "Bulgular kesitsel/tek örneklem niteliğinde olduğundan nedensel bir çıkarım yapılmamalıdır", "Sonuçların tekrarlanabilirliği bağımsız örneklemlerle sınanmalıdır"),
    corr_pos = list("%s ile %s arasında pozitif yönde %s düzeyde bir ilişki bulunmaktadır", "%s arttıkça %s da artma eğilimindedir (%s düzeyde ilişki)"),
    corr_neg = list("%s ile %s arasında negatif yönde %s düzeyde bir ilişki bulunmaktadır", "%s arttıkça %s azalma eğilimindedir (%s düzeyde ilişki)"),
    corr_ns = list("%s ile %s arasında anlamlı bir ilişki saptanmamıştır"),
    corr_caution = list("Korelasyon nedensellik göstermez; ilişkiler üçüncü değişkenlerden etkilenmiş olabilir", "İlişkilerin yönü ve büyüklüğü tanımlayıcı niteliktedir, nedensel bir yorum yapılmamalıdır"),
    chi_sig = list("%s ile %s arasında anlamlı bir ilişki bulunmaktadır; kategorilerin dağılımı birbirinden bağımsız değildir", "Bulgular %s ve %s değişkenlerinin ilişkili olduğuna işaret etmektedir"),
    chi_ns = list("%s ile %s arasında anlamlı bir ilişki saptanmamıştır; kategorilerin dağılımı bağımsız görünmektedir"),
    chi_es = list("İlişkinin gücü %s düzeydedir", "Etki büyüklüğü ilişkinin %s olduğunu göstermektedir"),
    chi_small = list("Beklenen sayıların küçük olabileceği hücrelerde Fisher kesin testi sonucu esas alınmalıdır"),
    reg_sig = list("Model %s değişkenini anlamlı biçimde yordamakta ve varyansın %s'ini açıklamaktadır", "Yordayıcılar birlikte %s varyansının %s'ini açıklamaktadır"),
    reg_ns = list("Model %s değişkenini anlamlı biçimde yordamamaktadır"),
    reg_pred = list("Anlamlı yordayıcı(lar): %s; diğer değişkenlerin katkısı anlamlı değildir", "%s anlamlı yordayıcı olarak öne çıkmaktadır"),
    reg_nopred = list("Tek tek yordayıcıların hiçbiri anlamlı değildir"),
    reg_caution = list("Regresyon katsayıları ilişkisel niteliktedir; nedensel yorum için deneysel tasarım gerekir", "Çoklu bağlantı ve artık varsayımları model yorumunda göz önünde bulundurulmalıdır"),
    rel_good = list("İç tutarlılık %s düzeydedir; ölçek puanları güvenilir kabul edilebilir"),
    rel_poor = list("İç tutarlılık %s düzeydedir; ölçek puanları temkinle yorumlanmalı, madde yapısı gözden geçirilmelidir"),
    generic_sig = list("Analiz istatistiksel olarak anlamlı bir sonuç vermiştir", "Sınanan etki anlamlı bulunmuştur"),
    generic_ns = list("Analiz istatistiksel olarak anlamlı bir sonuç vermemiştir", "Sınanan etki anlamlı bulunmamıştır")
  ),
  en = list(
    sig_diff = list("%s differed significantly across %s groups", "Scores on %s varied significantly by %s", "The findings indicate a statistically significant difference in %s across %s groups"),
    ns_diff = list("No significant difference in %s was found across %s groups", "%s did not differ by %s", "The findings show no significant group difference in %s across %s"),
    higher = list("the %s group scored higher than the %s group", "scores were higher in the %s group than in the %s group"),
    es = list("The effect size was %s, suggesting that the difference %s", "The observed effect was %s and %s"),
    es_tail = c(negligible = "may be of little practical importance", small = "may be of limited practical importance", medium = "may be practically meaningful", large = "appears practically substantial"),
    ns_es = list("The non-significant result may also reflect limited statistical power for the sample size", "Although not significant, small effects can reach significance in larger samples"),
    assump_viol = list("Because homogeneity of variances was violated, %s was preferred and the results should be read with that correction in mind", "The use of %s reflects unequal variances across groups"),
    norm_viol = list("Because normality was violated, the non-parametric result should be given more weight", "Given the non-normal distribution, the rank-based test is the more reliable indicator"),
    posthoc = list("Pairwise comparisons showed significant differences for %s", "Significant differences were concentrated in the %s comparisons"),
    posthoc_none = list("Although the omnibus test was significant, no individual pair differed significantly"),
    multi = list("Although a multiple-comparison correction was applied, the findings come from a single sample and should be generalised cautiously", "The design is observational, so no causal inference should be drawn", "Replication in independent samples is needed to confirm the results"),
    corr_pos = list("%s and %s were positively associated at a %s level", "Higher %s tended to accompany higher %s (a %s association)"),
    corr_neg = list("%s and %s were negatively associated at a %s level", "Higher %s tended to accompany lower %s (a %s association)"),
    corr_ns = list("No significant association was found between %s and %s"),
    corr_caution = list("Correlation does not imply causation; the associations may reflect third variables", "The associations are descriptive and should not be interpreted causally"),
    chi_sig = list("%s and %s were significantly associated; the category distributions are not independent", "The findings suggest that %s and %s are related"),
    chi_ns = list("No significant association was found between %s and %s; the category distributions appear independent"),
    chi_es = list("The strength of the association was %s", "The effect size indicates a %s association"),
    chi_small = list("Where expected counts may be small, Fisher's exact test should be taken as the reference result"),
    reg_sig = list("The model significantly predicted %s, explaining %s of its variance", "Together the predictors explained %s of the variance in %s"),
    reg_ns = list("The model did not significantly predict %s"),
    reg_pred = list("Significant predictor(s): %s; the remaining variables did not contribute significantly", "%s emerged as significant predictor(s)"),
    reg_nopred = list("None of the individual predictors was significant"),
    reg_caution = list("Regression coefficients are associational; causal claims would require an experimental design", "Multicollinearity and residual assumptions should be considered when interpreting the model"),
    rel_good = list("Internal consistency was %s; the scale scores can be considered reliable"),
    rel_poor = list("Internal consistency was %s; scale scores should be interpreted cautiously and the item set reviewed"),
    generic_sig = list("The analysis yielded a statistically significant result", "The tested effect was significant"),
    generic_ns = list("The analysis did not yield a statistically significant result", "The tested effect was not significant")
  )
)

cap1 <- function(x) { s <- substr(x, 1, 1); paste0(toupper(s), substr(x, 2, nchar(x))) }
fin <- function(sentences) { s <- sentences[nzchar(sentences) & !is.na(sentences)]; if (!length(s)) return(""); paste0("<p>", paste0(cap1(html_escape(s)), ".", collapse = " "), "</p>") }

#' Rule-based interpretation paragraph for a summary
rule_interpretation <- function(s, lang = "tr", seed = NULL) {
  rng <- make_rng(seed); bk <- B[[if (lang == "en") "en" else "tr"]]
  P <- function(key, ...) sprintf(pick(bk[[key]], rng), ...)
  out <- tryCatch(switch(s$type,
    ttestIS = ri_ttestIS(s, lang, bk, P, rng), ttestPS = ri_ttestPS(s, lang, bk, P, rng), ttestOneS = ri_ttestOneS(s, lang, bk, P, rng),
    anovaOneW = ri_anovaOneW(s, lang, bk, P, rng), ANOVA = , ancova = ri_ANOVA(s, lang, bk, P, rng), anovaRM = ri_anovaRM(s, lang, bk, P, rng),
    anovaNP = ri_anovaNP(s, lang, bk, P, rng), anovaRMNP = ri_anovaRMNP(s, lang, bk, P, rng), corrMatrix = ri_corr(s, lang, bk, P, rng),
    linReg = ri_linReg(s, lang, bk, P, rng), logRegBin = , logRegOrd = , logRegMulti = ri_logReg(s, lang, bk, P, rng),
    contTables = ri_contTables(s, lang, bk, P, rng), contTablesPaired = ri_generic(s, lang, bk, P, rng, s$p),
    propTest2 = ri_generic(s, lang, bk, P, rng, s$rows[[1]]$p), propTestN = ri_generic(s, lang, bk, P, rng, s$p),
    reliability = ri_reliability(s, lang, bk, P, rng), efa = , pca = ri_efa(s, lang, bk, P, rng), cfa = ri_cfa(s, lang, bk, P, rng),
    mancova = ri_generic(s, lang, bk, P, rng, if (length(s$tests)) s$tests[[1]]$p else NA), descriptives = character(),
    character()), error = function(e) character())
  fin(out)
}

ri_generic <- function(s, lang, bk, P, rng, p) c(if (sig(p)) P("generic_sig") else P("generic_ns"), P("multi"))

ri_ttestIS <- function(s, lang, bk, P, rng) {
  out <- character()
  for (r in s$rows) {
    lev_viol <- !is.null(r$levene) && !is.na(r$levene$p) && r$levene$p < .05
    norm_viol <- !is.null(r$norm) && !is.na(r$norm$p) && r$norm$p < .05
    te <- if (lev_viol && !is.null(r$welch)) r$welch else if (!is.null(r$student)) r$student else r$welch
    use_mann <- norm_viol && !is.null(r$mann)
    p <- if (use_mann) r$mann$p else te$p
    out <- c(out, if (sig(p)) P("sig_diff", r$var, s$group) else P("ns_diff", r$var, s$group))
    if (sig(p) && !is.null(r$g1)) { hi <- if (isTRUE(r$g1$m > r$g2$m)) c(r$g1$name, r$g2$name) else c(r$g2$name, r$g1$name); out <- c(out, P("higher", hi[1], hi[2])) }
    es <- if (use_mann) r$mann$es else te$es; est <- if (use_mann) "rrb" else "d"
    if (sig(p) && !is.na(es)) { m <- es_magnitude(es, est); out <- c(out, sprintf(pick(bk$es, rng), if (lang == "tr") mag_tr[[m]] else mag_en[[m]], bk$es_tail[[m]])) } else if (!sig(p)) out <- c(out, P("ns_es"))
    if (lev_viol && !is.null(r$welch)) out <- c(out, P("assump_viol", if (lang == "tr") "Welch t-testi" else "Welch's t-test"))
    if (norm_viol && !is.null(r$mann)) out <- c(out, P("norm_viol"))
  }
  c(out, P("multi"))
}

ri_ttestPS <- function(s, lang, bk, P, rng) {
  out <- character()
  for (r in s$rows) {
    norm_viol <- !is.null(r$norm) && !is.na(r$norm$p) && r$norm$p < .05
    use_w <- norm_viol && !is.null(r$wilcoxon); p <- if (use_w) r$wilcoxon$p else r$student$p
    pair <- paste0(r$var1, " – ", r$var2)
    out <- c(out, if (sig(p)) P("sig_diff", pair, if (lang == "tr") "ölçüm" else "measurement") else P("ns_diff", pair, if (lang == "tr") "ölçüm" else "measurement"))
    if (sig(p) && !is.null(r$d1)) { hi <- if (isTRUE(r$d1$m > r$d2$m)) c(r$var1, r$var2) else c(r$var2, r$var1); out <- c(out, P("higher", hi[1], hi[2])) }
    es <- if (use_w) r$wilcoxon$es else r$student$es
    if (sig(p) && !is.na(es)) { m <- es_magnitude(es, if (use_w) "rrb" else "d"); out <- c(out, sprintf(pick(bk$es, rng), if (lang == "tr") mag_tr[[m]] else mag_en[[m]], bk$es_tail[[m]])) } else if (!sig(p)) out <- c(out, P("ns_es"))
    if (norm_viol && !is.null(r$wilcoxon)) out <- c(out, P("norm_viol"))
  }
  c(out, P("multi"))
}

ri_ttestOneS <- function(s, lang, bk, P, rng) {
  out <- character()
  for (r in s$rows) { p <- r$student$p
    out <- c(out, if (sig(p)) (if (lang == "tr") sprintf("%s ortalaması test değeri olan %s'den anlamlı biçimde farklıdır", r$var, fmt_num(s$testValue)) else sprintf("The mean of %s differed significantly from the test value of %s", r$var, fmt_num(s$testValue))) else (if (lang == "tr") sprintf("%s ortalaması test değerinden anlamlı biçimde farklı değildir", r$var) else sprintf("The mean of %s did not differ significantly from the test value", r$var)))
    if (sig(p) && !is.na(r$student$es)) { m <- es_magnitude(r$student$es, "d"); out <- c(out, sprintf(pick(bk$es, rng), if (lang == "tr") mag_tr[[m]] else mag_en[[m]], bk$es_tail[[m]])) } else if (!sig(p)) out <- c(out, P("ns_es")) }
  c(out, P("multi"))
}

posthoc_pairs_txt <- function(pairs, lang, pkey = NULL) {
  sigp <- Filter(function(p) { pv <- if (is.list(p$p)) { if (!is.null(pkey) && pkey %in% names(p$p)) p$p[[pkey]] else p$p[[1]] } else p$p; sig(pv) }, pairs)
  if (!length(sigp)) return(NULL)
  join_words(vapply(sigp, function(p) paste0(p$a, "–", p$b), character(1)), lang)
}

ri_anovaOneW <- function(s, lang, bk, P, rng) {
  out <- character()
  for (r in s$rows) {
    lev_viol <- !is.null(r$levene) && !is.na(r$levene$p) && r$levene$p < .05
    te <- if (lev_viol && !is.null(r$welch)) r$welch else if (!is.null(r$fisher)) r$fisher else r$welch
    out <- c(out, if (sig(te$p)) P("sig_diff", r$dep, s$group) else P("ns_diff", r$dep, s$group))
    if (sig(te$p) && length(r$groups)) { ms <- vapply(r$groups, function(g) g$m, numeric(1)); nm <- vapply(r$groups, function(g) g$name, character(1)); out <- c(out, P("higher", nm[which.max(ms)], nm[which.min(ms)])) }
    if (sig(te$p) && length(r$posthoc)) { pt <- posthoc_pairs_txt(r$posthoc, lang); out <- c(out, if (is.null(pt)) P("posthoc_none") else P("posthoc", pt)) }
    if (!sig(te$p)) out <- c(out, P("ns_es"))
    if (lev_viol && !is.null(r$welch)) out <- c(out, P("assump_viol", if (lang == "tr") "Welch ANOVA" else "Welch's ANOVA"))
    if (!is.null(r$norm) && !is.na(r$norm$p) && r$norm$p < .05) out <- c(out, P("norm_viol"))
  }
  c(out, P("multi"))
}

ri_ANOVA <- function(s, lang, bk, P, rng) {
  out <- character()
  for (t in s$terms) {
    nm <- gsub("[:✻*]", " × ", t$name); is_int <- grepl("×", nm)
    lab <- if (lang == "tr") (if (is_int) paste0(nm, " etkileşimi") else paste0(nm, " temel etkisi")) else (if (is_int) paste0("the ", nm, " interaction") else paste0("the main effect of ", nm))
    out <- c(out, if (lang == "tr") paste0(cap1(lab), " ", if (sig(t$p)) "anlamlıdır" else "anlamlı değildir") else paste0(cap1(lab), " was ", if (sig(t$p)) "significant" else "not significant"))
    es <- if (!is.na(t$etaSqP)) t$etaSqP else if (!is.na(t$etaSq)) t$etaSq else t$omegaSq
    if (sig(t$p) && !is.na(es)) { m <- es_magnitude(es, "etap"); out <- c(out, sprintf(pick(bk$es, rng), if (lang == "tr") mag_tr[[m]] else mag_en[[m]], bk$es_tail[[m]])) }
  }
  ints <- Filter(function(t) grepl("[:✻*]", t$name) && sig(t$p), s$terms)
  if (length(ints)) out <- c(out, if (lang == "tr") "Etkileşim anlamlı olduğundan temel etkiler tek başına değil, düzey kombinasyonlarına göre yorumlanmalıdır" else "Because the interaction is significant, main effects should be interpreted in light of the level combinations rather than alone")
  for (key in names(s$posthoc)) { pt <- posthoc_pairs_txt(s$posthoc[[key]], lang); if (!is.null(pt)) out <- c(out, P("posthoc", paste0(key, ": ", pt))) }
  if (!is.null(s$homo) && !is.na(s$homo$p) && s$homo$p < .05) out <- c(out, if (lang == "tr") "Varyans homojenliği sağlanmadığından F testleri temkinle yorumlanmalıdır" else "Because homogeneity of variances was violated, the F tests should be interpreted cautiously")
  c(out, P("multi"))
}

ri_anovaRM <- function(s, lang, bk, P, rng) {
  out <- character(); lay <- if (length(s$within)) s$within[[1]]$correction else "none"
  for (w in Filter(function(w) w$correction == lay, s$within)) { out <- c(out, if (lang == "tr") paste0(w$name, " denek-içi etkisi ", if (sig(w$p)) "anlamlıdır" else "anlamlı değildir") else paste0("The within-subjects effect of ", w$name, " was ", if (sig(w$p)) "significant" else "not significant"))
    if (sig(w$p) && !is.na(w$partEta)) { m <- es_magnitude(w$partEta, "etap"); out <- c(out, sprintf(pick(bk$es, rng), if (lang == "tr") mag_tr[[m]] else mag_en[[m]], bk$es_tail[[m]])) } }
  for (b in s$between) out <- c(out, if (lang == "tr") paste0(b$name, " gruplar-arası etkisi ", if (sig(b$p)) "anlamlıdır" else "anlamlı değildir") else paste0("The between-subjects effect of ", b$name, " was ", if (sig(b$p)) "significant" else "not significant"))
  if (any(vapply(s$spher, function(sp) !is.na(sp$p) && sp$p <= .05, logical(1)))) out <- c(out, if (lang == "tr") "Küresellik ihlal edildiğinden düzeltilmiş serbestlik dereceleri esas alınmalıdır" else "Because sphericity was violated, corrected degrees of freedom should be relied upon")
  c(out, P("multi"))
}

ri_anovaNP <- function(s, lang, bk, P, rng) {
  out <- character()
  for (r in s$rows) { out <- c(out, if (sig(r$p)) P("sig_diff", r$dep, s$group) else P("ns_diff", r$dep, s$group))
    if (sig(r$p) && !is.na(r$es)) { m <- es_magnitude(r$es, "eps"); out <- c(out, sprintf(pick(bk$es, rng), if (lang == "tr") mag_tr[[m]] else mag_en[[m]], bk$es_tail[[m]])) }
    pairs <- if (!is.null(r$dunn)) lapply(r$dunn, function(d) list(a = d$a, b = d$b, p = d$padj)) else if (!is.null(r$dscf)) lapply(r$dscf, function(d) list(a = d$a, b = d$b, p = d$p)) else list()
    if (sig(r$p) && length(pairs)) { pt <- posthoc_pairs_txt(pairs, lang); out <- c(out, if (is.null(pt)) P("posthoc_none") else P("posthoc", pt)) }
    if (!sig(r$p)) out <- c(out, P("ns_es")) }
  c(out, P("multi"))
}

ri_anovaRMNP <- function(s, lang, bk, P, rng) {
  out <- c(if (sig(s$p)) (if (lang == "tr") "Tekrarlı ölçümler arasında anlamlı bir fark bulunmaktadır" else "The repeated measurements differed significantly") else (if (lang == "tr") "Tekrarlı ölçümler arasında anlamlı bir fark bulunmamaktadır" else "The repeated measurements did not differ significantly"))
  if (sig(s$p) && length(s$pairs)) { pt <- posthoc_pairs_txt(s$pairs, lang); out <- c(out, if (is.null(pt)) P("posthoc_none") else P("posthoc", pt)) }
  c(out, P("multi"))
}

ri_corr <- function(s, lang, bk, P, rng) {
  out <- character()
  for (p in s$pairs) {
    use <- if (!is.na(p$r)) list(v = p$r, p = p$rp) else if (!is.na(p$rho)) list(v = p$rho, p = p$rhop) else list(v = p$tau, p = p$taup)
    if (is.na(use$v)) next
    m <- magw(use$v, "r", lang)
    out <- c(out, if (!sig(use$p)) P("corr_ns", p$a, p$b) else if (use$v > 0) P("corr_pos", p$a, p$b, m) else P("corr_neg", p$a, p$b, m))
  }
  c(out, P("corr_caution"))
}

ri_linReg <- function(s, lang, bk, P, rng) {
  m <- s$models[[length(s$models)]]; out <- character()
  r2 <- if (lang == "tr") paste0("%", fmt_num(100 * m$r2, 1)) else paste0(fmt_num(100 * m$r2, 1), "%")
  out <- c(out, if (!is.na(m$p) && !sig(m$p)) P("reg_ns", s$dep) else if (lang == "tr") sprintf(pick(bk$reg_sig, rng), s$dep, r2) else sprintf(pick(bk$reg_sig, rng), if (grepl("^Together", pick(bk$reg_sig, rng))) r2 else s$dep, if (grepl("^Together", pick(bk$reg_sig, rng))) s$dep else r2))
  sigt <- Filter(function(t) !grepl("Intercept|Sabit", t$term) && sig(t$p), m$terms)
  out <- c(out, if (length(sigt)) P("reg_pred", join_words(vapply(sigt, function(t) t$term, character(1)), lang)) else P("reg_nopred"))
  if (!is.na(m$vif_max) && is.finite(m$vif_max) && m$vif_max >= 5) out <- c(out, if (lang == "tr") "Yüksek VIF değeri çoklu bağlantı sorununa işaret etmektedir; katsayılar kararsız olabilir" else "The high VIF indicates multicollinearity; coefficients may be unstable")
  c(out, P("reg_caution"))
}

ri_logReg <- function(s, lang, bk, P, rng) {
  m <- s$models[[length(s$models)]]; out <- character()
  out <- c(out, if (!is.na(m$p) && !sig(m$p)) P("reg_ns", s$dep) else (if (lang == "tr") sprintf("Model %s değişkenini anlamlı biçimde yordamaktadır", s$dep) else sprintf("The model significantly predicted %s", s$dep)))
  sigt <- Filter(function(t) !grepl("Intercept|Sabit", t$term) && sig(t$p), m$terms)
  if (length(sigt)) { desc <- vapply(sigt, function(t) { if (!is.na(t$odds)) paste0(t$term, " (", if (lang == "tr") (if (t$odds > 1) "olasılığı artırıyor" else "olasılığı azaltıyor") else (if (t$odds > 1) "increases the odds" else "decreases the odds"), ")") else t$term }, character(1)); out <- c(out, P("reg_pred", join_words(desc, lang))) } else out <- c(out, P("reg_nopred"))
  if (!is.na(m$accuracy)) out <- c(out, if (lang == "tr") sprintf("Doğru sınıflandırma oranı %s'dir", paste0("%", fmt_num(100 * m$accuracy, 1))) else sprintf("Classification accuracy was %s", paste0(fmt_num(100 * m$accuracy, 1), "%")))
  c(out, P("reg_caution"))
}

ri_contTables <- function(s, lang, bk, P, rng) {
  t <- s$tests[[1]]; out <- character()
  out <- c(out, if (sig(t$p)) P("chi_sig", s$rows, s$cols) else P("chi_ns", s$rows, s$cols))
  V <- if (!is.na(t$df) && t$df == 1 && !is.na(t$phi)) t$phi else t$cramer
  if (sig(t$p) && !is.null(V) && !is.na(V)) { m <- if (!is.na(t$df) && t$df == 1) magw(V, "phi", lang) else { md <- suppressWarnings(min(s$dims, na.rm = TRUE)); k <- es_magnitude_v(V, if (is.finite(md)) md else 2); if (lang == "tr") mag_tr[[k]] else mag_en[[k]] }; out <- c(out, P("chi_es", m)) }
  if (!is.na(t$fisherp)) out <- c(out, P("chi_small"))
  c(out, P("multi"))
}

ri_reliability <- function(s, lang, bk, P, rng) {
  a <- if (!is.na(s$alpha)) s$alpha else s$omega
  lvl <- if (is.na(a)) NA else if (a >= .9) (if (lang == "tr") "mükemmel" else "excellent") else if (a >= .8) (if (lang == "tr") "iyi" else "good") else if (a >= .7) (if (lang == "tr") "kabul edilebilir" else "acceptable") else if (a >= .6) (if (lang == "tr") "sorgulanabilir" else "questionable") else (if (lang == "tr") "düşük" else "poor")
  out <- if (!is.na(a) && a >= .7) P("rel_good", lvl) else P("rel_poor", lvl)
  low <- Filter(function(i) !is.na(i$itemRestCor) && i$itemRestCor < .30, s$items)
  if (length(low)) out <- c(out, if (lang == "tr") sprintf("%s maddelerinin madde-kalan korelasyonu düşüktür; çıkarılmaları güvenirliği artırabilir", join_words(vapply(low, function(i) i$name, character(1)), lang)) else sprintf("Items %s have low item-rest correlations; removing them may improve reliability", join_words(vapply(low, function(i) i$name, character(1)), lang)))
  out
}

ri_efa <- function(s, lang, bk, P, rng) {
  out <- character()
  if (!is.na(s$kmo)) out <- c(out, if (lang == "tr") paste0("KMO değeri örneklem yeterliliğinin ", if (s$kmo >= .8) "iyi" else if (s$kmo >= .6) "kabul edilebilir" else "yetersiz", " olduğunu göstermektedir") else paste0("The KMO value indicates ", if (s$kmo >= .8) "good" else if (s$kmo >= .6) "acceptable" else "inadequate", " sampling adequacy"))
  if (!is.null(s$bartlett) && !is.na(s$bartlett$p)) out <- c(out, if (sig(s$bartlett$p)) (if (lang == "tr") "Bartlett testi anlamlı olduğundan korelasyon matrisi faktör analizine uygundur" else "Bartlett's test was significant, so the correlation matrix is suitable for factor analysis") else (if (lang == "tr") "Bartlett testi anlamlı olmadığından faktör çözümü temkinle ele alınmalıdır" else "Bartlett's test was not significant, so the factor solution should be treated cautiously"))
  if (length(s$factors)) { last <- s$factors[[length(s$factors)]]; out <- c(out, if (lang == "tr") sprintf("%d faktörlü çözüm toplam varyansın %s'ini açıklamaktadır; bu oran %s", length(s$factors), paste0("%", fmt_num(last$varCum, 1)), if (last$varCum >= 50) "yeterli kabul edilebilir" else "görece düşüktür ve madde yapısı gözden geçirilebilir") else sprintf("The %d-factor solution explained %s of the total variance, which is %s", length(s$factors), paste0(fmt_num(last$varCum, 1), "%"), if (last$varCum >= 50) "acceptable" else "relatively low, so the item set may be reviewed")) }
  c(out, if (lang == "tr") "Faktör adlandırması içerik geçerliliği açısından uzman değerlendirmesi gerektirir" else "Factor naming requires expert judgement of content validity")
}

ri_cfa <- function(s, lang, bk, P, rng) {
  good <- c(if (!is.na(s$cfi)) s$cfi >= .90, if (!is.na(s$rmsea)) s$rmsea <= .08, if (!is.na(s$srmr)) s$srmr <= .08)
  c(if (length(good) && all(good)) (if (lang == "tr") "Uyum indeksleri modelin veriyle kabul edilebilir düzeyde uyuştuğunu göstermektedir" else "The fit indices indicate an acceptable fit of the model to the data") else (if (lang == "tr") "Uyum indeksleri yetersiz uyuma işaret etmektedir; model yeniden belirlenmelidir" else "The fit indices indicate inadequate fit; the model should be respecified"),
    if (!is.na(s$p) && sig(s$p)) (if (lang == "tr") "Anlamlı ki-kare büyük örneklemlerde beklenen bir durumdur; yaklaşık uyum indeksleri esas alınmalıdır" else "A significant chi-square is expected in large samples; approximate fit indices should be relied upon") else "")
}
