# jmvReport — AI Report Writer for jamovi

**English below · [Türkçe](#türkçe)**

`jmvReport` is a [jamovi](https://www.jamovi.org) module that writes the **Method (Statistical Analysis)** and **Results** paragraphs of your report in APA 7 style, in **Turkish or English**, for the analyses you ran in jamovi.

- **Report Writer (from saved file)** — reads *every* analysis stored in a saved jamovi file (`.omv`), recomputes the results and writes the text for all of them at once.
- **Analyses with Report** — wrapper analyses (independent samples t-test, one-way ANOVA / Kruskal-Wallis, correlation matrix, contingency tables) that show the report text directly under the tables.

### How the text is produced

| Layer | What it does | Requirements |
|---|---|---|
| **0 — Template engine** | Deterministic APA sentences generated in R from the computed statistics. Every number in the report comes from here. | none (works everywhere) |
| **1 — Local AI polish (optional)** | A local language model served by [Ollama](https://ollama.com) rewrites the template text into more fluent prose. The output is **verified number-by-number** against the template; if any number is added, dropped or changed, the template text is shown instead. | Ollama installed and running (`ollama pull qwen3.5:4b`) |

No cloud service, no API key, no data leaves your computer.

### Supported analyses (Report Writer)

Descriptives, independent/paired/one-sample t-tests (incl. Welch, Mann-Whitney, Wilcoxon), one-way ANOVA (Fisher/Welch + post hoc), factorial ANOVA / ANCOVA, repeated measures ANOVA, Kruskal-Wallis, Friedman, correlation matrix, linear regression, binomial/ordinal/multinomial logistic regression, contingency tables (χ², Fisher, φ/Cramér's V), McNemar, binomial test, χ² goodness of fit, reliability (α/ω), EFA/PCA, CFA, MANOVA/MANCOVA. Other analyses are listed with their tables.

### Installation

1. Download `jmvReport_x.y.z.jmo` from the releases page.
2. In jamovi: **Modules ▸ jamovi library ▸ Sideload** (⋮ menu) ▸ choose the `.jmo` file.
3. (Optional, for AI polishing) install Ollama and pull a model: `ollama pull qwen3.5:4b`. Larger models (e.g. `qwen3.5:8b`) write better Turkish but need more RAM; on a CPU-only laptop expect 1–3 minutes per analysis.

### Usage

**Report Writer:** run your analyses ▸ **save the file** (`.omv`) ▸ `AI Report ▸ Report Writer` ▸ leave *File* empty (the most recently saved `.omv` in Documents/Desktop/Downloads is used) or paste the path ▸ choose language ▸ tick **Generate report**.

**Analyses with Report:** `AI Report ▸ Analyses with Report ▸ …` — set variables as usual; the report text appears under the tables.

Copy the text from the results panel (right-click ▸ Copy) into your manuscript and edit as needed. The text reports only what the data show; interpretation and discussion remain the author's responsibility.

### Building from source

```r
install.packages("jmvtools", repos = c("https://repo.jamovi.org", "https://cran.r-project.org"))
jmvtools::install()   # builds and installs into the local jamovi
Rscript -e 'testthat::test_dir("tests/testthat")'
```

---

## Türkçe

`jmvReport`, jamovi'de yaptığınız analizler için **Yöntem (İstatistiksel Analiz)** ve **Bulgular** paragraflarını APA 7 biçiminde, **Türkçe veya İngilizce** yazan bir jamovi modülüdür.

- **Report Writer (from saved file)** — kaydedilmiş jamovi dosyasındaki (`.omv`) **tüm** analizleri okur, sonuçları yeniden hesaplar ve hepsinin metnini tek seferde yazar.
- **Analyses with Report** — bağımsız örneklemler t-testi, tek yönlü ANOVA / Kruskal-Wallis, korelasyon matrisi ve çapraz tablolar için rapor metnini doğrudan tabloların altında gösteren sarmalayıcı analizler.

### Metin nasıl üretilir?

| Katman | Ne yapar | Gereksinim |
|---|---|---|
| **0 — Şablon motoru** | Hesaplanan istatistiklerden R'da üretilen kesin APA cümleleri. Rapordaki her sayı buradan gelir. | yok (her yerde çalışır) |
| **1 — Yerel yapay zeka (isteğe bağlı)** | [Ollama](https://ollama.com) ile çalışan yerel bir dil modeli şablon metnini akıcılaştırır. Çıktı şablonla **sayı sayı karşılaştırılır**; bir sayı eklenir, silinir veya değişirse şablon metni gösterilir. | Ollama kurulu ve çalışıyor (`ollama pull qwen3.5:4b`) |

Bulut servisi yok, API anahtarı yok; veriniz bilgisayarınızdan çıkmaz.

### Kurulum

1. `jmvReport_x.y.z.jmo` dosyasını indirin.
2. jamovi'de **Modules ▸ jamovi library ▸ Sideload** (⋮ menüsü) ▸ `.jmo` dosyasını seçin.
3. (İsteğe bağlı) Ollama kurun ve model indirin: `ollama pull qwen3.5:4b`. Daha büyük modeller (ör. `qwen3.5:8b`) daha iyi Türkçe yazar ama daha çok RAM ister; yalnızca CPU'lu bir dizüstünde analiz başına 1–3 dakika bekleyin.

### Kullanım

**Report Writer:** analizleri yapın ▸ **dosyayı kaydedin** (`.omv`) ▸ `AI Report ▸ Report Writer` ▸ *File* kutusunu boş bırakın (Belgeler/Masaüstü/İndirilenler'deki en son kaydedilen `.omv` kullanılır) ya da yolu yapıştırın ▸ dili seçin ▸ **Generate report** kutusunu işaretleyin.

**Analyses with Report:** `AI Report ▸ Analyses with Report ▸ …` — değişkenleri her zamanki gibi seçin; rapor metni tabloların altında görünür.

Metni sonuç panelinden kopyalayıp (sağ tık ▸ Copy) makalenize yapıştırın ve gerektiği gibi düzenleyin. Metin yalnızca verinin gösterdiğini raporlar; yorum ve tartışma yazarın sorumluluğundadır.

### Lisans

GPL-3 · Fikret Bartu Yurdacan, 2026
