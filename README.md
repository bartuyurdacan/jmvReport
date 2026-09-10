# jmvReport — AI Report Writer for jamovi

`jmvReport` is an English-only jamovi module that writes APA-style **Method
(Statistical Analysis)** and **Results** text from analyses performed in jamovi.

- **Report Writer (from saved file)** reads the analyses stored in a saved
  `.omv` file, recomputes supported results, and creates one report.
- **Analyses with Report** provides wrappers for independent-samples t tests,
  one-way ANOVA/Kruskal–Wallis tests, correlations, and contingency tables.

## How reporting works

The deterministic R template engine is always available and remains the source
of every statistical value. AI is optional and disabled by default. When AI is
enabled, it may add a concise interpretation and, if requested, improve the
wording. Rewritten text is checked against the computed values; jmvReport falls
back to the template text if the check fails.

The AI backend can be selected per analysis:

- **Auto** tries a running Ollama instance first and then the managed built-in
  runtime.
- **Ollama** uses the local or network Ollama server specified in the options.
- **Built-in llama.cpp** runs on `127.0.0.1` with a per-session access token.
- **OpenAI-compatible server** connects to a user-supplied `/v1` endpoint. Set
  the optional API key in the `JMVREPORT_API_KEY` environment variable.

With Ollama or the built-in backend, data stays on the computer. A custom
OpenAI-compatible endpoint receives the result digest sent to it, so its privacy
policy and data-handling terms apply.

## Built-in AI setup

The built-in backend is optional and is not bundled with the module. Open
**AI Report ▸ Local AI Setup**, then select **Install built-in AI**. This explicit
action downloads a pinned, platform-specific llama.cpp runtime and a verified
Qwen3.5 4B GGUF model of approximately 2.4 GB. SHA-256 verification is required
before either download is used. Files are stored in the user's cache.

The managed runtime supports Linux and macOS on x86-64 or ARM64, and Windows on
x86-64. Ollama and OpenAI-compatible servers remain available on other systems.

## Supported saved-file analyses

Descriptives; independent, paired, and one-sample t tests; Welch,
Mann–Whitney, and Wilcoxon tests; one-way and factorial ANOVA; ANCOVA; repeated
measures ANOVA; Kruskal–Wallis and Friedman tests; correlations; linear and
logistic regression; contingency tables; McNemar and proportion tests;
reliability; EFA/PCA; CFA; and MANOVA/MANCOVA. Unsupported analyses are retained
in the analysis list with their stored tables.

## Installation and use

1. Download the `.jmo` file for your operating system from the releases page.
2. In jamovi, choose **Modules ▸ jamovi library ▸ Sideload**, then select it.
3. Run analyses normally, or open **AI Report ▸ Report Writer (from saved
   file)** after saving the `.omv` file.
4. Enable AI only when an AI interpretation is wanted. Template reporting does
   not require an AI backend.

Review and edit generated prose before publication. Statistical interpretation
and scientific conclusions remain the author's responsibility.

## Building from source

```r
install.packages("jmvtools", repos = c("https://repo.jamovi.org", "https://cran.r-project.org"))
jmvtools::prepare()
jmvtools::install()
testthat::test_dir("tests/testthat")
```

GPL-3 · Fikret Bartu Yurdacan, 2026
