# Changelog

## 0.1.1 — 2026-09-09
- Speed: AI now makes ONE short call per analysis (interpretation on a plain-text digest) instead of three; iris one-way ANOVA + Kruskal-Wallis on a CPU-only laptop went from 276 s to ~80 s.
- New "AI interpretation of the findings" paragraph (wrappers and Report Writer); numbers are verified, no new numbers allowed.
- "Also rewrite the Results wording with AI (slower)" option (off by default) restores the full rewrite; interpretation is generated first so truncation never loses it.
- Token/speed line (calls, prompt + generated tokens, tokens/s, seconds) shown under the report and in the Report Writer Info box.
- Method paragraph is no longer sent to the model (template text only).

## 0.1.0 — 2026-09-09
- First release.
- Report Writer (from saved .omv): re-runs every jmv analysis stored in the file, writes Method + Results (TR/EN), lists analyses, optional raw tables.
- Wrapper analyses with report text: independent samples t-test, one-way ANOVA / Kruskal-Wallis, correlation matrix, contingency tables.
- Template engine (Layer 0) for 24 jmv analysis types; generic table listing for the rest.
- Optional local AI polishing via Ollama with number-fidelity verification and automatic fallback.
