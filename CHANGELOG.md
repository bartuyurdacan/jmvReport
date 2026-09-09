# Changelog

## 0.1.0 — 2026-09-09
- First release.
- Report Writer (from saved .omv): re-runs every jmv analysis stored in the file, writes Method + Results (TR/EN), lists analyses, optional raw tables.
- Wrapper analyses with report text: independent samples t-test, one-way ANOVA / Kruskal-Wallis, correlation matrix, contingency tables.
- Template engine (Layer 0) for 24 jmv analysis types; generic table listing for the rest.
- Optional local AI polishing via Ollama with number-fidelity verification and automatic fallback.
