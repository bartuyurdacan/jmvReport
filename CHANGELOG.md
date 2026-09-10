# Changelog

## 0.2.1 — 2026-09-10

- Made AI interpretation a dedicated AI-only call with no response-marker dependency.
- Kept optional Results polishing separate and protected by number-fidelity checks.
- Added an OpenAI API quick setup using the official endpoint, `gpt-4.1-mini`,
  and the standard `OPENAI_API_KEY` environment variable.
- Improved fallback messages so interpretation failures are not described as
  deterministic interpretation.

## 0.2.0 — 2026-09-10

- Made the module and generated reports English-only.
- Kept deterministic APA templates as the default reporting path; AI is now
  optional and disabled by default.
- Added Auto, Ollama, managed built-in llama.cpp, and OpenAI-compatible AI
  backend choices.
- Added an explicit Local AI Setup analysis. It downloads nothing until the
  user starts installation and verifies pinned downloads with SHA-256.
- Added per-session authentication and loopback-only networking for the managed
  llama.cpp server.
- Preserved number-fidelity checks and automatic template fallback.

## 0.1.1 — 2026-09-09

- Reduced AI work to one short interpretation call per analysis by default.
- Added an optional verified Results-wording rewrite.
- Added token and timing information.
- Kept Method text deterministic.

## 0.1.0 — 2026-09-09

- First release.
