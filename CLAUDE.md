# Project context for Claude Code

This repo is Toryn Schafer's academic website (toryn.netlify.app), built
with Quarto. It also holds `README.md`, which is the user's GitHub profile
README (this repo is `schafert/schafert`); leave it alone unless asked.

## Layout and deploys

- **`quarto-site/` is the whole site.** Pages: `index.qmd` (home, `about:
  template: trestles`), `publications.qmd`, `cv.qmd`, `teaching.qmd`.
  No executable R/Python chunks anywhere, so CI needs only Quarto.
- **Deploys are automatic.** Any push touching `quarto-site/**` runs
  `.github/workflows/quarto-publish.yml`, which renders and publishes to
  the Netlify project `toryn` (id in `quarto-site/_publish.yml`) using the
  `NETLIFY_AUTH_TOKEN` repo secret. That Netlify project is NOT linked to
  GitHub; the Action deploys it.
- Extra files served as-is are listed under `project: resources:` in
  `_quarto.yml`: `_redirects` (old `/files/TSchafer_CV.pdf` address →
  `/TSchafer_CV.pdf`), the Google Search Console verification file
  `google3f70da9411146510.html` (must stay at site root), and
  `files/*.pdf` (JSM 2019 proceedings paper, energy-price preprint).
- Local Quarto is 1.10.19, same as CI. `quarto publish netlify` needs an
  interactive terminal; it fails under Claude Code's `!` prefix.

## Publications

- `quarto-site/LifeWork.bib` is the single source. Quarto's citeproc
  (`bibliography:` + `nocite: '@*'` + `apa-cv.csl`) renders it on both the
  Publications page and the CV.
- Monthly `.github/workflows/orcid-sync.yml` runs
  `.github/scripts/orcid_sync.py` (stdlib Python). It compares the ORCID
  record with the bib by ORCID put-code (`quarto-site/_orcid-seen.txt`),
  then DOI, then title, and opens a PR on branch `orcid-sync` with
  DOI-sourced BibTeX. It skips if a previous `orcid-sync` PR is still open.
  Several ORCID titles are old working titles that differ from the bib,
  which is why the put-code list exists.
- Preprints are wanted. They get `journal = {Preprint}`, which the user
  replaces in the PR (e.g. "Preprint; Under Review at Ecography").

## CV

- `quarto-site/cv.qmd` is the source of truth for the CV. The user's old
  OneDrive `TSchafer_CV.tex` was folded in on 2026-10-07 and is retired.
- It renders to HTML and to `TSchafer_CV.pdf` via Typst. HTML-only markup
  must sit inside `::: {.content-visible when-format="html"}`. Check the
  PDF after layout changes (tables need dash-width hints for column sizes).
- Talks are labeled "(Invited)" only when officially invited.

## History

- Until 2026-10 the site was a Wowchemy/Hugo site with an R script that
  turned the bib into per-paper Markdown files. It was migrated to Quarto
  for native citeproc and because the legacy `wowchemy-hugo-modules` path
  was unmaintained. Cut over 2026-10-07; Hugo files removed 2026-10-08.
  Everything is recoverable from git history (before commit `a9bc8c9`).
- The old site is frozen on Netlify as project `toryn-hugo`, unlinked from
  GitHub, as a fallback. The user may delete it.
