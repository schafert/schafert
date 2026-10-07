# Project context for Claude Code

This repo is Toryn Schafer's academic website (toryn.netlify.app). It was
migrated from a legacy Hugo theme to Quarto. Read this before doing anything
else here.

## Current state (as of 2026-10-07)

**`quarto-site/` is the LIVE site.** Migration complete and cut over:

- Rendered and checked locally with Quarto, then published. Netlify project
  `toryn` (id in `quarto-site/_publish.yml`) serves toryn.netlify.app.
- Deploys are automatic: any push touching `quarto-site/**` runs
  `.github/workflows/quarto-publish.yml` (uses the `NETLIFY_AUTH_TOKEN` repo
  secret). This Netlify project is NOT linked to GitHub; the Action deploys it.
- The old Hugo site still exists as Netlify project `toryn-hugo`, unlinked
  from GitHub (frozen at its last deploy) as a fallback. The Hugo files
  (`content/`, `layouts/`, `config.yaml`, `netlify.toml`, `R/`) are still in
  the repo but no longer deployed anywhere. The `Sync publications from
  BibTeX` workflow is disabled in GitHub (`gh workflow disable`), not deleted.
  Remove all of this once the user is confident in the new site.

## How we got here

1. Original problem: the publications list was maintained by hand as ~19
   individual Hugo `.md` files, badly out of sync with the author's real
   BibTeX file (`content/publication_list/LifeWork.bib`), and the R script
   that was supposed to regenerate them (`R/generate_pubs.R`) had real bugs:
   filename collisions from 20-char title truncation, no preservation of
   manually-added URLs on regenerate, and conference papers (`booktitle`)
   silently dropped because the script only read `journal`.

2. Fixed `R/generate_pubs.R` (citekey-based filenames, preserves URLs,
   handles booktitle) and rebuilt `LifeWork.bib` from the user's actual CV
   as the single source of truth — see git log for `che-castaldo2021critical`
   through `acosta2026machine`, 17 entries. A GitHub Actions workflow
   (`.github/workflows/sync-publications.yml`) runs this R script whenever
   the bib file changes and commits the regenerated `.md` files.

3. First Netlify deploy after that broke: `toml: basic strings cannot have
   new lines` — the R script was writing bib abstracts containing literal
   newlines directly into TOML basic strings. Fixed by adding a `clean_str()`
   helper in the R script that strips newlines and escapes quotes before
   writing title/publication/abstract fields. **If you see this error class
   again, check `clean_str()` is being applied everywhere a bib field gets
   written into a TOML string.**

4. Separately, an audit found the Hugo site imports the *legacy*
   `wowchemy-hugo-modules` path (pre-rename; Wowchemy became HugoBlox in
   2024), which is known to break unpredictably on Hugo version bumps since
   it's no longer actively maintained at that path. This is why a platform
   migration was considered at all, not just a publications-automation fix.

5. Compared HugoBlox (upgrade in place), Academic Pages/Jekyll, and Quarto
   with live screenshots. User chose **Quarto**, specifically because its
   native citeproc (`bibliography:` + `nocite: '@*'` + CSL) can drive both
   the website's publication list AND the CV's publication section from the
   *same* `LifeWork.bib`, eliminating the custom R→TOML pipeline entirely.

6. Built `quarto-site/` from scratch: `index.qmd` (bio, ported from the real
   `content/authors/admin/_index.md` — NOT the placeholder demo content
   still sitting in `content/home/*.md`, which is Wowchemy starter-kit
   boilerplate and should NOT be migrated), `publications.qmd` (pure
   citeproc, no custom code), `cv.qmd` (ported from `static/files/
   TSchafer_CV.pdf` — grants, ~40 talks, awards, teaching, service, referee
   list, mentoring — with publications pulled from the same bib), and
   `teaching.qmd`. Deliberately wrote zero executable R/Python code chunks
   in any `.qmd` file, so CI needs no R/Python setup at all — just Quarto
   itself.

## Next steps (user's stated priorities)

- Publications: monthly `.github/workflows/orcid-sync.yml` runs
  `.github/scripts/orcid_sync.py`, which compares the ORCID record with
  `LifeWork.bib` (by ORCID put-code via `quarto-site/_orcid-seen.txt`, then
  DOI, then title) and opens a PR. Preprints are wanted; they get a
  `journal = {Preprint}` placeholder the user replaces in the PR. Several
  ORCID titles are old working titles that differ from the bib, which is why
  the put-code list exists.
- CV: `cv.qmd` is now the source of truth for the CV (the user's old
  OneDrive `TSchafer_CV.tex` was folded in on 2026-10-07 and is retired).
  It renders to HTML and to `TSchafer_CV.pdf` via Typst. Keep `cv.qmd`
  free of HTML-only markup outside `content-visible when-format="html"`.
- Note: `quarto publish netlify` needs an interactive terminal; it fails
  under Claude Code's `!` prefix. Local Quarto 1.4 can't build the Typst PDF;
  CI uses the latest Quarto.

## Original migration steps (done, kept for history)

1. `cd quarto-site && quarto preview` — first real test of all of this.
   Check Home, Publications, CV, Teaching pages render correctly and look
   right. This has never been rendered before.
2. Fix whatever `quarto preview` surfaces. Likely candidates: the `about:
   template: trestles` layout on `index.qmd`, the `mortarboard-fill` /
   `person-vcard` Bootstrap icon names in `_quarto.yml` and `index.qmd`
   (verified these icons exist in Bootstrap Icons via web search, but not
   rendered), the `.btn .btn-outline-primary role="button"` link attribute
   syntax on the CV download link.
3. Follow the rest of `quarto-site/DEPLOY.md`: `quarto publish netlify`
   (creates `_publish.yml`, commit it), add `NETLIFY_AUTH_TOKEN` repo
   secret, then `.github/workflows/quarto-publish.yml` takes over on future
   pushes to `quarto-site/**`.
4. Once the user has compared the new Netlify site against the live one and
   is happy, move `toryn.netlify.app` over to it (Netlify domain settings)
   and retire the Hugo site's config.

## Known follow-ups, not yet done

- A manuscript is on the CV but not in `LifeWork.bib`: Hoose, Frisbie,
  Schafer, et al., "Landscape drivers of scaled quail occurrence...", in
  revision for *Journal of Wildlife Management*. Add it once it has a DOI —
  it's already listed under CV → Works in Progress by hand for now.
- `content/home/*.md` (about/accomplishments/contact/experience/skills
  widgets) is almost entirely unedited Wowchemy demo content (`active:
  false` on most, and the one active widget is empty) — confirmed while
  auditing, never migrated because there was nothing real to migrate. Worth
  a final check with the user before fully retiring the Hugo site, in case
  any of it was meant to be filled in rather than deleted.
- PDF CV automation: done 2026-10-07 (Quarto Typst output of `cv.qmd`).
