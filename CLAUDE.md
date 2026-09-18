# Project context for Claude Code

This repo is Toryn Schafer's academic website (toryn.netlify.app), currently
mid-migration from a legacy Hugo theme to Quarto. Read this before doing
anything else here.

## Current state (as of 2026-09-17)

Two sites coexist in this repo right now:

1. **`content/`, `layouts/`, `config.yaml`, etc. — the LIVE Hugo site.**
   Do not break this. It's built with `hugo --gc --minify -b $URL` per
   `netlify.toml` and deploys to the live toryn.netlify.app.

2. **`quarto-site/` — a NEW Quarto site, built but UNTESTED and NOT yet
   deployed anywhere.** This is the migration target. See
   `quarto-site/DEPLOY.md` for full deploy instructions.

**Critical caveat: the Quarto site was built by an agent with no Quarto or R
installed in its sandbox.** Every file was hand-written and only
mechanically checked (YAML parses, `:::` div fences balance, referenced
files exist on disk) — none of it has actually been rendered. Treat
`quarto-site/` as a first draft that needs a real `quarto preview` pass, not
finished work.

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

## Immediate next steps

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
- No PDF-from-bib automation for the CV's publication list beyond what
  Quarto's citeproc already does on the HTML page — the downloadable PDF
  linked from `cv.qmd` is still the static `TSchafer_CV.pdf`, not
  regenerated from the bib. If the user wants the PDF itself automated too,
  RenderCV (YAML→Typst→PDF) was discussed as the natural pairing but not
  built.
