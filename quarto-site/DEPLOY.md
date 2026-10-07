# Deploying this site

This is a new Quarto site, built alongside your existing Hugo site so nothing
live breaks while it's being set up. Your current site at toryn.netlify.app
is untouched — this becomes a *second*, separate Netlify site until you're
ready to cut over.

## One-time setup (do this once, locally)

1. **Install Quarto**: https://quarto.org/docs/get-started/ (just the CLI —
   no R or Python needed, since none of these pages execute code).

2. **Preview it** to check everything renders correctly before going further:
   ```
   cd quarto-site
   quarto preview
   ```
   This opens a live-reloading local copy in your browser. Check the Home,
   Publications, CV, and Teaching pages. I could not render this myself while
   building it (no Quarto/R available in my sandbox), so this is the first
   real test of the syntax — if anything looks broken, tell me and I'll fix it.

3. **Connect it to Netlify** (creates a new site, separate from your current one):
   ```
   quarto publish netlify
   ```
   This will open a browser to authenticate with Netlify, create a new site,
   and generate a `_publish.yml` file in this folder recording the new site's
   ID. **Commit that `_publish.yml` file** — the GitHub Action needs it.

4. **Add your Netlify token to GitHub** so the Action can deploy on your behalf:
   - Go to https://app.netlify.com/user/applications → **New access token**
   - Copy the token
   - In your GitHub repo: **Settings → Secrets and variables → Actions →
     New repository secret**
   - Name it `NETLIFY_AUTH_TOKEN`, paste the token

5. **Push.** From then on, any push that touches `quarto-site/` (including
   editing `LifeWork.bib`) triggers `.github/workflows/quarto-publish.yml`,
   which renders and deploys automatically — same pattern as the Hugo
   automation, just simpler since there's no R build step in CI.

## Going live

Once you're happy with the new site: in Netlify, go to the new site's
**Domain settings** and move `toryn.netlify.app` over to it (or point your
custom domain at it, if you have one). Then the old Hugo site's Netlify
config can be retired. Nothing forces this — take your time comparing the two
live side by side first.

## Publications workflow going forward

New papers arrive automatically. On the 1st of each month,
`.github/workflows/orcid-sync.yml` checks the ORCID record for works not yet
in `LifeWork.bib` and opens a pull request adding them, using the
publisher's DOI metadata. Review the entry (for a preprint, replace the
`Preprint` placeholder with the journal it is under review at), then merge.
Merging publishes the site. Run it any time from the repo's **Actions** tab
(**Sync publications from ORCID → Run workflow**).

To skip an ORCID work for good, delete its entry from `LifeWork.bib` in the
pull request but keep its line in `_orcid-seen.txt`, then merge.

Adding a paper by hand still works too: edit `LifeWork.bib`, push.

## The CV

`cv.qmd` is the single source for the CV. Every deploy builds both the CV web
page and `TSchafer_CV.pdf` (via Typst, bundled with Quarto) from it, and the
publication list in both comes from `LifeWork.bib`. The old address
`/files/TSchafer_CV.pdf` redirects to the new PDF (see `_redirects`).

Building the PDF locally needs a recent Quarto (1.10 works; 1.4 does not).
