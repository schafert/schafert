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

Same as before, just simpler: edit `LifeWork.bib` in this folder, push. No R
script step — Quarto's citeproc renders the bibliography directly into both
`publications.qmd` and the Publications section of `cv.qmd` natively.
