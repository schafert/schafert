#!/usr/bin/env python3
"""Propose a news item and CV update when a CRAN package has a new release.

Run by .github/workflows/cran-check.yml. For each package in PACKAGES, reads
the version and publication date from its CRAN page and compares the version
with the one in the CV's Software entry (the line in cv.qmd linking to
package=<name>). If CRAN is ahead, it updates that line's version and year and
adds a dated news item to the home page feed, in date order.

Standard library only, so the workflow needs no pip install.
"""

import os
import re
import urllib.request
from datetime import date
from pathlib import Path

from news import add_news

PACKAGES = ["dsp"]
SITE = Path(__file__).resolve().parents[2] / "quarto-site"
CV = SITE / "cv.qmd"
SUMMARY = Path(os.environ.get("SUMMARY_FILE", "cran-check-summary.md"))


def cran_release(pkg):
    """Return (version, published date) from the package's CRAN page."""
    url = f"https://cran.r-project.org/web/packages/{pkg}/index.html"
    req = urllib.request.Request(url, headers={
        "User-Agent": "schafert-website-cran-check (https://toryn.netlify.app)",
    })
    with urllib.request.urlopen(req, timeout=60) as r:
        html = r.read().decode("utf-8")

    def field(name):
        m = re.search(rf"<td>{name}:</td>\s*<td>([^<]+)</td>", html)
        if not m:
            raise RuntimeError(f"no {name} field on {url}")
        return m.group(1).strip()

    return field("Version"), date.fromisoformat(field("Published"))


def main():
    cv = CV.read_text()
    report = []
    for pkg in PACKAGES:
        version, published = cran_release(pkg)
        m = re.search(rf"^.*package={pkg}\b.*$", cv, re.M)
        if not m:
            raise RuntimeError(f"no line linking to package={pkg} in cv.qmd")
        line = m.group(0)
        old = re.search(r"R package version ([\d.\-]+)", line).group(1).rstrip(".")
        if old == version:
            print(f"{pkg}: CV already lists {version}.")
            continue

        new_line = line.replace(f"version {old}", f"version {version}")
        new_line = re.sub(r"\(\d{4}\)", f"({published.year})", new_line, count=1)
        cv = cv.replace(line, new_line)

        link = f"https://CRAN.R-project.org/package={pkg}"
        add_news(f"Released version {version} of the R package [{pkg}]({link}).",
                 published)
        report.append(f"- **{pkg}** {old} → {version}, published on CRAN "
                      f"{published.isoformat()} ([CRAN page]({link})).")

    if not report:
        return
    CV.write_text(cv)
    SUMMARY.write_text(
        "New CRAN releases found:\n\n" + "\n".join(report)
        + "\n\nThis branch updates the version in the CV's Software section "
        "and adds a news item to the home page. Edit the news item in "
        "`quarto-site/index.qmd` (for example, to say what changed), then merge "
        "to publish.\n\n"
        "If the release is not worth a news item, delete that line but keep "
        "the CV change, then merge. Closing this pull request instead means "
        "the release will be proposed again next month.\n"
    )
    print(SUMMARY.read_text())


if __name__ == "__main__":
    main()
