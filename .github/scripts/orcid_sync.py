#!/usr/bin/env python3
"""Propose new LifeWork.bib entries from the ORCID record.

Run by .github/workflows/orcid-sync.yml. Reads the public ORCID works list,
skips anything already handled (its ORCID put-code is in _orcid-seen.txt) or
already in the bib (same DOI or near-identical title), and appends a BibTeX
entry for each remaining work. Entries come from the publisher's DOI metadata
when there is a DOI, otherwise from what ORCID has.

Standard library only, so the workflow needs no pip install.
"""

import difflib
import json
import os
import re
import sys
import urllib.request
from pathlib import Path

ORCID_ID = "0000-0001-5594-7697"
SITE = Path(__file__).resolve().parents[2] / "quarto-site"
BIB = SITE / "LifeWork.bib"
SEEN = SITE / "_orcid-seen.txt"
SUMMARY = Path(os.environ.get("SUMMARY_FILE", "orcid-sync-summary.md"))

STOPWORDS = {"a", "an", "the", "of", "on", "in", "for", "and", "to", "with", "using"}
KEEP_FIELDS = ["author", "title", "journal", "booktitle", "year", "volume",
               "number", "pages", "doi", "url"]


def get(url, accept):
    req = urllib.request.Request(url, headers={
        "Accept": accept,
        "User-Agent": "schafert-website-orcid-sync (https://toryn.netlify.app)",
    })
    with urllib.request.urlopen(req, timeout=60) as r:
        return r.read().decode("utf-8")


def norm_title(t):
    t = re.sub(r"\\['`^\"~=.]|[{}]", "", t or "")
    return re.sub(r"[^a-z0-9]+", " ", t.lower()).strip()


def norm_doi(d):
    d = (d or "").strip().lower()
    return re.sub(r"^https?://(dx\.)?doi\.org/", "", d)


def parse_bibtex_fields(entry):
    """Return {field: value} for one BibTeX entry, handling nested braces."""
    fields = {}
    body = entry[entry.index(",") + 1:]
    i = 0
    while i < len(body):
        m = re.compile(r"\s*([A-Za-z]+)\s*=\s*").match(body, i)
        if not m:
            break
        name, i = m.group(1).lower(), m.end()
        if body[i] == "{":
            depth, j = 0, i
            while True:
                depth += {"{": 1, "}": -1}.get(body[j], 0)
                if depth == 0:
                    break
                j += 1
            value, i = body[i + 1:j], j + 1
        elif body[i] == '"':
            j = body.index('"', i + 1)
            value, i = body[i + 1:j], j + 1
        else:
            m2 = re.compile(r"[^,}\s]+").match(body, i)
            value, i = m2.group(0), m2.end()
        fields[name] = re.sub(r"\s+", " ", value).strip()
        i = body.find(",", i)
        if i < 0:
            break
        i += 1
    return fields


def existing_bib():
    text = BIB.read_text()
    entries = re.split(r"\n(?=@)", text)
    dois, titles, keys = set(), [], set()
    for e in entries:
        m = re.match(r"@\w+\{([^,]+),", e.strip())
        if not m:
            continue
        keys.add(m.group(1))
        f = parse_bibtex_fields(e.strip())
        if f.get("doi"):
            dois.add(norm_doi(f["doi"]))
        if f.get("title"):
            titles.append(norm_title(f["title"]))
    return dois, titles, keys


def read_seen():
    seen = set()
    if SEEN.exists():
        for line in SEEN.read_text().splitlines():
            code = line.split("#", 1)[0].strip()
            if code:
                seen.add(code)
    return seen


def make_key(fields, keys):
    first_author = fields.get("author", "anon").split(" and ")[0]
    last = first_author.split(",")[0] if "," in first_author else first_author.split()[-1]
    last = re.sub(r"[^a-z-]", "", last.lower().replace(" ", "-"))
    words = [w for w in norm_title(fields.get("title", "")).split() if w not in STOPWORDS]
    base = f"{last}{fields.get('year', '')}{words[0] if words else ''}"
    key, n = base, 2
    while key in keys:
        key, n = f"{base}{chr(ord('a') + n - 2)}", n + 1
    keys.add(key)
    return key


def format_entry(kind, key, fields):
    lines = [f"@{kind}{{{key},"]
    present = [f for f in KEEP_FIELDS if fields.get(f)]
    for n, name in enumerate(present):
        end = "}" if n == len(present) - 1 else ","
        lines.append(f"  {name:<8} = {{{fields[name]}}}{end}")
    return "\n".join(lines)


def entry_for(work):
    """Build (bibtex_type, fields, source_note) for one ORCID work summary."""
    doi = work["doi"]
    fields, kind, source = {}, "article", "ORCID record (no DOI)"
    if doi:
        try:
            raw = get(f"https://doi.org/{doi}", "application/x-bibtex").strip()
            kind = re.match(r"@(\w+)\{", raw).group(1).lower()
            fields = parse_bibtex_fields(raw)
            source = f"publisher metadata for DOI {doi}"
        except Exception as e:  # fall back to ORCID's own metadata
            print(f"warning: DOI lookup failed for {doi}: {e}", file=sys.stderr)
    fields.setdefault("title", work["title"])
    fields.setdefault("year", work["year"] or "")
    if work["journal"] and not fields.get("booktitle"):
        fields.setdefault("journal", work["journal"])
    if doi:
        fields["doi"] = doi
        fields["url"] = f"https://doi.org/{doi}"
    elif work["url"]:
        fields.setdefault("url", work["url"])
    if work["type"] == "preprint":
        # DOI metadata for preprints has no journal; placeholder to edit in the PR.
        kind, fields["journal"] = "article", "Preprint"
    if kind not in ("article", "inproceedings", "incollection", "book", "misc"):
        kind = "misc"
    return kind, fields, source


def main():
    data = json.loads(get(f"https://pub.orcid.org/v3.0/{ORCID_ID}/works",
                          "application/json"))
    seen = read_seen()
    bib_dois, bib_titles, keys = existing_bib()

    new_entries, seen_lines, report = [], [], []
    for group in data["group"]:
        summaries = group["work-summary"]
        codes = {str(s["put-code"]) for s in summaries}
        if codes & seen:
            continue
        s = summaries[0]
        ext = group.get("external-ids", {}).get("external-id", [])
        doi = next((norm_doi(e["external-id-value"]) for e in ext
                    if e["external-id-type"] == "doi"), "")
        work = {
            "code": str(s["put-code"]),
            "title": s["title"]["title"]["value"],
            "type": s["type"],
            "year": ((s.get("publication-date") or {}).get("year") or {}).get("value"),
            "journal": (s.get("journal-title") or {}).get("value"),
            "url": (s.get("url") or {}).get("value"),
            "doi": doi,
        }
        label = f"{work['title']} ({work['year']}, ORCID {work['type']})"

        t = norm_title(work["title"])
        close = difflib.get_close_matches(t, bib_titles, n=1, cutoff=0.9)
        if (doi and doi in bib_dois) or close:
            # Already in the bib under some form; just remember it.
            seen_lines.append(f"{work['code']}  # already in bib: {work['title']}")
            report.append(f"- Already in `LifeWork.bib`, marked as seen: {label}")
            continue

        kind, fields, source = entry_for(work)
        key = make_key(fields, keys)
        new_entries.append(format_entry(kind, key, fields))
        seen_lines.append(f"{work['code']}  # {key}")
        note = " **Replace the `Preprint` placeholder with the journal.**" \
            if work["type"] == "preprint" else ""
        report.append(f"- **New:** `{key}`: {label}, from {source}.{note}")

    if not seen_lines:
        print("Nothing new on ORCID.")
        return

    if new_entries:
        BIB.write_text(BIB.read_text().rstrip() + "\n\n"
                       + "\n\n".join(new_entries) + "\n")
    with SEEN.open("a") as f:
        f.write("\n".join(seen_lines) + "\n")

    SUMMARY.write_text(
        "New works found on your ORCID record "
        f"([{ORCID_ID}](https://orcid.org/{ORCID_ID})):\n\n"
        + "\n".join(report)
        + "\n\nEdit the entries in `quarto-site/LifeWork.bib` on this branch if "
        "anything needs fixing, then merge to publish.\n\n"
        "To skip a work without adding it, delete its entry from "
        "`LifeWork.bib` but keep its line in `quarto-site/_orcid-seen.txt`, "
        "then merge. It will not be proposed again.\n"
    )
    print(SUMMARY.read_text())


if __name__ == "__main__":
    main()
