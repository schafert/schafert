"""Shared helper for scripts that add items to the home page news feed."""

import calendar
import re
from datetime import date
from pathlib import Path

INDEX = Path(__file__).resolve().parents[2] / "quarto-site" / "index.qmd"
MONTHS = {name: n for n, name in enumerate(calendar.month_name) if name}


def month_label(when):
    return f"{calendar.month_name[when.month]} {when.year}"


def news_date(line):
    """Date of a news item like '- **July 2026:** ...', or None."""
    m = re.match(r"- \*\*([A-Z][a-z]+) (\d{4}):\*\*", line)
    if not m or m.group(1) not in MONTHS:
        return None
    return date(int(m.group(2)), MONTHS[m.group(1)], 1)


def add_news(text, when):
    """Insert '- **Month YYYY:** text' into the news feed, newest first."""
    item = f"- **{month_label(when)}:** {text}\n"
    lines = INDEX.read_text().splitlines(keepends=True)
    start = next(i for i, l in enumerate(lines) if l.startswith("::: {.news-feed}"))
    end = next(i for i in range(start + 1, len(lines)) if lines[i].strip() == ":::")
    at = end
    for i in range(start + 1, end):
        d = news_date(lines[i])
        if d and d <= date(when.year, when.month, 1):
            at = i
            break
    lines.insert(at, item)
    INDEX.write_text("".join(lines))
