"""
scrape_Vic.py
-------------
Scrapes the statewide voting intention tables from:
  "Opinion polling for the 2026 Victorian state election" (Wikipedia)

Produces a CSV with exactly the same columns and coding as scrape.py
(same OUTPUT_FIELDS, party codes, NA handling, IND+OTH merging, 2PP pairing).
All table parsing is reused from scrape.py, so the two stay in sync.

Differences from the federal page handled here:
  - Tables are not split under year headings; the year is read from the
    Date column (e.g. '21-24 Nov 2025') and carried down the table.
    The year is then stripped from 'Date' so it looks like the federal output.
  - The firm column is headed 'Firm' rather than 'Polling firm'.
  - There are no Client / Interview mode columns (written as NA / blank,
    as scrape.py does for empty cells).
  - Electorate-level and leader tables are skipped.

Uses ONLY the Python standard library (no pip installs required).

Usage (from the repo root):
    python scripts/scrape_Vic.py
    python scripts/scrape_Vic.py --output my_file.csv
    python scripts/scrape_Vic.py --html local_page.html
"""

import os
import re
import csv
import sys
import argparse

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import scrape  # noqa: E402  (federal scraper: parsing helpers reused)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

URL = (
    "https://en.wikipedia.org/wiki/"
    "Opinion_polling_for_the_2026_Victorian_state_election"
)

DEFAULT_OUTPUT = "data/raw/data_Vic.csv"

# Year assumed for rows when no year can be found in the table at all
DEFAULT_YEAR = 2026

# Headings whose tables are NOT statewide voting intention
_EXCLUDE_HEADING_RE = re.compile(
    r"district|electorate|seat|regional|by-?election|leader|premier|"
    r"approval|satisfaction|preferred",
    re.I,
)

_YEAR_RE = re.compile(r"\b((?:19|20)\d{2})\b")


# ---------------------------------------------------------------------------
# Column classification: also accept 'Firm' as the polling-firm header
# ---------------------------------------------------------------------------

_classify_columns_federal = scrape.classify_columns


def classify_columns(grid, num_rows, num_cols):
    fixed_cols, pv_cols, tpp_cols = _classify_columns_federal(grid, num_rows, num_cols)
    if "Polling firm" not in fixed_cols:
        for ci in range(num_cols):
            chain = [c.lower() for c in scrape.column_label_chain(grid, num_rows, ci)]
            if any(re.sub(r"\[.*?\]", "", c).strip() in ("firm", "pollster") for c in chain):
                fixed_cols["Polling firm"] = ci
                break
    return fixed_cols, pv_cols, tpp_cols


# parse_table() looks this up in scrape's namespace
scrape.classify_columns = classify_columns


# ---------------------------------------------------------------------------
# Find statewide voting intention tables
# ---------------------------------------------------------------------------

def split_rows(table):
    """Same header/data split as scrape.parse_table()."""
    header_rows, data_rows = [], []
    passed_header = False
    for tr in table.find_all("tr"):
        has_th = any(c.tag == "th" for c in tr.children)
        has_td = any(c.tag == "td" for c in tr.children)
        if not passed_header:
            if has_th and not has_td:
                header_rows.append(tr)
            else:
                passed_header = True
                if has_td:
                    data_rows.append(tr)
        elif has_td or has_th:
            data_rows.append(tr)
    return header_rows, data_rows


def is_voting_intention_table(table):
    header_rows, _ = split_rows(table)
    header_text = " ".join(tr.all_text() for tr in header_rows).lower()
    return "primary" in header_text and "2pp" in header_text


def find_statewide_tables(html):
    """
    Return [(heading_year_or_None, table)] for every wikitable with both a
    'Primary vote' and a '2PP vote' header group, in document order,
    skipping tables under electorate / leader headings.
    """
    parser = scrape.TableParser()
    parser.feed(html)

    headings = {}       # level -> text of the current heading at that level
    results = []
    for child in parser.root.children:
        if child.tag in ("h2", "h3", "h4"):
            level = int(child.tag[1])
            headings[level] = child.all_text().strip()
            for lower in [l for l in headings if l > level]:
                del headings[lower]
        elif child.tag == "table" and child.has_class("wikitable"):
            context = list(headings.values())
            if any(_EXCLUDE_HEADING_RE.search(h) for h in context):
                continue
            if not is_voting_intention_table(child):
                continue
            heading_year = None
            for h in context:
                m = _YEAR_RE.fullmatch(h.strip())
                if m:
                    heading_year = int(m.group(1))
            results.append((heading_year, child))

    if not results:
        raise RuntimeError(
            "No statewide voting intention tables found. "
            "The page structure may have changed."
        )
    print(f"Found {len(results)} statewide voting intention table(s)")
    return results


# ---------------------------------------------------------------------------
# Split one table into per-year sub-tables, then parse with scrape.parse_table
# ---------------------------------------------------------------------------

def split_table_by_year(table, heading_year):
    """
    Return [(year, Node)] where each Node is a copy of the table holding the
    header rows plus the data rows for that year (document order kept).
    """
    header_rows, data_rows = split_rows(table)
    grid, n_hrows, num_cols = scrape.build_header_grid(header_rows)
    fixed_cols, _, _ = classify_columns(grid, n_hrows, num_cols)
    date_ci = fixed_cols.get("Date", 0)

    # Date text for every row (rowspans expanded)
    tracker = scrape.RowSpanTracker(num_cols)
    dates = []
    for tr in data_rows:
        cells = tracker.expand(tr)
        dates.append(cells[date_ci] if date_ci < len(cells) else "")

    if heading_year is not None:
        start_year = heading_year
    else:
        found = [int(y) for d in dates for y in _YEAR_RE.findall(d)]
        # Tables run newest first, so undated rows at the top are the latest year
        start_year = max(found) if found else DEFAULT_YEAR

    groups = []         # [(year, [tr, ...])]
    current = start_year
    for tr, date in zip(data_rows, dates):
        years = _YEAR_RE.findall(date)
        if years and heading_year is None:
            current = int(years[-1])
        if not groups or groups[-1][0] != current:
            groups.append((current, []))
        groups[-1][1].append(tr)

    out = []
    for year, rows in groups:
        node = scrape.Node("table", table.attrs.items())
        node.children = list(header_rows) + rows
        out.append((year, node))
    return out


def tidy_record(rec):
    """Strip the year from Date; fix Date_lb when a range spans two years."""
    raw = scrape.fix_encoding(rec["Date"])
    years = _YEAR_RE.findall(raw)
    date = _YEAR_RE.sub("", raw)
    date = re.sub(r"\s+", " ", date).strip()
    date = re.sub(r"\s+-", " -", date)
    rec["Date"] = date
    if years:
        rec["Date_lb"] = scrape.make_date_lb(date, int(years[0]))
    return rec


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Scrape Victorian state election polling tables to long-format CSV."
    )
    parser.add_argument("--url",    default=URL,
                        help="Wikipedia URL (default: 2026 Victorian poll page)")
    parser.add_argument("--html",   default=None,
                        help="Path to a locally saved HTML file (skips network fetch)")
    parser.add_argument("--output", default=DEFAULT_OUTPUT,
                        help=f"Path for the output CSV (default: {DEFAULT_OUTPUT})")
    args = parser.parse_args()

    output_path = scrape.resolve_output_path(args.output)

    if args.html:
        print(f"\nLoading HTML from: {args.html}")
        with open(args.html, encoding="utf-8") as f:
            html = f.read()
    else:
        print(f"\nFetching: {args.url}")
        html = scrape.fetch_html(args.url)

    print("\nLocating statewide voting intention tables...")
    tables = find_statewide_tables(html)

    all_records = []
    for heading_year, table in tables:
        for year, sub in split_table_by_year(table, heading_year):
            print(f"\nParsing {year} rows...")
            records = [tidy_record(r) for r in scrape.parse_table(sub, year)]
            all_records.extend(records)
            pv  = sum(1 for r in records if r["pv_party"] != "NA")
            tpp = sum(1 for r in records if r["2pp_party_1"] != "NA")
            print(f"  -> {pv} PV rows, {tpp} 2PP rows")

    pv_total  = sum(1 for r in all_records if r["pv_party"] != "NA")
    tpp_total = sum(1 for r in all_records if r["2pp_party_1"] != "NA")
    polls     = len({(r["Year"], r["Date"], r["Polling firm"]) for r in all_records})

    print(f"\nCombined results:")
    print(f"  Unique polls : {polls}")
    print(f"  PV rows      : {pv_total}")
    print(f"  2PP rows     : {tpp_total}")
    print(f"  Total rows   : {len(all_records)}")

    with open(output_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=scrape.OUTPUT_FIELDS)
        writer.writeheader()
        writer.writerows(all_records)

    print(f"\nCSV written -> {output_path}")


if __name__ == "__main__":
    main()

