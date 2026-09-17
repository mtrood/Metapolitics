"""
scrape_aus_polls.py
-------------------
Scrapes all polling tables under the 'Voting intention' subheading from:
  "Opinion polling for the next Australian federal election" (Wikipedia)

Each table represents a year. All tables are combined into one long-format
CSV, with a 'Year' column to differentiate them.

Each row is either:
  - a primary vote datapoint  -> pv_party / pv_prop filled; 2pp cols empty
  - a 2PP datapoint           -> 2pp cols filled; pv cols empty

Uses ONLY the Python standard library (no pip installs required).
Tested on Python 3.7+.

Usage:
    python scrape_aus_polls.py
    python scrape_aus_polls.py --output my_file.csv
    python scrape_aus_polls.py --html local_page.html
"""

import re
import csv
import sys
import argparse
import urllib.request
from html.parser import HTMLParser

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

URL = (
    "https://en.wikipedia.org/wiki/"
    "Opinion_polling_for_the_next_Australian_federal_election"
)

HEADER_TO_PARTY = {
    "alp":    "ALP",
    "lib":    "LIB",
    "lnp":    "LNP",
    "nat":    "NAT",
    "grn":    "GRN",
    "onp":    "ONP",
    "ind":    "IND",    # 2025 table — will be merged into OTH in post-processing
    "oth":    "OTH",
    "others": "OTH",   # 2026 table uses "Others" header
    "l/np":   "L/NP",
}

OUTPUT_FIELDS = [
    "Year", "Date", "Date_lb",
    "Polling firm", "Client", "Interview mode", "Sample size",
    "pv_party", "pv_prop",
    "2pp_party_1", "2pp_party_1_prop",
    "2pp_party_2", "2pp_party_2_prop",
]

# Month name -> month number
MONTH_MAP = {
    "january": 1, "february": 2, "march": 3, "april": 4,
    "may": 5, "june": 6, "july": 7, "august": 8,
    "september": 9, "october": 10, "november": 11, "december": 12,
    "jan": 1, "feb": 2, "mar": 3, "apr": 4,
    "jun": 6, "jul": 7, "aug": 8,
    "sep": 9, "oct": 10, "nov": 11, "dec": 12,
}

# ---------------------------------------------------------------------------
# Fetch
# ---------------------------------------------------------------------------

def fetch_html(url):
    req = urllib.request.Request(
        url,
        headers={"User-Agent": "Mozilla/5.0 (compatible; AusPollScraper/4.0)"},
    )
    with urllib.request.urlopen(req, timeout=30) as resp:
        raw = resp.read()
    # Decode as UTF-8, replacing any bad bytes so mojibake never reaches output
    return raw.decode("utf-8", errors="replace")


# ---------------------------------------------------------------------------
# HTML parser
# ---------------------------------------------------------------------------

class Node:
    """Minimal DOM node."""
    def __init__(self, tag, attrs):
        self.tag      = tag
        self.attrs    = dict(attrs)
        self.children = []
        self.text     = ""

    def get(self, attr, default=""):
        return self.attrs.get(attr, default)

    def all_text(self):
        parts = [self.text]
        for child in self.children:
            parts.append(child.all_text())
        return " ".join(p for p in parts if p).strip()

    def find_all(self, tag):
        results = []
        queue = list(self.children)
        while queue:
            node = queue.pop(0)
            if node.tag == tag:
                results.append(node)
            queue.extend(node.children)
        return results

    def find(self, tag):
        for node in self.find_all(tag):
            return node
        return None

    def has_class(self, cls):
        return cls in self.get("class", "").split()


class TableParser(HTMLParser):
    TRACKED = {"table", "tbody", "thead", "tr", "th", "td", "h2", "h3", "h4"}

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.root   = Node("root", {})
        self._stack = [self.root]

    def handle_starttag(self, tag, attrs):
        if tag not in self.TRACKED:
            return
        node = Node(tag, attrs)
        self._stack[-1].children.append(node)
        self._stack.append(node)

    def handle_endtag(self, tag):
        if tag not in self.TRACKED:
            return
        for i in range(len(self._stack) - 1, 0, -1):
            if self._stack[i].tag == tag:
                self._stack = self._stack[:i]
                return

    def handle_data(self, data):
        if len(self._stack) > 1:
            self._stack[-1].text += data


# ---------------------------------------------------------------------------
# Find all (year, table) pairs under the 'Voting intention' subheading
# ---------------------------------------------------------------------------

def find_voting_intention_tables(html):
    """
    Parse the HTML and return a list of (year: int, table: Node) tuples
    for every wikitable that sits under the 'Voting intention' section,
    in document order.

    Each year-level heading (e.g. '2026', '2025') introduces one table.
    We stop collecting once we leave the Voting intention section (i.e.
    we hit another top-level h2 that is not a year).
    """
    parser = TableParser()
    parser.feed(html)

    # Walk root children in document order, collecting headings and tables
    doc_order = []
    for child in parser.root.children:
        if child.tag in ("h2", "h3", "h4"):
            doc_order.append(("heading", child))
        elif child.tag == "table":
            doc_order.append(("table", child))

    # Locate the 'Voting intention' section boundaries
    in_voting_intention = False
    results = []          # list of (year, table)
    current_year = None

    for kind, obj in doc_order:
        if kind == "heading":
            text = obj.all_text().strip()

            # Detect entry into 'Voting intention'
            if re.search(r"voting\s+intention", text, re.I):
                in_voting_intention = True
                current_year = None
                continue

            if not in_voting_intention:
                continue

            # Detect exit: an h2 that is NOT a year signals the next section
            year_match = re.search(r"\b(20\d{2})\b", text)
            if year_match:
                current_year = int(year_match.group(1))
            elif obj.tag == "h2":
                # Top-level non-year heading — we've left Voting intention
                break

        elif kind == "table" and in_voting_intention:
            if obj.has_class("wikitable") and current_year is not None:
                results.append((current_year, obj))

    if not results:
        raise RuntimeError(
            "No tables found under the 'Voting intention' section. "
            "The page structure may have changed."
        )

    print(f"Found {len(results)} table(s) under 'Voting intention': "
          f"{[y for y, _ in results]}")
    return results


# ---------------------------------------------------------------------------
# Header grid
# ---------------------------------------------------------------------------

def build_header_grid(header_rows):
    grid = {}
    for ri, tr in enumerate(header_rows):
        ci = 0
        cells = [c for c in tr.children if c.tag in ("th", "td")]
        for cell in cells:
            while (ri, ci) in grid:
                ci += 1
            text = cell.all_text()
            cs = int(cell.get("colspan") or 1)
            rs = int(cell.get("rowspan") or 1)
            for r in range(rs):
                for c in range(cs):
                    grid[(ri + r, ci + c)] = text
            ci += cs

    if not grid:
        return grid, 0, 0
    num_rows = max(k[0] for k in grid) + 1
    num_cols = max(k[1] for k in grid) + 1
    return grid, num_rows, num_cols


def column_label_chain(grid, num_rows, col):
    chain, seen = [], set()
    for r in range(num_rows):
        v = grid.get((r, col), "").strip()
        if v and v not in seen:
            chain.append(v)
            seen.add(v)
    return chain


def classify_columns(grid, num_rows, num_cols):
    FIXED_KEYWORDS = {
        "date":         "Date",
        "polling firm": "Polling firm",
        "client":       "Client",
        "interview":    "Interview mode",
        "sample":       "Sample size",
    }

    fixed_cols, pv_cols, tpp_cols = {}, {}, {}

    for ci in range(num_cols):
        chain       = column_label_chain(grid, num_rows, ci)
        chain_lower = [c.lower() for c in chain]

        matched = False
        for kw, name in FIXED_KEYWORDS.items():
            if any(kw in lbl for lbl in chain_lower):
                fixed_cols[name] = ci
                matched = True
                break
        if matched:
            continue

        in_primary = any("primary" in lbl for lbl in chain_lower)
        in_tpp     = any("2pp" in lbl for lbl in chain_lower)

        leaf = chain[-1].strip() if chain else ""
        # Strip footnote markers (e.g. "[a]", "[1]") and trailing whitespace
        # before lookup so "Others [a]" correctly maps to "OTH"
        leaf_clean = re.sub(r"\[.*?\]", "", leaf).strip()
        party_code = HEADER_TO_PARTY.get(leaf_clean.lower())
        if party_code is None:
            continue

        if in_tpp and not in_primary:
            tpp_cols[party_code] = ci
        elif in_primary and not in_tpp:
            pv_cols[party_code] = ci
        else:
            if party_code not in pv_cols:
                pv_cols[party_code] = ci
            elif party_code not in tpp_cols:
                tpp_cols[party_code] = ci

    return fixed_cols, pv_cols, tpp_cols


# ---------------------------------------------------------------------------
# Row-span tracker
# ---------------------------------------------------------------------------

class RowSpanTracker:
    def __init__(self, num_cols):
        self.num_cols = num_cols
        self._carry   = {}

    def expand(self, tr):
        result = [None] * self.num_cols

        for ci, (remaining, text) in list(self._carry.items()):
            result[ci] = text
            if remaining <= 1:
                del self._carry[ci]
            else:
                self._carry[ci] = (remaining - 1, text)

        ci = 0
        cells = [c for c in tr.children if c.tag in ("td", "th")]
        for cell in cells:
            while ci < self.num_cols and result[ci] is not None:
                ci += 1
            if ci >= self.num_cols:
                break
            text = cell.all_text()
            cs = int(cell.get("colspan") or 1)
            rs = int(cell.get("rowspan") or 1)
            for c in range(cs):
                if ci + c < self.num_cols:
                    result[ci + c] = text
                    if rs > 1:
                        self._carry[ci + c] = (rs - 1, text)
            ci += cs

        return [v if v is not None else "" for v in result]


# ---------------------------------------------------------------------------
# Value helpers
# ---------------------------------------------------------------------------

_NULL_RE = re.compile(
    r"^[\u2014\u2013\u2012\-]+$"
    r"|^[\u2014\u2013\u2012\-]+\s*[Nn]/[Aa]$"
    r"|^[Nn]/[Aa]$"
)

# (unused - replaced by fix_encoding's explicit replacement list)

def fix_encoding(text):
    """
    Fix mojibake that arises when UTF-8 en/em dashes are decoded as latin-1.
    We use only unicode escapes so the source file stays ASCII-safe.
    """
    replacements = [
        # UTF-8 en-dash (0xE2 0x80 0x93) decoded as latin-1
        ("\u00e2\u0080\u0093", "-"),
        # UTF-8 em-dash (0xE2 0x80 0x94) decoded as latin-1
        ("\u00e2\u0080\u0094", "-"),
        # Unicode en/em dashes -> plain hyphen
        ("\u2013", "-"),
        ("\u2014", "-"),
        ("\u2012", "-"),
    ]
    for bad, good in replacements:
        text = text.replace(bad, good)
    return text

def is_null(text):
    return not text.strip() or bool(_NULL_RE.match(text.strip()))


def parse_pct(text):
    t = fix_encoding(text).strip()
    if is_null(t):
        return None
    t = re.sub(r"\[.*?\]", "", t).strip()
    t = t.rstrip("%").strip()
    try:
        return float(t)
    except ValueError:
        return None


def clean_text(text):
    t = fix_encoding(text).strip()
    if is_null(t):
        return ""
    return re.sub(r"\[.*?\]", "", t).strip()


def clean_client(text):
    """
    Return 'NA' if the client field is blank/dash/empty, otherwise clean text.

    Wikipedia sometimes injects a hidden .sr-only <span> whose text content
    (captured by all_text()) looks like:
      '-.mw-parser-output .sr-only{border:0;...}N/a'
    We detect this by checking for the '.mw-parser-output' marker and treat
    the whole cell as null.  We also strip any remaining CSS-block text.
    """
    # If the Wikipedia CSS sentinel is present, the real value is null
    if ".mw-parser-output" in text or ".sr-only" in text:
        return "NA"
    # Strip CSS blocks of the form  selector{...}
    t = re.sub(r"[^{]*\{[^}]*\}", "", text)
    # Strip footnote markers
    t = re.sub(r"\[.*?\]", "", t).strip()
    # Apply encoding fix and null check
    t = fix_encoding(t).strip()
    if is_null(t) or not t:
        return "NA"
    return t


# ---------------------------------------------------------------------------
# Date lower-bound construction
# ---------------------------------------------------------------------------

def make_date_lb(date_str, year):
    """
    Extract the FIRST day and month from a date range string and return
    a date in D/M/YYYY format.

    Examples:
      '1-7 June'      -> '1/6/2026'
      '25-31 May'     -> '25/5/2026'
      '13 May'        -> '13/5/2026'
      '29 Apr-5 May'  -> '29/4/2026'
    """
    s = fix_encoding(date_str).strip()

    # Split on the range separator (-) to isolate the left-hand side
    # e.g. '25-31 May' -> left='25', rest='31 May'
    #      '29 Apr-5 May' -> left='29 Apr', rest='5 May'
    #      '13 May' -> no split, treat whole string as left
    parts = re.split(r"-", s, maxsplit=1)
    left = parts[0].strip()   # '25' or '29 Apr' or '13 May'

    # Extract first number from left side
    day_m = re.search(r"(\d{1,2})", left)
    if not day_m:
        return ""
    day = int(day_m.group(1))

    # Find the first month name in the full string (covers '25-31 May' where
    # the month only appears on the right, and '29 Apr-5 May' where it is left)
    month_m = re.search(r"([A-Za-z]{3,})", s)
    if not month_m:
        return ""
    month_word = month_m.group(1).lower()

    month_num = MONTH_MAP.get(month_word[:3])
    if month_num is None:
        return ""

    return f"{day}/{month_num}/{year}"


# ---------------------------------------------------------------------------
# Event-row detection
# ---------------------------------------------------------------------------

_EVENT_RE = re.compile(
    r"election|coalition|budget|dissolved|by-election|"
    r"canavan|taylor|ley|joyce|strait|hormuz|replac|elected|joins|"
    r"liberal.national|nat.*party",
    re.I,
)


def is_event_row(tr, num_cols):
    cells = [c for c in tr.children if c.tag in ("td", "th")]
    if not cells:
        return True
    if len(cells) == 1 and int(cells[0].get("colspan") or 1) >= num_cols - 2:
        return True
    if len(cells) <= 3 and _EVENT_RE.search(tr.all_text()):
        return True
    return False


# ---------------------------------------------------------------------------
# Core table parser
# ---------------------------------------------------------------------------

def parse_table(table, year):
    """
    Parse a single wikitable Node for the given year.
    Returns a list of long-format record dicts.
    """
    all_rows = table.find_all("tr")

    header_rows, data_rows = [], []
    passed_header = False
    for tr in all_rows:
        has_th = any(c.tag == "th" for c in tr.children)
        has_td = any(c.tag == "td" for c in tr.children)
        if not passed_header:
            if has_th and not has_td:
                header_rows.append(tr)
            else:
                passed_header = True
                if has_td:
                    data_rows.append(tr)
        else:
            if has_td or has_th:
                data_rows.append(tr)

    if not header_rows:
        print(f"  WARNING: no header rows found for {year} table — skipping.")
        return []

    grid, n_hrows, num_cols = build_header_grid(header_rows)
    fixed_cols, pv_cols, tpp_cols = classify_columns(grid, n_hrows, num_cols)

    print(f"  [{year}] cols={num_cols}  fixed={list(fixed_cols)}  "
          f"pv={list(pv_cols)}  2pp={list(tpp_cols)}")

    if not pv_cols and not tpp_cols:
        print(f"  WARNING: no party columns detected for {year} — skipping.")
        return []

    tracker = RowSpanTracker(num_cols)
    records  = []
    seen_pv  = set()   # tracks (poll_key, party) to prevent PV duplication

    for tr in data_rows:
        if is_event_row(tr, num_cols):
            tracker.expand(tr)
            continue

        cells = tracker.expand(tr)

        def get(col_name):
            idx = fixed_cols.get(col_name)
            return clean_text(cells[idx]) if (idx is not None and idx < len(cells)) else ""

        date   = get("Date")
        firm   = get("Polling firm")
        client = clean_client(cells[fixed_cols["Client"]]
                              if "Client" in fixed_cols and
                              fixed_cols["Client"] < len(cells) else "")
        mode   = get("Interview mode")
        size   = get("Sample size")

        if date.lower() in ("date", "") and not firm:
            continue
        if re.search(r"\belection\b", date, re.I):
            continue

        date_lb = make_date_lb(date, year)

        base = {
            "Year": year,
            "Date": date,
            "Date_lb": date_lb,
            "Polling firm": firm,
            "Client": client,
            "Interview mode": mode,
            "Sample size": size,
        }

        # ── Primary vote rows ──
        # Use a per-poll seen-set so that continuation rows (which carry over
        # the same date/firm via rowspan) never duplicate a party already
        # recorded for this poll.
        poll_key = (year, date, firm)

        # ── Merged LNP detection ──────────────────────────────────────────
        # The 2026 table has a three-level header: "L/NP" spans "LIB" and
        # "NAT" sub-columns. When a pollster reports a single combined value,
        # Wikipedia uses a colspan cell whose text is duplicated across all
        # three underlying column positions by our row-expansion logic.
        # We detect this by checking whether LIB, LNP (or L/NP), and NAT
        # columns all contain the identical non-null value, and if so emit
        # ONE "LNP" row instead of three duplicates.
        #
        # When the pollster reports separate LIB and NAT values, the three
        # raw cell texts will differ, so we fall through to the normal
        # per-party loop which emits individual LIB and NAT rows.

        merged_lnp_val = None

        # Candidates: the trio of sub-columns under the L/NP parent header.
        # The header may spell the parent as "LNP" or "L/NP" depending on
        # the year; sub-columns are always "LIB" and "NAT".
        lnp_parent = "LNP" if "LNP" in pv_cols else ("L/NP" if "L/NP" in pv_cols else None)
        lnp_trio = [p for p in ["LIB", lnp_parent, "NAT"] if p is not None]

        if lnp_parent and all(p in pv_cols for p in lnp_trio):
            idxs     = [pv_cols[p] for p in lnp_trio]
            if all(i < len(cells) for i in idxs):
                raw_vals = [fix_encoding(cells[i]).strip() for i in idxs]
                parsed   = [parse_pct(cells[i]) for i in idxs]
                # All positions share identical non-null text → merged cell
                if (len(set(raw_vals)) == 1
                        and parsed[0] is not None
                        and all(p == parsed[0] for p in parsed)):
                    merged_lnp_val = parsed[0]

        if merged_lnp_val is not None:
            rec_key = (poll_key, "LNP")
            if rec_key not in seen_pv:
                seen_pv.add(rec_key)
                # Mark all trio members as seen so they're skipped below
                for p in lnp_trio:
                    seen_pv.add((poll_key, p))
                records.append({
                    **base,
                    "pv_party": "LNP", "pv_prop": merged_lnp_val,
                    "2pp_party_1": "NA", "2pp_party_1_prop": "NA",
                    "2pp_party_2": "NA", "2pp_party_2_prop": "NA",
                })

        # ── IND + OTH combining (2025 table) ─────────────────────────────
        # The 2025 table has separate IND and OTH columns. Combine them
        # into a single OTH row (sum the proportions).
        ind_val = None
        oth_val = None
        if "IND" in pv_cols and "OTH" in pv_cols:
            ind_ci = pv_cols["IND"]
            oth_ci = pv_cols["OTH"]
            if ind_ci < len(cells) and oth_ci < len(cells):
                ind_val = parse_pct(cells[ind_ci])
                oth_val = parse_pct(cells[oth_ci])

        combine_ind_oth = (ind_val is not None or oth_val is not None)
        if combine_ind_oth:
            combined = (ind_val or 0) + (oth_val or 0)
            rec_key = (poll_key, "OTH")
            if rec_key not in seen_pv:
                seen_pv.add(rec_key)
                seen_pv.add((poll_key, "IND"))  # skip IND in main loop
                records.append({
                    **base,
                    "pv_party": "OTH", "pv_prop": round(combined, 1),
                    "2pp_party_1": "NA", "2pp_party_1_prop": "NA",
                    "2pp_party_2": "NA", "2pp_party_2_prop": "NA",
                })

        # ── Main per-party loop ───────────────────────────────────────────
        for party, ci in sorted(pv_cols.items(), key=lambda x: x[1]):
            if ci >= len(cells):
                continue
            val = parse_pct(cells[ci])
            if val is None:
                continue
            rec_key = (poll_key, party)
            if rec_key in seen_pv:
                continue          # already recorded (merged LNP, combined OTH, or dupe)
            seen_pv.add(rec_key)
            # Remap IND -> OTH for any table that only has IND with no separate OTH
            emit_party = "OTH" if party == "IND" else party
            records.append({
                **base,
                "pv_party": emit_party, "pv_prop": val,
                "2pp_party_1": "NA", "2pp_party_1_prop": "NA",
                "2pp_party_2": "NA", "2pp_party_2_prop": "NA",
            })

        # ── 2PP rows ──
        tpp_present = [
            (party, parse_pct(cells[ci]))
            for party, ci in sorted(tpp_cols.items(), key=lambda x: x[1])
            if ci < len(cells) and parse_pct(cells[ci]) is not None
        ]
        used = set()
        for i in range(len(tpp_present)):
            if i in used:
                continue
            for j in range(i + 1, len(tpp_present)):
                if j in used:
                    continue
                p1, v1 = tpp_present[i]
                p2, v2 = tpp_present[j]
                if abs(v1 + v2 - 100) <= 1.5:
                    records.append({
                        **base,
                        "pv_party": "NA", "pv_prop": "NA",
                        "2pp_party_1": p1, "2pp_party_1_prop": v1,
                        "2pp_party_2": p2, "2pp_party_2_prop": v2,
                    })
                    used.add(i)
                    used.add(j)
                    break

    return records


# ---------------------------------------------------------------------------
# CLI helpers
# ---------------------------------------------------------------------------

def resolve_output_path(args_output):
    """
    Determine the final output path.

    Default: 'data/raw/data.csv' (relative to the current working directory).
    The containing directory is created automatically if it does not exist.
    An explicit --output value overrides this default.
    """
    import os

    path = args_output

    out_dir = os.path.dirname(path)
    if out_dir and not os.path.exists(out_dir):
        os.makedirs(out_dir)
        print(f"Created directory: {out_dir}")

    return path


def main():
    parser = argparse.ArgumentParser(
        description="Scrape Australian election polling tables to long-format CSV."
    )
    parser.add_argument("--url",    default=URL,
                        help="Wikipedia URL (default: standard poll page)")
    parser.add_argument("--html",   default=None,
                        help="Path to a locally saved HTML file (skips network fetch)")
    parser.add_argument("--output", default="data/raw/data.csv",
                        help="Path for the output CSV (default: data/raw/data.csv)")
    args = parser.parse_args()

    output_path = resolve_output_path(args.output)

    if args.html:
        print(f"\nLoading HTML from: {args.html}")
        with open(args.html, encoding="utf-8") as f:
            html = f.read()
    else:
        print(f"\nFetching: {args.url}")
        html = fetch_html(args.url)

    print("\nLocating 'Voting intention' tables...")
    year_tables = find_voting_intention_tables(html)

    all_records = []
    for year, table in year_tables:
        print(f"\nParsing {year} table...")
        records = parse_table(table, year)
        all_records.extend(records)
        pv  = sum(1 for r in records if r["pv_party"])
        tpp = sum(1 for r in records if r["2pp_party_1"])
        print(f"  -> {pv} PV rows, {tpp} 2PP rows")

    pv_total  = sum(1 for r in all_records if r["pv_party"])
    tpp_total = sum(1 for r in all_records if r["2pp_party_1"])
    polls     = len({(r["Year"], r["Date"], r["Polling firm"]) for r in all_records})

    print(f"\nCombined results:")
    print(f"  Unique polls : {polls}")
    print(f"  PV rows      : {pv_total}")
    print(f"  2PP rows     : {tpp_total}")
    print(f"  Total rows   : {len(all_records)}")

    with open(output_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=OUTPUT_FIELDS)
        writer.writeheader()
        writer.writerows(all_records)

    print(f"\nCSV written -> {output_path}")


if __name__ == "__main__":
    main()
