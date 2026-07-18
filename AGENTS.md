# Schowe Family Book Project

Parsing German-language church ancestry records ("Familienbuch") for the
villages Neu-Schowe and Alt-Schowe (Batschka) into a structured JSON database.

## Files

| File | Purpose |
|---|---|
| `data/persons.txt` | Source: ~57k lines, 4,192 primary records delimited by `<nnnn>` |
| `data/unique_given_names.csv` | Canonical given names (mixed case) |
| `data/unique_surnames.csv` | Canonical surnames (**UPPERCASE** — compare with `str_to_upper()`) |
| `data/unique_places.csv` | Canonical place names |
| `data/unique_occupations.csv` | Canonical occupations (lowercase words) |
| `R/parse_persons.R` | The parser. Run with `source("R/parse_persons.R"); result <- run_parser()` |
| `data/persons.json` | Output: flat array of ~21.5k person objects |
| `data/qa_unresolved_refs.csv` | Flagged cross-references (direction quirks / missing targets) |
| `data/qa_note_lines.txt` | Lines kept as free-text notes (unstructured content) |

## Source format essentials

- Records delimited by `<nnnn>` (some have trailing spaces). Standalone
  ALL-CAPS lines between records are surname index headers (skip; spelling
  may differ from the following record).
- One primary person per record; optional spouse(s), numbered children,
  child spouses, all in one family block.
- Primary/spouse name lines are ALL-CAPS surname first (may contain inner
  `ö/ä/ü/ß`, e.g. `GRößER`); child lines are `1. Surname Given ...`.
- Event tags: `*` birth, `oo` marriage, `o‐o` (U+2010 hyphen!) **unmarried
  union** (NOT divorce), `~` baptism, `†` death, `b.` burial (only when
  followed by a date — `b.` also means "bei" in place names).
- Numbered unions: `1.oo`, `2.oo` = person's 1st/2nd marriage; `2.o‐o` at
  family level = primary's second union. Child numbering continues across
  unions; children of unmarried unions may carry the mother's surname.
- Dates `dd.mm.yyyy`, `Month yyyy`, `yyyy`, malformed `.mm.yyyy` (= yyyy-mm);
  qualifiers `um` (about), `vor` (before), `nach` (after), `zw. X und/‐ Y`
  (between). Output ISO, truncated to known precision.
- `NN.` = unknown name. `led.` = unmarried. `(†mit 55J)`/`4J2M`/`10T` = age
  at death (J/M/T = years/months/days). Religions: `ev.`/`ref.`/`kath.`
- Place abbreviations: `NS` = Neu-Schowe, `AS` = Alt-Schowe; `WoNS`/`WoAS` =
  resident in NS/AS.
- Facts (not separate persons): `TP:` godparents, `TZ:` witnesses,
  `Eltern:`/`Vater:`/`Mutter:` parents, `Tv.`/`Sv.` = Tochter/Sohn von,
  `Witwe von NAME <ref`, `Wohnort der Familie in X: street`,
  `letzter bekannter Wohnort: ...`, `aus PLACE` (origin), `lebt YYYY in ...`.
- `# ...` = note; `(...)`-only lines = source citations;
  `nach Korrektur unbesetzt` = intentionally vacant record.
- `NAME oo wieder ...` = remarriage clause (stored as structured note).
- **Wrapped lines**: TP/TZ lists and other lines wrap mid-name; the parser's
  `join_wrapped()` re-joins them (prev ends `,`/`:`, line starts `(†mit`,
  or TP/TZ continuation by given-name/surname heuristics).

## Cross-references

- `< n` back / `> n` forward reference to family n; `n.k` = child k of
  family n. Attach to the person on whose line they appear.
- Direction markers are unreliable for `Witwe von` lines (`<` may point
  forward) — treat direction as informational, resolve by target only.
- Resolution: person IDs are `fam.0` (primary), `fam.sN` (spouse N),
  `fam.K` (child K), `fam.KmN` (Nth spouse of child K). Reciprocal links
  stored in `referenced_by`.

## Parser architecture (R/parse_persons.R, tidyverse)

1. `join_wrapped()` — re-join wrapped lines (vectorized).
2. `parse_all()` — split into records; `parse_record()` is a per-line state
   machine (dispatch on line type; `cur` = person new info attaches to;
   pending-spouse mechanisms for family `oo` lines and child `oo` lines
   whose spouse appears on the following line, possibly mixed-case).
3. `resolve_refs()` — validate targets, set `resolved_id`, build
   `referenced_by`, collect issues.
4. `run_parser()` — writes `data/persons.json`, returns
   `list(persons, unresolved, qa)`.

Every person keeps `source` (raw lines) and every event keeps `raw`, so
nothing is lost to parsing bugs. Unparseable lines become `notes` and are
logged to `qa$fallback_notes`.

## Known caveats

- ~250 refs flagged for direction mismatch (mostly `Witwe von`); ~14 refs
  point to genuinely missing targets (likely book errata).
- `oo wieder` notes attach to the nearest person; exact subject sometimes
  ambiguous.
- Free-text German notes (~1k lines) are preserved verbatim in `notes`.
