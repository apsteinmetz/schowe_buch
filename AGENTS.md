# Schowe Family Book Project

Parsing German-language church ancestry records ("Familienbuch") for the
villages Neu-Schowe and Alt-Schowe (Batschka) into a structured JSON database,
and converting that database to GEDCOM 5.5.5.

## Files

| File | Purpose |
|---|---|
| `data/persons.txt` | Source: ~57k lines, 4,192 primary records delimited by `<nnnn>` |
| `data/unique_given_names.csv` | Canonical given names (mixed case) |
| `data/unique_surnames.csv` | Canonical surnames (**UPPERCASE**, may keep inner `ß` — compare via `str_to_upper()` keys of `surname_lookup`, never raw) |
| `data/unique_places.csv` | Canonical place names (contains some polluted compound entries, e.g. "Neu Pasua Popp" — see caveats) |
| `data/unique_occupations.csv` | Canonical occupations (Title-case words, e.g. "Taglöhner") |
| `R/parse_persons.R` | The parser. Run with `source("R/parse_persons.R"); result <- run_parser()` |
| `data/persons.json` | Output: flat array of ~21.5k person objects |
| `R/json_to_gedcom.R` | GEDCOM converter. `source(...); res <- convert_to_gedcom()` |
| `data/persons.ged` | Output: GEDCOM 5.5.5, ~13.9k INDI / ~4.6k FAM after merging |
| `data/qa_unresolved_refs.csv` | Parser-flagged cross-references (direction quirks / missing targets) |
| `data/qa_note_lines.txt` | Lines kept as free-text notes (unstructured content) |
| `data/qa_merge_unmatched.csv` | Converter: self-refs that could not be merged by name |
| `data/qa_merge_decisions.csv` | **Manual adjudications** of unmatched refs (see below) |

## Source format essentials

- Records delimited by `<nnnn>` (some have trailing spaces). Standalone
  ALL-CAPS lines between records are surname index headers (skip; spelling
  may differ from the following record).
- One primary person per record; optional spouse(s), numbered children,
  child spouses, all in one family block.
- Primary/spouse name lines are ALL-CAPS surname first (may contain inner
  `ö/ä/ü/ß`, e.g. `GRößER`); child lines are `1. Surname Given ...`.
  Name lines may be `SURNAME Given, occupation` — commas are stripped from
  tokens before classification.
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
  resident in NS/AS. Multi-word places are common: `Neu Werbas`,
  `Neu Banovzi` (variant `Neu Banovczi`), `Neu Pasua`, `Neu Sivatz`, ...
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
- A `self`-context ref means **"this couple continues at family n"** — the
  target may be the ref-bearer OR their partner (e.g. a `> n` on the wife's
  line often points to the husband's new primary record). Never assume the
  ref-bearer is the target; resolve by name.
- Direction markers are unreliable for `Witwe von` lines (`<` may point
  forward) — treat direction as informational, resolve by target only.
- Resolution: person IDs are `fam.0` (primary), `fam.sN` (spouse N),
  `fam.K` (child K), `fam.KmN` (Nth spouse of child K). Reciprocal links
  stored in `referenced_by`. The parser's `resolved_id` may *correct* the
  raw target (e.g. "child 3 not found; resolved by name match") — always
  prefer `resolved_id` over `target_family`/`target_child`.

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

Parsing subtleties (bugs fixed; keep in mind when editing):

- **Place vs surname on child `oo` lines** (`child_union()`): "Neu" is both
  a surname and the first word of many places. Multi-token place prefixes
  are tried *ascending from 2 tokens* and accepted only when the remainder
  starts with a known surname (or is empty); only then does the
  surname-first shortcut apply. Ascending order defends against polluted
  compound entries in the place list.
- **`ß` uppercasing**: `str_to_upper("Theiß")` = `"THEISS"` but canonical
  surnames keep `ß` (`THEIß`). Membership tests must use
  `names(surname_lookup)`, not `surnames` directly.
- **Trailing commas** on name-prefix tokens (`SURNAME Given, occupation`)
  are stripped in `apply_prefix()` before classification.

## GEDCOM converter (R/json_to_gedcom.R)

`convert_to_gedcom(json_path, out_path, merge_duplicates = TRUE,
qa_path, decisions_path)` writes `data/persons.ged` and returns
`list(n_indi, n_fam, n_merged_clusters, qa_unmatched, path)`.

Mapping: birth/baptism/death/burial → BIRT/CHR/DEAT/BURI; marriages and
`o‐o` unions become FAM records (`EVEN`/`TYPE Unmarried union` for `o‐o`);
qualifiers → ABT/BEF/AFT/`BET x AND y`; religion → RELI, occupation → OCCU,
residences → RESI+PLAC, age at death → AGE under DEAT. Godparents,
witnesses, parents, widow_of, origin, unresolved refs, free-text notes, and
all raw source lines are preserved as NOTEs. GEDCOM 5.5.5 specifics: UTF-8
**with BOM**, CRLF line endings, ≤255-char lines (CONC/CONT splitting via
`emit_text()`), xrefs `@I<id>@` with `.` → `_` (e.g. `12.2m1` → `@I12_2M1@`).

- **Sex inference** (source has none): given-name lookup built from role
  statistics (primaries ≈ male, spouses ≈ female) + suffix fallback;
  primary/child partner defaults to HUSB when ambiguous.

### Duplicate merging (`build_identity_map`)

- For each valid `context == "self"` ref: anchor the target set on
  `resolved_id` (anchor person + partners); source set = ref-bearer +
  partners. Match pairs by name: shared given-name token (edit distance ≤1
  for tokens ≥5 chars, `tok_eq`) with compatible surname, else ≥2 shared
  tokens across the full given+surname bag (absorbs given/surname split
  glitches). Union-find over matched pairs.
- **Couple-level elimination**: if ≥1 pair matched and exactly one
  source/target pair remains, accept a weaker match (compatible surname
  when a given name is `NN.`, or agreeing given names despite a mangled
  surname). Disagreeing given names never merge (siblings share surnames).
- Merged clusters emit one INDI under the primary's xref: events deduped by
  type+date+place, notes/sources concatenated per origin id, plus a
  "Merged from Familienbuch entries: ..." NOTE. After canonicalization,
  FAMs with the same two partners are combined (`dedupe_fam_units`).
- Refs that cannot be matched go to `data/qa_merge_unmatched.csv`.

### Manual adjudications (`data/qa_merge_decisions.csv`)

Columns: `from, raw, from_name, from_role, partners, leftover_targets,
verdict (merge|reject), to, rationale`. The converter reads this file and
force-merges `verdict == "merge"` rows (`from` → `to`). All 131 originally
unmatched refs were hand-adjudicated (July 2026): 18 merges (Stegh/Steg
spelling, junk-parsed `NN.` names anchored by a matched partner, duplicate
same-name targets, two user-approved given-name variants) and the rest
rejected with written rationale (reference lists to relatives' families,
targets with no record for the person, missing child records = book errata,
later wives of the same husband). **If parser changes renumber person ids,
this file must be re-checked.**

## Known caveats

- ~250 refs flagged for direction mismatch (mostly `Witwe von`); ~14 refs
  point to genuinely missing targets (likely book errata).
- `oo wieder` notes attach to the nearest person; exact subject sometimes
  ambiguous.
- Free-text German notes (~1k lines) are preserved verbatim in `notes`.
- `data/unique_places.csv` contains polluted compound entries
  ("Neu Pasua Popp", "Neuwerbas Klein", "Grumbach Nikolaus # siehe
  Auswanderer") — the parser defends against them, but the list itself has
  not been cleaned.
- Record 585.s2 is a phantom person parsed from the parenthetical
  `(er Witwer, sie ledig)` (a marital-status note, not a name line) — known,
  not yet fixed.
- Cross-referenced duplicates are merged only in the GEDCOM output;
  `persons.json` still contains one record per Familienbuch entry.
