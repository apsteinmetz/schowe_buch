# parse_persons.R ---------------------------------------------------------
# Parser for the Schowe family book (data/persons.txt).
# Produces a flat person list saved as data/persons.json plus QA info.

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(cli)
})

DASH <- "\u2010" # the non-ASCII hyphen used in the source file (o‐o)

# ---- canonical lists -----------------------------------------------------

read_col <- function(path) {
  x <- read_csv(path, show_col_types = FALSE)[[1]]
  x[!is.na(x) & str_trim(x) != ""]
}

places      <- read_col("data/unique_places.csv")
given_set   <- read_col("data/unique_given_names.csv")
surnames    <- read_col("data/unique_surnames.csv")
occ_set     <- read_col("data/unique_occupations.csv")

place_abbrev <- c(NS = "Neu-Schowe", AS = "Alt-Schowe")
place_set    <- union(places, names(place_abbrev))
surname_lookup <- set_names(surnames, str_to_upper(surnames))

month_num <- c(
  Januar = 1, Februar = 2, "M\u00e4rz" = 3, April = 4, Juni = 6, Juli = 7,
  August = 8, September = 9, Oktober = 10, November = 11, Dezember = 12,
  Mai = 5, Jan = 1, Feb = 2, Mar = 3, Apr = 4, Jun = 6, Jul = 7, Aug = 8,
  Sep = 9, Okt = 10, Oct = 10, Nov = 11, Dez = 12, Dec = 12
)

religion_map <- c(ev = "Evangelical", ref = "Reformed", kath = "Catholic")

qa <- new.env()
qa$fallback_notes <- character()
qa$errors <- character()

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- line joining (undo wrapped lines) -----------------------------------

join_wrapped <- function(x) {
  x <- str_trim(x, "right")
  prev <- lag(x, default = "")
  first_tok <- str_extract(x, "^[^\\s]+")
  prev_last_tok <- str_extract(prev, "[^\\s]+$")
  # mixed-case token whose uppercase form is a canonical surname
  is_mixed_surname <- !is.na(first_tok) &
    first_tok != str_to_upper(first_tok) &
    str_to_upper(first_tok) %in% surnames
  is_join <- x != "" & prev != "" &
    !str_detect(x, "^<\\d+>") &
    !str_detect(prev, "^<\\d+>") &
    (
      str_detect(prev, "[,:]$") |
        str_detect(x, "^\\(\u2020mit") |
        # a TP:/TZ: name list wrapped mid-name
        (str_detect(prev, "T[PZ]:") &
           (!is.na(first_tok) & first_tok %in% given_set |
              (is_mixed_surname & !is.na(prev_last_tok) &
                 prev_last_tok %in% given_set)))
    )
  grp <- cumsum(!is_join)
  unname(vapply(split(x, grp), paste, "", collapse = " "))
}

# ---- small parsing helpers ----------------------------------------------

# Cross-references: "< 34.2", "> 1251", "<4110"
extract_refs <- function(txt, context = "self") {
  m <- str_match_all(txt, "([<>])\\s*(\\d+)(?:\\.(\\d+))?")[[1]]
  refs <- list()
  if (nrow(m) > 0) {
    refs <- map(seq_len(nrow(m)), \(i) list(
      raw = m[i, 1],
      direction = if (m[i, 2] == "<") "back" else "forward",
      target_family = as.integer(m[i, 3]),
      target_child = if (is.na(m[i, 4])) NULL else as.integer(m[i, 4]),
      context = context
    ))
    txt <- str_remove_all(txt, "([<>])\\s*\\d+(\\.\\d+)?")
  }
  list(refs = refs, rest = str_squish(txt))
}

# Age at death "(†mit 55J)" / "(†mit 4J2M)" / "(†mit 10T als Johanna)"
extract_age <- function(txt) {
  m <- str_match(txt, "\\(\u2020mit ([^)]+)\\)")
  age <- NULL
  if (!is.na(m[1, 1])) {
    age <- m[1, 2] |>
      str_replace_all("(\\d{1,2})J", "\\1 years ") |>
      str_replace_all("(\\d{1,2})M", "\\1 months ") |>
      str_replace_all("(\\d{1,2})T", "\\1 days ") |>
      str_squish()
    txt <- str_remove(txt, "\\(\u2020mit [^)]+\\)")
  }
  list(age = age, rest = str_squish(txt))
}

take_religion <- function(txt) {
  m <- str_match(txt, "(?:\\(\\s*)?\\b(ev|ref|kath)\\.(?:\\s*\\))?")
  rel <- NULL
  if (!is.na(m[1, 1])) {
    rel <- religion_map[[m[1, 2]]]
    txt <- str_remove_all(txt, "(?:\\(\\s*)?\\b(ev|ref|kath)\\.(?:\\s*\\))?")
  }
  list(religion = rel, rest = str_squish(txt))
}

parse_date <- function(txt) {
  txt <- str_trim(txt)
  out <- list(date = NULL, qualifier = NULL, rest = txt)
  qm <- str_match(txt, "^(um|vor|nach)\\s+")
  if (!is.na(qm[1, 1])) {
    out$qualifier <- c(um = "about", vor = "before", nach = "after")[[qm[1, 2]]]
    txt <- str_sub(txt, str_length(qm[1, 1]) + 1)
  }
  pats <- list(
    zw  = paste0("^zw\\.\\s*(\\d{4})\\s*(?:und\\s|u\\.\\s*|[", DASH, "-])\\s*(\\d{4}),?\\s*"),
    dmy = "^(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4}),?\\s*",
    mon = paste0("^(", paste(names(month_num), collapse = "|"), ")\\s+(\\d{4}),?\\s*"),
    yr  = "^(\\d{4}),?\\s*"
  )
  m <- str_match(txt, pats$zw)
  if (!is.na(m[1, 1])) {
    out$date <- paste0(m[1, 2], "/", m[1, 3]); out$qualifier <- "between"
    out$rest <- str_sub(txt, str_length(m[1, 1]) + 1); return(out)
  }
  m <- str_match(txt, pats$dmy)
  if (!is.na(m[1, 1])) {
    out$date <- sprintf("%s-%02d-%02d", m[1, 4], as.integer(m[1, 3]), as.integer(m[1, 2]))
    out$rest <- str_sub(txt, str_length(m[1, 1]) + 1); return(out)
  }
  # malformed day-less date, e.g. ".03.1853" -> 1853-03
  m <- str_match(txt, "^\\.(\\d{1,2})\\.(\\d{4}),?\\s*")
  if (!is.na(m[1, 1])) {
    out$date <- sprintf("%s-%02d", m[1, 3], as.integer(m[1, 2]))
    out$rest <- str_sub(txt, str_length(m[1, 1]) + 1); return(out)
  }
  m <- str_match(txt, pats$mon)
  if (!is.na(m[1, 1])) {
    out$date <- sprintf("%s-%02d", m[1, 3], month_num[[m[1, 2]]])
    out$rest <- str_sub(txt, str_length(m[1, 1]) + 1); return(out)
  }
  m <- str_match(txt, pats$yr)
  if (!is.na(m[1, 1])) {
    out$date <- m[1, 2]
    out$rest <- str_sub(txt, str_length(m[1, 1]) + 1); return(out)
  }
  out$rest <- txt
  out
}

match_place <- function(txt) {
  txt <- str_trim(txt)
  if (txt == "") return(list(place = NULL, rest = ""))
  # "City, ST" (US state) first, since city alone may also be a known place
  m <- str_match(txt, "^(\\p{Lu}[\\p{L}.\\- ]*?, [A-Z]{2})\\b,?\\s*(.*)$")
  if (!is.na(m[1, 1])) return(list(place = m[1, 2], rest = str_trim(m[1, 3])))
  toks <- str_split_1(txt, "\\s+")
  for (k in rev(seq_len(min(4, length(toks))))) {
    cand <- str_remove(paste(toks[1:k], collapse = " "), ",$")
    if (cand %in% place_set) {
      place <- if (cand %in% names(place_abbrev)) place_abbrev[[cand]] else cand
      return(list(place = place,
                  rest = str_trim(paste(toks[-(1:k)], collapse = " "))))
    }
  }
  list(place = NULL, rest = txt)
}

# ---- event splitting -----------------------------------------------------

tag_pat <- paste0(
  "(?<![\\p{L}])(\\*|o", DASH, "o|oo(?![\\p{L}])|~|\u2020|b\\.(?=\\s*\\d))"
)

tag_type <- function(tag) {
  dplyr::case_when(
    tag == "*" ~ "birth",
    tag == "oo" ~ "marriage",
    tag == paste0("o", DASH, "o") ~ "union_unmarried",
    tag == "~" ~ "baptism",
    tag == "\u2020" ~ "death",
    .default = "burial"
  )
}

split_events <- function(txt) {
  locs <- str_locate_all(txt, tag_pat)[[1]]
  if (nrow(locs) == 0) return(list(prefix = str_trim(txt), chunks = list()))
  prefix <- str_trim(str_sub(txt, 1, locs[1, 1] - 1))
  chunks <- map(seq_len(nrow(locs)), function(i) {
    tag <- str_sub(txt, locs[i, 1], locs[i, 2])
    end <- if (i < nrow(locs)) locs[i + 1, 1] - 1 else str_length(txt)
    list(type = tag_type(tag),
         body = str_trim(str_sub(txt, locs[i, 2] + 1, end)))
  })
  list(prefix = prefix, chunks = chunks)
}

# ---- record parser -------------------------------------------------------

new_person <- function(id, family, role, position = NULL) {
  list(
    id = id, family = family, role = role, position = position,
    union = NULL, surname = NULL, given = NULL, surname_unknown = FALSE,
    religion = NULL, occupation = NULL, marital_status = NULL,
    age_at_death = NULL, events = list(), parents = character(),
    godparents = character(), witnesses = character(),
    residences = character(), origin = character(), widow_of = NULL,
    notes = character(), refs = list(), referenced_by = character(),
    source = character()
  )
}

canon_surname <- function(s) {
  if (is.null(s) || is.na(s)) return(NULL)
  hit <- unname(surname_lookup[str_to_upper(s)])
  out <- if (!is.na(hit)) hit else s
  # title-case names written in caps (allowing embedded ß, as in GRÖßER)
  if (str_detect(out, "^[\\p{Lu}\u00df]+$")) str_to_title(out) else out
}

parse_record <- function(fam, block) {
  persons <- list()
  cur <- NULL           # id of person new info attaches to
  union_no <- 0L        # family-level union counter
  pending_marriage <- NULL # list(person_id, event_index) awaiting spouse
  pending_child_spouse <- NULL # child id whose oo line had no inline spouse
  spouse_no <- 0L
  child_spouse_no <- list() # per child id
  record_notes <- character()

  get <- function(id) persons[[id]]
  put <- function(p) { persons[[p$id]] <<- p; invisible(p$id) }
  touch_source <- function(id, line) {
    p <- persons[[id]]; p$source <- c(p$source, line); persons[[p$id]] <<- p
  }
  add_note <- function(id, note) {
    if (is.null(id)) { record_notes <<- c(record_notes, note); return(invisible()) }
    p <- persons[[id]]; p$notes <- c(p$notes, note); persons[[p$id]] <<- p
  }
  add_refs <- function(id, refs) {
    if (length(refs) == 0 || is.null(id) || is.null(persons[[id]]))
      return(invisible())
    p <- persons[[id]]; p$refs <- c(p$refs, refs); persons[[p$id]] <<- p
  }

  # -- name prefix (given names, occupation, flags) on an existing person --
  apply_prefix <- function(id, prefix, surname_from_line = NULL) {
    p <- persons[[id]]
    r <- take_religion(prefix)
    if (!is.null(r$religion)) p$religion <- r$religion
    prefix <- r$rest
    if (str_detect(prefix, "\\bled\\.")) {
      p$marital_status <- "unmarried"
      prefix <- str_squish(str_remove_all(prefix, "\\bled\\."))
    }
    toks <- if (prefix == "") character() else str_split_1(prefix, "\\s+")
    if (is.null(surname_from_line) && is.null(p$surname) && !p$surname_unknown) {
      if (length(toks) > 0) { surname_from_line <- toks[1]; toks <- toks[-1] }
    }
    if (!is.null(surname_from_line)) {
      if (str_detect(surname_from_line, "^NN\\.?$")) {
        p$surname_unknown <- TRUE
      } else {
        p$surname <- canon_surname(surname_from_line)
      }
    }
    given <- character(); leftover <- character()
    for (t in toks) {
      if (str_detect(t, "^Wo(NS|AS)$")) {
        abbr <- str_remove(t, "^Wo")
        p$residences <- c(p$residences, place_abbrev[[abbr]])
      } else if (t %in% occ_set) {
        p$occupation <- t
      } else if (length(leftover) == 0 &&
                 (t %in% given_set | str_detect(t, "^\\p{Lu}\\.?$") |
                  str_detect(t, "^\\p{Lu}[\\p{Ll}]+$"))) {
        given <- c(given, t)
      } else {
        leftover <- c(leftover, t)
      }
    }
    if (length(given) > 0) p$given <- paste(given, collapse = " ")
    if (length(leftover) > 0) p$notes <- c(p$notes, paste(leftover, collapse = " "))
    persons[[p$id]] <<- p
  }

  # -- apply parsed event chunk contents to one person ----------------------
  add_event <- function(id, type, body, remarried = FALSE) {
    p <- persons[[id]]
    ev <- list(type = type, raw = str_squish(body))
    tz <- str_match(body, "T([PZ]):\\s*(.*)$")
    if (!is.na(tz[1, 1])) {
      nm <- str_split_1(tz[1, 3], ",\\s*") |> str_squish()
      if (tz[1, 2] == "P") p$godparents <- c(p$godparents, nm)
      else p$witnesses <- c(p$witnesses, nm)
      body <- str_remove(body, "T[PZ]:\\s*.*$")
    }
    rr <- extract_refs(body); body <- rr$rest
    d <- parse_date(body)
    ev$date <- d$date; ev$date_qualifier <- d$qualifier
    pl <- match_place(d$rest)
    ev$place <- pl$place
    rest <- pl$rest
    if (str_detect(rest, "\\bled\\.")) {
      p$marital_status <- "unmarried"
      rest <- str_squish(str_remove_all(rest, "\\bled\\."))
    }
    r <- take_religion(rest)
    if (!is.null(r$religion) && is.null(p$religion)) p$religion <- r$religion
    rest <- r$rest
    # residence marker WoNS / WoAS in event tail
    for (mm in str_match_all(rest, "\\bWo(NS|AS)\\b")[[1]][, 2]) {
      p$residences <- c(p$residences, place_abbrev[[mm]])
    }
    rest <- str_squish(str_remove_all(rest, "\\bWo(NS|AS)\\b"))
    if (remarried) ev$remarriage <- TRUE
    if (rest != "") ev$detail <- rest
    ev <- compact(ev)
    p$events <- c(p$events, list(ev))
    persons[[p$id]] <<- p
    add_refs(id, rr$refs)
    length(persons[[id]]$events)
  }

  # -- family-level union ---------------------------------------------------
  family_union <- function(n, type, body, line) {
    union_no <<- if (is.null(n)) union_no + 1L else max(union_no + 1L, as.integer(n))
    prim <- persons[["primary"]]
    prim_id <- paste0(fam, ".0")
    if (is.null(persons[[prim_id]])) { # record without named primary
      put(new_person(prim_id, fam, "primary"))
    }
    tz <- str_match(body, "T([PZ]):\\s*(.*)$")
    if (!is.na(tz[1, 1])) {
      nm <- str_split_1(tz[1, 3], ",\\s*") |> str_squish()
      p0 <- persons[[prim_id]]
      if (tz[1, 2] == "P") p0$godparents <- c(p0$godparents, nm)
      else p0$witnesses <- c(p0$witnesses, nm)
      persons[[prim_id]] <<- p0
      body <- str_trim(str_remove(body, "T[PZ]:\\s*.*$"))
    }
    rr <- extract_refs(body); body <- rr$rest
    d <- parse_date(body)
    pl <- match_place(d$rest)
    rest <- pl$rest
    ev <- compact(list(
      type = type, date = d$date, date_qualifier = d$qualifier,
      place = pl$place, union_number = union_no, raw = str_squish(line)
    ))
    if (rest != "") {
      # inline spouse on the family union line (uncommon)
      spouse_no <<- spouse_no + 1L
      sid <- paste0(fam, ".s", spouse_no)
      sp <- new_person(sid, fam, "spouse")
      sp$union <- union_no; sp$source <- line
      put(sp)
      apply_prefix(sid, rest)
      add_refs(sid, rr$refs)
      ev$spouse_id <- sid
      pending_marriage <<- NULL
    } else {
      pending_marriage <<- list(event_holder = prim_id)
      add_refs(prim_id, rr$refs)
    }
    p <- persons[[prim_id]]
    p$events <- c(p$events, list(ev))
    persons[[prim_id]] <<- p
    if (!is.null(pending_marriage)) {
      pending_marriage$event_index <<- length(p$events)
    }
    cur <<- prim_id
  }

  # -- marriage chunk of a child (or numbered union): inline spouse ---------
  child_union <- function(child_id, n, type, body, line) {
    remarried <- FALSE
    if (str_detect(body, "^wieder\\b")) {
      remarried <- TRUE; body <- str_squish(str_remove(body, "^wieder\\b"))
    }
    tzm <- str_match(body, "T([PZ]):\\s*(.*)$")
    tz_txt <- NULL
    if (!is.na(tzm[1, 1])) {
      tz_txt <- tzm; body <- str_remove(body, "T[PZ]:\\s*.*$")
    }
    rr <- extract_refs(body); body <- rr$rest
    d <- parse_date(body)
    # Try surname-first match before place matching:
    # if the first token is a known surname (case-insensitive), treat the
    # whole remainder as the spouse name rather than probing for a place.
    name_txt <- NULL
    place_val <- NULL
    rest_after_date <- str_trim(d$rest)
    first_tok <- str_extract(rest_after_date, "^[^\\s]+")
    if (!is.na(first_tok) && str_to_upper(first_tok) %in% surnames) {
      name_txt <- rest_after_date
      place_val <- NULL
    } else if (first_tok %in% c("NN.", "NN")) {
      name_txt <- rest_after_date
      place_val <- NULL
    } else {
      pl <- match_place(rest_after_date)
      place_val <- pl$place
      name_txt <- pl$rest
    }
    p <- persons[[child_id]]
    ev <- compact(list(
      type = type, date = d$date, date_qualifier = d$qualifier,
      place = place_val, union_number = n,
      remarriage = if (remarried) TRUE else NULL, raw = str_squish(line)
    ))
    spouse_id <- NULL
    if (name_txt != "") {
      csn <- (child_spouse_no[[child_id]] %||% 0L) + 1L
      child_spouse_no[[child_id]] <<- csn
      spouse_id <- paste0(child_id, "m", csn)
      sp <- new_person(spouse_id, fam, "child_spouse")
      sp$spouse_of <- child_id
      sp$source <- line
      put(sp)
      apply_prefix(spouse_id, name_txt)
      add_refs(spouse_id, rr$refs)
      ev$spouse_id <- spouse_id
    } else {
      add_refs(child_id, rr$refs)
    }
    pending_child_spouse <<- if (is.null(spouse_id)) child_id else NULL
    p <- persons[[child_id]]
    if (!is.null(tz_txt)) {
      nm <- str_split_1(tz_txt[1, 3], ",\\s*") |> str_squish()
      if (tz_txt[1, 2] == "P") p$godparents <- c(p$godparents, nm)
      else p$witnesses <- c(p$witnesses, nm)
    }
    p$events <- c(p$events, list(ev))
    persons[[child_id]] <<- p
    spouse_id
  }

  # -- apply a sequence of chunks starting at some person -------------------
  apply_chunks <- function(id, chunks, line) {
    target <- id
    for (ch in chunks) {
      role <- persons[[target]]$role
      if (ch$type %in% c("marriage", "union_unmarried") &&
          role %in% c("child", "child_spouse")) {
        anchor <- if (role == "child_spouse") persons[[target]]$spouse_of else target
        n <- (child_spouse_no[[anchor]] %||% 0L) + 1L
        sp <- child_union(anchor, n, ch$type, ch$body, line)
        if (!is.null(sp)) target <- sp
      } else if (ch$type %in% c("marriage", "union_unmarried") &&
                 role %in% c("primary", "spouse")) {
        family_union(NULL, ch$type, ch$body, line)
      } else {
        # age already stripped at line level; regular event
        add_event(target, ch$type, ch$body)
      }
    }
  }

  # ---- line dispatch ------------------------------------------------------
  for (line in block) {
    line <- str_trim(line)
    if (line == "") next
    if (str_detect(line, paste0(
      "^[\u00df\u00d6\u00dc\u00c4A-Z][\u00df\u00d6\u00dc\u00c4\u00f6\u00e4\u00fcA-Z",
      DASH, "-]+$"))) next # section header

    # strip age-at-death parens before any event splitting (contains †)
    ag <- extract_age(line)
    work <- ag$rest

    # split off an inline "# ..." note (from joined wrapped lines)
    inline_note <- NULL
    hm <- str_match(work, "^(.+?)\\s#\\s*(.*)$")
    if (!is.na(hm[1, 1])) { work <- hm[1, 2]; inline_note <- hm[1, 3] }

    if (str_detect(work, paste0("^(\\d+)\\.\\s*(oo|o", DASH, "o)"))) {
      m <- str_match(work, paste0("^(\\d+)\\.\\s*(oo|o", DASH, "o)\\s*(.*)$"))
      n <- as.integer(m[1, 2]); tag <- m[1, 3]; body <- m[1, 4]
      type <- if (tag == "oo") "marriage" else "union_unmarried"
      if (!is.null(cur) && persons[[cur]]$role %in% c("child", "child_spouse")) {
        anchor <- if (persons[[cur]]$role == "child_spouse")
          persons[[cur]]$spouse_of else cur
        sp <- child_union(anchor, n, type, body, line)
        if (!is.null(sp)) cur <- sp
      } else {
        family_union(n, type, body, line)
      }
    } else if (str_detect(work, "^(\\d+)\\.\\s*\\p{L}")) {
      m <- str_match(work, "^(\\d+)\\.\\s*(.*)$")
      pos <- as.integer(m[1, 2])
      # normalise missing spaces before event tags (e.g. "Peter* 1867" → "Peter * 1867")
      body <- str_replace_all(m[1, 3], "(?<=[\\p{L}])([*~†])", " \\1")
      pending_child_spouse <- NULL
      cid <- paste0(fam, ".", pos)
      ch <- new_person(cid, fam, "child", position = pos)
      ch$union <- max(union_no, 1L)
      ch$source <- line
      put(ch); cur <- cid
      se <- split_events(body)
      rp <- extract_refs(se$prefix)
      apply_prefix(cid, rp$rest)
      add_refs(cid, rp$refs)
      apply_chunks(cid, se$chunks, line)
    } else if (str_detect(work, paste0("^(oo|o", DASH, "o)([^\\p{L}]|$)"))) {
      m <- str_match(work, paste0("^(oo|o", DASH, "o)\\s*(.*)$"))
      tag <- m[1, 2]; body <- m[1, 3]
      type <- if (tag == "oo") "marriage" else "union_unmarried"
      # A bare oo/o-o with no inline content (body == "") is always a
      # family-level union — clear pending_child_spouse so the next ALL-CAPS
      # name line is not mistaken for a child's spouse.
      if (str_squish(body) == "") {
        pending_child_spouse <<- NULL
        family_union(NULL, type, body, line)
      } else if (!is.null(cur) && persons[[cur]]$role %in% c("child", "child_spouse")) {
        se <- split_events(paste(tag, body))
        apply_chunks(cur, se$chunks, line)
      } else {
        family_union(NULL, type, body, line)
      }
    } else if (str_detect(work, paste0("^", tag_pat))) {
      # line starts with an event tag: events of the current person
      if (is.null(cur)) { add_note(NULL, line); next }
      se <- split_events(work)
      apply_chunks(cur, se$chunks, line)
    } else if (str_detect(work, "\\boo wieder\\b")) {
      # "NAME oo wieder ..." remarriage clause
      rr <- extract_refs(work, context = "remarriage")
      add_note(cur, paste("Remarriage:", rr$rest))
      add_refs(cur %||% paste0(fam, ".0"), rr$refs)
    } else if (str_detect(work, "^T[PZ]:")) {
      m <- str_match(work, "^T([PZ]):\\s*(.*)$")
      if (!is.null(cur)) {
        p <- persons[[cur]]
        nm <- str_split_1(m[1, 3], ",\\s*") |> str_squish()
        if (m[1, 2] == "P") p$godparents <- c(p$godparents, nm)
        else p$witnesses <- c(p$witnesses, nm)
        persons[[cur]] <- p
      } else add_note(NULL, line)
    } else if (str_detect(work, "^(Eltern:?|Tv\\.|Sv\\.|Vater:|Mutter:)")) {
      m <- str_match(work, "^(Eltern:?|Tv\\.|Sv\\.|Vater:|Mutter:)\\s*(.*)$")
      rr <- extract_refs(m[1, 3], context = "parents")
      if (!is.null(cur)) {
        p <- persons[[cur]]
        pre <- c("Eltern" = "", "Eltern:" = "", "Tv." = "Tv. ", "Sv." = "Sv. ",
                 "Vater:" = "Vater: ", "Mutter:" = "Mutter: ")[[m[1, 2]]]
        p$parents <- c(p$parents,
                       paste0(pre, str_squish(str_split_1(rr$rest, ",\\s*"))))
        persons[[cur]] <- p
        add_refs(cur, rr$refs)
      } else add_note(NULL, line)
    } else if (str_detect(work, "^(Wohnort|letzter bekannter Wohnort|lebt |aus )")) {
      rr <- extract_refs(work, context = "residence")
      target <- if (str_detect(work, "^Wohnort der Familie"))
        paste0(fam, ".0") else cur
      if (!is.null(target) && !is.null(persons[[target]])) {
        p <- persons[[target]]
        if (str_detect(work, "^aus ")) {
          p$origin <- c(p$origin, str_squish(str_remove(rr$rest, "^aus ")))
        } else {
          p$residences <- c(p$residences, str_squish(rr$rest))
        }
        persons[[target]] <- p
        add_refs(target, rr$refs)
      } else add_note(NULL, line)
    } else if (str_detect(work, "^Witwe[r]? von ")) {
      rr <- extract_refs(work, context = "widow_of")
      if (!is.null(cur)) {
        p <- persons[[cur]]
        p$widow_of <- str_squish(str_remove(rr$rest, "^Witwe[r]? von "))
        persons[[cur]] <- p
        add_refs(cur, rr$refs)
      } else add_note(NULL, line)
    } else if (str_detect(work, "^#")) {
      add_note(cur, str_squish(str_remove(work, "^#\\s*")))
    } else if (str_detect(work, "^\\(") && str_detect(work, "\\)$")) {
      add_note(cur, str_remove_all(work, "^\\(|\\)$"))
    } else if (str_detect(work, "^[<>]\\s*\\d")) {
      rr <- extract_refs(work)
      add_refs(cur %||% paste0(fam, ".0"), rr$refs)
    } else if (!is.null(pending_child_spouse) && {
      ft <- str_extract(work, "^[^\\s]+")
      (!is.na(ft) && ft != str_to_upper(ft) &&
         str_to_upper(ft) %in% surnames) || str_detect(work, "^NN\\.?\\s")
    }) {
      # mixed-case name line: spouse of the child whose oo line had no name
      anchor <- pending_child_spouse
      pending_child_spouse <- NULL
      csn <- (child_spouse_no[[anchor]] %||% 0L) + 1L
      child_spouse_no[[anchor]] <- csn
      sid <- paste0(anchor, "m", csn)
      sp <- new_person(sid, fam, "child_spouse")
      sp$spouse_of <- anchor
      sp$source <- line
      put(sp)
      se <- split_events(work)
      rp <- extract_refs(se$prefix)
      apply_prefix(sid, rp$rest)
      add_refs(sid, rp$refs)
      apply_chunks(sid, se$chunks, line)
      # link the child's open marriage event to this spouse
      chp <- persons[[anchor]]
      idx <- length(chp$events)
      if (idx > 0 && chp$events[[idx]]$type %in% c("marriage", "union_unmarried") &&
          is.null(chp$events[[idx]]$spouse_id)) {
        chp$events[[idx]]$spouse_id <- sid
        persons[[anchor]] <- chp
      }
      cur <- sid
    } else if (str_detect(work, paste0(
      "^(NN\\.|[\u00df\u00d6\u00dc\u00c4A-Z][\u00df\u00d6\u00dc\u00c4\u00f6\u00e4\u00fcA-Z",
      DASH, "-]+)\\s"))) {
      # ALL-CAPS (or NN.) surname line: primary or spouse
      m <- str_match(work, paste0(
        "^(NN\\.|[\u00df\u00d6\u00dc\u00c4A-Z][\u00df\u00d6\u00dc\u00c4\u00f6\u00e4\u00fcA-Z",
        DASH, "-]+)\\s+(.*)$"))
      sur <- m[1, 2]; rest <- m[1, 3]
      pending_child_spouse <- NULL
      prim_id <- paste0(fam, ".0")
      if (is.null(persons[[prim_id]])) {
        id <- prim_id
        put(new_person(id, fam, "primary"))
      } else {
        spouse_no <- spouse_no + 1L
        id <- paste0(fam, ".s", spouse_no)
        sp <- new_person(id, fam, "spouse")
        sp$union <- max(union_no, 1L)
        put(sp)
        if (!is.null(pending_marriage)) {
          hp <- persons[[pending_marriage$event_holder]]
          idx <- pending_marriage$event_index %||% length(hp$events)
          if (idx > 0) {
            hp$events[[idx]]$spouse_id <- id
            persons[[hp$id]] <- hp
          }
          pending_marriage <- NULL
        }
      }
      cur <- id
      touch_source(id, line)
      se <- split_events(rest)
      rp <- extract_refs(se$prefix)
      apply_prefix(id, rp$rest, surname_from_line = sur)
      add_refs(id, rp$refs)
      apply_chunks(id, se$chunks, line)
    } else {
      # fallback: free-text note
      rr <- extract_refs(work, context = "note")
      add_note(cur, str_squish(rr$rest))
      add_refs(cur %||% paste0(fam, ".0"), rr$refs)
      qa$fallback_notes <- c(qa$fallback_notes, paste0("<", fam, "> ", line))
    }

    if (!is.null(inline_note) && !is.null(cur) && !is.null(persons[[cur]])) {
      add_note(cur, inline_note)
    }
    if (!is.null(ag$age) && !is.null(cur)) {
      p <- persons[[cur]]
      if (is.null(p$age_at_death)) p$age_at_death <- ag$age
      persons[[cur]] <- p
    }
    if (!is.null(cur) && !is.null(persons[[cur]])) touch_source(cur, line)
  }

  # placeholder / record-level notes
  if (length(persons) == 0) {
    put(new_person(paste0(fam, ".0"), fam, "primary"))
  }
  if (length(record_notes) > 0) {
    pid <- paste0(fam, ".0")
    if (!is.null(persons[[pid]])) {
      persons[[pid]]$notes <- c(persons[[pid]]$notes, record_notes)
    }
  }
  persons
}

# ---- run pass 1 ----------------------------------------------------------

parse_all <- function(path = "data/persons.txt") {
  raw <- read_lines(path)
  jl <- join_wrapped(raw)
  delims <- str_which(jl, "^<\\d+>$")
  fams <- as.integer(str_match(jl[delims], "^<(\\d+)>$")[, 2])
  ends <- c(delims[-1] - 1, length(jl))
  persons <- list()
  cli::cli_progress_bar("Parsing records", total = length(delims))
  for (i in seq_along(delims)) {
    block <- jl[(delims[i] + 1):ends[i]]
    res <- tryCatch(
      parse_record(fams[i], block),
      error = function(e) {
        qa$errors <- c(qa$errors, paste0("<", fams[i], "> ", conditionMessage(e)))
        list()
      }
    )
    persons <- c(persons, res)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  persons
}

# ---- pass 2: resolve cross references ------------------------------------

resolve_refs <- function(persons) {
  ids <- names(persons)
  fams <- map_int(persons, "family")
  fam_set <- unique(fams)
  unresolved <- list()
  cli::cli_progress_bar("Resolving cross-references", total = length(ids))
  for (id in ids) {
    cli::cli_progress_update()
    p <- persons[[id]]
    if (length(p$refs) == 0) next
    for (j in seq_along(p$refs)) {
      r <- p$refs[[j]]
      tf <- r$target_family
      tid <- if (!is.null(r$target_child)) {
        paste0(tf, ".", r$target_child)
      } else {
        paste0(tf, ".0")
      }
      r$resolved_id <- NULL
      r$valid <- FALSE
      issues <- character()
      if (!(tf %in% fam_set)) {
        issues <- c(issues, "target family not found")
      } else if (is.null(persons[[tid]])) {
        # Name-match fallback: if a child position is missing (off-by-one book
        # typo), try to match by surname against children of that family.
        matched_id <- NULL
        if (!is.null(r$target_child)) {
          ref_sur <- str_to_upper(p$surname %||% "")
          candidates <- keep(persons, ~.x$family == tf & .x$role == "child")
          name_match <- detect(candidates,
            ~str_to_upper(.x$surname %||% "") == ref_sur)
          if (!is.null(name_match)) {
            matched_id <- name_match$id
            issues <- c(issues, paste0("target child ", r$target_child,
                                       " not found; resolved by name match to ",
                                       matched_id))
          } else {
            issues <- c(issues, "target person not found")
          }
        } else {
          issues <- c(issues, "target person not found")
        }
        if (!is.null(matched_id)) {
          r$resolved_id <- matched_id
          r$valid <- TRUE
          persons[[matched_id]]$referenced_by <-
            unique(c(persons[[matched_id]]$referenced_by, id))
        }
      } else {
        r$resolved_id <- tid
        r$valid <- TRUE
        persons[[tid]]$referenced_by <-
          unique(c(persons[[tid]]$referenced_by, id))
      }
      dir_ok <- (r$direction == "back" && tf <= p$family) ||
        (r$direction == "forward" && tf >= p$family)
      if (!dir_ok) issues <- c(issues, "direction mismatch")
      if (length(issues) > 0) {
        r$issues <- issues
        unresolved[[length(unresolved) + 1]] <-
          list(person = id, ref = r$raw, issues = paste(issues, collapse = "; "))
      }
      p$refs[[j]] <- r
    }
    persons[[id]] <- p
  }
  cli::cli_progress_done()
  list(persons = persons, unresolved = unresolved)
}

# ---- main ----------------------------------------------------------------

run_parser <- function() {
  message("Parsing...")
  persons <- parse_all()
  message(length(persons), " persons parsed.")
  res <- resolve_refs(persons)
  out <- unname(map(res$persons, \(p) compact(p)))
  write_json(out, "data/persons.json", auto_unbox = TRUE, pretty = TRUE,
             null = "null")
  message("Wrote data/persons.json")
  invisible(list(persons = res$persons, unresolved = res$unresolved, qa = qa))
}
