# Convert data/persons.json (Schowe Familienbuch database) to GEDCOM 5.5.5.
#
# Usage:
#   source("R/json_to_gedcom.R")
#   res <- convert_to_gedcom()   # writes data/persons.ged
#
# Notes on the mapping:
# * The JSON has no sex field. Sex is inferred from a data-driven lookup
#   (primary persons are overwhelmingly male, spouses female in this book),
#   with a name-suffix heuristic as fallback. When a couple's sexes are
#   ambiguous, the primary/child partner is placed in the HUSB slot.
# * Cross-referenced duplicates ARE merged (merge_duplicates = TRUE): a
#   `context == "self"` ref means "this couple continues at family n", so the
#   source couple (child + child-spouse, or primary + spouse) is matched
#   against the target family's primary/spouses BY NAME (same surname plus a
#   shared given-name token). Matched pairs are unified with union-find and
#   emitted as single INDI records; refs that cannot be matched by name are
#   left unmerged and reported in the returned `qa_unmatched` data frame.
# * After merging, a child-couple FAM may duplicate the target family's own
#   union FAM (same two canonical partners); such FAMs are combined.
# * Raw source lines and free-text notes are preserved as NOTEs so nothing
#   is lost.

library(jsonlite)
library(purrr)
library(stringr)
library(cli)

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- small helpers -----------------------------------------------------------

# xref id from a person id like "12.2m1" -> "@I12_2M1@"
indi_xref <- function(id) paste0("@I", toupper(gsub("\\.", "_", id)), "@")

MONTHS <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
            "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# ISO (possibly truncated) date -> GEDCOM date value
ged_date_core <- function(iso) {
  parts <- strsplit(iso, "-", fixed = TRUE)[[1]]
  if (length(parts) == 3) {
    paste(as.integer(parts[3]), MONTHS[as.integer(parts[2])], parts[1])
  } else if (length(parts) == 2) {
    paste(MONTHS[as.integer(parts[2])], parts[1])
  } else {
    parts[1]
  }
}

ged_date <- function(date, qualifier = NULL) {
  if (is.null(date) || !nzchar(date)) return(NULL)
  if (!is.null(qualifier) && qualifier == "between") {
    # stored as "1981/1986"
    yrs <- strsplit(date, "/", fixed = TRUE)[[1]]
    if (length(yrs) == 2) return(paste("BET", yrs[1], "AND", yrs[2]))
    return(date)
  }
  core <- ged_date_core(date)
  prefix <- switch(qualifier %||% "",
                   about = "ABT ", before = "BEF ", after = "AFT ", "")
  paste0(prefix, core)
}

# "2 months 6 days" / "55 years" -> "2m 6d" / "55y"
ged_age <- function(txt) {
  out <- str_replace_all(txt, c(" years?" = "y", " months?" = "m", " days?" = "d"))
  str_squish(out)
}

# GEDCOM 5.5.5: max 255 chars per line; long values continue with CONC,
# embedded newlines with CONT. Splits avoid creating leading/trailing spaces.
emit_text <- function(level, tag, value, max_len = 200) {
  value <- gsub("@", "@@", value)
  paras <- strsplit(value, "\n", fixed = TRUE)[[1]]
  if (length(paras) == 0) paras <- ""
  out <- character(0)
  first_tag <- tag
  for (p in paras) {
    cur_tag <- first_tag
    cur_lvl <- if (identical(cur_tag, tag)) level else level + 1
    repeat {
      if (nchar(p) <= max_len) {
        out <- c(out, paste(cur_lvl, cur_tag, p))
        break
      }
      cut <- max_len
      # don't split adjacent to a space
      while (cut > 1 && (substr(p, cut, cut) == " " || substr(p, cut + 1, cut + 1) == " ")) {
        cut <- cut - 1
      }
      out <- c(out, paste(cur_lvl, cur_tag, substr(p, 1, cut)))
      p <- substr(p, cut + 1, nchar(p))
      cur_tag <- "CONC"
      cur_lvl <- level + 1
    }
    first_tag <- "CONT"
  }
  out
}

# --- sex inference -----------------------------------------------------------

first_given <- function(p) {
  g <- p$given %||% ""
  if (!nzchar(g)) return(NA_character_)
  strsplit(g, "[ ,]+")[[1]][1]
}

build_sex_lookup <- function(persons) {
  roles <- map_chr(persons, "role")
  firsts <- map_chr(persons, first_given)
  keep <- roles %in% c("primary", "spouse") & !is.na(firsts)
  # primaries in this book are (almost) all male, spouses of primaries female
  votes <- table(firsts[keep], ifelse(roles[keep] == "primary", "M", "F"))
  lookup <- ifelse(votes[, "M"] >= votes[, "F"], "M", "F")
  names(lookup) <- rownames(votes)
  lookup
}

infer_sex <- function(p, lookup) {
  f <- first_given(p)
  if (is.na(f) || f == "NN") return("U")
  if (!is.null(lookup[f]) && !is.na(lookup[f])) return(unname(lookup[f]))
  # fallback heuristic for names unseen in primary/spouse roles
  if (grepl("(a|e|in|th)$", f)) "F" else "M"
}

# --- identity resolution (duplicate merging) ---------------------------------

name_tokens <- function(p) {
  toks <- toupper(strsplit(p$given %||% "", "[ ,.]+")[[1]])
  setdiff(toks, c("", "NN"))
}

# token equality tolerant of spelling variants (Elisabeth/Elisabetha)
tok_eq <- function(a, b) {
  a == b || (nchar(a) >= 5 && nchar(b) >= 5 && utils::adist(a, b) <= 1)
}

n_shared <- function(ta, tb) {
  sum(vapply(ta, function(x) any(vapply(tb, tok_eq, TRUE, a = x)), TRUE))
}

# Same human? First try: shared given-name token + compatible surnames.
# (Women appear under their maiden name both as child and as spouse, so
# surname equality is the right test; unknown surnames are permissive.)
# Fallback: >= 2 shared tokens across the full name (given + surname bag),
# which absorbs parser glitches where the given/surname split differs
# between the two records (e.g. "Nickel Hans" vs "Hans Nickel /Henn/").
same_person <- function(a, b) {
  ga <- name_tokens(a)
  gb <- name_tokens(b)
  sa <- toupper(a$surname %||% "")
  sb <- toupper(b$surname %||% "")
  surname_ok <- !nzchar(sa) || !nzchar(sb) || tok_eq(sa, sb) ||
    isTRUE(a$surname_unknown) || isTRUE(b$surname_unknown)
  if (n_shared(ga, gb) >= 1 && surname_ok) return(TRUE)
  n_shared(c(ga, if (nzchar(sa)) sa), c(gb, if (nzchar(sb)) sb)) >= 2
}

# Weaker test used only for couple-level elimination (see
# build_identity_map): compatible surnames (tolerating NN. given names on
# either side), or a shared given token despite a mismatched surname.
weak_match <- function(a, b) {
  ga <- name_tokens(a)
  gb <- name_tokens(b)
  sa <- toupper(a$surname %||% "")
  sb <- toupper(b$surname %||% "")
  surname_ok <- !nzchar(sa) || !nzchar(sb) || tok_eq(sa, sb) ||
    isTRUE(a$surname_unknown) || isTRUE(b$surname_unknown)
  if (length(ga) && length(gb)) {
    # both given names known: they must agree (surname may be mangled);
    # disagreeing given names are never merged, same-surname siblings exist
    n_shared(ga, gb) >= 1
  } else {
    # a given name is missing (NN.): rely on the surname
    surname_ok
  }
}

# Persons forming a couple with p (used to resolve "this couple continues
# at family n" references).
partners_of <- function(p, fam_members) {
  switch(p$role,
    primary      = keep(fam_members, ~ .x$role == "spouse"),
    spouse       = keep(fam_members, ~ .x$role == "primary"),
    child        = keep(fam_members, ~ .x$role == "child_spouse" &&
                          identical(.x$spouse_of, p$id)),
    child_spouse = keep(fam_members, ~ identical(.x$id, p$spouse_of)),
    list()
  )
}

# Build id -> canonical id map from context=="self" refs, matching the
# source couple against the target couple by name. Returns list(canon,
# qa_unmatched).
build_identity_map <- function(persons, by_family, extra_pairs = NULL) {
  parent <- new.env(parent = emptyenv())
  pfind <- function(x) {
    r <- x
    while (!identical(get0(r, envir = parent, ifnotfound = r), r)) {
      r <- get(r, envir = parent)
    }
    r
  }
  punion <- function(a, b) {
    ra <- pfind(a); rb <- pfind(b)
    if (!identical(ra, rb)) assign(rb, ra, envir = parent)
  }

  qa <- list()
  for (p in persons) {
    for (ref in p$refs %||% list()) {
      if (!identical(ref$context, "self") || !isTRUE(ref$valid)) next
      tf <- as.character(ref$target_family)
      tmembers <- by_family[[tf]] %||% list()
      if (!length(tmembers)) next
      rid <- ref$resolved_id
      if (!is.null(rid) && rid %in% names(persons)) {
        # anchor on the parser's resolution (it may have corrected the raw
        # target, e.g. "child 3 not found; resolved by name match"), and
        # take the anchor's couple as the target set
        anchor <- persons[[rid]]
        targets <- c(list(anchor), partners_of(anchor, tmembers))
      } else {
        tc <- ref$target_child
        targets <- if (!is.null(tc)) {
          keep(tmembers, ~ (.x$role == "child" && identical(.x$position, as.integer(tc))) ||
                 (.x$role == "child_spouse" &&
                    identical(.x$spouse_of, paste0(tf, ".", tc))))
        } else {
          keep(tmembers, ~ .x$role %in% c("primary", "spouse"))
        }
      }
      sources <- c(list(p), partners_of(p, by_family[[as.character(p$family)]]))
      taken <- character(0)
      unmatched <- list()
      ambiguous <- FALSE
      for (s in sources) {
        hits <- keep(targets, ~ same_person(s, .x) && !.x$id %in% taken)
        if (length(hits) == 1) {
          punion(s$id, hits[[1]]$id)
          taken <- c(taken, hits[[1]]$id)
        } else {
          unmatched <- c(unmatched, list(s))
          if (length(hits) > 1) ambiguous <- TRUE
        }
      }
      # elimination: the couple travels together, so if the partner matched
      # and exactly one source/target pair is left, accept a weaker match
      # (compatible surname — tolerating NN. given names — or a shared
      # given token despite a mangled surname)
      remaining <- keep(targets, ~ !.x$id %in% taken)
      if (length(taken) >= 1 && length(unmatched) == 1 && length(remaining) == 1 &&
          !ambiguous && weak_match(unmatched[[1]], remaining[[1]])) {
        punion(unmatched[[1]]$id, remaining[[1]]$id)
        unmatched <- list()
      }
      if (any(map_lgl(unmatched, ~ identical(.x$id, p$id)))) {
        # the ref-bearing person itself couldn't be matched
        qa <- c(qa, list(data.frame(
          from = p$id, raw = ref$raw %||% "", target = ref$resolved_id %||% tf,
          reason = if (ambiguous) "ambiguous name match" else "no name match"
        )))
      }
    }
  }

  # manually adjudicated merges (data/qa_merge_decisions.csv)
  if (!is.null(extra_pairs) && nrow(extra_pairs)) {
    for (i in seq_len(nrow(extra_pairs))) {
      if (extra_pairs$from[i] %in% names(persons) &&
          extra_pairs$to[i] %in% names(persons)) {
        punion(extra_pairs$from[i], extra_pairs$to[i])
      }
    }
    qa <- keep(qa, ~ !.x$from[1] %in% extra_pairs$from)
  }

  canon <- vapply(names(persons), pfind, "")
  list(
    canon = canon,
    qa_unmatched = if (length(qa)) do.call(rbind, qa) else
      data.frame(from = character(0), raw = character(0),
                 target = character(0), reason = character(0))
  )
}

# Choose the cluster representative: prefer the primary record (richest),
# then spouse, child, child_spouse.
pick_representative <- function(ids, persons) {
  rank <- c(primary = 1, spouse = 2, child = 3, child_spouse = 4)
  ids[order(rank[map_chr(persons[ids], "role")], ids)][1]
}

# Combine all records of one cluster into a single person object.
merge_cluster <- function(ids, persons) {
  rep_id <- pick_representative(ids, persons)
  merged <- persons[[rep_id]]
  others <- persons[setdiff(ids, rep_id)]
  cat_field <- function(field) {
    unique(unlist(c(merged[[field]], map(others, field))))
  }
  for (f in c("occupation", "residences", "godparents", "witnesses",
              "notes", "parents")) {
    v <- cat_field(f)
    merged[[f]] <- if (length(v)) as.list(v) else NULL
  }
  for (f in c("religion", "age_at_death", "widow_of", "origin",
              "marital_status")) {
    v <- cat_field(f)
    merged[[f]] <- if (length(v)) v[[1]] else NULL
  }
  # events: keep all, dropping exact duplicates (same type + date + place)
  evs <- c(merged$events %||% list(), unlist(map(others, "events"), recursive = FALSE))
  keys <- map_chr(evs, ~ paste(.x$type, .x$date %||% .x$raw %||% "", .x$place %||% ""))
  merged$events <- evs[!duplicated(keys)]
  # refs: keep only those pointing outside the cluster
  refs <- c(merged$refs %||% list(), unlist(map(others, "refs"), recursive = FALSE))
  merged$refs <- keep(refs, ~ !((.x$resolved_id %||% "") %in% ids))
  # provenance
  merged$source <- unlist(map(persons[ids], function(q) {
    c(paste0("[", q$id, "]"), unlist(q$source %||% list()))
  }))
  merged$merged_from <- setdiff(ids, rep_id)
  merged
}

# --- event / person emission -------------------------------------------------

event_lines <- function(ev, level = 1) {
  tag <- switch(ev$type,
                birth = "BIRT", baptism = "CHR", death = "DEAT",
                burial = "BURI", marriage = "MARR", NULL)
  if (is.null(tag)) return(character(0))
  out <- paste(level, tag)
  d <- ged_date(ev$date, ev$date_qualifier)
  if (!is.null(d)) out <- c(out, paste(level + 1, "DATE", d))
  plc <- ev$place %||% ev$detail
  if (!is.null(plc)) out <- c(out, emit_text(level + 1, "PLAC", plc))
  # asserted event with no substructure needs Y (GEDCOM 5.5.5)
  if (length(out) == 1) out <- paste(level, tag, "Y")
  out
}

person_lines <- function(p, xref, sex, fams, famc, cid) {
  x <- character(0)
  x <- c(x, paste("0", xref, "INDI"))
  given <- p$given %||% ""
  surname <- if (isTRUE(p$surname_unknown)) "" else p$surname %||% ""
  x <- c(x, str_squish(paste0("1 NAME ", given, " /", surname, "/")))
  if (nzchar(given))   x <- c(x, paste("2 GIVN", given))
  if (nzchar(surname)) x <- c(x, paste("2 SURN", surname))
  if (sex %in% c("M", "F")) x <- c(x, paste("1 SEX", sex))

  for (ev in p$events %||% list()) {
    if (ev$type %in% c("marriage", "union_unmarried")) next  # handled in FAM
    lines <- event_lines(ev)
    if (ev$type == "death" && !is.null(p$age_at_death)) {
      lines <- c(lines, paste("2 AGE", ged_age(p$age_at_death)))
    }
    if (ev$type == "baptism" && !is.null(p$godparents)) {
      lines <- c(lines, emit_text(2, "NOTE",
        paste("Godparents (TP):", paste(unlist(p$godparents), collapse = ", "))))
    }
    x <- c(x, lines)
  }
  # ensure every person has a death record
  if (!any(map_chr(p$events %||% list(), "type") == "death")) {
    deat <- "1 DEAT Y"
    if (!is.null(p$age_at_death)) deat <- c(deat, paste("2 AGE", ged_age(p$age_at_death)))
    x <- c(x, deat)
  }

  if (!is.null(p$religion))  x <- c(x, paste("1 RELI", p$religion))
  for (occ in unlist(p$occupation %||% list())) x <- c(x, emit_text(1, "OCCU", occ))
  for (res in unlist(p$residences %||% list())) {
    x <- c(x, "1 RESI", emit_text(2, "PLAC", res))
  }

  # notes ---------------------------------------------------------------
  if (!is.null(p$merged_from) && length(p$merged_from)) {
    x <- c(x, emit_text(1, "NOTE", paste(
      "Merged from Familienbuch entries:",
      paste(c(p$id, p$merged_from), collapse = ", "))))
  }
  if (!is.null(p$parents)) {
    x <- c(x, emit_text(1, "NOTE",
      paste("Parents:", paste(unlist(p$parents), collapse = "; "))))
  }
  if (!is.null(p$widow_of)) {
    x <- c(x, emit_text(1, "NOTE", paste("Widow of:", p$widow_of)))
  }
  if (!is.null(p$origin)) {
    x <- c(x, emit_text(1, "NOTE", paste("Origin (aus):", p$origin)))
  }
  if (!is.null(p$marital_status)) {
    x <- c(x, emit_text(1, "NOTE", paste("Marital status:", p$marital_status)))
  }
  if (!is.null(p$witnesses)) {
    x <- c(x, emit_text(1, "NOTE",
      paste("Marriage witnesses (TZ):", paste(unlist(p$witnesses), collapse = ", "))))
  }
  for (ref in p$refs %||% list()) {
    if (isTRUE(ref$valid) && !is.null(ref$resolved_id)) {
      x <- c(x, emit_text(1, "NOTE", paste0(
        "Cross-reference (", ref$context %||% "self", "): '", ref$raw,
        "' -> related person ", indi_xref(cid(ref$resolved_id)))))
    } else {
      x <- c(x, emit_text(1, "NOTE",
        paste0("Unresolved cross-reference: '", ref$raw %||% "", "'")))
    }
  }
  for (nt in unlist(p$notes %||% list())) x <- c(x, emit_text(1, "NOTE", nt))
  src <- unlist(p$source %||% list())
  if (length(src)) {
    x <- c(x, emit_text(1, "NOTE",
      paste0("Familienbuch source lines:\n", paste(src, collapse = "\n"))))
  }

  for (fx in unique(famc[[p$id]] %||% character(0))) x <- c(x, paste("1 FAMC", fx))
  for (fx in unique(fams[[p$id]] %||% character(0))) x <- c(x, paste("1 FAMS", fx))
  x
}

# --- family construction -----------------------------------------------------

# Order a couple into (husband, wife) using inferred sex; the primary/child
# partner defaults to HUSB when ambiguous.
order_couple <- function(a, b, sex_of) {
  sa <- if (is.null(a)) "U" else sex_of[[a$id]]
  sb <- if (is.null(b)) "U" else sex_of[[b$id]]
  if (identical(sa, "F") || identical(sb, "M")) list(husb = b, wife = a)
  else list(husb = a, wife = b)
}

marr_lines_for <- function(members, match_fn) {
  # collect matching marriage/union events from the given persons,
  # preferring one that carries a date
  cands <- list()
  for (m in members) {
    for (ev in m$events %||% list()) {
      if (ev$type %in% c("marriage", "union_unmarried") && match_fn(ev)) {
        cands <- c(cands, list(ev))
      }
    }
  }
  if (!length(cands)) return(character(0))
  with_date <- keep(cands, ~ !is.null(.x$date))
  ev <- if (length(with_date)) with_date[[1]] else cands[[1]]
  if (ev$type == "union_unmarried") {
    out <- c("1 EVEN", "2 TYPE Unmarried union")
    d <- ged_date(ev$date, ev$date_qualifier)
    if (!is.null(d)) out <- c(out, paste("2 DATE", d))
    if (!is.null(ev$place)) out <- c(out, emit_text(2, "PLAC", ev$place))
    out
  } else {
    ev$type <- "marriage"
    event_lines(ev)
  }
}

# Build structured family units: list(fx, husb, wife, marr, chil) with
# original (pre-merge) person ids.
build_fam_units <- function(persons, by_family, sex_of) {
  units <- list()
  for (fam in by_family) {
    roles <- map_chr(fam, "role")
    fnum <- fam[[1]]$family
    primary <- fam[roles == "primary"]
    primary <- if (length(primary)) primary[[1]] else NULL
    spouses <- fam[roles == "spouse"]
    children <- fam[roles == "child"]
    children <- children[order(map_int(children, ~ .x$position %||% 0L))]
    child_spouses <- fam[roles == "child_spouse"]

    # -- primary's unions ------------------------------------------------
    spouse_union <- map_int(spouses, ~ .x$union %||% 1L)
    child_union <- map_int(children, ~ .x$union %||% 1L)
    unions <- sort(unique(c(spouse_union, child_union,
                            if (length(children) || length(spouses)) 1L)))
    for (u in unions) {
      sp <- spouses[spouse_union == u]
      sp <- if (length(sp)) sp[[1]] else NULL
      kids <- children[child_union == u]
      if (is.null(primary) && is.null(sp) && !length(kids)) next
      cpl <- order_couple(primary, sp, sex_of)
      units <- c(units, list(list(
        fx = paste0("@F", fnum, "U", u, "@"),
        husb = if (!is.null(cpl$husb)) cpl$husb$id,
        wife = if (!is.null(cpl$wife)) cpl$wife$id,
        marr = marr_lines_for(
          compact(list(primary, sp)),
          function(ev) (ev$union_number %||% 1L) == u &&
            (is.null(ev$spouse_id) || is.null(sp) || ev$spouse_id == sp$id)),
        chil = map_chr(kids, "id"),
        primary_fam = TRUE
      )))
    }

    # -- children's marriages --------------------------------------------
    for (cs in child_spouses) {
      sid <- cs$spouse_of
      if (is.null(sid)) next
      child <- keep(fam, ~ identical(.x$id, sid))
      if (!length(child)) next
      child <- child[[1]]
      cpl <- order_couple(child, cs, sex_of)
      units <- c(units, list(list(
        fx = paste0("@F", toupper(gsub("\\.", "_", cs$id)), "@"),
        husb = if (!is.null(cpl$husb)) cpl$husb$id,
        wife = if (!is.null(cpl$wife)) cpl$wife$id,
        marr = marr_lines_for(
          list(child, cs),
          function(ev) is.null(ev$spouse_id) || ev$spouse_id %in% c(cs$id, child$id)),
        chil = character(0),
        primary_fam = FALSE
      )))
    }
  }
  units
}

# After canonicalization, FAMs with the same two partners are duplicates
# (a child couple and the family they continue as). Combine them, preferring
# the primary-family unit (it carries the children).
dedupe_fam_units <- function(units, cid) {
  key <- map_chr(units, function(u) {
    h <- if (is.null(u$husb)) "" else cid(u$husb)
    w <- if (is.null(u$wife)) "" else cid(u$wife)
    if (!nzchar(h) || !nzchar(w)) return(u$fx)  # single-partner: never merge
    paste(sort(c(h, w)), collapse = "|")
  })
  merged <- list()
  alias <- character(0)
  for (grp in split(units, key)) {
    if (length(grp) == 1) { merged <- c(merged, grp); next }
    # keep the primary-family unit if present, else the first
    ord <- order(!map_lgl(grp, "primary_fam"))
    grp <- grp[ord]
    base <- grp[[1]]
    for (other in grp[-1]) {
      base$chil <- unique(c(base$chil, other$chil))
      # prefer marriage lines that carry a date
      if (!any(grepl("^2 DATE", base$marr)) && any(grepl("^2 DATE", other$marr))) {
        base$marr <- other$marr
      }
      alias[other$fx] <- base$fx
    }
    merged <- c(merged, list(base))
  }
  list(units = merged, alias = alias)
}

# --- main --------------------------------------------------------------------

convert_to_gedcom <- function(json_path = "data/persons.json",
                              out_path = "data/persons.ged",
                              merge_duplicates = TRUE,
                              qa_path = "data/qa_merge_unmatched.csv",
                              decisions_path = "data/qa_merge_decisions.csv") {
  cli_progress_step("Loading and indexing persons")
  persons <- read_json(json_path)
  names(persons) <- map_chr(persons, "id")
  by_family <- split(persons, map_int(persons, "family"))

  sex_of <- NULL  # filled below (after merging, so clusters share one sex)

  # -- identity map -----------------------------------------------------
  if (merge_duplicates) {
    cli_progress_step("Building identity map")
    extra <- NULL
    if (!is.null(decisions_path) && file.exists(decisions_path)) {
      dec <- utils::read.csv(decisions_path)
      extra <- dec[dec$verdict == "merge", c("from", "to")]
    }
    idm <- build_identity_map(persons, by_family, extra_pairs = extra)
    canon <- idm$canon
  } else {
    cli_progress_step("Skipping identity map (merge_duplicates = FALSE)")
    idm <- list(qa_unmatched = data.frame())
    canon <- setNames(names(persons), names(persons))
  }
  cid <- function(id) {
    v <- canon[id]
    if (is.na(v)) id else unname(v)
  }

  # -- merged person objects (keyed by canonical id) ---------------------
  cli_progress_step("Merging duplicate records")
  clusters <- split(names(persons), canon)
  merged_persons <- imap(clusters, function(ids, rep0) {
    if (length(ids) == 1) persons[[ids]] else merge_cluster(ids, persons)
  })
  # merge_cluster picks its own representative; re-key on it
  names(merged_persons) <- map_chr(merged_persons, "id")
  canon2 <- canon
  for (mp in merged_persons) {
    for (oid in c(mp$id, mp$merged_from %||% character(0))) canon2[oid] <- mp$id
  }
  canon <- canon2

  cli_progress_step("Inferring sex for persons")
  sex_lookup <- build_sex_lookup(persons)
  sex_of <- map(persons, infer_sex, lookup = sex_lookup)

  # -- family units -------------------------------------------------------
  cli_progress_step("Building and deduplicating family units")
  units <- build_fam_units(persons, by_family, sex_of)
  dd <- dedupe_fam_units(units, cid)
  units <- dd$units

  cli_progress_step("Creating FAM records")
  fams <- new.env(parent = emptyenv())  # canonical person id -> FAMS xrefs
  famc <- new.env(parent = emptyenv())
  add_link <- function(env, pid, fx) {
    pid <- cid(pid)
    assign(pid, c(env[[pid]], fx), envir = env)
  }

  fam_records <- map(units, function(u) {
    rec <- paste("0", u$fx, "FAM")
    if (!is.null(u$husb)) rec <- c(rec, paste("1 HUSB", indi_xref(cid(u$husb))))
    if (!is.null(u$wife)) rec <- c(rec, paste("1 WIFE", indi_xref(cid(u$wife))))
    rec <- c(rec, u$marr)
    for (k in u$chil) rec <- c(rec, paste("1 CHIL", indi_xref(cid(k))))
    if (!is.null(u$husb)) add_link(fams, u$husb, u$fx)
    if (!is.null(u$wife)) add_link(fams, u$wife, u$fx)
    for (k in u$chil) add_link(famc, k, u$fx)
    rec
  })

  cli_progress_step("Creating INDI records and writing output")
  fams_l <- as.list(fams)
  famc_l <- as.list(famc)
  indi_records <- map(merged_persons, function(p) {
    person_lines(p, indi_xref(p$id), sex_of[[p$id]], fams_l, famc_l, cid)
  })

  header <- c(
    "0 HEAD",
    "1 GEDC",
    "2 VERS 5.5.5",
    "2 FORM LINEAGE-LINKED",
    "3 VERS 5.5.5",
    "1 CHAR UTF-8",
    "1 SOUR SCHOWE_PARSER",
    "2 NAME Schowe Familienbuch JSON-to-GEDCOM converter",
    "2 VERS 2.0",
    paste("1 DATE", toupper(format(Sys.Date(), "%d %b %Y"))),
    "1 SUBM @U1@",
    "0 @U1@ SUBM",
    "1 NAME Schowe Family Book Project"
  )

  lines <- c(header, unlist(indi_records), unlist(fam_records), "0 TRLR")
  lines[1] <- paste0("\ufeff", lines[1])  # 5.5.5 requires a UTF-8 BOM

  con <- file(out_path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(lines), con, sep = "\r\n", useBytes = TRUE)

  if (merge_duplicates && !is.null(qa_path) && nrow(idm$qa_unmatched)) {
    utils::write.csv(idm$qa_unmatched, qa_path, row.names = FALSE)
  }

  cli_progress_step("Done")
  
  invisible(list(
    n_indi = length(indi_records),
    n_fam = length(fam_records),
    n_merged_clusters = sum(map_int(merged_persons, ~ length(.x$merged_from %||% character(0))) > 0),
    qa_unmatched = idm$qa_unmatched,
    path = out_path
  ))
}

convert_to_gedcom()
