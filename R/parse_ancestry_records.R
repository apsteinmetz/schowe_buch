# =============================================================================
# Schowe Ancestry Records Parser
# =============================================================================
# Parses German church ancestry records from persons.txt into structured data
# =============================================================================

library(tidyverse)
library(stringi)

# =============================================================================
# LOAD REFERENCE DATA
# =============================================================================

#' Load occupation list from CSV file
#' @return Character vector of German occupation names
load_occupations <- function() {
  occ_file <- "data/occupations.csv"
  if (file.exists(occ_file)) {
    occ_data <- read_csv(occ_file, show_col_types = FALSE)
    return(occ_data$occupation)
  } else {
    # Fallback to hardcoded list if CSV not found
    warning("Occupations CSV not found, using fallback list")
    return(c(
      "Bäcker",
      "Böttcher",
      "Buchbinder",
      "Drechsler",
      "Fleischer",
      "Fischer",
      "Gastwirt",
      "Gärtner",
      "Gerber",
      "Glaser",
      "Handwerker",
      "Kaufmann",
      "Knecht",
      "Krämer",
      "Küfer",
      "Küfner",
      "Küster",
      "Landwirt",
      "Lehrer",
      "Maler",
      "Maurer",
      "Metzger",
      "Müller",
      "Notar",
      "Pfarrer",
      "Richter",
      "Sattler",
      "Schäfer",
      "Schlosser",
      "Schmied",
      "Schmiedemeister",
      "Schneider",
      "Schreiner",
      "Schulze",
      "Schuhmacher",
      "Schuster",
      "Seiler",
      "Stellmacher",
      "Tagelöhner",
      "Taglöhner",
      "Tischler",
      "Uhrmacher",
      "Wagner",
      "Weber",
      "Wirt",
      "Zimmermann"
    ))
  }
}

# Load occupations once at package/script load time
OCCUPATIONS <- load_occupations()

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Convert German date format to ISO format
#' @param date_str Date string in German format (dd.mm.yyyy or partial)
#' @return Date string in ISO format (yyyy-mm-dd or partial)
convert_date <- function(date_str) {
  if (is.na(date_str) || date_str == "") {
    return(NA_character_)
  }

  # Handle "ABT", "BEF", "BET" prefixes
  prefix <- ""
  if (str_detect(date_str, "^(ABT|BEF|BET)\\s*")) {
    prefix <- str_extract(date_str, "^(ABT|BEF|BET)\\s*")
    date_str <- str_remove(date_str, "^(ABT|BEF|BET)\\s*")
  }

  # Full date: dd.mm.yyyy
  if (str_detect(date_str, "^\\d{2}\\.\\d{2}\\.\\d{4}$")) {
    parts <- str_split(date_str, "\\.")[[1]]
    return(paste0(prefix, parts[3], "-", parts[2], "-", parts[1]))
  }

  # Month and year only: .mm.yyyy or mm.yyyy
  if (str_detect(date_str, "^\\.?\\d{2}\\.\\d{4}$")) {
    date_str <- str_remove(date_str, "^\\.")
    parts <- str_split(date_str, "\\.")[[1]]
    return(paste0(prefix, parts[2], "-", parts[1]))
  }

  # Year only
  if (str_detect(date_str, "^\\d{4}$")) {
    return(paste0(prefix, date_str))
  }

  # Month name and year (e.g., "Juni 1852")
  months_de <- c(
    "Januar" = "01",
    "Februar" = "02",
    "März" = "03",
    "April" = "04",
    "Mai" = "05",
    "Juni" = "06",
    "Juli" = "07",
    "August" = "08",
    "September" = "09",
    "Oktober" = "10",
    "November" = "11",
    "Dezember" = "12",
    "Jan" = "01",
    "Feb" = "02",
    "Mar" = "03",
    "Apr" = "04",
    "Jun" = "06",
    "Jul" = "07",
    "Aug" = "08",
    "Sep" = "09",
    "Oct" = "10",
    "Nov" = "11",
    "Dec" = "12"
  )

  for (month_name in names(months_de)) {
    if (str_detect(date_str, month_name)) {
      year <- str_extract(date_str, "\\d{4}")
      if (!is.na(year)) {
        return(paste0(prefix, year, "-", months_de[month_name]))
      }
    }
  }

  # Return as-is if can't parse
  paste0(prefix, date_str)
}

#' Apply text substitutions for tags and abbreviations
#' @param text_vec Character vector of text lines
#' @return Modified character vector
apply_substitutions <- function(text_vec) {

  # ==========================================================================
  # EARLY PLACE TOKEN SUBSTITUTIONS
  # Handle unambiguous place patterns first to simplify later parsing
  # ==========================================================================


  # "Kis Ker" is always a place (300+ occurrences)
  text_vec <- str_replace_all(text_vec, "\\bKis Ker\\b", "PLAC_KISKER")

  # US state pattern: "Word, OH" -> "PLAC_OH_Word"

  # Any word followed by ", OH" is a place (1000+ occurrences)
  # This handles: Cleveland, OH | Cuyahoga, OH | Lorain, OH etc.
  text_vec <- str_replace_all(
    text_vec,
    "([A-ZÄÖÜa-zäöü]+),\\s*OH\\b",
    "PLAC_OH_\\1"
  )

  # ==========================================================================
  # EVENT TAGS
  # ==========================================================================

  # Event tags - be careful with * as it's common
  # Allow optional space around event markers (but not newlines!)
  text_vec <- str_replace_all(text_vec, "(?<![a-zA-Z])\\*", " BIRT ")
  text_vec <- str_replace_all(text_vec, "(?<=\\d)\\.oo[ \\t]*", " NMARR ") # numbered marriage like 2.oo, 3.oo
  text_vec <- str_replace_all(text_vec, "(?<=\\d)\\.o‐o", " NMARR_DIV ") # numbered divorced/informal like 2.o-o
  text_vec <- str_replace_all(text_vec, "(?<![0-9.])oo[ \\t]*", " MARR ") # regular marriage (allow no space after, but not newline)
  text_vec <- str_replace_all(text_vec, "o‐o", " MARR_DIV ") # divorced/informal marriage
  text_vec <- str_replace_all(text_vec, "~", " BAPM ")
  text_vec <- str_replace_all(text_vec, "†", " DEAT ")
  # Burial: "b." followed by date (digits). Must have space or † before, allows optional space after (not newline)
  text_vec <- str_replace_all(
    text_vec,
    "(?<=\\s|†)b\\.[ \\t]*(?=\\d)",
    " BURI "
  )
  text_vec <- str_replace_all(text_vec, "^# ", "NOTE ")
  text_vec <- str_replace_all(text_vec, "\n# ", "\nNOTE ")

  # Date modifiers
  text_vec <- str_replace_all(text_vec, "\\bum ", " ABT ")
  text_vec <- str_replace_all(text_vec, "\\bvor ", " BEF ")
  text_vec <- str_replace_all(text_vec, "\\bzw\\.", " BET ")

  # Place abbreviations - need word boundaries
  text_vec <- str_replace_all(text_vec, " NS(?=\\s|$|,|\\n)", " PLAC_NS ")
  text_vec <- str_replace_all(text_vec, " AS(?=\\s|$|,|\\n)", " PLAC_AS ")
  text_vec <- str_replace_all(text_vec, "\\bWoNS\\b", " RESI_NS ")
  text_vec <- str_replace_all(text_vec, "\\bWoAS\\b", " RESI_AS ")

  # Religion
  text_vec <- str_replace_all(text_vec, " ev\\.", " RELI_EV ")
  text_vec <- str_replace_all(text_vec, " ref\\.", " RELI_REF ")
  text_vec <- str_replace_all(text_vec, " kath\\.", " RELI_CATH ")

  # Witnesses and godparents - allow at start of line or after space
  text_vec <- str_replace_all(text_vec, "(^|\\s)TZ:", "\\1WITN ")
  text_vec <- str_replace_all(text_vec, "(^|\\s)TP:", "\\1GODP ")

  # Unknown name
  text_vec <- str_replace_all(text_vec, "\\bNN\\.", " UNKNOWN ")

  # Clean up multiple spaces within lines but preserve newlines
  lines <- str_split(text_vec, "\n")[[1]]
  lines <- map_chr(lines, str_squish)
  paste(lines, collapse = "\n")
}

#' Extract age at death from string like "(†mit 55J)" or "(†mit 2M6T)"
#' @param text Text containing age at death
#' @return List with years, months, days
extract_age_at_death <- function(text) {
  if (is.na(text)) {
    return(list(years = NA, months = NA, days = NA))
  }

  age_match <- str_extract(text, "\\(†mit ([^)]+)\\)")
  if (is.na(age_match)) {
    return(list(years = NA, months = NA, days = NA))
  }

  age_str <- str_extract(age_match, "(?<=\\(†mit )[^)]+")

  years <- as.numeric(str_extract(age_str, "\\d+(?=J)"))
  months <- as.numeric(str_extract(age_str, "\\d+(?=M)"))
  days <- as.numeric(str_extract(age_str, "\\d+(?=T)"))

  # Handle fractional years like "99 ½ J"
  if (is.na(years) && str_detect(age_str, "½")) {
    base_years <- as.numeric(str_extract(age_str, "\\d+(?=\\s*½)"))
    if (!is.na(base_years)) {
      years <- base_years
      months <- 6
    }
  }

  list(years = years, months = months, days = days)
}

#' Extract cross-reference from text
#' @param text Text containing reference like "> 1234" or "< 1234.5"
#' @return List with type (forward/back) and reference number
extract_cross_ref <- function(text) {
  if (is.na(text)) {
    return(list(type = NA, ref = NA))
  }

  # Forward reference
  fwd <- str_extract(text, ">\\s*[\\d.]+")
  if (!is.na(fwd)) {
    ref <- str_extract(fwd, "[\\d.]+")
    return(list(type = "forward", ref = ref))
  }

  # Back reference
  back <- str_extract(text, "<\\s*[\\d.]+")
  if (!is.na(back)) {
    ref <- str_extract(back, "[\\d.]+")
    return(list(type = "back", ref = ref))
  }

  list(type = NA, ref = NA)
}

#' Parse a name line (SURNAME Given_name [occupation] [religion])
#' @param line Name line text
#' @return List with surname, given_name, occupation, religion
parse_name_line <- function(line) {
  # Remove cross-references for parsing
  line_clean <- str_remove_all(line, "[<>]\\s*[\\d.]+")

  # Extract religion if present
  religion <- NA_character_
  if (str_detect(line_clean, "RELI_EV")) {
    religion <- "Evangelical"
    line_clean <- str_remove(line_clean, "\\s*RELI_EV\\s*")
  } else if (str_detect(line_clean, "RELI_REF")) {
    religion <- "Reformed"
    line_clean <- str_remove(line_clean, "\\s*RELI_REF\\s*")
  } else if (str_detect(line_clean, "RELI_CATH")) {
    religion <- "Catholic"
    line_clean <- str_remove(line_clean, "\\s*RELI_CATH\\s*")
  }

  # Extract occupation - check with word boundary first, then handle concatenated cases
  occupation <- NA_character_

  # Sort occupations by length (longest first) to avoid partial matches
  sorted_occupations <- OCCUPATIONS[order(-nchar(OCCUPATIONS))]

  for (occ in sorted_occupations) {
    # First try with word boundary (e.g., "Stefan Landwirt" or "Stefan, Landwirt")
    if (str_detect(line_clean, paste0("[,\\s]", occ, "(?=\\s|$|,)"))) {
      occupation <- occ
      line_clean <- str_remove(line_clean, paste0(",?\\s*", occ))
      break
    }
    # Then try concatenated case (e.g., "StefanLandwirt" - occupation at end of a word)
    # Match occupation at end of string or followed by whitespace
    if (str_detect(line_clean, paste0("(?<=[a-zäöüß])", occ, "(?=\\s|$)"))) {
      occupation <- occ
      line_clean <- str_remove(line_clean, paste0("(?<=[a-zäöüß])", occ))
      break
    }
  }

  # Extract surname (uppercase) and given name
  # Pattern: SURNAME Given_name(s)
  # Note: surname may contain lowercase umlauts due to OCR quirks (e.g., "GRößER", "FUSSGäNGER")
  name_match <- str_match(
    line_clean,
    "^([A-ZÄÖÜ][A-ZÄÖÜßäöü\\.]+)\\s+(.+?)\\s*$"
  )

  if (!is.na(name_match[1, 1])) {
    surname <- name_match[1, 2]
    given_name <- str_squish(name_match[1, 3])
    # Remove any remaining tags from given name
    given_name <- str_remove_all(
      given_name,
      "\\s*(BIRT|DEAT|BAPM|MARR|BURI|PLAC_NS|PLAC_AS|RESI_NS|RESI_AS|ABT|BEF|BET|NOTE|WITN|GODP|UNKNOWN).*$"
    )
    given_name <- str_squish(given_name)
  } else {
    surname <- NA_character_
    given_name <- str_squish(line_clean)
  }

  # Handle UNKNOWN surname
  if (!is.na(surname) && surname == "UNKNOWN") {
    surname <- NA_character_
  }

  list(
    surname = surname,
    given_name = given_name,
    occupation = occupation,
    religion = religion
  )
}

#' Decode place tokens back to readable place names
#' @param place Place string possibly containing tokens
#' @return Decoded place name
decode_place_token <- function(place) {
  if (is.na(place) || place == "") {
    return(NA_character_)
  }

  # Handle PLAC_KISKER
  if (place == "PLAC_KISKER") {
    return("Kis Ker")
  }

  # Handle PLAC_OH_* tokens (e.g., PLAC_OH_Cleveland -> Cleveland, OH)
  if (str_detect(place, "^PLAC_OH_")) {
    city <- str_remove(place, "^PLAC_OH_")
    return(paste0(city, ", OH"))
  }

  # Handle standard place abbreviations
  if (place == "PLAC_NS") {
    return("Neu Schowe")
  }
  if (place == "PLAC_AS") {
    return("Alt Schowe")
  }

  # Return as-is if no token found
  place
}

#' Extract place from a text section, handling place tokens
#' @param section Text section to extract place from
#' @param after_date If TRUE, look for place after a date pattern
#' @return Decoded place name or NA
extract_place <- function(section, after_date = TRUE) {
  if (is.na(section)) {
    return(NA_character_)
  }


  # First check for place tokens (most reliable)
  place_token <- str_extract(section, "PLAC_(NS|AS|KISKER|OH_[A-ZÄÖÜa-zäöü]+)")
  if (!is.na(place_token)) {
    return(decode_place_token(place_token))
  }

  # Fall back to pattern matching for non-tokenized places
  if (after_date) {
    # Place after a year
    place <- str_extract(
      section,
      "(?<=\\d{4}\\s)[A-ZÄÖÜa-zäöü/,\\-\\s]+(?=\\s+BURI|\\s+BAPM|\\s+\\(|\\s*$)"
    )
    if (!is.na(place)) {
      return(str_squish(place))
    }
  }

  NA_character_
}

#' Extract events from a text line
#' @param line Text line with event tags
#' @return List of events with type, date, place
extract_events <- function(line) {
  events <- list()

  # Birth - extract section from BIRT up to next event tag
  if (str_detect(line, "BIRT")) {
    birth_section <- str_extract(
      line,
      "BIRT\\s+.*?(?=\\s+DEAT|\\s+BAPM|\\s+MARR|\\s+BURI|\\s+NOTE|\\s+WITN|\\s+GODP|$)"
    )
    if (!is.na(birth_section)) {
      date_raw <- str_extract(
        birth_section,
        "(ABT\\s+)?\\d{1,2}\\.\\d{2}\\.\\d{4}|(ABT\\s+)?\\d{4}|(ABT\\s+)?\\.\\d{2}\\.\\d{4}|[A-Za-zä]+\\s+\\d{4}"
      )
      place <- extract_place(birth_section, after_date = TRUE)
      events$birth <- list(
        date = convert_date(str_squish(date_raw %||% "")),
        place = place
      )
    }
  }

  # Death - extract section from DEAT up to next event tag or end
  if (str_detect(line, "DEAT")) {
    death_section <- str_extract(
      line,
      "DEAT\\s+.*?(?=\\s+BAPM|\\s+BURI|\\s+MARR|\\s+NOTE|$)"
    )
    if (!is.na(death_section)) {
      date_raw <- str_extract(
        death_section,
        "(ABT\\s+|BEF\\s+)?\\d{1,2}\\.\\d{2}\\.\\d{4}|(ABT\\s+|BEF\\s+)?\\d{4}"
      )

      # Extract place - check for tokens first, then patterns
      place_token <- str_extract(death_section, "PLAC_(NS|AS|KISKER|OH_[A-ZÄÖÜa-zäöü]+)")
      if (!is.na(place_token)) {
        place <- decode_place_token(place_token)
      } else {
        # Place after a date (year)
        place <- str_extract(
          death_section,
          "(?<=\\d{4}\\s)[A-ZÄÖÜa-zäöü/,\\-\\s]+(?=\\s+BURI|\\s+\\(|$)"
        )
        if (is.na(place) && is.na(date_raw)) {
          # Place with no date - extract text after DEAT that looks like a place name
          place <- str_extract(
            death_section,
            "(?<=DEAT\\s)[A-ZÄÖÜa-zäöü][A-ZÄÖÜa-zäöü/,\\-\\s]*(?=\\s*$)"
          )
        }
        if (!is.na(place)) {
          place <- str_squish(place)
        }
      }

      age <- extract_age_at_death(death_section)
      events$death <- list(
        date = convert_date(str_squish(date_raw %||% "")),
        place = place,
        age_years = age$years,
        age_months = age$months,
        age_days = age$days
      )
    }
  }
          death_section,
          "(?<=\\d{4}\\s)[A-ZÄÖÜa-zäöü/,\\-\\s]+(?=\\s+BURI|\\s+\\(|$)"
        ),
        # Place with no date - extract text after DEAT that looks like a place name
        is.na(date_raw) ~ str_extract(
          death_section,
          "(?<=DEAT\\s)[A-ZÄÖÜa-zäöü][A-ZÄÖÜa-zäöü/,\\-\\s]*(?=\\s*$)"
        ),
        TRUE ~ NA_character_
      )
      age <- extract_age_at_death(death_section)
      events$death <- list(
        date = convert_date(str_squish(date_raw %||% "")),
        place = str_squish(place),
        age_years = age$years,
        age_months = age$months,
        age_days = age$days
      )
    }
  }

  # Burial
  if (str_detect(line, "BURI")) {
    buri_section <- str_extract(line, "BURI\\s+[^BAPM|MARR|NOTE]*")
    if (!is.na(buri_section)) {
      date_raw <- str_extract(buri_section, "\\d{1,2}\\.\\d{2}\\.\\d{4}")
      place <- str_extract(buri_section, "PLAC_(NS|AS)")
      place <- case_when(
        place == "PLAC_NS" ~ "Neu Schowe",
        place == "PLAC_AS" ~ "Alt Schowe",
        TRUE ~ NA_character_
      )
      events$burial <- list(
        date = convert_date(str_squish(date_raw %||% "")),
        place = place
      )
    }
  }

  # Baptism
  if (str_detect(line, "BAPM")) {
    bapm_section <- str_extract(line, "BAPM\\s+[^DEAT|MARR|BURI|NOTE]*")
    if (!is.na(bapm_section)) {
      date_raw <- str_extract(
        bapm_section,
        "\\d{1,2}\\.\\d{2}\\.\\d{4}|\\d{1,2}\\.\\d{1,2}\\.\\d{4}"
      )
      godparents <- str_extract(bapm_section, "(?<=GODP\\s).+$")
      events$baptism <- list(
        date = convert_date(str_squish(date_raw %||% "")),
        godparents = str_squish(godparents)
      )
    }
  }

  events
}

# =============================================================================
# MAIN PARSING FUNCTIONS
# =============================================================================

#' Split file into individual records
#' @param file_path Path to persons.txt
#' @return List of record text blocks
split_into_records <- function(file_path) {
  # Read entire file
  text <- read_file(file_path)

  # Split on record markers <nnn> that appear at the start of a line
  # This avoids splitting on inline references like (Doppelhochzeit <3861>)
  records <- str_split(text, "(?=(\r?\n|^)<\\d+>)")[[1]]

  # Remove empty first element if exists
  records <- records[records != ""]

  # Extract record numbers and content
  record_list <- map(records, function(rec) {
    # Get record number
    num <- str_extract(rec, "(?<=<)\\d+(?=>)")
    if (is.na(num)) {
      return(NULL)
    }

    # Get content after the <nnn> tag
    content <- str_remove(rec, "^<\\d+>\\s*")

    list(
      record_id = as.integer(num),
      raw_text = content
    )
  })

  # Remove NULLs
  compact(record_list)
}

#' Parse a single record into structured data
#' @param record List with record_id and raw_text
#' @return List with parsed record data, or NULL for empty records
parse_record <- function(record) {
  record_id <- record$record_id
  text <- record$raw_text

  # Check for empty record
  if (str_squish(text) == "") {
    return(NULL)
  }

  # Apply substitutions
  text <- apply_substitutions(text)

  # Split into lines and remove empty lines and family headers
  lines <- str_split(text, "\n")[[1]]
  lines <- lines[str_squish(lines) != ""]
  # Remove family header lines (all uppercase, alone on line) but NOT MARR/MARR_DIV tags
  lines <- lines[!str_detect(lines, "^[A-ZÄÖÜ]+$") | str_detect(lines, "^MARR")]

  if (length(lines) == 0) {
    return(list(
      record_id = record_id,
      status = "empty",
      primary = NULL,
      spouses = list(),
      children = list()
    ))
  }

  # Initialize result
  result <- list(
    record_id = record_id,
    status = "parsed",
    primary = list(),
    spouses = list(),
    children = list(),
    notes = character()
  )

  # State tracking
  current_context <- "primary"
  current_spouse_num <- 0
  current_child <- NULL
  pending_child_marriage_lines <- character()

  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]

    # Handle notes
    if (str_detect(line, "^NOTE\\s")) {
      result$notes <- c(result$notes, str_remove(line, "^NOTE\\s+"))
      i <- i + 1
      next
    }

    # Handle Eltern: line
    if (str_detect(line, "^Eltern:")) {
      parents <- str_remove(line, "^Eltern:\\s*")
      parent_parts <- str_split(parents, ",\\s*")[[1]]

      if (current_context == "spouse" && current_spouse_num > 0) {
        result$spouses[[current_spouse_num]]$father <- str_squish(parent_parts[
          1
        ])
        if (length(parent_parts) > 1) {
          result$spouses[[
            current_spouse_num
          ]]$mother <- str_squish(parent_parts[2])
        }
      } else if (current_context == "primary") {
        result$primary$father <- str_squish(parent_parts[1])
        if (length(parent_parts) > 1) {
          result$primary$mother <- str_squish(parent_parts[2])
        }
      }
      i <- i + 1
      next
    }

    # Handle child line (starts with digit followed by period)
    # Allow optional space after period (e.g., "6. Name" or "6.Name")
    # Exclude lines that are numbered spouse markers (e.g., "2. MARR_DIV" or "2. MARR")
    if (
      str_detect(line, "^\\d+\\.\\s?[A-ZÄÖÜa-zäöü]") &&
        !str_detect(line, "^\\d+\\.\\s*MARR")
    ) {
      # Save previous child if exists
      if (!is.null(current_child)) {
        result$children[[length(result$children) + 1]] <- current_child
      }

      current_context <- "child"
      child_num <- as.integer(str_extract(line, "^\\d+"))
      child_line <- str_remove(line, "^\\d+\\.\\s*")

      # Parse child basic info
      current_child <- parse_child_line(child_line, child_num)

      i <- i + 1
      next
    }

    # Handle numbered marriage line for child (like "2.oo" or "3.oo" starting a line)
    # After substitution, these become "2 NMARR" or "3 NMARR"
    # Only match if we have a current child being built AND:
    # - No witnesses on this line (if there are witnesses TZ/WITN, it's the primary's remarriage)
    # - The next line is NOT an uppercase surname line (which would indicate spouse name for primary)
    if (
      !is.null(current_child) &&
        (str_detect(line, "^\\d+\\s*\\.\\s*MARR") ||
          str_detect(line, "^\\d+\\s*NMARR")) &&
        !str_detect(line, "WITN|TZ:")
    ) {
      # Peek at next line - if it's an uppercase surname line, this is primary's remarriage
      next_line <- if (i + 1 <= length(lines)) lines[i + 1] else ""
      is_spouse_name_next <- str_detect(
        next_line,
        "^[A-ZÄÖÜ][A-ZÄÖÜß\\.]*\\s+[A-ZÄÖÜa-zäöüß]"
      ) &&
        !str_detect(
          next_line,
          "^(BIRT|DEAT|BAPM|BURI|NOTE|MARR|WITN|GODP|PLAC|\\d)"
        )

      if (!is_spouse_name_next) {
        # Child's numbered marriage (spouse name on same line)
        marr_info <- parse_child_marriage_line(line)
        if (is.null(current_child$marriages)) {
          current_child$marriages <- list()
        }
        current_child$marriages[[
          length(current_child$marriages) + 1
        ]] <- marr_info
        i <- i + 1
        next
      }
      # If spouse name is on next line, fall through to primary remarriage handler
    }

    # Handle standalone marriage line after child (no number prefix)
    if (
      current_context == "child" &&
        str_detect(line, "^MARR\\s") &&
        !is.null(current_child)
    ) {
      marr_info <- parse_child_marriage_line(line)
      if (is.null(current_child$marriages)) {
        current_child$marriages <- list()
      }
      current_child$marriages[[
        length(current_child$marriages) + 1
      ]] <- marr_info
      i <- i + 1
      next
    }

    # Handle primary/spouse marriage line (standalone MARR or MARR_DIV or MARR Place)
    # Matches: "MARR", "MARR_DIV", "MARR Neu Pasua" (place but no date)
    # Does NOT match: "MARR 12.11.1889" (has date - handled below)
    if (
      (str_detect(line, "^\\s*MARR\\s*$") ||
        str_detect(line, "^\\s*MARR_DIV\\s*$") ||
        (str_detect(line, "^\\s*MARR\\s+[A-Za-zÄÖÜäöüß]") &&
          !str_detect(line, "^\\s*MARR\\s+(\\d|ABT|BEF|BET)")))
    ) {
      current_spouse_num <- current_spouse_num + 1
      current_context <- "spouse"

      marr_type <- ifelse(
        str_detect(line, "MARR_DIV"),
        "divorced/informal",
        "marriage"
      )

      # Extract place if present (MARR Place format)
      marr_place <- NA_character_
      if (str_detect(line, "^\\s*MARR\\s+[A-Za-zÄÖÜäöüß]")) {
        marr_place <- str_extract(line, "(?<=MARR\\s)[A-Za-zÄÖÜäöüß\\s]+")
        marr_place <- str_squish(marr_place)
      }

      result$spouses[[current_spouse_num]] <- list(
        spouse_num = current_spouse_num,
        marriage_type = marr_type,
        marriage_date = NA_character_,
        marriage_place = marr_place
      )

      i <- i + 1
      next
    }

    # Handle numbered remarriage line for primary person (like "2.oo" or "3.oo" or "2.o-o")
    # After substitution, these become "2 NMARR" or "3 NMARR" or "2 NMARR_DIV"
    # This can appear after children from a previous marriage, so we allow it
    # even when current_context == "child" (it switches context back to spouse)
    if (str_detect(line, "^\\d+\\s*NMARR(_DIV)?")) {
      # Save any pending child before switching to spouse context
      if (!is.null(current_child)) {
        result$children[[length(result$children) + 1]] <- current_child
        current_child <- NULL
      }

      marr_num <- as.integer(str_extract(line, "^\\d+"))
      current_spouse_num <- marr_num
      current_context <- "spouse"

      # Determine marriage type based on whether it's NMARR_DIV
      marr_type <- ifelse(
        str_detect(line, "NMARR_DIV"),
        "divorced/informal",
        "marriage"
      )

      marr_info <- parse_marriage_line(line)
      result$spouses[[current_spouse_num]] <- c(
        list(spouse_num = current_spouse_num, marriage_type = marr_type),
        marr_info
      )

      i <- i + 1
      next
    }

    # Handle marriage line with date/place/witnesses (primary's marriage)
    # Match MARR followed by date (with optional ABT/BEF/BET prefix)
    if (
      str_detect(line, "^MARR\\s+(\\d|ABT|BEF|BET)") &&
        current_context != "child"
    ) {
      current_spouse_num <- current_spouse_num + 1
      current_context <- "spouse"

      marr_info <- parse_marriage_line(line)
      result$spouses[[current_spouse_num]] <- c(
        list(spouse_num = current_spouse_num, marriage_type = "marriage"),
        marr_info
      )

      i <- i + 1
      next
    }

    # Handle spouse name line (after MARR) - exclude event tag lines
    # Note: surname may contain lowercase umlauts due to OCR quirks (e.g., "GRößER", "FUSSGäNGER")
    if (
      current_context == "spouse" &&
        str_detect(line, "^[A-ZÄÖÜ][A-ZÄÖÜßäöü\\.]*\\s+[A-ZÄÖÜa-zäöüß]") &&
        !str_detect(line, "^(BIRT|DEAT|BAPM|BURI|NOTE|MARR|WITN|GODP|PLAC)")
    ) {
      spouse_info <- parse_person_line(line)

      # Extract cross-reference
      ref <- extract_cross_ref(line)
      if (!is.na(ref$type)) {
        spouse_info$ref_type <- ref$type
        spouse_info$ref <- ref$ref
      }

      # Merge with existing spouse data
      if (
        current_spouse_num > 0 && current_spouse_num <= length(result$spouses)
      ) {
        result$spouses[[current_spouse_num]] <- c(
          result$spouses[[current_spouse_num]],
          spouse_info
        )
      }

      i <- i + 1
      next
    }

    # Handle spouse remarriage line "[Name] oo wieder..."
    if (str_detect(line, "MARR wieder")) {
      remarriage_info <- parse_remarriage_line(line)
      if (current_spouse_num > 0) {
        result$spouses[[current_spouse_num]]$remarriage <- remarriage_info
      }
      i <- i + 1
      next
    }

    # Handle primary person line (first line with uppercase surname)
    # Note: surname may contain lowercase umlauts due to OCR quirks (e.g., "GRößER", "FUSSGäNGER")
    if (
      current_context == "primary" &&
        is.null(result$primary$surname) &&
        str_detect(line, "^[A-ZÄÖÜ][A-ZÄÖÜßäöü\\.]*\\s+[A-ZÄÖÜa-zäöüß]")
    ) {
      primary_info <- parse_person_line(line)

      # Extract cross-reference (back reference for primary)
      ref <- extract_cross_ref(line)
      if (!is.na(ref$type)) {
        primary_info$ref_type <- ref$type
        primary_info$ref <- ref$ref
      }

      # Check for origin "aus [place]"
      origin <- str_extract(line, "(?<=\\baus\\s)[A-ZÄÖÜa-zäöü\\s]+(?=\\s|$)")
      if (!is.na(origin)) {
        primary_info$origin <- str_squish(origin)
      }

      result$primary <- primary_info
      i <- i + 1
      next
    }

    # Handle additional event lines for primary person
    if (
      current_context == "primary" &&
        !is.null(result$primary$surname) &&
        str_detect(line, "(BIRT|DEAT|BAPM|BURI)")
    ) {
      events <- extract_events(line)
      for (evt_name in names(events)) {
        result$primary[[evt_name]] <- events[[evt_name]]
      }

      # Also check for cross-reference on event line
      ref <- extract_cross_ref(line)
      if (!is.na(ref$type) && is.null(result$primary$ref)) {
        result$primary$ref_type <- ref$type
        result$primary$ref <- ref$ref
      }

      i <- i + 1
      next
    }

    # Handle additional event lines for spouse
    if (
      current_context == "spouse" &&
        current_spouse_num > 0 &&
        str_detect(line, "(BIRT|DEAT|BAPM|BURI)")
    ) {
      events <- extract_events(line)
      for (evt_name in names(events)) {
        result$spouses[[current_spouse_num]][[evt_name]] <- events[[evt_name]]
      }
      i <- i + 1
      next
    }

    # Handle standalone godparent line (GODP on its own line after child entry)
    if (str_detect(line, "^GODP\\s") && !is.null(current_child)) {
      godparents <- str_remove(line, "^GODP\\s+")
      if (is.null(current_child$baptism)) {
        current_child$baptism <- list(
          date = NA_character_,
          godparents = godparents
        )
      } else {
        current_child$baptism$godparents <- godparents
      }
      i <- i + 1
      next
    }

    # Check for untagged informational lines that should be captured as notes
    # Lines like "Wohnort in Schowe: ...", "Wohnort der Familie: ...", etc.
    # These are lines that don't start with recognized tags or patterns
    # AND contain recognizable note keywords (Wohnort, letzter, etc.)
    if (
      !str_detect(
        line,
        "^(BIRT|DEAT|BAPM|BURI|NOTE|MARR|NMARR|MARR_DIV|WITN|GODP|PLAC|RESI|Eltern:|<\\d+>|[A-ZÄÖÜ][A-ZÄÖÜßäöü\\.]*\\s+[A-ZÄÖÜa-zäöüß]|\\d+\\.\\s?[A-ZÄÖÜa-zäöü]|\\d+\\s*NMARR)"
      ) &&
        str_squish(line) != "" &&
        !str_detect(line, "^\\d+\\s*$") &&
        # Must contain note-like keywords to be treated as a note (not just names/continuation)
        (str_detect(
          line,
          "(Wohnort|letzter|bekannter|Familie|Adresse|Beruf|Anmerkung|siehe|Haus|Gasse|Straße)"
        ) ||
          str_detect(line, "^[a-zäöü]"))
    ) {
      # Also capture lowercase continuation lines
      # This is an untagged informational line - add as note
      result$notes <- c(result$notes, str_squish(line))
      i <- i + 1
      next
    }

    # Default: move to next line
    i <- i + 1
  }

  # Save final child
  if (!is.null(current_child)) {
    result$children[[length(result$children) + 1]] <- current_child
  }

  result
}

#' Parse a child line
#' @param line Child line text (without the number prefix)
#' @param child_num Child number
#' @return List with child data
parse_child_line <- function(line, child_num) {
  result <- list(child_num = child_num)

  # Extract surname and given name
  # Pattern: Surname Given [religion] [events...]
  # Allow periods in given name for middle initials (e.g., "Peter F.")
  name_match <- str_match(
    line,
    "^([A-ZÄÖÜa-zäöüß]+)\\s+([A-ZÄÖÜa-zäöüß\\.\\s]+?)(?=\\s+RELI|\\s+BIRT|\\s+DEAT|\\s+MARR|\\s+BAPM|$)"
  )
  if (!is.na(name_match[1, 1])) {
    result$surname <- name_match[1, 2]
    result$given_name <- str_squish(name_match[1, 3])
  }

  # Extract religion
  if (str_detect(line, "RELI_EV")) {
    result$religion <- "Evangelical"
  } else if (str_detect(line, "RELI_REF")) {
    result$religion <- "Reformed"
  } else if (str_detect(line, "RELI_CATH")) {
    result$religion <- "Catholic"
  }

  # Extract events
  events <- extract_events(line)
  result <- c(result, events)

  # Check for single status
  if (str_detect(line, "\\bled\\.")) {
    result$marital_status <- "single"
  }

  # Extract cross-reference (on same line as child info)
  ref <- extract_cross_ref(line)
  if (!is.na(ref$type)) {
    result$ref_type <- ref$type
    result$ref <- ref$ref
  }

  # Check for marriage on same line
  if (str_detect(line, "MARR\\s+\\d")) {
    marr_info <- parse_child_marriage_line(line)
    result$marriages <- list(marr_info)
  }

  result
}

#' Parse a child marriage line
#' @param line Marriage line text
#' @return List with marriage details
parse_child_marriage_line <- function(line) {
  result <- list()

  # Check for numbered marriage (handles "1 NMARR", "2 NMARR", "3 NMARR" etc)
  # After substitution, "1.oo" becomes "1 NMARR", "2.oo" becomes "2 NMARR"
  marr_num <- str_extract(line, "^\\d+(?=\\s*\\.?\\s*N?MARR)")
  if (!is.na(marr_num)) {
    result$marriage_num <- as.integer(marr_num)
  } else {
    result$marriage_num <- 1L
  }

  # Extract date (including ABT/BEF/BET prefixes)
  # Date may appear anywhere in the line, not necessarily right after MARR tag
  # Format: dd.mm.yyyy (with optional prefix)
  date_match <- str_extract(
    line,
    "(ABT\\s+|BEF\\s+|BET\\s+)?\\d{2}\\.\\d{2}\\.\\d{4}"
  )
  result$marriage_date <- convert_date(date_match)

  # Extract place - check for PLAC_ tags first
  place <- str_extract(line, "PLAC_(NS|AS)")
  if (!is.na(place)) {
    result$marriage_place <- ifelse(
      place == "PLAC_NS",
      "Neu Schowe",
      "Alt Schowe"
    )
  } else {
    # Place can be in different positions:
    # Format 1: "1 NMARR Beschka 12.11.1889 Feth Konrad > 655" (place before date)
    # Format 2: "MARR 02.04.1953 Cuyahoga, OH Geyer Johanna > 31" (place after date)
    # Format 3: "1 NMARR Neu Pasua Kauder Wilhelm > 1812" (no date, place before name)
    after_marr <- str_remove(line, "^\\d*\\s*\\.?\\s*N?MARR(_DIV)?\\s*")

    if (!is.na(date_match)) {
      # Try to extract place before the date first
      place_text <- str_extract(
        after_marr,
        "^[^\\d]+(?=\\d{2}\\.\\d{2}\\.\\d{4})"
      )
      if (!is.na(place_text) && str_squish(place_text) != "") {
        result$marriage_place <- str_squish(place_text)
      } else {
        # Place is after the date - extract text between date and spouse name
        # Pattern: date place SpouseSurname SpouseGiven > ref
        # Place can include commas and state abbreviations (e.g., "Cuyahoga, OH")
        after_date <- str_remove(
          after_marr,
          "^(ABT\\s+|BEF\\s+|BET\\s+)?\\d{2}\\.\\d{2}\\.\\d{4}\\s*"
        )
        # Place ends where the spouse name begins - spouse surname starts with capital
        # and is followed by given name. Place may contain commas, state abbreviations.
        # Pattern: Place (may include ", ST") SpouseSurname SpouseGiven > ref
        # Look for the pattern: word(s) possibly with comma, then Surname Given > ref
        # The place ends at the last comma+space+2-letter-code before the name,
        # or at the transition from lowercase/abbreviation to uppercase surname
        place_name_match <- str_match(
          after_date,
          "^(.+?,\\s*[A-Z]{2})\\s+([A-ZÄÖÜa-zäöüß]+)\\s+([A-ZÄÖÜa-zäöüß]+)\\s*[><]"
        )
        if (!is.na(place_name_match[1, 1])) {
          result$marriage_place <- str_squish(place_name_match[1, 2])
        } else {
          # Try simpler pattern: place without state abbreviation
          place_name_match <- str_match(
            after_date,
            "^([A-ZÄÖÜa-zäöü,\\s]+?)\\s+([A-ZÄÖÜa-zäöüß]+)\\s+([A-ZÄÖÜa-zäöüß]+)\\s*[><]"
          )
          if (!is.na(place_name_match[1, 1])) {
            result$marriage_place <- str_squish(place_name_match[1, 2])
          }
        }
      }
    } else {
      # No date - place is everything before the spouse name (surname + given name before >)
      # Look for the name pattern at the end: Surname Given > ref
      place_match <- str_match(
        after_marr,
        "^(.*?)([A-ZÄÖÜa-zäöüß]+\\s+[A-ZÄÖÜa-zäöüß]+)\\s*>"
      )
      if (!is.na(place_match[1, 1])) {
        result$marriage_place <- str_squish(place_match[1, 2])
      }
    }
  }

  # Extract spouse name - look for name pattern before cross-ref
  # Format: Surname Given [religion] > ref
  # First clean the line: remove MARR prefix, place (if found), date, and PLAC_ tags
  clean_line <- str_remove(line, "^\\d*\\s*\\.?\\s*N?MARR(_DIV)?\\s*")

  # Remove place if it was found
  if (
    !is.null(result$marriage_place) &&
      !is.na(result$marriage_place) &&
      result$marriage_place != ""
  ) {
    clean_line <- str_remove(clean_line, fixed(result$marriage_place))
  }

  # Remove date
  clean_line <- str_remove(
    clean_line,
    "(ABT\\s+|BEF\\s+|BET\\s+)?\\d{2}\\.\\d{2}\\.\\d{4}\\s*"
  )
  clean_line <- str_remove(clean_line, "PLAC_(NS|AS)\\s*")
  clean_line <- str_squish(clean_line)

  # Now extract spouse name: Surname Given [religion] > ref
  name_match <- str_match(
    clean_line,
    "^([A-ZÄÖÜa-zäöüß]+)\\s+([A-ZÄÖÜa-zäöüß]+(?:\\s+[A-ZÄÖÜa-zäöüß]+)?)(?:\\s+(?:ref\\.?|ev\\.?|RELI_\\w+))?\\s*[><]"
  )
  if (!is.na(name_match[1, 1])) {
    result$spouse_name <- paste(name_match[1, 2], name_match[1, 3])
  } else {
    # Try without cross-ref (name at end of line)
    name_match <- str_match(
      clean_line,
      "^([A-ZÄÖÜa-zäöüß]+)\\s+([A-ZÄÖÜa-zäöüß]+(?:\\s+[A-ZÄÖÜa-zäöüß]+)?)(?:\\s+(?:ref\\.?|ev\\.?|RELI_\\w+))?\\s*$"
    )
    if (!is.na(name_match[1, 1])) {
      result$spouse_name <- paste(name_match[1, 2], name_match[1, 3])
    }
  }

  # Extract cross-reference
  ref <- extract_cross_ref(line)
  if (!is.na(ref$type)) {
    result$ref_type <- ref$type
    result$ref <- ref$ref
  }

  result
}

#' Parse a marriage line with date/place/witnesses
#' @param line Marriage line
#' @return List with marriage details
parse_marriage_line <- function(line) {
  result <- list()

  date_match <- str_extract(line, "\\d{2}\\.\\d{2}\\.\\d{4}")
  result$marriage_date <- convert_date(date_match)

  place <- str_extract(line, "PLAC_(NS|AS)")
  if (!is.na(place)) {
    result$marriage_place <- ifelse(
      place == "PLAC_NS",
      "Neu Schowe",
      "Alt Schowe"
    )
  } else {
    other_place <- str_extract(
      line,
      "(?<=\\d{4}\\s)[A-ZÄÖÜa-zäöü,/\\-\\s]+?(?=\\s+WITN|\\s*$)"
    )
    result$marriage_place <- str_squish(other_place)
  }

  witnesses <- str_extract(line, "(?<=WITN\\s).+$")
  result$witnesses <- str_squish(witnesses)

  result
}

#' Parse a remarriage line
#' @param line Remarriage line text
#' @return List with remarriage details
parse_remarriage_line <- function(line) {
  result <- list()

  date_match <- str_extract(line, "\\d{2}\\.\\d{2}\\.\\d{4}")
  result$remarriage_date <- convert_date(date_match)

  place <- str_extract(line, "PLAC_(NS|AS)")
  if (!is.na(place)) {
    result$remarriage_place <- ifelse(
      place == "PLAC_NS",
      "Neu Schowe",
      "Alt Schowe"
    )
  }

  # Extract the new spouse name (after "mit")
  new_spouse <- str_extract(
    line,
    "(?<=\\bmit\\s)[A-ZÄÖÜa-zäöüß\\s]+(?=\\s*>|$)"
  )
  result$remarriage_to <- str_squish(new_spouse)

  ref <- extract_cross_ref(line)
  if (!is.na(ref$type)) {
    result$remarriage_ref_type <- ref$type
    result$remarriage_ref <- ref$ref
  }

  result
}

#' Parse a person line for name and events
#' @param line Text line with person info
#' @return List with parsed person data
parse_person_line <- function(line) {
  # Parse name components
  name_info <- parse_name_line(line)

  # Extract events
  events <- extract_events(line)

  # Combine
  result <- c(name_info, events)

  # Check for residence
  if (str_detect(line, "RESI_NS")) {
    result$residence <- "Neu Schowe"
  } else if (str_detect(line, "RESI_AS")) {
    result$residence <- "Alt Schowe"
  }

  # Check for "led." (single/unmarried)
  if (str_detect(line, "\\bled\\.")) {
    result$marital_status <- "single"
  }

  result
}

# =============================================================================
# DATA FRAME CONVERSION
# =============================================================================

#' Convert parsed records to tidy data frames
#' @param parsed_records List of parsed records (may contain NULLs for empty records)
#' @return List of data frames (persons, marriages, children)
records_to_dataframes <- function(parsed_records) {
  # Remove NULL entries (empty records)
  parsed_records <- compact(parsed_records)

  # Primary persons table
  persons <- map_dfr(parsed_records, function(rec) {
    # Skip records with no surname parsed (empty records)
    if (is.null(rec$primary$surname)) {
      return(tibble())
    }

    p <- rec$primary
    tibble(
      record_id = rec$record_id,
      status = rec$status,
      surname = p$surname %||% NA_character_,
      given_name = p$given_name %||% NA_character_,
      occupation = p$occupation %||% NA_character_,
      religion = p$religion %||% NA_character_,
      birth_date = p$birth$date %||% NA_character_,
      birth_place = p$birth$place %||% NA_character_,
      death_date = p$death$date %||% NA_character_,
      death_place = p$death$place %||% NA_character_,
      death_age_years = p$death$age_years %||% NA_integer_,
      death_age_months = p$death$age_months %||% NA_integer_,
      death_age_days = p$death$age_days %||% NA_integer_,
      burial_date = p$burial$date %||% NA_character_,
      burial_place = p$burial$place %||% NA_character_,
      baptism_date = p$baptism$date %||% NA_character_,
      godparents = p$baptism$godparents %||% NA_character_,
      father = p$father %||% NA_character_,
      mother = p$mother %||% NA_character_,
      origin = p$origin %||% NA_character_,
      residence = p$residence %||% NA_character_,
      ref_type = p$ref_type %||% NA_character_,
      ref = p$ref %||% NA_character_,
      notes = paste(rec$notes, collapse = "; ")
    )
  })

  # Spouses table
  spouses <- map_dfr(parsed_records, function(rec) {
    if (length(rec$spouses) == 0) {
      return(tibble())
    }

    map_dfr(rec$spouses, function(sp) {
      tibble(
        record_id = rec$record_id,
        spouse_num = sp$spouse_num %||% NA_integer_,
        marriage_type = sp$marriage_type %||% NA_character_,
        marriage_date = sp$marriage_date %||% NA_character_,
        marriage_place = sp$marriage_place %||% NA_character_,
        witnesses = sp$witnesses %||% NA_character_,
        surname = sp$surname %||% NA_character_,
        given_name = sp$given_name %||% NA_character_,
        religion = sp$religion %||% NA_character_,
        birth_date = sp$birth$date %||% NA_character_,
        birth_place = sp$birth$place %||% NA_character_,
        death_date = sp$death$date %||% NA_character_,
        death_place = sp$death$place %||% NA_character_,
        burial_date = sp$burial$date %||% NA_character_,
        baptism_date = sp$baptism$date %||% NA_character_,
        godparents = sp$baptism$godparents %||% NA_character_,
        father = sp$father %||% NA_character_,
        mother = sp$mother %||% NA_character_,
        ref_type = sp$ref_type %||% NA_character_,
        ref = sp$ref %||% NA_character_,
        remarriage_note = sp$remarriage_note %||% NA_character_
      )
    })
  })

  # Children table
  children <- map_dfr(parsed_records, function(rec) {
    if (length(rec$children) == 0) {
      return(tibble())
    }

    map_dfr(rec$children, function(ch) {
      # Handle marriages
      marriages_df <- if (length(ch$marriages) > 0) {
        map_dfr(ch$marriages, function(m) {
          tibble(
            marriage_num = m$marriage_num %||% NA_integer_,
            marriage_date = m$marriage_date %||% NA_character_,
            marriage_place = m$marriage_place %||% NA_character_,
            spouse_name = m$spouse_name %||% NA_character_,
            marriage_ref_type = m$ref_type %||% NA_character_,
            marriage_ref = m$ref %||% NA_character_
          )
        })
      } else {
        tibble(
          marriage_num = NA_integer_,
          marriage_date = NA_character_,
          marriage_place = NA_character_,
          spouse_name = NA_character_,
          marriage_ref_type = NA_character_,
          marriage_ref = NA_character_
        )
      }

      # Base child info repeated for each marriage (or once if no marriages)
      # Build note based on given_name containing Totgeburt (stillborn)
      child_note <- NA_character_
      if (
        !is.null(ch$given_name) &&
          str_detect(
            ch$given_name %||% "",
            regex("Totgeburt", ignore_case = TRUE)
          )
      ) {
        child_note <- "Stillborn"
      }

      base_info <- tibble(
        record_id = rec$record_id,
        child_num = ch$child_num %||% NA_integer_,
        surname = ch$surname %||% NA_character_,
        given_name = ch$given_name %||% NA_character_,
        religion = ch$religion %||% NA_character_,
        birth_date = ch$birth$date %||% NA_character_,
        birth_place = ch$birth$place %||% NA_character_,
        death_date = ch$death$date %||% NA_character_,
        death_place = ch$death$place %||% NA_character_,
        death_age_years = ch$death$age_years %||% NA_integer_,
        death_age_months = ch$death$age_months %||% NA_integer_,
        death_age_days = ch$death$age_days %||% NA_integer_,
        burial_date = ch$burial$date %||% NA_character_,
        baptism_date = ch$baptism$date %||% NA_character_,
        godparents = ch$baptism$godparents %||% NA_character_,
        marital_status = ch$marital_status %||% NA_character_,
        ref_type = ch$ref_type %||% NA_character_,
        ref = ch$ref %||% NA_character_,
        note = child_note
      )

      bind_cols(
        base_info |> slice(rep(1, nrow(marriages_df))),
        marriages_df
      )
    })
  })

  list(
    persons = persons,
    spouses = spouses,
    children = children
  )
}

# =============================================================================
# RECONCILIATION
# =============================================================================

#' Parse a reference string into record_id and child_num
#' @param ref_str Reference string like "2310" or "9.1"
#' @return List with record_id and child_num (NA if not specified)
parse_ref <- function(ref_str) {
  if (is.na(ref_str) || ref_str == "") {
    return(list(record_id = NA_integer_, child_num = NA_integer_))
  }

  parts <- str_split(ref_str, "\\.")[[1]]
  record_id <- as.integer(parts[1])
  child_num <- if (length(parts) > 1) as.integer(parts[2]) else NA_integer_

  list(record_id = record_id, child_num = child_num)
}

#' Normalize name for comparison (lowercase, remove extra spaces)
#' @param name Name string
#' @return Normalized name
normalize_name <- function(name) {
  if (is.na(name) || name == "") {
    return("")
  }
  tolower(str_squish(name))
}

#' Check if two names match (either exact or one contains the other)
#' Names may contain extra words like occupation (Bauer) or place (Kis Ker)
#' @param name1 First name
#' @param name2 Second name
#' @return TRUE if names match
names_match <- function(name1, name2) {
  n1 <- normalize_name(name1)
  n2 <- normalize_name(name2)

  if (n1 == "" || n2 == "") {
    return(FALSE)
  }

  # Exact match
  if (n1 == n2) {
    return(TRUE)
  }

  # One contains the other (for cases like "Peter" vs "Peter Ludwig")
  if (str_detect(n1, fixed(n2)) || str_detect(n2, fixed(n1))) {
    return(TRUE)
  }

  # Check if given names match (last word of each)
  given1 <- str_extract(n1, "\\S+$")
  given2 <- str_extract(n2, "\\S+$")
  if (!is.na(given1) && !is.na(given2) && given1 == given2) {
    return(TRUE)
  }

  # Check if key name components overlap

  # Names may have extra words (occupation like "Bauer", place like "Kis Ker")
  # Consider a match if first 2 words of one name are both found in the other
  words1 <- str_split(n1, "\\s+")[[1]]
  words2 <- str_split(n2, "\\s+")[[1]]

  if (length(words1) >= 2 && length(words2) >= 2) {
    # Check if first 2 words of name1 are in name2
    key_words1 <- words1[1:2]
    if (all(key_words1 %in% words2)) {
      return(TRUE)
    }
    # Check if first 2 words of name2 are in name1
    key_words2 <- words2[1:2]
    if (all(key_words2 %in% words1)) {
      return(TRUE)
    }
  }

  FALSE
}

#' Reconcile cross-references using bidirectional verification
#' @param dataframes List of data frames from records_to_dataframes
#' @return List with reconciled data and conflict report
reconcile_references <- function(dataframes) {
  persons <- dataframes$persons
  spouses <- dataframes$spouses
  children <- dataframes$children

  conflicts <- tibble(
    source_table = character(),
    source_record_id = integer(),
    source_item = character(),
    ref_type = character(),
    ref = character(),
    issue = character(),
    details = character()
  )

  # Initialize assignment columns
  if (nrow(children) > 0) {
    children$ref_assignment <- NA_character_
    children$ref_verified <- FALSE
  }
  if (nrow(spouses) > 0) {
    spouses$ref_verified <- FALSE
  }

  # ==========================================================================
  # STEP 1: Process forward references on children's marriages
  # Forward ref on child's marriage (> XXXX) typically points to spouse's record
  # where spouse is the primary person
  # ==========================================================================

  if (nrow(children) > 0) {
    children_fwd_refs <- children |>
      filter(marriage_ref_type == "forward" & !is.na(marriage_ref))

    for (i in seq_len(nrow(children_fwd_refs))) {
      row <- children_fwd_refs[i, ]
      ref_info <- parse_ref(row$marriage_ref)

      if (is.na(ref_info$record_id)) {
        next
      }

      # The forward reference points to a record where we expect:
      # - The primary person is the spouse of this child
      # - There should be a back reference to this child (record.child_num)
      ref_person <- persons |> filter(record_id == ref_info$record_id)

      if (nrow(ref_person) == 0) {
        conflicts <- bind_rows(
          conflicts,
          tibble(
            source_table = "children",
            source_record_id = row$record_id,
            source_item = paste(
              "child",
              row$child_num,
              "marriage",
              row$marriage_num
            ),
            ref_type = "forward",
            ref = row$marriage_ref,
            issue = "ref_record_not_found",
            details = paste(
              "Referenced record",
              ref_info$record_id,
              "does not exist"
            )
          )
        )
        next
      }

      # Check if the referenced record's spouse has a back ref to this child
      expected_back_ref <- paste0(row$record_id, ".", row$child_num)
      ref_spouses <- spouses |> filter(record_id == ref_info$record_id)

      # Look for matching back reference in spouses
      back_ref_match <- ref_spouses |>
        filter(ref_type == "back" & ref == expected_back_ref)

      # Also check for forward reference on spouse that points to this child
      # (bidirectional forward refs case, e.g., child > 3504, spouse > 3709.4)
      fwd_ref_match <- ref_spouses |>
        filter(ref_type == "forward" & ref == expected_back_ref)

      # Also check if the PRIMARY person of ref record has the back ref
      # (This happens when the child becomes the primary person in their own record)
      primary_has_back_ref <- !is.na(ref_person$ref_type[1]) &&
        ref_person$ref_type[1] == "back" &&
        ref_person$ref[1] == expected_back_ref

      if (
        nrow(back_ref_match) > 0 ||
          nrow(fwd_ref_match) > 0 ||
          primary_has_back_ref
      ) {
        # Bidirectional match found
        child_idx <- which(
          children$record_id == row$record_id &
            children$child_num == row$child_num &
            children$marriage_num == row$marriage_num
        )

        if (nrow(back_ref_match) > 0 || nrow(fwd_ref_match) > 0) {
          # Ref is on spouse - this child married into that record
          children$ref_assignment[child_idx] <- "spouse"
          children$ref_verified[child_idx] <- TRUE

          # Also mark the spouse's ref as verified
          spouse_idx <- which(
            spouses$record_id == ref_info$record_id &
              ((spouses$ref_type == "back" & spouses$ref == expected_back_ref) |
                (spouses$ref_type == "forward" &
                  spouses$ref == expected_back_ref))
          )
          if (length(spouse_idx) > 0) {
            spouses$ref_verified[spouse_idx] <- TRUE
          }
        } else {
          # Back ref is on primary - this child IS the primary person of ref record
          children$ref_assignment[child_idx] <- "child"
          children$ref_verified[child_idx] <- TRUE
        }
      } else {
        # No circular back reference found
        # Check if the primary person has ANY back reference (to their parents)
        # If so, this is still a valid link - the forward ref points to the spouse
        ref_primary_name <- paste(
          ref_person$surname[1],
          ref_person$given_name[1]
        )
        child_name <- paste(row$surname, row$given_name)
        spouse_name <- row$spouse_name %||% ""

        # Check if ref record's primary has a back ref (even if not circular)
        primary_has_any_back_ref <- !is.na(ref_person$ref_type[1]) &&
          ref_person$ref_type[1] == "back" &&
          !is.na(ref_person$ref[1])

        # If primary has a back ref and name matches spouse OR child, resolve it
        if (primary_has_any_back_ref) {
          child_idx <- which(
            children$record_id == row$record_id &
              children$child_num == row$child_num &
              children$marriage_num == row$marriage_num
          )

          if (names_match(ref_primary_name, child_name)) {
            # Forward ref points to child's own record
            children$ref_assignment[child_idx] <- "child"
            children$ref_verified[child_idx] <- TRUE
          } else {
            # Forward ref points to spouse's record (most common case)
            # The primary person of ref record is the spouse
            children$ref_assignment[child_idx] <- "spouse"
            children$ref_verified[child_idx] <- TRUE
          }
        } else if (names_match(ref_primary_name, spouse_name)) {
          # No back ref but name matches spouse - accept as valid
          child_idx <- which(
            children$record_id == row$record_id &
              children$child_num == row$child_num &
              children$marriage_num == row$marriage_num
          )
          children$ref_assignment[child_idx] <- "spouse"
          children$ref_verified[child_idx] <- TRUE
        } else if (names_match(ref_primary_name, child_name)) {
          # No back ref but name matches child - accept as valid
          child_idx <- which(
            children$record_id == row$record_id &
              children$child_num == row$child_num &
              children$marriage_num == row$marriage_num
          )
          children$ref_assignment[child_idx] <- "child"
          children$ref_verified[child_idx] <- TRUE
        } else if (
          (is.na(spouse_name) || spouse_name == "") &&
            !names_match(ref_primary_name, child_name)
        ) {
          # Spouse name wasn't parsed but ref primary doesn't match child name
          # Assume ref is to the spouse (common case for minimal records)
          child_idx <- which(
            children$record_id == row$record_id &
              children$child_num == row$child_num &
              children$marriage_num == row$marriage_num
          )
          children$ref_assignment[child_idx] <- "spouse"
          children$ref_verified[child_idx] <- TRUE
        } else {
          # Cannot determine - add to conflicts
          conflicts <- bind_rows(
            conflicts,
            tibble(
              source_table = "children",
              source_record_id = row$record_id,
              source_item = paste(
                "child",
                row$child_num,
                "marriage",
                row$marriage_num
              ),
              ref_type = "forward",
              ref = row$marriage_ref,
              issue = "no_back_ref_no_name_match",
              details = paste(
                "Expected back ref",
                expected_back_ref,
                "not found. Primary:",
                ref_primary_name,
                "Child:",
                child_name,
                "Spouse:",
                spouse_name
              )
            )
          )
        }
      }
    }
  }

  # ==========================================================================
  # STEP 2: Process back references on children (< X.Y)
  # Back ref on child means this child appears as a child in another record
  # ==========================================================================

  if (nrow(children) > 0) {
    children_back_refs <- children |>
      filter(ref_type == "back" & !is.na(ref))

    for (i in seq_len(nrow(children_back_refs))) {
      row <- children_back_refs[i, ]
      ref_info <- parse_ref(row$ref)

      if (is.na(ref_info$record_id)) {
        next
      }

      # This back reference points to where this person was listed as a child
      # We should find a matching child in that record
      if (!is.na(ref_info$child_num)) {
        matching_child <- children |>
          filter(
            record_id == ref_info$record_id & child_num == ref_info$child_num
          )

        if (nrow(matching_child) > 0) {
          # Verify names match
          this_name <- paste(row$surname, row$given_name)
          that_name <- paste(
            matching_child$surname[1],
            matching_child$given_name[1]
          )

          if (!names_match(this_name, that_name)) {
            conflicts <- bind_rows(
              conflicts,
              tibble(
                source_table = "children",
                source_record_id = row$record_id,
                source_item = paste("child", row$child_num),
                ref_type = "back",
                ref = row$ref,
                issue = "name_mismatch",
                details = paste("This:", this_name, "Ref:", that_name)
              )
            )
          }
        }
      }
    }
  }

  # ==========================================================================
  # STEP 3: Verify spouse back references
  # ==========================================================================

  if (nrow(spouses) > 0) {
    spouse_back_refs <- spouses |>
      filter(ref_type == "back" & !is.na(ref) & !ref_verified)

    for (i in seq_len(nrow(spouse_back_refs))) {
      row <- spouse_back_refs[i, ]
      ref_info <- parse_ref(row$ref)

      if (is.na(ref_info$record_id)) {
        next
      }

      # This spouse has a back ref - they should appear as a child in the referenced record
      if (!is.na(ref_info$child_num)) {
        matching_child <- children |>
          filter(
            record_id == ref_info$record_id & child_num == ref_info$child_num
          )

        if (nrow(matching_child) > 0) {
          # Verify names match
          spouse_name <- paste(row$surname, row$given_name)
          child_name <- paste(
            matching_child$surname[1],
            matching_child$given_name[1]
          )

          if (names_match(spouse_name, child_name)) {
            spouse_idx <- which(
              spouses$record_id == row$record_id &
                spouses$spouse_num == row$spouse_num
            )
            spouses$ref_verified[spouse_idx] <- TRUE

            # Check if child has forward ref back to this record
            if (!is.na(matching_child$marriage_ref[1])) {
              child_ref_info <- parse_ref(matching_child$marriage_ref[1])
              if (
                !is.na(child_ref_info$record_id) &&
                  child_ref_info$record_id == row$record_id
              ) {
                # Bidirectional verified
                child_idx <- which(
                  children$record_id == ref_info$record_id &
                    children$child_num == ref_info$child_num &
                    children$marriage_num == matching_child$marriage_num[1]
                )
                if (length(child_idx) > 0) {
                  # Take only first match if multiple found
                  child_idx <- child_idx[1]
                  if (!children$ref_verified[child_idx]) {
                    children$ref_assignment[child_idx] <- "spouse"
                    children$ref_verified[child_idx] <- TRUE
                  }
                }
              }
            }
          } else {
            conflicts <- bind_rows(
              conflicts,
              tibble(
                source_table = "spouses",
                source_record_id = row$record_id,
                source_item = paste("spouse", row$spouse_num),
                ref_type = "back",
                ref = row$ref,
                issue = "name_mismatch",
                details = paste("Spouse:", spouse_name, "Child:", child_name)
              )
            )
          }
        } else {
          conflicts <- bind_rows(
            conflicts,
            tibble(
              source_table = "spouses",
              source_record_id = row$record_id,
              source_item = paste("spouse", row$spouse_num),
              ref_type = "back",
              ref = row$ref,
              issue = "ref_child_not_found",
              details = paste(
                "Child",
                ref_info$child_num,
                "not found in record",
                ref_info$record_id
              )
            )
          )
        }
      }
    }
  }

  list(
    persons = persons,
    spouses = spouses,
    children = children,
    conflicts = conflicts
  )
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

#' Parse the entire persons.txt file
#' @param file_path Path to persons.txt
#' @return List with parsed data frames and metadata
parse_ancestry_file <- function(file_path) {
  message("Reading and splitting records...")
  records <- split_into_records(file_path)
  message(paste("Found", length(records), "records"))

  message("Parsing records...")
  parsed <- map(records, parse_record, .progress = TRUE)

  # Count and report excluded empty records
  n_excluded <- sum(sapply(parsed, is.null))
  if (n_excluded > 0) {
    message(paste("Excluded", n_excluded, "empty records"))
  }

  message("Converting to data frames...")
  dataframes <- records_to_dataframes(parsed)

  message("Reconciling cross-references...")
  reconciled <- reconcile_references(dataframes)

  message("Done!")
  message(paste("Persons:", nrow(reconciled$persons)))
  message(paste("Spouses:", nrow(reconciled$spouses)))
  message(paste("Children:", nrow(reconciled$children)))
  message(paste("Reference conflicts:", nrow(reconciled$conflicts)))

  reconciled
}

# =============================================================================
# RUN PARSER
# =============================================================================

# Uncomment to run:
# result <- parse_ancestry_file("data/persons.txt")
#
# # Access the data frames:
# persons <- result$persons
# spouses <- result$spouses
# children <- result$children
# conflicts <- result$conflicts
#
# # Save to files:
# write_csv(persons, "data/parsed_persons.csv")
# write_csv(spouses, "data/parsed_spouses.csv")
# write_csv(children, "data/parsed_children.csv")
# write_csv(conflicts, "data/reference_conflicts.csv")
