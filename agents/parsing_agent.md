---
title: "parsing trainer"
---
## PLAN OUTLINE
Your task is to work with me to create a parser that takes a file of German language church ancestry records, parses it and creates a structured database of the records.  The input text file  is somewhat structured but the records are highly variant.  You will step through the records and ask me for help identifying data elements and structure. You will use the information to build a robust parser in R that correctly handles these various records.

## EXECUTION PLAN
The "data/persons.txt" text file is a sequential set of primary records delimited by a tag of the form <nnnn>.  Look at one record at a time and make an initial estimate of the record structure and contents.  Ask me about confusing parts and ask me to confirm each data element before moving to the next record.  Ask me to simply type "Y" to accept all fields and move to the next record.

As we step through the records modify the parsing logic R code to accomodate new knowledge of how to parse a record. This is an iterative process.   After a certain number of records you should notice that nothing new is being learned about how to parse.  Ask me if you can finish the job, saving the parsing code and executing the code to create the database.  Below are some aids to parsing.

## HELPFUL HINTS TO GET THE JOB STARTED

### Possible people within a main record.  
There will be exactly one primary person.  There may be zero or more of the other people.
  primary person in record, 
  spouse of primary,
  child of primary,
  spouse of primary child,
  child of child of primary

  The record may include people names who are godparents and witnesses but they are not linked by a specific relationship.

## Tag Symbols
There are various punctuation marks that represent data tags.  The punctuation mark shoud be converted to named data fields. Here are some aids to identifying data tags.

### Events
These will be followed by some combination of a date, a place and another person, or no additional information.
```
  text_vec <- gsub("\\*", "BIRT ", text_vec)
  text_vec <- gsub("oo", "MARR ", text_vec)
  text_vec <- gsub("o‐o", " MARR NOTE Divorced", text_vec)
  text_vec <- gsub("~", " BAPM ", text_vec)
  text_vec <- gsub("# ", "NOTE ", text_vec)
  text_vec <- gsub("†", " DEAT ", text_vec)
  text_vec <- gsub("b\\. ", " BURI ", text_vec)
  text_vec <- gsub("# ", "NOTE ", text_vec)
```

### Cross references
These are CRITICAL to understanding ancesstors and descendants.  Numbers at the end of a line preceeded by < or > represent a link to another person in the file. "<" is a back refrerence to an earlier person.  ">" is a forward reference.  An integer number is a reference to a primary person.  A number with a decimal portion such as "> 34.2" is a reference to the  2nd child of primary person 34 who appears earlier in the file.

### People who might be mentioned but aren't  linked in record
"WITN" witness
"GODP" godparent
 
### Events may or may not be followed by a place where the event happened.
Here are a the most common place abbreviations
```
  text_vec <- gsub("OH |OH\\n", "Ohio ", text_vec)
  text_vec <- gsub(" AS", " PLAC Alt Schowe ", text_vec)
  text_vec <- gsub(" NS", " PLAC Neu Schowe ", text_vec)
```

### Date extraction helpers and modifiers
Dates should be converted to the form yyyy-mm-dd minus whatever portion is missing.  Only the year, for instance.
```
date_regex <- paste0("(\\d{2}\\.\\d{2}\\.\\d{4})|(ABT \\d{4})")
date_regex_2 <- paste0("(\\d{4}-\\d{2}-\\d{2})|(ABT \\d{4})")

  text_vec <- gsub("um ", " ABT ", text_vec) # about
  text_vec <- gsub("vor ", " BEF ", text_vec) # before
  text_vec <- gsub("zw.", " BET ", text_vec) #between
  text_vec <- gsub(" TZ:", " WITN ", text_vec)
  
  # age at event
  text_vec <- gsub("\\(†mit (.*?)\\)", "AGE \\1 ", text_vec)
  text_vec <- gsub("([0-9]{1,2})J", "\\1 years ", text_vec)
  text_vec <- gsub("([0-9]{1,2})M", "\\1 months ", text_vec)
  text_vec <- gsub("([0-9]{1,2})T", "\\1 days ", text_vec)

months <- c(
    "Januar",
    "Februar",
    "März",
    "April",
    "Mai",
    "Juni",
    "Juli",
    "August",
    "September",
    "Oktober",
    "November",
    "Dezember",
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
```
  ### Other tags

  ```
  text_vec <- gsub(" ev\\.", " RELI Evangelical ", text_vec)
  text_vec <- gsub(" ref\\.", " RELI Reformed ", text_vec)
  text_vec <- gsub(" kath\\.", " RELI Catholic ", text_vec)
  text_vec <- gsub(" TZ:", " WITN ", text_vec)
  text_vec <- gsub(" TP:", " GODP ", text_vec)
  text_vec <- gsub(" NN.", " Unknown ", text_vec)
```
### Name extraction helper
This regex illustrates the German special characters that might be in names
```
name_regex <- "([ßÖÜÄA-Z\\.]+ [ßÖÜÄA-Z][öäüa-z]+( [ÖÜÄA-Z][öäüa-z]+)?)"
```

### Place helpers
These are prefixes that are commonly found in place names in German-speaking regions. They can indicate various characteristics of the place, such as its age (Neu for new, Alt for old), size (Groß for large, Klein for small), location (Ober for upper, Unter for lower), or religious significance (St. for Saint, Sankt for Saint). "Bad" typically indicates a spa town.

```
place_prefixes <- c(
  "Neu",
  "Alt",
  "Groß",
  "Gross",
  "Klein",
  "Ober",
  "Unter",
  "St\\.",
  "Sankt",
  "Lager",
  "Bad"
)

  us_states <- c(
    "OH",
    "PA",
    "NY",
    "CA",
    "TX",
    "FL",
    "IL",
    "MI",
    "WI",
    "MN",
    "IA",
    "IN",
    "KY",
    "WV",
    "VA",
    "NC",
    "SC",
    "GA",
    "TN",
    "AL",
    "MS",
    "LA",
    "AR",
    "MO",
    "KS",
    "NE",
    "SD",
    "ND",
    "MT",
    "WY",
    "CO",
    "NM",
    "AZ",
    "UT",
    "NV",
    "ID",
    "WA",
    "OR",
    "AK",
    "HI",
    "CT",
    "RI",
    "MA",
    "VT",
    "NH",
    "ME",
    "NJ",
    "DE",
    "MD",
    "DC"
  )

 common_locales= c(
        "Ohio",
        "Cleveland",
        "Cuyahoga",
        "Elyria",
        "USA",
        "Syrmien",
        "Titel",
        "Nadalj",
        "Mettweiler",
        "Linden",
        "Baumholder",
        "Werbas",
        "Schowe"
      )
```
   