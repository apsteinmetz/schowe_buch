---
title: "ancestry record parsing trainer"
---
## PLAN OUTLINE
Your task is to work with me to create a parser that takes a file of German language church ancestry records, parses it and creates a structured JSON database of the records for each individual.  The input text file is somewhat structured but the records are highly variant.  

## EXECUTION PLAN
The "data/persons.txt" text file is a sequential set of primary records delimited by a tag of the form <nnnn>.  The records include names, events, dates and places of those events.  The primary record may be considered a family.  Many family recordss are interlinked through marraige or descendents. Look at one record at a time and make an initial estimate of the record structure and contents.  Ask me about confusing parts and ask me to confirm each data element before moving to the next record.   While German words in data are allowed, field names should be in English. Ask me to simply type "Y" to accept all fields and move to the next record.

As we step through the records modify the parsing logic R code to accomodate new knowledge of how to parse a record. This is an iterative process.   After a certain number of records you should notice that nothing new is being learned about how to parse.  Ask me if you can finish the job, saving the parsing code and executing the code to create the database.

Once that is done, randomly select a number of records to validate the work and ask me if they look correct.

### Cross references

These are CRITICAL to understanding ancestors and descendants.  Numbers at the end of a line preceeded by < or > represent a link to another primary record in the file. "<" is a back refrerence to an earlier family.  ">" is a forward reference.  A following integer number is a reference to a family.  A number with a decimal portion refers to a child in a family. For example,  "> 34.2" is a reference to the  2nd child or spouse of the 2nd child in the primary record 34 that appears earlier in the file.  Each person record must include the proper cross-reference if it is present in the source file.

After the parsing step is completed resolve the cross references. 
DO NOT finalize a person record until you have resolved any forward or back references.  Test these for validity once all records have been created.

Consider this execution plan my proposal. Before getting started, present your own suggestions for enhancments or improvments to my execution plan.

## HELPFUL HINTS TO GET THE JOB STARTED

Below are some aids to parsing.

### Possible people within a main record.  
There will be exactly one primary person.  There may be zero or more of the other people.
  primary person in record, 
  spouse of primary,
  child of primary,
  spouse of primary child,
  child of child of primary

### Children in the family are denoted by a numbered list

## Tag Symbols
There are various punctuation marks that represent EVENT data tags.  The punctuation mark shoud be converted to named data fields. Here are some aids to identifying data tags.

"NN." apprearing anywhwere means unknown.

### Events
These will be followed by some combination of a date, a place and another person, or no additional information.
  "*" Birth
  "oo" Marraige
  "o‐o" Divorce
  "~", Baptism
  "# " Note
  "†" Death
  "b. " Burial

### People who might be mentioned but aren't  linked in record
These can be treated as FACTs not as individual people so a new record does not have to be created for names associated with these tags.
"TZ:" witness
"TP" godparent
 
#### Date extraction helpers and modifiers
Dates should be converted to the form yyyy-mm-dd minus whatever portion is missing.  Only the year, for instance.

```
date_regex <- paste0("(\\d{2}\\.\\d{2}\\.\\d{4})|(ABT \\d{4})")
date_regex_2 <- paste0("(\\d{4}-\\d{2}-\\d{2})|(ABT \\d{4})")
```

##### dates may be qualfied with abbreviations denoting appoximations
"um " about
"vor " before
"zw." between

##### Sometimes the age at the event, typically death, is given. If the event date has already been provided it can be ignored as redundant. Here are the identifying regexes.
```
  text_vec <- gsub("\\(†mit (.*?)\\)", "AGE \\1 ", text_vec)
  text_vec <- gsub("([0-9]{1,2})J", "\\1 years ", text_vec)
  text_vec <- gsub("([0-9]{1,2})M", "\\1 months ", text_vec)
  text_vec <- gsub("([0-9]{1,2})T", "\\1 days ", text_vec)
```
#### Month Names
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

#### Religions
  " ev." Evangelical
  " ref.", Reformed
  " kath." Catholic

### Name extraction
This regex illustrates the German special characters that might be in names
```
name_regex <- "([ßÖÜÄA-Z\\.]+ [ßÖÜÄA-Z][öäüa-z]+( [ÖÜÄA-Z][öäüa-z]+)?)"
```

The canonical list of given names is here:
data/unique_given_names.csv
Other words are not given names.

The canonical list of surnames is here:
data/unique_surnames.csv
Other words are not surnnames.

### Events may or may not be followed by a place where the event happened.

The file data/unique_places.csv has the canonical list of place names. Other words are not places.

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

```

### Occupations
Sometimes the primary person in a family record has their occupation listed after their name.  It can be confusing becuase some names are the same word as an occupation. Surnames are capitalized while occuptations are not.  The canonical list of occupations is in:
data/unique_occupations.csv 
Other words are not occupations.