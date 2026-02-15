library(tidyverse)
source('R/parse_ancestry_records.R')

result <- parse_ancestry_file('data/persons.txt')

cat("\nSaving CSVs...\n")
write_csv(result$persons, 'data/parsed_persons.csv')
write_csv(result$spouses, 'data/parsed_spouses.csv')
write_csv(result$children, 'data/parsed_children.csv')
write_csv(result$conflicts, 'data/reference_conflicts.csv')

cat("Files saved successfully!\n")
