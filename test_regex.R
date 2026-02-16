library(stringr)

line <- "Keck Philipp BIRT ABT 1920 DEAT MISSING Russland"

# Test different patterns
cat("Line:", line, "\n\n")

# Original pattern
result1 <- str_extract(
  line,
  "DEAT\\s+.*?(?=\\s+BAPM|\\s+BURI|\\s+MARR|\\s+NOTE|$)"
)
cat("Pattern 1 (original):", result1, "\n")

# Without lookahead
result2 <- str_extract(line, "DEAT\\s+.*")
cat("Pattern 2 (no lookahead):", result2, "\n")

# Simpler pattern
result3 <- str_extract(line, "DEAT .+")
cat("Pattern 3 (space + .+):", result3, "\n")

# Test if \s+ works at all
cat("\nDoes str_detect find DEAT\\s+?", str_detect(line, "DEAT\\s+"), "\n")
cat("Does str_detect find DEAT ?", str_detect(line, "DEAT "), "\n")

# Try fixed lookahead
result4 <- str_extract(line, "DEAT\\s+[^$]+")
cat("Pattern 4 (negated $):", result4, "\n")
