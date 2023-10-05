# Galaxy - assignment 5
# ------------------------

library(tidyverse)

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.

raw_file <- readLines(con = "https://www.sao.ru/lv/lvgdb/article/suites_dw_Table1.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table - line number 14

substr(x = raw_file, start = 1, stop = 3)

L <- (substr(x = raw_file, start = 1, stop = 2) == "--") %>% 
  which() %>% 
  min(
  )


# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function

cat(raw_file[1:(L-2)], sep = "\n", file = "variable_descriptions.txt")

# Extract the variable names (i.e. line (L-1)), store the names in a vector.

variable_names <- 
  str_split(string = raw_file[(L-1)], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

comma_sepererated_values <- 
  raw_file[(L+1):810] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

comma_sepererated_values_names <- 
  c(paste(variable_names, collapse = ","),
    comma_sepererated_values)

# Make it a csv-file
cat(comma_sepererated_values_names, sep = "\n", file = "clean_data.csv")

# Read the finished .csv back into R in the normal way.
galaxies <- read.csv("clean_data.csv")


