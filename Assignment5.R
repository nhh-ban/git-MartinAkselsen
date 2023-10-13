# Galaxy - assignment 5
# ------------------------

library(dplyr)
library(ggplot2)

# Task 2
# ----

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

# Task 3 
# ----

# I choose a_26 to be the main variable to investigate, because this is a 
# variable that tells us the size of each galaxy

galaxies %>% 
  ggplot(aes(x = a_26)) +
  geom_histogram(bins = 70, fill = "blue", color = "white") +
  xlab("Linear diameter of galaxy") +
  ylab("Count") +
  theme_bw()

# The plot shows that small galaxies are not under-represented. Larger galaxies
# however, is under-represented. This is the opposite from what the problem says


# Task 4
# ---- 

raw_file4 <- readLines(con = "https://www.sao.ru/lv/lvgdb/article/UCNG_Table4.txt")

# Identify the line number J of the separator line between the column names and
# the rest of the data table 
substr(x = raw_file4, start = 1, stop = 3) # Find that it is line 2

# Create J as a symbol for line 2

J <- (substr(x = raw_file4, start = 1, stop = 3) == "---") %>% 
  which() %>% 
  min()

# Save the variable names 
variable_names_task4 <- 
  str_split(string = raw_file4[(J-1)], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim
  
# Read the data into a csv file without the lines

csv_task4 <- 
  raw_file4[(J+1):764] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

csv_task4_names <- 
  c(paste(variable_names_task4, collapse = ","),
    csv_task4)


cat(csv_task4_names, sep = "\n", file = "task4_clean.csv")

# Read the data
UCNG <- read_csv("task4_clean.csv")

# Combine the two tables into one dataframe
combined_df <- left_join(galaxies, UCNG, by = "name")

# Plot the velocity and distance in a scatterplot

combined_df %>% 
  ggplot(aes(x = cz, y = D)) +
  geom_point() +
  geom_smooth() +
  ylab("Distance from us") +
  xlab("Velocity") +
  theme_bw()

# It looks like his observations were right. The galaxies with higher velocity
# are the ones that are furthest away from us


# Problem 5 
# ----

# Task 5 
# ----

# Read the data
library(nycflights13)
flights <- nycflights13::flights

# Exercises 5.2.4

# 1 

# All flights that had an arrival delay of over 2 hours
filter(flights, arr_delay > 2)

# All flights that flew to Houson (IAH or HOU)
filter(flights, dest %in% c("IAH", "HOU"))

# Were operated by United, American or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))

# Departed in summer 
filter(flights, month %in% c("7", "8", "9"))

# Arrived more than 2 hours late, but didnt leave late
filter(flights, arr_delay >= 2, dep_delay <= 0)


# 2 - use the between-function to simplify some of the answers
filter(flights, between(month, 7, 9))


# 3 - missing dep-_time
sum(is.na(flights$dep_time))

# There are 8 255 flights with missing dep_time. 
flights %>% 
  filter(is.na(dep_time))
# The rows woth missing dep_time values, also miss dep_delay, arr_time and
# arr_delay. This might mean that those flights were scheduled, but never
# detartured


# Exercises 5.3.1

# 1 - sort na-values to the start
flights %>% 
  arrange(desc(is.na(dep_time)))


# 2 - find the most delayed flights
flights %>% 
  arrange(desc(dep_delay))

# The flight that left earliest
flights %>% 
  arrange(year, month, day, dep_time)


# 3 - fastest flights
flights <-  flights %>% 
  mutate(speed = distance / air_time)

flights %>% 
  arrange(speed)


# 4 - shortest and longest travel
flights %>% 
  arrange(distance)

flights %>% 
  arrange(desc(distance))


# Exercises 5.4.1

# 1 - select variables in different ways
flights %>% 
  select(dep_time, dep_delay, arr_time, arr_delay)

flights %>% 
  select(starts_with(c("dep", "arr")))


# 2 - include a variable multiple times
flights %>% 
  select(dep_time, dep_time)
# It only shows the variable once




flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)


# Exercises 5.5.2

# 1 - change format on dep_time and sched_dep_time to minutes from midnight
flights$dep_time <- (flights$dep_time %/% 100) * 60 + 
  (flights$dep_time %% 100)

flights$dep_time <- sprintf("%02d:%02d",
                            flights$dep_time %/% 100, 
                            flights$dep_time %% 100)
 
flights$sched_dep_time <- (flights$sched_dep_time %/% 100) * 60 + 
  (flights$sched_dep_time %% 100)

flights$sched_dep_time <- sprintf("%02d:%02d",
                            flights$sched_dep_time %/% 100, 
                            flights$sched_dep_time %% 100)


# endring
