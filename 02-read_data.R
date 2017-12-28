library("readxl")
library("tidyverse")

# read the xlsx files into R ---------------------------------------------------

# list the files in the data directory 
files <- list.files("data/", pattern = "^clb_[0-9]{6,}_[0-9]{6,}\\.xlsx$")

# read the files into one data frame
df <- map_df(paste0("data/", files), ~ read_xlsx(.x, col_types = "text"))

# clean the names
names(df) <- names(df) %>%
  tolower() %>%
  gsub(" ", "_", ., fixed = TRUE)

# change the column 'date' to date type and sort the data frame
df <- df %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)
