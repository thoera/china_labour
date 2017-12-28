library("tidyverse")
library("ggmap")

source("02-read_data.R", encoding = "UTF-8")

# get a list of the provinces
province <- df %>%
  distinct(province) %>%
  filter(province != 0) %>%
  pull(province)

# find the longitude and the latitude
long_lat <- map_df(
  province,
  ~ geocode(location = .x, source = "dsk")
)

long_lat <- cbind(province, long_lat)

# save the results
write_delim(long_lat, "data/long_lat.csv", delim = ";")
