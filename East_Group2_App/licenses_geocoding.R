# Load libraries
library(stringr)
library(janitor)
library(tidygeocoder)

licenses_raw <- read_csv("Business_Licenses.csv") %>%
  clean_names()

# Build full address  active flag
licenses_clean <- licenses_raw %>%
  mutate(
    full_address = paste(street_address, city, state, zip_code, sep = ", "),
    active = status == "Active"
  )

# Geocode addresses
licenses_geocoded <- licenses_clean %>%
  geocode(
    address = full_address,
    method  = "osm",
    lat     = "lat",
    long    = "lon"
  )

write_csv(licenses_geocoded, "licenses_geocoded.csv")
