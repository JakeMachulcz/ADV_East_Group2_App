#load libraries
library(tidyverse)
library(sf)
library(leaflet)

#get parks data
parks <- read.csv("Parks_Locations_and_Features.csv")

#convert dataset to sf
lat_lon_crs = 4326
parks_sf <- st_as_sf(
  parks,
  coords = c("Lon", "Lat"),
  crs = lat_lon_crs
)

#project to meters to measure distance
sb_utm = 32616 #utm code for South Bend gotten from the internet
parks_proj <- st_transform(parks_sf, sb_utm)


### Bridget -- Crime data ----------------------------------------------------------------------------
#get crime data
crime <- read.csv("SB_PoliceData.csv")

#remove offense column since it is all NA
crime_simpleB <- crime[c('lat','lon','date','street')]

##convert crime data to be usable with parks
crime_sf <- st_as_sf(
  crime_simpleB,
  coords = c("lon", "lat"),
  crs = lat_lon_crs
)
crime_proj <- st_transform(crime_sf, sb_utm)

#confirm date column is the right type
crime_proj$date <- as.Date(crime_proj$date)

#simplify park data for this analysis
parks_projB <- parks_proj[c('Park_Name', "geometry")] 

#function to filter datasets and call generating functions
respond_to_inputsB <- function(selected_parks, crime_radius, start_date, end_date) {
  #filter for selected parks
  parks_filteredB <- parks_projB
  if (!is.null(selected_parks)) {
    parks_filteredB <- parks_filteredB %>% filter(Park_Name %in% selected_parks)
  }
  
  #filter crimes by date
  crimes_filtered <- crime_proj
  if (!is.null(start_date) && !is.null(end_date)) {
    crimes_filtered <- crime_proj %>%
      filter(date >= start_date & date <= end_date)
  }
  
  #create parks with buffer by radius
  parks_filteredB <- parks_filteredB %>%
    mutate(buffer = st_buffer(geometry, dist = crime_radius))
  
  #return filtered datasets
  list(parks = parks_filteredB, crimes = crimes_filtered)
  
}

#function to create crime map
generate_crime_map <- function(prepped_data) {
  #join park and crime data
  parks_with_crime <- st_join(
    prepped_data$crimes,
    prepped_data$parks %>% st_set_geometry("buffer"),
    join = st_within
  )
  
  #calculate number of crimes per park and remove crimes not near a park
  crime_counts <- parks_with_crime %>%
    st_drop_geometry() %>%
    count(Park_Name, name = "crime_count") %>%
    filter(!is.na(Park_Name))
  
  #join crime_count to df
  park_dataB <- prepped_data$parks %>%
    left_join(crime_counts, by = "Park_Name") %>%
    mutate(crime_count = replace_na(crime_count, 0))
  
  #convert back to use with leaflet
  parks_mapB <- st_transform(park_dataB, lat_lon_crs)
  
  #return map
  return(parks_mapB)
}

##function to create crime time series
generate_crime_timeseries <- function(prepped_data) {
  parks_bufferB <- prepped_data$parks %>%
    st_set_geometry("buffer")
  
  #get crimes that happened near a park
  crimes_within <- st_join(prepped_data$crimes, parks_bufferB, join = st_within)
  
  #label crimes that happened near the park
  crimes_by_loc <- crimes_within %>%
    mutate(location = ifelse(!is.na(Park_Name), "Within", "Outside")) %>%
    st_drop_geometry()
  
  #find volume and percentage of crimes near parks
  crime_ts <- crimes_by_loc %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(crime_count = sum(location == "Within"), crime_perc = crime_count / n() * 100)
  
  #create a scaling of percent for graphing
  crime_ts <- crime_ts %>%
    mutate(crime_perc_scaled = crime_perc / 100 * max(crime_ts$crime_count))
  
  #return timeseries
  return(crime_ts)
}


###Danielle - Business License data  ----------------------------------------------------------------------------
licenses <- read.csv("Business_Licenses.csv")





###Jake - Parks(?) data  --------------------------------------------------------------------------------------










#Erich -- Facilities data  -------------------------------------------------------------------------------------
facilities <- read.csv("Public_Facilities.csv")

facilities <- facilities %>%
  mutate(
    POPL_ADDR1 = POPL_ADDR1 |>
      gsub("\r\n", ", ", x = _) |>
      sub("\\s*\\(.*$", "", x = _),
    POPL_ZIP = as.character(POPL_ZIP)
  ) |>
  rename(
    Name = POPL_NAME,
    Type = POPL_TYPE,
    Address = POPL_ADDR1,
    City = POPL_CITY,
    state = POPL_STATE,
    zip_Code = POPL_ZIP,
    Phone = POPL_PHONE
  ) |>
  filter(!is.na(Lat), !is.na(Lon))

#convert Facilities data to sf
facilities_sf <- st_as_sf(
  facilities,
  coords = c("Lon", "Lat"),
  crs = lat_lon_crs,
  remove = FALSE
) |>
  st_transform(sb_utm)


#Facilities Map
generate_facilities_near_parks <- function(
    selected_parks = NULL, radius_m = 500) {
  
  parks_filtered <- parks_proj
  if (!is.null(selected_parks) && length(selected_parks) > 0) {
    parks_filtered <- parks_proj %>% filter(Park_Name %in% selected_parks)
  }
  
  # Buffer selected parks
  parks_buf <- parks_filtered %>%
    mutate(buffer = st_buffer(geometry, dist = radius_m))
  
  # Which facilities fall inside ANY selected park buffer?
  hits <- lengths(st_intersects(
    facilities_sf, st_as_sf(parks_buf, sf_column_name = "buffer") %>% 
      st_geometry())) > 0
  
  facilities_near <- facilities_sf %>%
    mutate(near_park = hits) %>%
    filter(near_park)
  
  # Optional: count how many facilities are near each selected park (summary for popup/table)
  # (More advanced; skip if you want simple)
  list(
    parks_buf = parks_buf,
    facilities_near = facilities_near
  )
}




