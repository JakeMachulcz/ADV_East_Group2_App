#load libraries
library(tidyverse)
library(sf)
library(leaflet)

#get parks data
parks <- read.csv("Parks_Locations_and_Features.csv")




### Bridget -- Crime data ----------------------------------------------------------------------------
#get crime data
crime <- read.csv("SB_PoliceData.csv")

#remove offense column since it is all NA
crime_simpleB <- crime[c('lat','lon','date','street')]

#simplify park data for this analysis
parks_simpleB <- parks[c('Park_Name', "Lon", "Lat")] 


## crime map code ##
#convert both datasets to sf
lat_lon_crs = 4326
parks_sf <- st_as_sf(
  parks_simpleB,
  coords = c("Lon", "Lat"),
  crs = lat_lon_crs
)

crime_sf <- st_as_sf(
  crime_simpleB,
  coords = c("lon", "lat"),
  crs = lat_lon_crs
)

#project to meters to measure distance
sb_utm = 32616 #utm code for South Bend gotten from the internet
parks_proj  <- st_transform(parks_sf, sb_utm)
crime_proj  <- st_transform(crime_sf, sb_utm)

#confirm date column is the right type
crime_proj$date <- as.Date(crime_proj$date)

#function to create crime map with dynamic radius and start/end dates
generate_crime_map <- function(crime_radius, start_date = NULL, end_date = NULL) {
  #create parks with buffer by radius
  parks_buffered <- parks_proj %>%
    mutate(buffer = st_buffer(geometry, dist = crime_radius))
  
  #filter crimes by date
  crimes_filtered <- crime_proj
  if (!is.null(start_date) && !is.null(end_date)) {
    crimes_filtered <- crime_proj %>%
      filter(date >= start_date & date <= end_date)
  }
  
  #join park and crime data
  parks_with_crime <- st_join(
    crimes_filtered,
    parks_buffered %>% st_set_geometry("buffer"),
    join = st_within
  )

  #calculate number of crimes per park and remove crimes not near a park
  crime_counts <- parks_with_crime %>%
    st_drop_geometry() %>%
    count(Park_Name, name = "crime_count") %>%
    filter(!is.na(Park_Name))

  #join crime_count to df
  parks_buffered <- parks_buffered %>%
    left_join(crime_counts, by = "Park_Name") %>%
    mutate(crime_count = replace_na(crime_count, 0))
  
  #convert back to use with leaflet
  parks_map <- st_transform(parks_buffered, lat_lon_crs)
  
  #return map
  return(parks_map)
}

## crime data series code ##
generate_crime_timeseries <- function(selected_parks = NULL, crime_radius, start_date, end_date) {
  #filter for selected parks
  parks_filtered <- parks_proj
  if (!is.null(selected_parks) && length(selected_parks) > 0) {
    parks_filtered <- parks_filtered %>% filter(Park_Name %in% selected_parks)
  }
  
  #filter for crime dates
  crimes_filtered <- crime_proj %>%
    filter(date >= start_date & date <= end_date)
  
  #create buffers around selected parks
  parks_buffered <- parks_filtered %>%
    st_buffer(dist = crime_radius)
  
  #get crimes that happened near a park
  crimes_inside <- st_intersection(crimes_filtered, parks_buffered) %>%
    st_drop_geometry() %>%
    distinct(row_number())

  #label crimes that happened near the park
  crimes_filtered <- crimes_filtered %>%
    mutate(location = ifelse(row_number() %in% row_number(crimes_inside), "Within", "Outside"))
  
  #find volume and percentage of crimes near parks
  crime_ts <- crimes_filtered %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(crime_count = sum(location == "Within"), crime_perc = crime_count / n() * 100)
  
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
    parks_filtered <- parks_proj %>% dplyr::filter(Park_Name %in% selected_parks)
  }
  
  parks_buf <- parks_filtered %>%
    mutate(buffer = st_buffer(geometry, dist = radius_m))
  
  buf_geom <- st_geometry(parks_buf %>% st_set_geometry("buffer"))
  
  hits <- lengths(st_intersects(facilities_sf, buf_geom)) > 0
  
  facilities_all <- facilities_sf %>%
    mutate(near_park = hits)
  
  list(
    parks_buf = parks_buf,
    facilities_all = facilities_all
  )
}




