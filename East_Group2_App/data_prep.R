library(tidyverse)

calls <- read.csv("311_Phone_Call_Log_Mod.csv")
licenses <- read.csv("Business_Licenses.csv")
parks <- read.csv("Parks_Locations_and_Features.csv")
facilities <- read.csv("Public_Facilities.csv")


facilities <- facilities %>%
  mutate(
    POPL_ADDR1 = gsub("\r\n", ", ", POPL_ADDR1),
    POPL_ZIP = as.character(POPL_ZIP)
  )