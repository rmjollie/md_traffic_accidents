library(here)
library(rio)
library(tidyverse)
library(fastDummies)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 100L)

crash <- import(here("data", "Maryland_Statewide_Vehicle_Crashes.csv"))
circum_person <- import(here("data", "Maryland_Statewide_Vehicle_Crashes_-_Circum_person.csv"))
road <- import(here("data", "Maryland_Statewide_Vehicle_Crashes_-_Circum_road.csv"))
vehicle <- import(here("data", "Maryland_Statewide_Vehicle_Crashes_-_Vehicle_Details.csv"))
weather <- import(here("data", "Maryland_Statewide_Vehicle_Crashes_-_Circum_weather.csv"))
person <- import(here("data", "Maryland_Statewide_Vehicle_Crashes_-_Person_Details__Anonymized_.csv"))

### select and modify from crash dataframe
crash_small <- crash %>% filter(YEAR>2018)
crash_small <- crash_small %>% select(REPORT_NO, LIGHT_DESC, JUNCTION_CODE, 
                                      COLLISION_TYPE_CODE, RD_DIV_CODE,
                                      REFERENCE_ROAD_NAME, DISTANCE, FEET_MILES_FLAG, ACC_DATE)
## drop duplicated report ids (drops 8 from the sample)
crash_small <- crash_small %>% filter(REPORT_NO!="AE6305001S", 
                                      REPORT_NO!="BSPDA158000F", 
                                      REPORT_NO!="MCP32110012",
                                      REPORT_NO!="AE60230038")

### select and modify weather
weather_small <- weather %>% filter(YEAR>2018)
weather_small <- weather_small %>% select(c(REPORT_NO, CONTRIB_CODE))
weather_small <- weather_small %>% rename("weather_code" = "CONTRIB_CODE")
weather_small <- weather_small %>% transform(weather_code=ifelse(weather_code==82.88, NA, weather_code))
weather_mod <- weather_small %>% dummy_cols(select_columns = "weather_code", ignore_na = TRUE, remove_selected_columns = TRUE)

weather_mod <- weather_mod %>% group_by(REPORT_NO) %>% summarise(weather_code_0=max(weather_code_0), 
                                                                   weather_code_41=max(weather_code_41),
                                                                   weather_code_42=max(weather_code_42),
                                                                   weather_code_43=max(weather_code_43),
                                                                   weather_code_44=max(weather_code_44),
                                                                   weather_code_45=max(weather_code_45),
                                                                   weather_code_46=max(weather_code_46),
                                                                   weather_code_47=max(weather_code_47),
                                                                   )


