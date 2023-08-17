library(here)
library(rio)
library(tidyverse)

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
## drop duplicated report ids (there are 8 total)
crash_small <- crash_small %>% filter(REPORT_NO!="AE6305001S", 
                                      REPORT_NO!="BSPDA158000F", 
                                      REPORT_NO!="MCP32110012",
                                      REPORT_NO!="AE60230038")


which(colnames(crash_small)=="REPORT_NO")
length(unique(crash_small$REPORT_NO))

crash_mod <- crash_small %>%
  group_by(REPORT_NO) %>%
  mutate(row_count=row_number()) %>%
  ungroup()

temp <- crash_mod %>% filter(REPORT_NO=="BSPDA158000F")

vehicle <- vehicle %>%
  group_by(REPORT_NO) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

vehicle <- vehicle %>% 
  pivot_wider(id_cols = REPORT_NO, 
              names_from = row_id, 
              values_from = VEHICLE_ID, 
              names_prefix = "vehicle_id_"
              )

################# Person Data Cleaning ####################
person_small <- person %>% filter(YEAR>2020)

person_small <- person_small %>% select(REPORT_NO, SEX_CODE, CONDITION_CODE, INJ_SEVER_CODE, OCC_SEAT_POS_CODE,
                            PED_VISIBLE_CODE, PED_LOCATION_CODE, PED_OBEY_CODE,
                            MOVEMENT_CODE, PERSON_TYPE, ALCOHOL_TEST_CODE,
                            DRUG_TEST_CODE, BAC_CODE, EQUIP_PROB_CODE, SAF_EQUIP_CODE,
                            EJECT_CODE, PERSON_ID, AIRBAG_DEPLOYED)

person %>% group_by(REPORT_NO) %>% summarise(n=n()) %>% arrange(desc(n)) %>% print(n=50)

personZG04 <- person %>% filter(REPORT_NO=="ZG0472000D")
vehicleZG04 <- vehicle %>% filter(REPORT_NO=="ZG0472000D")
crash %>% filter(REPORT_NO=="ZG0472000D")

person <- person %>%
  group_by(REPORT_NO) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

person_mutated <- person_small %>%
  group_by(REPORT_NO) %>%
  mutate(row_count=row_number(), 
         people_involved=n(), 
         impaired=case_when(CONDITION_CODE==1 ~ 1,
                            CONDITION_CODE>1&CONDITION_CODE<99 ~ 2,
                            T ~ NA_character_),
         highest_injury=max(INJ_SEVER_CODE),
         driver_bac=ifelse(PERSON_TYPE=="D" & BAC_CODE>0, BAC_CODE,NA)) %>%
  ungroup()

person_small <- person_small %>% mutate(impaired=case_when(CONDITION_CODE==1 ~ 1,
                                                           T ~ NA_character_))

person_wide <- person %>% 
  pivot_wider(id_cols = REPORT_NO, 
              names_from = row_id, 
              values_from = c(SEX_CODE:AIRBAG_DEPLOYED)
              )

PERSON <- person %>% select(SEX_CODE, CONDITION_CODE, INJ_SEVER_CODE, OCC_SEAT_POS_CODE,
                  PED_VISIBLE_CODE, PED_LOCATION_CODE, PED_OBEY_CODE,
                  MOVEMENT_CODE, PERSON_TYPE, ALCOHOL_TEST_CODE,
                  DRUG_TEST_CODE, BAC_CODE, EQUIP_PROB_CODE, SAF_EQUIP_CODE,
                  EJECT_CODE, PERSON_ID, AIRBAG_DEPLOYED)

df_list <- list(crash, circum_person, road, vehicle, weather, person)
all_data <- df_list %>% reduce(outer_join, by="REPORT_NO")

small_data <- all_data %>% select(C(REPORT_NO, REPORT_TYPE,
                                    ACC_DATE, ACC_TIME, AGENCY_CODE,
                                    COUNTY_DESC,
                                    LATITUDE, LONGITUDE, REFERENCE_ROAD_NAME,
                                    HARM_EVENT_DESC1, HARM_EVENT_DESC2,
                                    
                                    
                                    COLLISION_TYPE_CODE, JUNCTION_CODE,
                                    LIGHT_DESC, SURF_COND_CODE, RD_COND_CODE,
                                    RD_DIV_CODE, FIX_OBJ_CODE, WEATHER_DESC,
                                    SIGNAL_FLAG_DESC, C_M_ZONE_FLAG))