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
crash_small <- crash_small %>% distinct(REPORT_NO, .keep_all = TRUE)
## drop reports that are not identical, but have the same REPORT_NO, likely a mistake (there are 2 total, 4 rows)
crash_small <- crash_small %>% filter(REPORT_NO!="AE60230038",
                                      REPORT_NO!="MCP32110012"
                                      )


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
### select and modify road
road_small <- road %>% filter(YEAR>2018)
road_small <- road_small %>% select(c(REPORT_NO, CONTRIB_CODE, CONTRIB_CODE_DESC))
road_small <- road_small %>% rename("road_code" = "CONTRIB_CODE")
road_small <- road_small %>% transform(road_code=ifelse(road_code>69.88, NA, road_code))
road_mod <- road_small %>% dummy_cols(select_columns = "road_code", ignore_na = TRUE, remove_selected_columns = TRUE)

road_mod <- road_mod %>% group_by(REPORT_NO) %>% summarise(road_code_0=max(road_code_0),
                                                           road_code_61=max(road_code_61),
                                                           road_code_62=max(road_code_62),
                                                           road_code_63=max(road_code_63),
                                                           road_code_64=max(road_code_64),
                                                           road_code_65=max(road_code_65),
                                                           road_code_66=max(road_code_66),
                                                           road_code_67=max(road_code_67),
                                                           road_code_69.88=max(road_code_69.88)
                                                           )

### select and modify person
person_small <- person %>% filter(YEAR>2018)
person_small <- person_small %>% select(REPORT_NO, SEX_CODE, CONDITION_CODE, INJ_SEVER_CODE, OCC_SEAT_POS_CODE,
                                        PED_VISIBLE_CODE, PED_LOCATION_CODE, PED_OBEY_CODE,
                                        MOVEMENT_CODE, PERSON_TYPE, ALCOHOL_TEST_CODE,
                                        DRUG_TEST_CODE, BAC_CODE, EQUIP_PROB_CODE, SAF_EQUIP_CODE,
                                        EJECT_CODE, PERSON_ID, AIRBAG_DEPLOYED)
person_small <- person_small %>% mutate(SEX_NUM=ifelse(SEX_CODE=="M", 1, 0),
                                        IMPAIRED=case_when(CONDITION_CODE==1 ~ 0L,
                                                           CONDITION_CODE>1 & CONDITION_CODE<99 ~ 1L,
                                                           T ~ NA_integer_),
                                        ANY_INJURY=ifelse(INJ_SEVER_CODE>1,1,0),
                                        PED_INVOLVED=ifelse(PERSON_TYPE=="P",1,0),
                                        UNSAFE_EQUIP=ifelse(EQUIP_PROB_CODE>1&EQUIP_PROB_CODE<99,1,0)
                                        )
person_mod <- person_small %>% group_by(REPORT_NO) %>% summarise(injury=max(INJ_SEVER_CODE),
                                                                 injury_count=sum(ANY_INJURY),
                                                                 person_count=n(),
                                                                 sex_prop=sum(SEX_NUM)/n(),
                                                                 ped_involved=max(PED_INVOLVED),
                                                                 bac=max(BAC_CODE),
                                                                 unsafe_equip=max(UNSAFE_EQUIP)
                                                                 )



length(unique(road_small$REPORT_NO))
length((road_mod$REPORT_NO))
temp <- road_small %>% filter(road_code==62)
temp1 <- road_mod %>% filter(road_code_62==1)
