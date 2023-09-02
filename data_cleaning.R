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
crash_small <- crash %>% select(REPORT_NO, LIGHT_DESC, JUNCTION_DESC, 
                                      COLLISION_TYPE_DESC, RD_DIV_DESC,
                                      REFERENCE_ROAD_NAME, DISTANCE, FEET_MILES_FLAG, ACC_DATE,
                                      LATITUDE, LONGITUDE)
crash_small <- crash_small %>% distinct(REPORT_NO, .keep_all = TRUE)
## drop reports that are not identical, but have the same REPORT_NO, likely a mistake (there are 19 total, 38 rows)
unknown_reports <- list("AE4268001B", "AE4573000G", "AE60230038", "CP0138000K", "DA34520002", "DA3493000F", "DA39470011",
                        "DA39470017", "DA39470018", "DM8430000B", "MCP21950009", "MCP2195000B", "MCP2195000C",
                        "MCP2672000J", "MCP2672000K", "MCP2871002J", "MCP29840009", "MCP32110012", "ZF03060007")
crash_small <- crash_small %>% filter(!REPORT_NO %in% unknown_reports)

crash_small <- crash_small %>% mutate(feet_from_loc = ifelse(FEET_MILES_FLAG=="M", DISTANCE*5280, DISTANCE))
crash_small <- crash_small %>% mutate(location_name = ifelse(is.na(feet_from_loc), feet_from_loc, paste0(feet_from_loc, " feet from ", REFERENCE_ROAD_NAME)))

library(lubridate)
crash_small <- crash_small %>% mutate(date = ymd(ACC_DATE))

crash_small <- crash_small %>% select(-c(REFERENCE_ROAD_NAME:FEET_MILES_FLAG, ACC_DATE, feet_from_loc))

### select and modify weather
library(fastDummies)
weather_small <- weather %>% select(c(REPORT_NO, CONTRIB_CODE))
weather_small <- weather_small %>% filter(!REPORT_NO %in% unknown_reports)
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
road_small <- road %>% select(c(REPORT_NO, CONTRIB_CODE, CONTRIB_CODE_DESC))
road_small <- road_small %>% filter(!REPORT_NO %in% unknown_reports)
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
person_small <- person %>% select(REPORT_NO, SEX_CODE, CONDITION_CODE, INJ_SEVER_CODE, OCC_SEAT_POS_CODE,
                                        PED_VISIBLE_CODE, PED_LOCATION_CODE, PED_OBEY_CODE,
                                        MOVEMENT_CODE, PERSON_TYPE, ALCOHOL_TEST_CODE,
                                        DRUG_TEST_CODE, BAC_CODE, EQUIP_PROB_CODE, SAF_EQUIP_CODE,
                                        EJECT_CODE, PERSON_ID, AIRBAG_DEPLOYED)
person_small <- person_small %>% filter(!REPORT_NO %in% unknown_reports)
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
### select and modify vehicle
vehicle_small <- vehicle %>% select(c(REPORT_NO, DAMAGE_CODE, MOVEMENT_DESC, 
                                            VEH_YEAR, GVW_CODE, BODY_TYPE_CODE, 
                                            DRIVERLESS_FLAG, SPEED_LIMIT, HARM_EVENT_DESC))
vehicle_small <- vehicle_small %>% filter(!REPORT_NO %in% unknown_reports)

vehicle_small <- vehicle_small %>% mutate(ANY_DAMAGE=ifelse(DAMAGE_CODE>1&DAMAGE_CODE<88,1,0),
                                          WEIGHT=case_when(GVW_CODE==1 ~ 1L,
                                                           GVW_CODE==2 ~ 2L,
                                                           GVW_CODE==3 ~3L,
                                                           T ~ NA_integer_),
                                          DAMAGE_SEVER=case_when(DAMAGE_CODE==1~1L,
                                                                 DAMAGE_CODE==2~2L,
                                                                 DAMAGE_CODE==3~3L,
                                                                 DAMAGE_CODE==4~4L,
                                                                 DAMAGE_CODE==5~5L,
                                                                 T~NA_integer_),
                                          )
vehicle_mod <- vehicle_small %>% group_by(REPORT_NO) %>% summarise(damage=max(DAMAGE_SEVER),
                                                                   damage_count=sum(ANY_DAMAGE),
                                                                   vehicle_count=n(),
                                                                   speed_limit=max(SPEED_LIMIT),
                                                                   )

### merge dataframes
df_list <- list(person_mod, vehicle_mod, weather_mod, road_mod)
crash_df <- df_list %>% reduce(full_join, by="REPORT_NO")

crash_df <- left_join(crash_small, crash_df, by="REPORT_NO")

write.csv(crash_df, "data/clean_crash_data.csv", row.names=FALSE)
