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
crash_small <- crash %>% filter(YEAR<2020)
crash_small <- crash_small %>% select(REPORT_NO, LIGHT_DESC, JUNCTION_DESC, 
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
weather_small <- weather %>% filter(YEAR<2020)
weather_small <- weather_small %>% select(c(REPORT_NO, CONTRIB_CODE))
weather_small <- weather_small %>% filter(!REPORT_NO %in% unknown_reports)
weather_small <- weather_small %>% rename("weather_code" = "CONTRIB_CODE")
weather_small <- weather_small %>% transform(weather_code=ifelse(weather_code==82.88, NA, weather_code))
weather_mod <- weather_small %>% dummy_cols(select_columns = "weather_code", ignore_na = TRUE, remove_selected_columns = TRUE)

weather_mod <- weather_mod %>% group_by(REPORT_NO) %>% summarise(clear_weather=max(weather_code_0), 
                                                                   smoke=max(weather_code_41),
                                                                   sleet=max(weather_code_42),
                                                                   blowing_sand=max(weather_code_43),
                                                                   crosswind=max(weather_code_44),
                                                                   rain_snow=max(weather_code_45),
                                                                   animal=max(weather_code_46),
                                                                   blinded=max(weather_code_47),
                                                                   )
### select and modify road
road_small <- road %>% filter(YEAR<2020)
road_small <- road_small %>% select(c(REPORT_NO, CONTRIB_CODE, CONTRIB_CODE_DESC))
road_small <- road_small %>% filter(!REPORT_NO %in% unknown_reports)
road_small <- road_small %>% rename("road_code" = "CONTRIB_CODE")
road_small <- road_small %>% transform(road_code=ifelse(road_code>69.88, NA, road_code))
road_mod <- road_small %>% dummy_cols(select_columns = "road_code", ignore_na = TRUE, remove_selected_columns = TRUE)

road_mod <- road_mod %>% group_by(REPORT_NO) %>% summarise(clear_road=max(road_code_0),
                                                           road_work=max(road_code_61),
                                                           wet=max(road_code_62),
                                                           ice=max(road_code_63),
                                                           holes=max(road_code_64),
                                                           construction=max(road_code_65),
                                                           light_out=max(road_code_66),
                                                           shoulder=max(road_code_67),
                                                           worn_road=max(road_code_69.88)
                                                           )

### select and modify person
person_small <- person %>% filter(YEAR<2020)
person_small <- person_small %>% select(REPORT_NO, SEX_CODE, CONDITION_CODE, INJ_SEVER_CODE, OCC_SEAT_POS_CODE,
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
                                                                 unsafe_equip=max(UNSAFE_EQUIP),
                                                                 impaired = sum(IMPAIRED)
                                                                 )
### select and modify vehicle
vehicle_small <- vehicle %>% filter(YEAR<2020)
vehicle_small <- vehicle_small %>% select(c(REPORT_NO, DAMAGE_CODE, MOVEMENT_DESC, 
                                            VEH_YEAR, GVW_CODE, BODY_TYPE_DESC, 
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
vehicle_small$MOVEMENT_DESC <- na_if(vehicle_small$MOVEMENT_DESC,"Unknown")
vehicle_small$MOVEMENT_DESC <- na_if(vehicle_small$MOVEMENT_DESC,"Not Applicable")
vehicle_small$MOVEMENT_DESC <- na_if(vehicle_small$MOVEMENT_DESC,"Other")
vehicle_small <- vehicle_small %>% dummy_cols(select_columns = "MOVEMENT_DESC", ignore_na = TRUE, remove_selected_columns = TRUE)
vehicle_small <- vehicle_small %>% mutate(bike_involved=ifelse(HARM_EVENT_DESC=="Bicycle",1,0))
vehicle_small$BODY_TYPE_DESC <- na_if(vehicle_small$BODY_TYPE_DESC,"")
vehicle_small$BODY_TYPE_DESC <- na_if(vehicle_small$BODY_TYPE_DESC,"Unknown")
vehicle_small$BODY_TYPE_DESC <- na_if(vehicle_small$BODY_TYPE_DESC,"Not Applicable")
vehicle_small$BODY_TYPE_DESC <- na_if(vehicle_small$BODY_TYPE_DESC,"Other")
vehicle_small <- vehicle_small %>% dummy_cols(select_columns = "BODY_TYPE_DESC", ignore_na = TRUE, remove_selected_columns = TRUE)
vehicle_mod1 <- vehicle_small %>% group_by(REPORT_NO) %>% summarise(damage=max(DAMAGE_SEVER),
                                                                   damage_count=sum(ANY_DAMAGE),
                                                                   vehicle_count=n(),
                                                                   speed_limit=max(SPEED_LIMIT),
                                                                   accelerate=max(MOVEMENT_DESC_Accelerating),
                                                                   backing=max(MOVEMENT_DESC_Backing),
                                                                   change_lanes=max(`MOVEMENT_DESC_Changing Lanes`),
                                                                   driverless=max(`MOVEMENT_DESC_Driverless Moving Vehicle`),
                                                                   entering_lane=max(`MOVEMENT_DESC_Entering Traffic Lane`),
                                                                   leaving_lane=max(`MOVEMENT_DESC_Leaving Traffic Lane`),
                                                                   left_turn=max(`MOVEMENT_DESC_Making Left Turn`),
                                                                   right_turn=max(`MOVEMENT_DESC_Making Right Turn`),
                                                                   u_turn=max(`MOVEMENT_DESC_Making U Turn`),
                                                                   constant=max(`MOVEMENT_DESC_Moving Constant Speed`),
                                                                   curve=max(`MOVEMENT_DESC_Negotiating a Curve`),
                                                                   parked=max(MOVEMENT_DESC_Parked),
                                                                   parking=max(MOVEMENT_DESC_Parking),
                                                                   passing=max(MOVEMENT_DESC_Passing),
                                                                   right_red=max(`MOVEMENT_DESC_Right Turn on Red`),
                                                                   skidding=max(MOVEMENT_DESC_Skidding),
                                                                   slowing=max(`MOVEMENT_DESC_Slowing or Stopping`),
                                                                   start_lane=max(`MOVEMENT_DESC_Starting From Lane`),
                                                                   start_park=max(`MOVEMENT_DESC_Starting From Parked`),
                                                                   stopped_lane=max(`MOVEMENT_DESC_Stopped in Traffic Lane`)
                                                                   )
vehicle_mod2 <- vehicle_small %>% group_by(REPORT_NO) %>% summarise(bike_involved=max(bike_involved),
                                                                   suv=max(`BODY_TYPE_DESC_(Sport) Utility Vehicle`),
                                                                   atv=max(`BODY_TYPE_DESC_All Terrain Vehicle (ATV)`),
                                                                   light_truck=max(`BODY_TYPE_DESC_Cargo Van/Light Truck 2 axles (10,000 lbs (4,536 kg) or less)`),
                                                                   ambulance_on=max(`BODY_TYPE_DESC_Ambulance/Emergency`),
                                                                   ambulance_off=max(`BODY_TYPE_DESC_Ambulance/Non Emergency`),
                                                                   distance_bus=max(`BODY_TYPE_DESC_Cross Country Bus`),
                                                                   farm_veh=max(`BODY_TYPE_DESC_Farm Vehicle`),
                                                                   fire_on=max(`BODY_TYPE_DESC_Fire Vehicle/Emergency`),
                                                                   fire_off=max(`BODY_TYPE_DESC_Fire Vehicle/Non Emergency`),
                                                                   limo=max(BODY_TYPE_DESC_Limousine),
                                                                   slow_veh=max(`BODY_TYPE_DESC_Low Speed Vehicle`),
                                                                   motorbike=max(BODY_TYPE_DESC_Motorcycle),
                                                                   moped=max(BODY_TYPE_DESC_Moped),
                                                                   heavy_truck=max(`BODY_TYPE_DESC_Medium/Heavy Truck 2 axles (10,000 lbs (4,536 kg) or less)`),
                                                                   bus_other=max(`BODY_TYPE_DESC_Other Bus`),
                                                                   light_truck_other=max(`BODY_TYPE_DESC_Other Light Trucks (10,000 lbs (4,536 kg))`),
                                                                   passenger_car=max(`BODY_TYPE_DESC_Passenger Car`),
                                                                   pickup_truck=max(`BODY_TYPE_DESC_Pickup Truck`),
                                                                   police_on=max(`BODY_TYPE_DESC_Police Vehicle/Emergency`),
                                                                   police_off=max(`BODY_TYPE_DESC_Police Vehicle/Non Emergency`),
                                                                   rv=max(`BODY_TYPE_DESC_Recreational Vehicle`),
                                                                   school_bus=max(`BODY_TYPE_DESC_School Bus`),
                                                                   station_wagon=max(`BODY_TYPE_DESC_Station Wagon`),
                                                                   commuter_bus=max(`BODY_TYPE_DESC_Transit Bus`),
                                                                   tractor=max(`BODY_TYPE_DESC_Truck Tractor`),
                                                                   van=max(BODY_TYPE_DESC_Van)
                                                                   )

### merge dataframes
df_list <- list(person_mod, vehicle_mod1, vehicle_mod2, weather_mod, road_mod)
crash_df <- df_list %>% reduce(full_join, by="REPORT_NO")

crash_df <- left_join(crash_small, crash_df, by="REPORT_NO")

write.csv(crash_df, "data/clean_crash_data.csv", row.names=FALSE)
