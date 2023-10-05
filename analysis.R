library(here)
library(rio)
library(tidyverse)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 100L)

crash_df <- import(here("data", "clean_crash_data.csv"))

crash_df$LIGHT_DESC <- na_if(crash_df$LIGHT_DESC, "")
crash_df$LIGHT_DESC <- na_if(crash_df$LIGHT_DESC, "Unknown")
crash_df$LIGHT_DESC <- na_if(crash_df$LIGHT_DESC, "Not Applicable")
crash_df$LIGHT_DESC <- na_if(crash_df$LIGHT_DESC, "Other")

crash_df$JUNCTION_DESC <- na_if(crash_df$JUNCTION_DESC, "")
crash_df$JUNCTION_DESC <- na_if(crash_df$JUNCTION_DESC, "Not Applicable")

crash_df$RD_DIV_DESC <- na_if(crash_df$RD_DIV_DESC, "")
crash_df$RD_DIV_DESC <- na_if(crash_df$RD_DIV_DESC, "Unknown")
crash_df$RD_DIV_DESC <- na_if(crash_df$RD_DIV_DESC, "Not Applicable")
crash_df$RD_DIV_DESC <- na_if(crash_df$RD_DIV_DESC, "Other")

crash_df$COLLISION_TYPE_DESC <- na_if(crash_df$COLLISION_TYPE_DESC, "Unknown")
crash_df$COLLISION_TYPE_DESC <- na_if(crash_df$COLLISION_TYPE_DESC, "Not Applicable")
crash_df$COLLISION_TYPE_DESC <- na_if(crash_df$COLLISION_TYPE_DESC, "Other")

crash_df <- crash_df %>% dummy_cols(select_columns = "LIGHT_DESC", ignore_na = TRUE, remove_selected_columns = TRUE)
crash_df <- crash_df %>% dummy_cols(select_columns = "JUNCTION_DESC", ignore_na = TRUE, remove_selected_columns = TRUE)
crash_df <- crash_df %>% dummy_cols(select_columns = "RD_DIV_DESC", ignore_na = TRUE, remove_selected_columns = TRUE)
crash_df <- crash_df %>% dummy_cols(select_columns = "COLLISION_TYPE_DESC", ignore_na = TRUE, remove_selected_columns = TRUE)

crash_df <- crash_df %>% rename("dark_unknown" = "LIGHT_DESC_Dark - Unknown Lighting",
                                "dark_lights" = "LIGHT_DESC_Dark Lights On", 
                                "dark_none" =  "LIGHT_DESC_Dark No Lights",
                                "dawn" = "LIGHT_DESC_Dawn",
                                "daylight" = "LIGHT_DESC_Daylight",
                                "dusk" = "LIGHT_DESC_Dusk",
                                "alley" = "JUNCTION_DESC_Alley",
                                "driveway_comm" = "JUNCTION_DESC_Commercial Driveway",
                                "crossover" = "JUNCTION_DESC_Crossover Related",
                                "interchange" = "JUNCTION_DESC_Interchange Related",
                                "intersection" = "JUNCTION_DESC_Intersection",
                                "inter_related" = "JUNCTION_DESC_Intersection Related",
                                "not_intersect" = "JUNCTION_DESC_Non Intersection",
                                "railway" = "JUNCTION_DESC_Railway Grade Crossing",
                                "driveway_res" = "JUNCTION_DESC_Residential Driveway",
                                "one_way" = "RD_DIV_DESC_One-way Trafficway",
                                "median" = "RD_DIV_DESC_Two-way, Divided, Positive Median Barrier",
                                "paint_median" = "RD_DIV_DESC_Two-way, Divided, Unprotected (painted >4 feet) Median",
                                "no_median" = "RD_DIV_DESC_Two-way, Not Divided",
                                "angle_left" = "COLLISION_TYPE_DESC_Angle Meets Left Turn",
                                "angle_left_head" = "COLLISION_TYPE_DESC_Angle Meets Left Turn Head On",
                                "angle_right" = "COLLISION_TYPE_DESC_Angle Meets Right Turn",
                                "head" = "COLLISION_TYPE_DESC_Head On",
                                "head_left" = "COLLISION_TYPE_DESC_Head On Left Turn",
                                "both_left_opposite" = "COLLISION_TYPE_DESC_Opposite Direction Both Left Turn",
                                "opposite_sideswipe" = "COLLISION_TYPE_DESC_Opposite Direction Sideswipe",
                                "both_left_same" = "COLLISION_TYPE_DESC_Same Direction Both Left Turn",
                                "both_left_turn" = "COLLISION_TYPE_DESC_Same Direction Left Turn",
                                "rear_same" = "COLLISION_TYPE_DESC_Same Direction Rear End",
                                "rear_left_same" = "COLLISION_TYPE_DESC_Same Direction Rear End Left Turn",
                                "rear_right_same" = "COLLISION_TYPE_DESC_Same Direction Rear End Right Turn",
                                "right_same" = "COLLISION_TYPE_DESC_Same Direction Right Turn",
                                "same_sidewipe" = "COLLISION_TYPE_DESC_Same Direction Sideswipe",
                                "same_angle" = "COLLISION_TYPE_DESC_Same Movement Angle",
                                "single" = "COLLISION_TYPE_DESC_Single Vehicle"
                                )

tree_df <- crash_df %>% mutate(severity=case_when(injury>=3 | damage >=4 ~ 1L,
                                                  injury<3 | damage <4 ~ 0L,
                                                  T ~ NA_integer_))

table(crash_df$)
(74940+88662)/571313
table(crash_df$RD_DIV_DESC)
(61858+12247+1041)/571313
table(crash_df$COLLISION_TYPE_DESC)
(71736+15380+5805)/571313
