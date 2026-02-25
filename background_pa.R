library(tidyverse)
library(haven)

set.seed(2026)

# Add imd10 from zones dataset
add_imd <- function(df, zones) {
  left_join(df, 
            zones |> 
              dplyr::select(oaID, imd10) |> 
              distinct())
}

# Read zones that contain IMD10
ZONES_CSV  <- "/media/ali/Expansion/backup_tabea/manchester-main/input/zoneSystem.csv"
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)

# Read in the inputs baseline synthetic data
# from: cedar-grp-drive/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/base_120226/microData/pp_exposure_2021.csv
synth_data <- read_csv("data/pp_exposure_2021.csv")

# Rename var
synth_data <- synth_data |> rename(oaID = zone)

# Add imd10 to each OA
synth_data <- add_imd(synth_data, zones)

# Recategorise imd10 to imd with 5 categories
# Calculate travel PA
# Recategorise numeric gender to string
synth_data <- synth_data |> mutate(imd = (imd10 + 1) %/% 2,
                                   travel_PA = mmetHr_cycle + mmetHr_walk,
                                   gender = if_else(gender == 2, "Female", "Male"))