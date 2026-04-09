library(tidyverse)
library(haven)
library(ggplot2)

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
#synth_data <- read_csv("data/pp_exposure_2021.csv")

#synth_data <- read_csv("/media/ali/Expansion/backup_tabea/Ali/manchester/scenOutput/base/microData/pp_exposure_2021.csv")

synth_data <- read_csv("/media/ali/Expansion/backup_tabea/Ali/manchester/scenOutput/base/microData/pp_exposure_2021.csv")
#synth_data <- read_csv("/media/ali/Expansion/backup_tabea/Ali/manchester/input/health/pp_exposure_2021_base_161025.csv")


# Read in the HSE survey data
# from: cedar-grp-drive/Marina/physicalActivity/Health for England Survey/hse16_eul_v5.dta
raw_hse <- read_dta("data/hse16_eul_v5.dta")

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

# Assign age groups
assign_age_group <- function(age) {
  if (is.na(age)) return(NA)
  else if (age >= 16 & age <= 24) return("16-24")
  else if (age >= 25 & age <= 34) return("25-34")
  else if (age >= 35 & age <= 44) return("35-44")
  else if (age >= 45 & age <= 54) return("45-54")
  else if (age >= 55 & age <= 64) return("55-64")
  else if (age >= 65 & age <= 74) return("65-74")
  else if (age >= 75) return("75+")
  else return(NA) # for ages < 16 or missing
}

# Create age groups
synth_data$age_group <- sapply(synth_data$age, assign_age_group)

# synth_data |> 
#   filter(age_group == "16-24", gender == "Female", imd == 1) |> 
#   mutate(traval_PA = mmetHr_walk + mmetHr_cycle, 
#          total_PA = travel_PA + mmetHr_otherSport) |> 
#   group_by(age_group, gender, imd) |> 
#   reframe(med_total_pa = median(total_PA), 
#           med_travel_pa = median(travel_PA), 
#           med_sports_pa = med_total_pa - med_travel_pa)

#hse <- read_csv("data/processed_hse.csv")
hse <- read_csv("/media/ali/Expansion/backup_tabea/Ali/manchester/input/health/HSE/processed_hse.csv")

# Based on age_group, gender and imd levels, sample from df_B to df_A
assign_sports_PA <- function(df_A, df_B) {
  df_A <- df_A |>
    group_by(age_group, gender, imd) |>
    mutate(
      nonTransportPA = {
        # Extract group keys as local variables first
        current_age_group <- unique(age_group)
        current_gender    <- unique(gender)
        current_imd       <- unique(imd)
        
        matches <- df_B |>
          filter(age_group == current_age_group,
                 gender    == current_gender,
                 imd       == current_imd) |>
          pull(nonTransportPA)
        
        if (length(matches) == 0) {
          warning(paste("No match found for stratum:",
                        current_age_group, current_gender, current_imd,
                        "- using global B distribution"))
          sample(df_B$nonTransportPA, n(), replace = TRUE)
        } else {
          sample(matches, n(), replace = TRUE)
        }
      }
    ) |>
    ungroup()
  
  return(df_A)
}


# Call functions
synth_data1 <- assign_sports_PA(synth_data, hse)

synth_data1 <- synth_data1 |> mutate(total_PA = travel_PA + mmetHr_otherSport)

# Combine for comparison
compare_df <- bind_rows(
  #hse |> select(mmetHr_sport_manual, gender, imd, age_group) |> mutate(source = "HSE (sports + manual)"),
  synth_data |> select(total_PA, gender, imd, age_group) |> mutate(source = "JIBE")
)

# Density plot
ggplot(compare_df, aes(x = mmetHr_sport_manual, fill = source)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of mmetHr_sport_manual: Synthetic Population (imputed) vs HSE (original)",
       x = "mmetHr_sport_manual", y = "Density") +
  theme_minimal()

# Summary statistics
compare_df |>
  group_by(age_group, gender, imd, source) |>
  summarise(
    mean   = mean(mmetHr_sport_manual, na.rm = TRUE),
    `12.5th` = quantile(mmetHr_sport_manual, 0.125),
    `25th.` = quantile(mmetHr_sport_manual, 0.25),
    `37.5th.` = quantile(mmetHr_sport_manual, 0.375),
    `50th` = quantile(mmetHr_sport_manual, 0.5),
    `75th.` = quantile(mmetHr_sport_manual, 0.75)
  ) |> View()

# Combine for comparison
synth_df_summary <- bind_rows(
  synth_data |> 
    mutate(total_pa = travel_PA + mmetHr_sport_manual) |> 
    select(total_pa, gender, imd, age_group) |> 
    mutate(source = "JIBE Total PA (travel + sports + manual)")
)

# Summary statistics
synth_df_summary |>
  group_by(age_group, gender, imd, source) |>
  summarise(
    mean   = mean(total_pa, na.rm = TRUE),
    `12.5th` = quantile(total_pa, 0.125),
    `25th.` = quantile(total_pa, 0.25),
    `37.5th.` = quantile(total_pa, 0.375),
    `50th` = quantile(total_pa, 0.5),
    `75th.` = quantile(total_pa, 0.75)
  ) |> View()
