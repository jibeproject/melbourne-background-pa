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
synth_data <- read_csv("data/pp_exposure_2021.csv")

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

# Collect relevant vars and calculate total_PA from time_totalpa * 4, and add age_group
hse <- raw_hse |> 
  filter(Age16g5 > 0) |> 
  rowwise() |>
  select(id = SerialA,
         gender = Sex,
         age_group = ag16g10,
         time_totalpa = hrs10tot08,
         imd = qimd) |> 
  filter(age_group > 0 & time_totalpa >= 0) |> 
  mutate(
    total_PA = time_totalpa * 4,
    gender = if_else(gender == 2, "Female", "Male"),
    age_group = case_when(
      age_group == 1 ~ "16-24",
      age_group == 2 ~ "25-34",
      age_group == 3 ~ "35-44",
      age_group == 4 ~ "45-54",
      age_group == 5 ~ "55-64",
      age_group == 6 ~ "65-74",
      age_group == 7 ~ "75+",
      TRUE ~ NA_character_))


# Based on age_group, gender and imd levels, sample from df_B to df_A
assign_sports_PA <- function(df_A, df_B) {
  df_A <- df_A %>%
    group_by(age_group, gender, imd) %>%
    mutate(
      total_PA = {
        # Extract group keys as local variables first
        current_age_group <- unique(age_group)
        current_gender    <- unique(gender)
        current_imd       <- unique(imd)
        
        matches <- df_B %>%
          filter(age_group == current_age_group,
                 gender    == current_gender,
                 imd       == current_imd) %>%
          pull(total_PA)
        
        if (length(matches) == 0) {
          warning(paste("No match found for stratum:",
                        current_age_group, current_gender, current_imd,
                        "- using global B distribution"))
          sample(df_B$total_PA, n(), replace = TRUE)
        } else {
          sample(matches, n(), replace = TRUE)
        }
      },
      sports_PA = pmax(total_PA - travel_PA, 0)
    ) %>%
    ungroup()
  
  return(df_A)
}

# Call functions
synth_data <- assign_sports_PA(synth_data, hse)

# Combine for comparison
compare_df <- bind_rows(
  hse %>% select(total_PA, gender, imd, age_group) %>% mutate(source = "HSE (original)"),
  synth_data %>% select(total_PA, gender, imd, age_group) %>% mutate(source = "Synthetic Population (imputed)")
)

# Density plot
ggplot(compare_df, aes(x = total_PA, fill = source)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of total_PA: Synthetic Population (imputed) vs HSE (original)",
       x = "total_PA", y = "Density") +
  theme_minimal()

# Summary statistics
compare_df %>%
  group_by(age_group, gender, imd, source) %>%
  summarise(
    mean   = mean(total_PA, na.rm = TRUE),
    median = median(total_PA, na.rm = TRUE),
    sd     = sd(total_PA, na.rm = TRUE),
    p25    = quantile(total_PA, 0.25, na.rm = TRUE),
    p75    = quantile(total_PA, 0.75, na.rm = TRUE)
  )