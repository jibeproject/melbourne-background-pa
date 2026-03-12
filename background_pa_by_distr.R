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
ZONES_CSV  <- "data/zoneSystem.csv"
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)

# Read in the inputs baseline synthetic data
# from: cedar-grp-drive/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/base_120226/microData/pp_exposure_2021.csv
# Z:/HealthImpact/Data/Country/UK/JIBE/manchester/input/health/pp_exposure_2021_base_161025.csv
synth_data <- read_csv("data/pp_exposure_2021_base_161025.csv")

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
hse <- read_csv("data/processed_hse.csv")

# Function to assign sports_PA from HSE data
# Sample sports_PA stratified by age_group, gender, imd so that both overall
# and grouped total_PA distributions match df_B
assign_sports_PA <- function(df_A, df_B) {
  df_A <- df_A |>
    group_by(age_group, gender, imd) |>
    mutate(
      # Step 1: Get stratum-specific total_PA from df_B (computed once per group)
      age_val = age_group[1],
      gender_val = gender[1],
      imd_val = imd[1],
      
      # Step 2: Generate ranks within stratum based on travel_PA
      n_stratum = n(),
      ranks = rank(travel_PA),
      
      # Step 3: Map ranks to stratum-specific total_PA quantiles
      quantile_positions = (ranks - 0.5) / n_stratum,
      quantile_positions = pmin(pmax(quantile_positions, 0), 1),
      
      # Step 4: Sample total_PA from stratum distribution
      total_PA_target = {
        matches <- df_B |>
          filter(age_group == age_val[1],
                 gender    == gender_val[1],
                 imd       == imd_val[1]) |>
          pull(total_PA) |>
          sort()
        
        if (length(matches) > 0) {
          quantile(matches, probs = quantile_positions, type = 7)
        } else {
          warning(sprintf(
            "No match found for stratum: %s %s %s - using global B distribution",
            age_val[1], gender_val[1], imd_val[1]
          ))
          quantile(sort(df_B$total_PA), probs = quantile_positions, type = 7)
        }
      },
      
      # Step 5: Calculate sports_PA
      sports_PA = total_PA_target - travel_PA,
      sports_PA = pmax(sports_PA, 0),
      total_PA = travel_PA + sports_PA,
      
      # Step 6: Clean up temporary columns
      age_val = NULL,
      gender_val = NULL,
      imd_val = NULL,
      n_stratum = NULL,
      ranks = NULL,
      quantile_positions = NULL,
      total_PA_target = NULL
    ) |>
    ungroup()
  
  return(df_A)
}

# Call functions
synth_data2 <- assign_sports_PA(synth_data, hse)

# Combine for comparison
compare_df <- bind_rows(
  hse |> select(total_PA, gender, imd, age_group) |> mutate(source = "HSE (original)"),
  synth_data2 |> select(total_PA, gender, imd, age_group) |> mutate(source = "Synthetic Population (imputed)")
)

compare_df |>
  group_by(source) |>
  summarise(
    mean   = mean(total_PA, na.rm = TRUE),
    median = median(total_PA, na.rm = TRUE),
    sd     = sd(total_PA, na.rm = TRUE),
    p25    = quantile(total_PA, 0.25, na.rm = TRUE),
    p75    = quantile(total_PA, 0.75, na.rm = TRUE)
  ) 

# Density plot
ggplot(compare_df, aes(x = total_PA, fill = source)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of total_PA: Synthetic Population (imputed) vs HSE (original)",
       x = "total_PA", y = "Density") +
  theme_minimal()

# Summary statistics
compare_df |>
  group_by(age_group, gender, imd, source) |>
  summarise(
    mean   = mean(total_PA, na.rm = TRUE),
    median = median(total_PA, na.rm = TRUE),
    sd     = sd(total_PA, na.rm = TRUE),
    p25    = quantile(total_PA, 0.25, na.rm = TRUE),
    p75    = quantile(total_PA, 0.75, na.rm = TRUE)
  ) #|> write_csv("grouped_dist_total_pa.csv")