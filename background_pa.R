library(tidyverse)
library(haven)

# Read in the inputs baseline synthetic data
# from: cedar-grp-drive/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/base_120226/microData/pp_exposure_2021.csv
synth_data <- read_csv("data/pp_exposure_2021.csv")


# Read in the HSE survey data
# from: cedar-grp-drive/Marina/physicalActivity/Health for England Survey/hse16_eul_v5.dta
raw_hse <- read_dta("data/hse16_eul_v5.dta")

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
    mmetHr_totalpa_hse = time_totalpa * 4,
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

# View
hse |> group_by(gender, age_group, imd) |> reframe(median_pa = median(mmetHr_totalpa_hse)) |> View()

