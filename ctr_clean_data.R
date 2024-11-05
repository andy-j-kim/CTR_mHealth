# Load necessary libraries
library(dplyr)
library(haven)
library(here)
library(lubridate)

### User can use here::here() to automatically set file path ----

# Load the dataset
data <- read_dta(paste0(here(),"/CTR UCSF_Oct 2017_Dec 2019.dta"))

# Drop rows where disposition equals 0 ----
data <- data %>%
  filter(disposition != 0)

# Check how many people are in the dataset
n_people <- nrow(data)
print(n_people)  # Should print 3580 after filtering

# Create hospitalSL variable
data <- data %>%
  mutate(hospitalSL = case_when(
    hospital == "HRL" ~ 1,
    hospital == "HRE" ~ 2,
    hospital == "HLD" ~ 3,
    hospital == "HCP" ~ 4,
    TRUE ~ NA_real_
  ))

# Define labels for hospitalSL
hospitalSL_labels <- c("HRL" = 1, "HRE" = 2, "HLD" = 3, "HCP" = 4)

# Create a labeled factor for hospitalSL
data$hospitalSL <- factor(data$hospitalSL, levels = c(1, 2, 3, 4), labels = names(hospitalSL_labels))

# Clone variables (create copies)
clone_vars <- c("i_mechanism", "i_loc", "i_activity", "i_alcohol", "i_rtirole", "i_intentperp",
                "inj_bruise_hn", "inj_sprain_hn", "inj_bullet_hn", "inj_lacsuper_hn",
                "fu_radio_xray", "fu_radio_ultra", "fu_radio_ct", "fu_radio_mri",
                "disposition")

for (var in clone_vars) {
  data[[paste0(var, "_SL")]] <- data[[var]]
}

# Clone variables for multiple injury types
injury_types <- c("inj_bruiseabrasion", "inj_sprainstrain", "inj_bullet", "inj_lacsuper",
                  "inj_lacdeep", "inj_avulsionamp", "inj_fractureclosed", "inj_fractureopen",
                  "inj_dislocation", "inj_burn", "inj_visceraexpose", "inj_hematoma",
                  "inj_neurodef", "inj_diminishpulse")

for (injury in injury_types) {
  for (i in 1:12) {
    data[[paste0(injury, "___", i, "_SL")]] <- data[[paste0(injury, "___", i)]]
  }
}

# Save the modified dataset if needed
# write_dta(data, "/path/to/save/modified_dataset.dta")

# Display the first few rows of the modified dataset
print(head(data))

# Renaming variables
data <- data %>%
  rename(airway_intervention_air = airway_intervention___noninvasiv)

# Referral versus local
data <- data %>%
  mutate(referral = ifelse(hospitalSL == 3, 1, 0),
         local = ifelse(hospitalSL != 3, 1, 0))

# HEAD CT
data <- data %>%
  mutate(headct = ifelse(radio_ctscan_head %in% 1:3, 1, 0),
         headctrecnotperf = ifelse(radio_ctscan_head == 0, 1, 0))

# Transfer Out
data <- data %>%
  mutate(transferout = ifelse(disposition == 6, 1, 0))

# TBI
data <- data %>%
  mutate(gcs_SL = gcs_eyes + gcs_motor + gcs_verbal,
         tbi_SL = ifelse(gcs_SL < 15 | hncs_heais > 0 | severity_ind___unconscious == 1, 1, 0),
         mild_SL = ifelse(gcs_SL > 12 & gcs_SL < 16, 1, 0),
         moderate_SL = ifelse(gcs_SL > 8 & gcs_SL < 13, 1, 0),
         severe_SL = ifelse(gcs_SL > 2 & gcs_SL < 9, 1, 0))

# Time to variables
# Convert to POSIXct
data <- data %>%
  mutate(arrivaldate_n = as.POSIXct(arrivaldate, format = "%m/%d/%Y %H:%M"),
         admit_time_only = arrivaldate_n - lubridate::as.duration(as.numeric(difftime(arrivaldate_n, dofc(arrivaldate_n), units = "mins")) * 60))

# Time adjustments for fluid time
data <- data %>%
  mutate(fluidtime_only = treat_fluidtime - lubridate::as.duration(as.numeric(difftime(treat_fluidtime, dofc(treat_fluidtime), units = "mins")) * 60),
         admit_time_only = ifelse(!is.na(fluidtime_only) & (fluidtime_only > 780 & (admit_time_only > 780 | admit_time_only < 720)) | 
                                    (fluidtime_only < 180 & admit_time_only > 660), 
                                  admit_time_only + (12 * 60 * 60), 
                                  admit_time_only),
         deltatime_fluid = fluidtime_only - admit_time_only,
         deltatime_fluid = ifelse(deltatime_fluid > -(60 * 60) & deltatime_fluid < 0, NA, deltatime_fluid),
         deltatime_fluid = deltatime_fluid + (24 * 60 * 60) ifelse(deltatime_fluid < 0))

# Combine blood time
data <- data %>%
  mutate(combined_bloodtime = ifelse(is.na(treat_bloodtime), bloodinitiatedtime, treat_bloodtime),
         blood_time = combined_bloodtime - lubridate::as.duration(as.numeric(difftime(combined_bloodtime, dofc(combined_bloodtime), units = "mins")) * 60),
         delta_blood = blood_time - admit_time_only,
         delta_blood = ifelse(delta_blood > -(240 * 60) & delta_blood < 0, NA, delta_blood),
         delta_blood = delta_blood + (24 * 60 * 60) ifelse(delta_blood < 0))

# Times for CT and Consults
# Head CT time
data <- data %>%
  mutate(ct_time = radio_ctscan_headperformed - lubridate::as.duration(as.numeric(difftime(radio_ctscan_headperformed, dofc(radio_ctscan_headperformed), units = "mins")) * 60),
         delta_ct = ct_time - admit_time_only,
         delta_ct = ifelse(delta_ct > -(240 * 60) & delta_ct < 0, NA, delta_ct),
         delta_ct = delta_ct + (24 * 60 * 60) ifelse(delta_ct < 0))

# Consult times
data <- data %>%
  mutate(consultcall_time = consult_neurocalled - lubridate::as.duration(as.numeric(difftime(consult_neurocalled, dofc(consult_neurocalled), units = "mins")) * 60),
         delta_consultcall = consultcall_time - admit_time_only,
         delta_consultcall = ifelse(delta_consultcall > -(240 * 60) & delta_consultcall < 0, NA, delta_consultcall),
         delta_consultcall = delta_consultcall + (24 * 60 * 60) ifelse(delta_consultcall < 0))

# Presentation time
data <- data %>%
  mutate(prestime = admit_date_only - injurydate,
         prestime = ifelse(prestime < 0, NA, prestime))

# Disposition time
data <- data %>%
  mutate(dispotime = dispo_time - lubridate::as.duration(as.numeric(difftime(dispo_time, dofc(dispo_time), units = "mins")) * 60),
         deltadispo = dispotime - admit_time_only,
         deltadispo = ifelse(deltadispo > -(240 * 60) & deltadispo < 0, NA, deltadispo),
         deltadispo = deltadispo + (24 * 60 * 60) ifelse(deltadispo < 0))

# Death variable for superlearner project
data <- data %>%
  mutate(deathSL = ifelse(!is.na(disposition) | !is.na(fu_disposition_condition), 0,
                          ifelse(disposition == 4 | fu_disposition_condition == 4, 1, NA)))

# Renaming disposition variables
data <- data %>%
  rename(disposition_2017_19 = disposition,
         fu_disposition_condition_2017_19 = fu_disposition_condition) %>%
  filter(!is.na(deathSL))

# Dropping unnecessary columns
data <- data %>%
  select(-ctr_page_1_timestamp, -ctr_page_2_timestamp, -ctr_page_3_timestamp)

# Saving the data
save(data, file = "/Users/ahayashi/Desktop/CTR_SL_2017_2019_Updated_05022023.RData")

# Load next dataset
data2 <- read.dta("/Users/ahayashi/Desktop/CTR Datasets-Original/STATA/CTR UCLA (Jan 2020 to Aug 2022).dta")

# Creating hospitalSL variable
data2 <- data2 %>%
  mutate(hospitalSL = case_when(
    hospital == "HRL" ~ 1,
    hospital == "HRE" ~ 2,
    hospital == "HLD" ~ 3,
    hospital == "HCP" ~ 4,
    TRUE ~ NA_real_
  ))

# Labeling hospitalSL variable
data2 <- data2 %>%
  mutate(hospitalSL = factor(hospitalSL, levels = 1:4, labels = c("HRL", "HRE", "HLD", "HCP")))

# Cloning variables
vars_to_clone <- c("i_mechanism", "i_loc", "i_activity", "i_alcohol", "i_rtirole", 
                   "i_intentperp", "inj_bruise_hn", "inj_sprain_hn", "inj_bullet_hn", 
                   "inj_lacsuper_hn", "fu_radio_xray", "fu_radio_ultra", "fu_radio_ct", 
                   "fu_radio_mri", "disposition")

for (var in vars_to_clone) {
  data2[[paste0(var, "_SL")]] <- data2[[var]]
}

# Clone variables with suffix
injury_vars <- c("inj_bruiseabrasion", "inj_sprainstrain", "inj_bullet", "inj_lacsuper", 
                 "inj_lacdeep", "inj_avulsionamp", "inj_fractureclosed", "inj_fractureopen", 
                 "inj_dislocation", "inj_burn", "inj_visceraexpose", "inj_hematoma", 
                 "inj_neurodef")

for (var in injury_vars) {
  for (i in 1:12) {
    data2[[paste0(var, "___", i, "_SL")]] <- data2[[paste0(var, "___", i)]]
  }
}

# Clone variables
inj_diminishpulse___1_SL <- inj_diminishpulse___1
inj_diminishpulse___2_SL <- inj_diminishpulse___2
inj_diminishpulse___3_SL <- inj_diminishpulse___3
inj_diminishpulse___4_SL <- inj_diminishpulse___4
inj_diminishpulse___5_SL <- inj_diminishpulse___5
inj_diminishpulse___6_SL <- inj_diminishpulse___6
inj_diminishpulse___7_SL <- inj_diminishpulse___7
inj_diminishpulse___8_SL <- inj_diminishpulse___8
inj_diminishpulse___9_SL <- inj_diminishpulse___9
inj_diminishpulse___10_SL <- inj_diminishpulse___10
inj_diminishpulse___11_SL <- inj_diminishpulse___11
inj_diminishpulse___12_SL <- inj_diminishpulse___12

# Rename variable
colnames(data)[which(colnames(data) == "airway_intervention___noninvasiv")] <- "airway_intervention___air"

# Referral versus local
data$referral <- ifelse(data$hospitalSL == 3, 1, 0)
data$local <- ifelse(data$hospitalSL != 3, 1, 0)

# HEAD CT
data$headct <- ifelse(data$radio_ctscan_head %in% c(1, 2, 3), 1, 0)

data$headctrecnotperf <- ifelse(data$radio_ctscan_head == 0, 1, 0)

# Transfer Out
data$transferout <- ifelse(data$disposition == 6, 1, 0)

# TBI
data$gcs_SL <- data$gcs_eyes + data$gcs_motor + data$gcs_verbal

data$tbi_SL <- ifelse(data$gcs_SL < 15 | data$hncs_heais > 0 | data$severity_ind___unconscious == 1, 1, 0)

data$mild_SL <- ifelse(data$gcs_SL > 12 & data$gcs_SL < 16, 1, 0)

data$moderate_SL <- ifelse(data$gcs_SL > 8 & data$gcs_SL < 13, 1, 0)

data$severe_SL <- ifelse(data$gcs_SL > 2 & data$gcs_SL < 9, 1, 0)

# Load necessary libraries
library(lubridate)

# Convert arrival date from string to POSIXct
data$arrivaldate_n <- as.POSIXct(data$arrivaldate, format="%Y-%m-%d %H:%M")

# Extract time only from admission, no date
data$admit_time_only <- data$arrivaldate_n - as.numeric(as.Date(data$arrivaldate_n))

data$admit_date_only <- as.Date(data$arrivaldate_n)

# FLUIDS
# Convert treat_fluidtime from string to numeric
data$treat_fluidtime_n <- as.numeric(as.POSIXct(data$treat_fluidtime, format="%H:%M")) - as.numeric(as.POSIXct("1960-01-01 00:00"))

# Time to fluids
data$deltatime_fluid <- data$treat_fluidtime_n - data$admit_time_only
data$deltatime_fluid1 <- data$deltatime_fluid / (60 * 1000)  # convert to minutes

# BLOOD
data$treat_bloodtime_n <- as.numeric(as.POSIXct(data$treat_bloodtime, format="%H:%M")) - as.numeric(as.POSIXct("1960-01-01 00:00"))
data$bloodinitiatedtime_n <- as.numeric(as.POSIXct(data$bloodinitiatedtime, format="%H:%M")) - as.numeric(as.POSIXct("1960-01-01 00:00"))

data$combined_bloodtime <- ifelse(is.na(data$treat_bloodtime_n), data$bloodinitiatedtime_n, data$treat_bloodtime_n)

# Extract time from blood
data$delta_blood <- data$combined_bloodtime - data$admit_time_only
data$delta_blood1 <- data$delta_blood / (60 * 1000)  # convert to minutes

# HEAD CT
data$ct_time <- as.numeric(as.POSIXct(data$radio_ctscan_headperformed, format="%H:%M"))
data$delta_ct <- data$ct_time - data$admit_time_only
data$delta_ct1 <- data$delta_ct / (60 * 1000)  # convert to minutes

# CONSULT CALLED
data$consultcall_time <- as.numeric(as.POSIXct(data$consult_neurocalled, format="%H:%M"))
data$delta_consultcall <- data$consultcall_time - data$admit_time_only
data$delta_consultcall1 <- data$delta_consultcall / (60 * 1000)  # convert to minutes

# CONSULT ARRIVED
data$consultarr_time <- as.numeric(as.POSIXct(data$consult_neuroarrived, format="%H:%M"))
data$delta_consultarr <- data$consultarr_time - data$admit_time_only
data$delta_consultarr1 <- data$delta_consultarr / (60 * 1000)  # convert to minutes

# PRESENTATION
data$prestime <- as.numeric(data$admit_date_only - as.Date(data$injurydate, format="%Y-%m-%d"))
data$prestime[data$prestime < 0] <- NA

# DISPOSITION
data$dispotime <- as.numeric(as.POSIXct(data$dispo_time, format="%H:%M"))
data$deltadispo <- data$dispotime - data$admit_time_only
data$deltadispo1 <- data$deltadispo / (60 * 1000)  # convert to minutes

# DEATH variable for superlearner project
data$deathSL <- NA
data$deathSL[data$disposition != "" & data$fu_disposition_condition != ""] <- 0
data$deathSL[data$disposition == 4 | data$fu_disposition_condition == 4] <- 1

# Disposition variable for superlearner project
names(data)[names(data) == "disposition"] <- "disposition_2020_22"
names(data)[names(data) == "fu_disposition_condition"] <- "fu_disposition_condition_2020_22"

# Drop if deathSL is missing
data <- data[!is.na(data$deathSL), ]

# Drop unwanted columns
data <- data[, !names(data) %in% c("ctr_page_1_timestamp", "ctr_page_2_timestamp", "ctr_page_3_timestamp")]

# Save the data frame to a .rds file (R's equivalent of .dta)
saveRDS(data, "/Users/ahayashi/Desktop/CTR_SL_2020_2022_Updated_05022023.rds")

# Load necessary libraries
library(lubridate)
library(dplyr)

# Load the dataset
data <- read.dta("/Users/ahayashi/Desktop/CTR Datasets-Original/STATA/CCTR UCLA (Aug 2022 - Jan 23 2023).dta")

# Count the number of rows (people in dataset)
n_people <- nrow(data)
cat("Number of people in dataset:", n_people, "\n")

# SL Variables: Referral versus local
data <- data %>%
  mutate(
    referral = ifelse(hospitalSL == 3, 1, 0),
    local = ifelse(hospitalSL != 3, 1, 0)
  )

# HEAD CT
data <- data %>%
  mutate(
    headct = ifelse(radio_ctscan_head %in% c(1, 2, 3), 1, 0),
    headctrecnotperf = ifelse(radio_ctscan_head %in% c(0, 1, 2, 3), 0, NA),
    headctrecnotperf = ifelse(radio_ctscan_head == 0, 1, headctrecnotperf)
  )

# TBI
data <- data %>%
  mutate(
    gcs_SL = gcs_eyes + gcs_motor + gcs_verbal,
    tbi_SL = ifelse(gcs_SL < 15 | hncs_heais > 0 | severity_ind___unconscious == 1, 1, 0),
    mild_SL = ifelse(gcs_SL > 12 & gcs_SL < 16, 1, 0),
    moderate_SL = ifelse(gcs_SL > 8 & gcs_SL < 13, 1, 0),
    severe_SL = ifelse(gcs_SL > 2 & gcs_SL < 9, 1, 0)
  )

# Time to variables
# Convert arrival time and date
data <- data %>%
  mutate(
    admit_time_only = as.numeric(hms::as_hms(arrivaltime)),
    arrivaldate_n = as.Date(arrivaldate, format="%Y-%m-%d")
  )

# FLUIDS
data <- data %>%
  mutate(
    treat_fluidtime_n = as.numeric(hms::as_hms(treat_colloidtime)),
    deltatime_fluid = treat_fluidtime_n - admit_time_only,
    deltatime_fluid1 = deltatime_fluid / 60  # convert to minutes
  )

# BLOOD
data <- data %>%
  mutate(
    treat_bloodtime_n = as.numeric(hms::as_hms(treat_bloodtime)),
    bloodinitiatedtime_n = as.numeric(hms::as_hms(bloodinitiatedtime)),
    combined_bloodtime = ifelse(is.na(treat_bloodtime_n), bloodinitiatedtime_n, treat_bloodtime_n),
    delta_blood = combined_bloodtime - admit_time_only,
    delta_blood1 = delta_blood / 60  # convert to minutes
  )

# HEAD CT time calculations
data <- data %>%
  mutate(
    ct_time = as.numeric(hms::as_hms(radio_ctscan_headperformed)),
    delta_ct = ct_time - admit_time_only,
    delta_ct1 = delta_ct / 60  # convert to minutes
  )

# CONSULT CALLED
data <- data %>%
  mutate(
    consultcall_time = as.numeric(hms::as_hms(consult_neurocalled)),
    delta_consultcall = consultcall_time - admit_time_only,
    delta_consultcall1 = delta_consultcall / 60  # convert to minutes
  )

# CONSULT ARRIVED
data <- data %>%
  mutate(
    consultarr_time = as.numeric(hms::as_hms(consult_neuroarrived)),
    delta_consultarr = consultarr_time - admit_time_only,
    delta_consultarr1 = delta_consultarr / 60  # convert to minutes
  )

# PRESENTATION
data <- data %>%
  mutate(
    prestime = as.numeric(arrivaldate_n - as.Date(injurydate, format="%Y-%m-%d")),
    prestime = ifelse(prestime < 0, NA, prestime)
  )

# DISPOSITION
data <- data %>%
  mutate(
    dispotime = as.numeric(hms::as_hms(dispo_time)),
    deltadispo = dispotime - admit_time_only,
    deltadispo1 = deltadispo / 60  # convert to minutes
  )

# Death variable for superlearner project
data <- data %>%
  mutate(
    deathSL = NA,
    deathSL = ifelse(!is.na(disposition) & !is.na(fu_disposition_condition), 0, deathSL),
    deathSL = ifelse(disposition == 4 | fu_disposition_condition == 4, 1, deathSL)
  )

# Renaming disposition variables
data <- data %>%
  rename(
    disposition_2022_22 = disposition,
    fu_disposition_condition_2022_23 = fu_disposition_condition
  )

# Drop if deathSL is missing
data <- data %>% filter(!is.na(deathSL))

# Drop unwanted columns
data <- data %>% select(-ctr_page_1_timestamp, -ctr_page_2_timestamp, -ctr_page_3_timestamp)

# Save the cleaned dataset
saveRDS(data, "/Users/ahayashi/Desktop/CTR_SL_2022_2023_Updated_05022023.rds")

# Count remaining rows
cat("Remaining number of entries:", nrow(data), "\n")

# Load the first dataset
data1 <- readRDS("/Users/ahayashi/Desktop/CTR_SL_2022_2023_Updated_05022023.rds")

# Load the second dataset
data2 <- readRDS("/Users/ahayashi/Desktop/CTR_SL_2020_2022_Updated_05022023.rds")

# Append the datasets
combined_data <- bind_rows(data1, data2)

# Save the combined dataset
saveRDS(combined_data, "/Users/ahayashi/Desktop/Combined_CTR_SL_2020_2023.rds")

# Optionally, count the number of entries in the combined dataset
cat("Total number of entries in combined dataset:", nrow(combined_data), "\n")

# Load the two datasets
data_2017_2022 <- read_dta("/Users/ahayashi/Desktop/CTR_SL_2017_2022_Appended_05022023.dta")
data_2022_2023 <- read_dta("/Users/ahayashi/Desktop/CTR_SL_2022_2023_Updated_05022023.dta")

# Append the datasets (force append: ignoring conflicts in column names and types)
appended_data <- bind_rows(data_2017_2022, data_2022_2023)

# Save the combined dataset back to .dta format
write_dta(appended_data, "/Users/ahayashi/Desktop/CTR_SL_2017_2022_Appended_05022023.dta")

# Assuming you have the final dataset already loaded (or after appending as in the previous code)
# Save the dataset
write_dta(appended_data, "/Users/ahayashi/Desktop/CTR_SL_2017_2023_Final_05022023_TotalPop.dta")

# Count the number of rows
n_rows <- nrow(appended_data)
print(n_rows)

library(haven)
library(dplyr)

# Load the dataset
data <- read_dta("/Users/ahayashi/Desktop/CTR_SL_2017_2023_Final_05022023_TotalPop.dta")

# Define gcs
data <- data %>%
  mutate(gcs = gcs_eyes + gcs_motor + gcs_verbal)

# Define TBI, mild, moderate, and severe categories
data <- data %>%
  mutate(tbi = ifelse(gcs < 15 | hncs_heais > 0 | severity_ind___unconscious == 1, 1, NA),
         mild = ifelse(gcs > 12 & gcs < 16, 1, NA),
         moderate = ifelse(gcs > 8 & gcs < 13, 1, NA),
         severe = ifelse(gcs > 2 & gcs < 9, 1, NA))

# Save the updated data
write_dta(data, "/Users/ahayashi/Desktop/CTR_SL_2017_2023_Final_05022023_TotalPop.dta")

# Drop rows where hospitalSL is missing
data <- data %>% filter(!is.na(hospitalSL))

# Save after dropping
write_dta(data, "/Users/ahayashi/Desktop/SL FINAL FILES/Appended/CTR_SL_2017_2023_Final_05022023_TotalPop_hospitaldropped.dta")

# Filter only TBI population
data_tbi <- data %>% filter(tbi == 1 & !is.na(gcs))

# Save TBI only data
write_dta(data_tbi, "/Users/ahayashi/Desktop/CTR_SL_2017_2023_TBIonly_hospitaldropped.dta")

# Consolidation of variables
data_tbi <- data_tbi %>%
  mutate(
    fu_abd_severity___SL = case_when(
      fu_abd_severity___1 == 1 | fu_abd_severity == 1 ~ 1,
      fu_abd_severity___2 == 1 | fu_abd_severity == 2 ~ 2,
      fu_abd_severity___3 == 1 | fu_abd_severity == 3 ~ 3,
      fu_abd_severity___4 == 1 | fu_abd_severity == 4 ~ 4,
      fu_abd_severity___5 == 1 | fu_abd_severity == 5 ~ 5,
      fu_abd_severity___6 == 1 | fu_abd_severity == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    fu_chest_severity___SL = case_when(
      fu_chest_severity___1 == 1 | fu_chest_severity == 1 ~ 1,
      fu_chest_severity___2 == 1 | fu_chest_severity == 2 ~ 2,
      fu_chest_severity___3 == 1 | fu_chest_severity == 3 ~ 3,
      fu_chest_severity___4 == 1 | fu_chest_severity == 4 ~ 4,
      fu_chest_severity___5 == 1 | fu_chest_severity == 5 ~ 5,
      fu_chest_severity___6 == 1 | fu_chest_severity == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    fu_extrem_severity___SL = case_when(
      fu_extrem_severity___1 == 1 | fu_extrem_severity == 1 ~ 1,
      fu_extrem_severity___2 == 1 | fu_extrem_severity == 2 ~ 2,
      fu_extrem_severity___3 == 1 | fu_extrem_severity == 3 ~ 3,
      fu_extrem_severity___4 == 1 | fu_extrem_severity == 4 ~ 4,
      fu_extrem_severity___5 == 1 | fu_extrem_severity == 5 ~ 5,
      fu_extrem_severity___6 == 1 | fu_extrem_severity == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    fu_face_severity___SL = case_when(
      fu_face_severity___1 == 1 | fu_face_severity == 1 ~ 1,
      fu_face_severity___2 == 1 | fu_face_severity == 2 ~ 2,
      fu_face_severity___3 == 1 | fu_face_severity == 3 ~ 3,
      fu_face_severity___4 == 1 | fu_face_severity == 4 ~ 4,
      fu_face_severity___5 == 1 | fu_face_severity == 5 ~ 5,
      fu_face_severity___6 == 1 | fu_face_severity == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    fu_general_severity___SL = case_when(
      fu_general_severity___1 == 1 | fu_general_severity == 1 ~ 1,
      fu_general_severity___2 == 1 | fu_general_severity == 2 ~ 2,
      fu_general_severity___3 == 1 | fu_general_severity == 3 ~ 3,
      fu_general_severity___4 == 1 | fu_general_severity == 4 ~ 4,
      fu_general_severity___5 == 1 | fu_general_severity == 5 ~ 5,
      fu_general_severity___6 == 1 | fu_general_severity == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    fu_head_severity___SL = case_when(
      fu_head_severity___1 == 1 | fu_head_severity == 1 ~ 1,
      fu_head_severity___2 == 1 | fu_head_severity == 2 ~ 2,
      fu_head_severity___3 == 1 | fu_head_severity == 3 ~ 3,
      fu_head_severity___4 == 1 | fu_head_severity == 4 ~ 4,
      fu_head_severity___5 == 1 | fu_head_severity == 5 ~ 5,
      fu_head_severity___6 == 1 | fu_head_severity == 6 ~ 6,
      TRUE ~ NA_real_
    ),
    tbi_severity_SL = case_when(
      mild == 1 ~ 1,
      moderate == 1 ~ 2,
      severe == 1 ~ 3,
      TRUE ~ NA_real_
    )
  )

# Save the final dataset
write_dta(data_tbi, "/Users/ahayashi/Desktop/CTR_SL_2017_2023_TBIonly_hospitaldropped_01172024.dta")


