# Load necessary libraries
library(haven)   # For reading .dta files
library(dplyr)   # For data manipulation
library(lubridate) # For date and time manipulation
library(here)    # For directory ease
library(tidyr)     # For data tidying
library(purrr)     # For functional programming
library(stringr)   # For string manipulation

# Set file paths for input and output files using the 'here' package
input_file1 <- here("data","CTR_UCSF_Oct_2017_Dec_2019.csv")
input_file2 <- here("data", "CTR_UCLA_Jan_2020_Aug_2022.csv")
input_file3 <- here("data", "CTR_UCLA_Aug_2022_Jan_23_2023.csv")

output_file1 <- here("data","CTR_SL_2017_2019_Updated.rds")
output_file2 <- here("data","CTR_SL_2020_2022_Updated.rds")
output_file3 <- here("data","CTR_SL_2022_2023_Updated.rds")
output_file_appended <- here("data","CTR_SL_2017_2023_Final_05022023_TotalPop.rds")
output_file_tbi_only <- here("data","CTR_SL_2017_2023_TBIonly_hospitaldropped.rds")
output_file_consolidated <- here("data","CTR_SL_2017_2023_TBIonly_hospitaldropped_consolidated.rds")

# Define a function to process each dataset
process_dataset <- function(data, period) {
  # Drop if discharged home to make congruous and mergeable with restricted registries
  data <- data %>% filter(disposition != 0)
  
  # Map hospital names to codes
  data <- data %>%
    mutate(
      hospitalSL = case_when(
        hospital == "HRL" ~ 1,
        hospital == "HRE" ~ 2,
        hospital == "HLD" ~ 3,
        hospital == "HCP" ~ 4,
        TRUE ~ NA_real_
      ),
      # Create labels for hospitalSL (optional, for your reference)
      hospitalSL_label = factor(hospitalSL, levels = 1:4, labels = c("HRL", "HRE", "HLD", "HCP"))
    )
  
  # Clone variables with _SL suffix
  variables_to_clone <- c(
    "i_loc", "i_activity", "i_alcohol", "i_rtirole", "i_intentperp",
    "inj_bruise_hn", "inj_sprain_hn", "inj_bullet_hn", "inj_lacsuper_hn",
    "fu_radio_xray", "fu_radio_ultra", "fu_radio_ct", "fu_radio_mri", "disposition"
  )
  
  data <- data %>%
    mutate(across(all_of(variables_to_clone), .names = "{.col}_SL"))
  
  # Clone variables with specific prefixes and numbers
  prefixes <- c(
    "inj_bruiseabrasion___", "inj_sprainstrain___", "inj_bullet___",
    "inj_lacsuper___", "inj_lacdeep___", "inj_avulsionamp___",
    "inj_fractureclosed___", "inj_fractureopen___", "inj_dislocation___",
    "inj_burn___", "inj_visceraexpose___", "inj_hematoma___",
    "inj_neurodef___", "inj_diminishpulse___"
  )
  
  for (prefix in prefixes) {
    vars_to_clone <- names(data)[grepl(paste0("^", prefix), names(data))]
    data <- data %>%
      mutate(across(all_of(vars_to_clone), .names = "{.col}_SL"))
  }
  
  # Rename variable
  data <- data %>%
    rename(airway_intervention___air = airway_intervention___noninvasive_air)
  
  # Create referral and local variables
  data <- data %>%
    mutate(
      referral = ifelse(hospitalSL == 3, 1, 0),
      local = ifelse(hospitalSL != 3, 1, 0)
    )
  
  # Create head CT variables
  data <- data %>%
    mutate(
      headct = ifelse(radio_ctscan_head %in% c(1, 2, 3), 1, 0),
      headctrecnotperf = case_when(
        radio_ctscan_head == 0 ~ 1,
        radio_ctscan_head %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Create transfer out variable
  data <- data %>%
    mutate(
      transferout = ifelse(disposition == 6, 1, 0)
    )
  
  # Create TBI variables
  data <- data %>%
    mutate(
      gcs_SL = gcs_eyes + gcs_motor + gcs_verbal,
      tbi_SL = ifelse(gcs_SL < 15 | hncs_heais > 0 | severity_ind___unconscious == 1, 1, NA_real_),
      mild_SL = ifelse(gcs_SL > 12 & gcs_SL < 16, 1, NA_real_),
      moderate_SL = ifelse(gcs_SL > 8 & gcs_SL < 13, 1, NA_real_),
      severe_SL = ifelse(gcs_SL > 2 & gcs_SL < 9, 1, NA_real_)
    )
  
  # Create deathSL variable
  data <- data %>%
    mutate(
      deathSL = case_when(
        disposition == 4 | fu_disposition_condition == 4 ~ 1,
        (!is.na(disposition) & disposition != 4) | (!is.na(fu_disposition_condition) & fu_disposition_condition != 4) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Rename disposition variables to keep track of different periods
  data <- data %>%
    rename(
      !!paste0("disposition_", period) := disposition,
      !!paste0("fu_disposition_condition_", period) := fu_disposition_condition
    )
  
  # Drop observations with missing deathSL
  data <- data %>% filter(!is.na(deathSL))
  
  # Drop unnecessary timestamp variables
  data <- data %>% select(-starts_with("ctr_page"))
  
  return(data)
}

process_ds3 <- function(ds3){
  ds3 <- ds3 %>%
    mutate(
      referral = ifelse(hospital == 3, 1,
                        ifelse(!is.na(hospital), 0, NA_real_)),
      local = ifelse(hospital != 3, 1,
                     ifelse(hospital == 3, 0, NA_real_))
    )
  
  # **HEAD CT
  ds3 <- ds3 %>%
    mutate(
      headct = ifelse(radio_ctscan_head %in% c(1, 2, 3), 1, 0),
      headctrecnotperf = case_when(
        radio_ctscan_head == 0 ~ 1,
        radio_ctscan_head %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # ****************************************
  # **************** TBI *******************
  # ****************************************
  
  # Calculate GCS
  ds3 <- ds3 %>%
    mutate(
      gcs_SL = gcs_eyes + gcs_motor + gcs_verbal
    )
  
  # Create TBI variables
  ds3 <- ds3 %>%
    mutate(
      #### severity_ind___2 is unconscious
      tbi_SL = ifelse(gcs_SL < 15 | hncs_heais > 0 | severity_ind___2 == 1, 1, NA_real_),
      mild_SL = ifelse(gcs_SL > 12 & gcs_SL < 16, 1, NA_real_),
      moderate_SL = ifelse(gcs_SL > 8 & gcs_SL < 13, 1, NA_real_),
      severe_SL = ifelse(gcs_SL > 2 & gcs_SL < 9, 1, NA_real_)
    )
  
  # *********************************
  # ******* Time to variables *******
  # *********************************
  
  # Convert arrival time from string to POSIXct (time only)
  ds3 <- ds3 %>%
    mutate(
      admit_time_only = parse_date_time(arrivaltime, orders = "HM", tz = "UTC")
    )
  
  # Convert arrival date from string to Date
  ds3 <- ds3 %>%
    mutate(
      arrivaldate_n = as.Date(arrivaldate, format = "%Y-%m-%d")
    )
  
  # **FLUIDS
  
  # Convert 'treat_colloidtime' to POSIXct
  ds3 <- ds3 %>%
    mutate(
      treat_fluidtime_n = parse_date_time(treat_colloidtime, orders = "HM", tz = "UTC")
    )
  
  # Calculate time to fluids
  ds3 <- ds3 %>%
    mutate(
      deltatime_fluid = as.numeric(difftime(treat_fluidtime_n, admit_time_only, units = "mins")),
      # Adjust for negative times (crossing midnight)
      deltatime_fluid = ifelse(deltatime_fluid < 0, deltatime_fluid + 1440, deltatime_fluid),
      deltatime_fluid1 = deltatime_fluid
    )
  
  # **BLOOD
  
  # Assuming your data is in a data frame called 'data'
  
  # Define the base date to match Stata's origin date
  base_date <- as.Date("1960-01-01")
  
  # Create 'combined_bloodtime' as the first non-missing value
  ds3 <- ds3 %>%
    mutate(
      combined_bloodtime = as.POSIXct(paste(base_date, coalesce(treat_bloodtime, bloodinitiatedtime)), format = "%Y-%m-%d %H:%M", tz = "UTC")
    )
  
  # Ensure 'admit_time_only' is parsed as POSIXct on 'base_date'
  ds3 <- ds3 %>%
    mutate(
      admit_time_only = as.POSIXct(paste(base_date, arrivaltime), format = "%Y-%m-%d %H:%M", tz = "UTC")
    )
  
  # Compute 'delta_blood' as the difference in minutes
  ds3 <- ds3 %>%
    mutate(
      delta_blood = as.numeric(difftime(combined_bloodtime, admit_time_only, units = "mins")),
      # Adjust negative differences by adding 1440 minutes (24 hours)
      delta_blood = ifelse(delta_blood < 0, delta_blood + 1440, delta_blood),
      delta_blood1 = delta_blood
    )
  
  # **HEAD CT
  
  # Extract time from Head CT
  ds3 <- ds3 %>%
    mutate(
      ct_time = parse_date_time(radio_ctscan_headperformed, orders = "HM", tz = "UTC"),
      delta_ct = as.numeric(difftime(ct_time, admit_time_only, units = "mins")),
      delta_ct = ifelse(delta_ct < 0, delta_ct + 1440, delta_ct),
      delta_ct1 = delta_ct
    )
  
  # Define the base date
  base_date <- as.Date("1960-01-01")
  
  # Ensure 'admit_time_only' is parsed as POSIXct datetime
  ds3 <- ds3 %>%
    mutate(
      admit_time_only = as.POSIXct(
        paste(base_date, arrivaltime),
        format = "%Y-%m-%d %H:%M",
        tz = "UTC"
      )
    )
  
  # **CONSULT CALLED
  ds3 <- ds3 %>%
    mutate(
      consultcall_time = as.POSIXct(
        paste(base_date, consult_neurocalled),
        format = "%Y-%m-%d %H:%M",
        tz = "UTC"
      ),
      delta_consultcall = as.numeric(
        difftime(consultcall_time, admit_time_only, units = "mins")
      ),
      delta_consultcall = ifelse(delta_consultcall < 0, delta_consultcall + 1440, delta_consultcall),
      delta_consultcall1 = delta_consultcall
    )
  
  # **CONSULT ARRIVED
  ds3 <- ds3 %>%
    mutate(
      consultarr_time = as.POSIXct(
        paste(base_date, consult_neuroarrived),
        format = "%Y-%m-%d %H:%M",
        tz = "UTC"
      ),
      delta_consultarr = as.numeric(
        difftime(consultarr_time, admit_time_only, units = "mins")
      ),
      delta_consultarr = ifelse(delta_consultarr < 0, delta_consultarr + 1440, delta_consultarr),
      delta_consultarr1 = delta_consultarr
    )
  
  # **PRESENTATION
  
  # Convert injury date to Date
  ds3 <- ds3 %>%
    mutate(
      injurydate_n = as.Date(injurydate, format = "%Y-%m-%d"),
      prestime = as.numeric(difftime(arrivaldate_n, injurydate_n, units = "days")),
      prestime = ifelse(prestime < 0, NA_real_, prestime)
    )
  
  # **DISPOSITION
  
  # Extract time from disposition
  ds3 <- ds3 %>%
    mutate(
      dispotime = parse_date_time(dispo_time, orders = "HM", tz = "UTC"),
      deltadispo = as.numeric(difftime(dispotime, admit_time_only, units = "mins")),
      deltadispo = ifelse(deltadispo < 0, deltadispo + 1440, deltadispo),
      deltadispo1 = deltadispo
    )
  
  # *Death variable for superlearner project
  
  ds3 <- ds3 %>%
    mutate(
      deathSL = case_when(
        disposition == 4 | fu_disposition_condition == 4 ~ 1,
        !is.na(disposition) | !is.na(fu_disposition_condition) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # *Renaming disposition variables
  ds3 <- ds3 %>%
    rename(
      disposition_2022_22 = disposition,
      fu_disposition_condition_2022_23 = fu_disposition_condition
    )
  
  # *Drop if deathSL is missing
  ds3 <- ds3 %>%
    filter(!is.na(deathSL))
  
  # Drop timestamp variables
  ds3 <- ds3 %>%
    select(-ctr_page_1_timestamp, -ctr_page_2_timestamp, -ctr_page_3_timestamp)
  
  # Optional: View the distribution of deathSL
  table(ds3$deathSL, useNA = "ifany")
  
  return(ds3)
  
}

# Process each dataset
ds1 <- read.csv(input_file1)
ds1_processed <- process_dataset(ds1, "2017_19")
saveRDS(ds1_processed, output_file1)

ds2 <- read.csv(input_file2)
ds2_processed <- process_dataset(ds2, "2020_22")
saveRDS(ds2_processed, output_file2)

ds3 <- read.csv(input_file3)
ds3_processed <- process_ds3(ds3)
#ds3_processed <- process_dataset(ds3, "2022_23")
saveRDS(ds3_processed, output_file3)

### MODIFY AFTER THIS POINT

# ---------------------------------------------------------------
# ********************* Append Datasets *************************
# ---------------------------------------------------------------

# Function to align variable types
align_variable_types <- function(df1, df2) {
  # Get variable names and types
  vars_df1 <- sapply(df1, class)
  vars_df2 <- sapply(df2, class)
  
  # Find variables with mismatched types
  common_vars <- intersect(names(vars_df1), names(vars_df2))
  type_mismatches <- common_vars[vars_df1[common_vars] != vars_df2[common_vars]]
  
  # Coerce mismatched variables to character
  for (var in type_mismatches) {
    df1[[var]] <- as.character(df1[[var]])
    df2[[var]] <- as.character(df2[[var]])
  }
  
  return(list(df1 = df1, df2 = df2))
}

# Align variable types between main data and data_append1
aligned_data <- align_variable_types(ds1_processed, ds2_processed)
data_main_aligned <- aligned_data$df1
data_append1_aligned <- aligned_data$df2

# Append data_append1 to data_main
data_combined <- bind_rows(data_main_aligned, data_append1_aligned)

# Align variable types between data_combined and data_append2
aligned_data2 <- align_variable_types(data_combined, ds3_processed)
data_combined_aligned <- aligned_data2$df1
data_append2_aligned <- aligned_data2$df2

# Append data_append2 to data_combined
data_combined <- bind_rows(data_combined_aligned, data_append2_aligned)

# Save the final combined dataset
saveRDS(data_combined, output_file_appended)

# ---------------------------------------------------------------
# ************* Defining TBI in Total Population ****************
# ---------------------------------------------------------------

# Calculate GCS
data_combined <- data_combined %>%
  mutate(
    gcs = gcs_eyes + gcs_motor + gcs_verbal
  )

# Create TBI variables
data_combined <- data_combined %>%
  mutate(
    tbi = ifelse(gcs < 15 | hncs_heais > 0 | severity_ind___unconscious == 1, 1, NA_real_),
    mild = ifelse(gcs > 12 & gcs < 16, 1, NA_real_),
    moderate = ifelse(gcs > 8 & gcs < 13, 1, NA_real_),
    severe = ifelse(gcs > 2 & gcs < 9, 1, NA_real_)
  )

# ---------------------------------------------------------------
# ************** Drop Missing Hospitals and Deaths ***************
# ---------------------------------------------------------------

# Drop observations with missing 'hospitalSL'
data_combined <- data_combined %>%
  filter(!is.na(hospitalSL))

# Drop observations where 'disposition_SL' == 4
data_combined <- data_combined %>%
  filter(disposition_SL != 4)

# Drop observations where 'lifesigns' == 0
data_combined <- data_combined %>%
  filter(lifesigns != 0)

# ---------------------------------------------------------------
# **************** Only TBI Population **************************
# ---------------------------------------------------------------

# Keep only TBI cases with non-missing GCS
data_tbi <- data_combined %>%
  filter(tbi == 1 & !is.na(gcs))

# Save the TBI dataset
saveRDS(data_tbi, output_file_tbi_only)

# ---------------------------------------------------------------
# ************ Consolidation of Variables ***********************
# ---------------------------------------------------------------

# Function to consolidate severity variables
consolidate_severity <- function(data, prefix) {
  data %>%
    mutate(
      !!paste0(prefix, "___SL") := case_when(
        !!sym(paste0(prefix, "___1")) == 1 | !!sym(prefix) == 1 ~ 1,
        !!sym(paste0(prefix, "___2")) == 1 | !!sym(prefix) == 2 ~ 2,
        !!sym(paste0(prefix, "___3")) == 1 | !!sym(prefix) == 3 ~ 3,
        !!sym(paste0(prefix, "___4")) == 1 | !!sym(prefix) == 4 ~ 4,
        !!sym(paste0(prefix, "___5")) == 1 | !!sym(prefix) == 5 ~ 5,
        !!sym(paste0(prefix, "___6")) == 1 | !!sym(prefix) == 6 ~ 6,
        TRUE ~ NA_real_
      )
    )
}

# Apply the function to each severity variable
severity_vars <- c("fu_abd_severity", "fu_chest_severity", "fu_extrem_severity",
                   "fu_face_severity", "fu_general_severity", "fu_head_severity")

for (var in severity_vars) {
  data_tbi <- consolidate_severity(data_tbi, var)
}

# Create 'fu_site_inf___SL' variable
data_tbi <- data_tbi %>%
  mutate(
    fu_site_inf___SL = ifelse(
      fu_site_inf___deep_inf == 1 | fu_site_inf___hardware == 1 | fu_site_inf___super_inf == 1,
      1, NA_real_)
  )

# Create 'tbi_severity_SL' variable
data_tbi <- data_tbi %>%
  mutate(
    tbi_severity_SL = case_when(
      mild == 1 ~ 1,
      moderate == 1 ~ 2,
      severe == 1 ~ 3,
      TRUE ~ NA_real_
    )
  )

# Create 'scenecaregiven___other_SL' variable
data_tbi <- data_tbi %>%
  mutate(
    scenecaregiven___other_SL = ifelse(
      scenecaregiven___iv == 1 |
        scenecaregiven___other == 1 |
        scenecaregiven___recovery_pos == 1 |
        scenecaregiven___topical_burn == 1 |
        scenecaregiven___tourniquet == 1,
      1, NA_real_)
  )

# Create 'fu_ortho_surgery___other' variable
data_tbi <- data_tbi %>%
  mutate(
    fu_ortho_surgery___other = ifelse(
      fu_ortho_surgery___2 == 1 |
        fu_ortho_surgery___3 == 1 |
        fu_ortho_surgery___4 == 1 |
        fu_ortho_surgery___5 == 1,
      1, NA_real_)
  )

# Create 'dispo_payment___other_SL' variable
data_tbi <- data_tbi %>%
  mutate(
    dispo_payment___other_SL = ifelse(
      dispo_payment___ins == 1 |
        dispo_payment___ngo == 1 |
        dispo_payment___other == 1 |
        dispo_payment___unknown == 1,
      1, NA_real_)
  )

# Create 'transferreason___other_SL' variable
data_tbi <- data_tbi %>%
  mutate(
    transferreason___other_SL = ifelse(
      transferreason___inability_pay == 1 |
        transferreason___other == 1 |
        transferreason___patient_pref == 1,
      1, NA_real_)
  )

# Create 'airway_intervention___SL' variable
data_tbi <- data_tbi %>%
  mutate(
    airway_intervention___SL = ifelse(
      airway_intervention___air == 1 |
        airway_intervention___cricoth == 1 |
        airway_intervention___endotrach == 1 |
        airway_intervention___sctioning == 1,
      1, NA_real_)
  )


# Save the consolidated TBI-only dataset
saveRDS(data_tbi, output_file_consolidated)
