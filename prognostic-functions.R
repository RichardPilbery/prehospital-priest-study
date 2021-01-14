library(tidyverse)

# This file contains functions to calculate the following prognostic indicators:
# Core PRIEST triage tool (including an abbreviated version omitting performance status).

# CRB-65
# PMEWS
# NEWS2
# The WHO decision-making algorithm for hospitalisation with pneumonia

# df <- data.frame(
#   avpu =  c("Voice", NA, "Unresponsive", "Alert"),
#   gcs_v = c(4,NA,2,5),
#   gcs_tot = c(14,NA,12,15),
#   rr = c(44, 12, 6, 30),
#   sys_bp = c(108, 90, 140, 80),
#   dia_bp = c(90, 80, 70, 60),
#   age = c(66, 55, 77, 88),
#   pr = c(50,120,140, 85),
#   temp = c(36.9, 40.0, 35.0, 37.0),
#   sa02 = c(98, 95, 94, 88),
#   air_oxygen = c(TRUE, FALSE, FALSE, TRUE),
#   stringsAsFactors = F
# )

# CRB-65 ---------------------------------

#' Calculate CRB-65 score
#'
#' @param avpu Level of consciousness based on AVPU (alert, voice, pain, unresponsive) score
#' @param gcs_v Verbal score for Glasgow Coma Scale
#' @param gcs_tot Total score for Glasgow Coma Scale
#' @param rr Respiratory rate in breaths/min
#' @param sys_bp Systolic blood pressure in mmHg
#' @param dia_bp Diastolic blood pressure in mmHg
#' @param age Age in years
#' @return The CRB-65 score
#' @examples
#' crb65("Voice", 4, 14, 24, 104, 90, 85)
#' crb65("Alert", 5, 15, 12, 140, 80, 50)
crb65 <- function(avpu, gcs_v, gcs_tot, rr, sys_bp, dia_bp, age) {
  
# Four parameters: 
# 1.	Confusion: GCS-V is less than 4 or GCS total is less than 15 or AVPU is recorded as V, P or U
  # Note that confusion has been added since the score implies that anything less than alert should
  # be given a score of 1
# 2.	Respiratory: rate of 30 breaths per minute or more
# 3.	Blood pressure: diastolic BP is 60mmHg or less or systolic BP is 90 mmHg or less
# 4.	 Age: 65 years or more
  
  
  confusion <- case_when(
    gcs_v < 4 ~ 1,
    gcs_tot < 15 ~ 1,
    tolower(avpu) %in% c("voice", "pain", "unresponsive", "confusion") ~ 1,
    is.na(avpu) && is.na(gcs_tot) && is.na(tolower(avpu)) ~ NA_real_,
    TRUE ~ 0
  )
  
  respiratory <- case_when(
    rr >= 30 ~ 1,
    is.na(rr) ~ NA_real_,
    TRUE ~ 0
  )
  
  bp <- case_when(
    dia_bp <= 60 ~ 1,
    sys_bp <= 90 ~ 1,
    is.na(dia_bp) && is.na(sys_bp) ~ NA_real_,
    TRUE ~ 0
  )
  
  age_in_years <- case_when(
    age >= 65 ~ 1,
    is.na(age) ~ NA_real_,
    TRUE ~ 0
  )

  total <- confusion + respiratory + bp + age_in_years
  #print(glue::glue("Confusion: {confusion} Resp Rate: {respiratory} BP: {bp} Age: {age}"))
  return(total)
  
}


# PMEWS ---------------------------------

#' Calculate Pandemic Medical Early Warning Score (PMEWS)
#'
#' @param rr Respiratory rate in breaths/min
#' @param sao2 Pulse oximetry value in %
#' @param pr Pulse rate in beats/min
#' @param sys_bp Systolic blood pressure in mmHg
#' @param temp Temperature in celsius
#' @param avpu Level of consciousness based on AVPU (alert, voice, pain, unresponsive) score
#' @param gcs_v Verbal score for Glasgow Coma Scale
#' @param gcs_tot Total score for Glasgow Coma Scale
#' @return The PMEWS value
#' @examples
#' pmews(12, 98, 104, 140, 40, "Voice", 4, 14)
#' pmews(24, 88, 124, 90, 36.9, "Alert", 5, 15)
pmews <- function(rr, sa02, pr, sys_bp, temp, avpu, gcs_v, gcs_tot) {
  
  respiratory <- case_when(
    rr <= 8  || rr >= 30 ~ 3,
    rr >= 26             ~ 2,
    rr >= 19             ~ 1,
    rr >= 9  && rr <= 18 ~ 0,
    TRUE                 ~ NA_real_
  )
  
  pulse_ox <- case_when(
    sa02 <= 89 ~ 3,
    sa02 <= 93 ~ 2,
    sa02 <= 96 ~ 1,
    sa02 >  96 ~ 0,
    TRUE       ~ NA_real_
  )

  
  pulse_rate <- case_when(
    pr <= 40  || pr >= 130 ~ 3,
    pr <= 50  || pr >= 111 ~ 2,
    pr >= 101              ~ 1,
    pr >= 51  && pr <= 100 ~ 0,
    TRUE                   ~ NA_real_
  )
  
  bp <- case_when(
    sys_bp <= 70  ~ 3,
    sys_bp <= 90  ~ 2,
    sys_bp <= 100 ~ 1,
    sys_bp >  100 ~ 0,
    TRUE          ~ NA_real_
  )
  
  temperature <- case_when(
    temp <= 35   || temp >= 39   ~ 2,
    temp <= 36   || temp >= 38   ~ 1,
    temp >= 36.1 && temp <= 37.9 ~ 0,
    TRUE                          ~ NA_real_
  )
  
  neuro <- case_when(
    tolower(avpu) %in% c("pain", "unresponsive") ~ 3,
    tolower(avpu) == "v"           ~ 2,
    gcs_v         <  4             ~ 1,
    gcs_tot       < 15             ~ 1,
    tolower(avpu) == "a"           ~ 0,
    TRUE                           ~ NA_real_
  )
  
  return(respiratory + pulse_ox + pulse_rate + bp + temperature + neuro)
  
}


# NEWS2 ---------------------------------

#' Calculate the National Early Warning Score (NEWS2)
#' Note that the hypercapnic respiratory modification
#' to the pulse oximetry score is not used
#'
#' @param rr Respiratory rate in breaths/min
#' @param sao2 Pulse oximetry value in %
#' @param pr Pulse rate in beats/min
#' @param sys_bp Systolic blood pressure in mmHg
#' @param temp Temperature in celsius
#' @param avpu Level of consciousness based on AVPU (alert, voice, pain, unresponsive) score
#' @param air_oxygen Patient receiving supplemental oxygen
#' @return The NEWS2 value
#' @examples
#' news2(12, 98, 104, 140, 40, "Voice", TRUE)
#' news2(24, 88, 124, 90, 36.9, "Alert", FALSE)
news2 <- function(rr, sa02, pr, sys_bp, temp, avpu, air_oxygen) {

  respiratory <- case_when(
    rr <= 8  || rr >= 25 ~ 3,
    rr >= 21 && rr <= 24 ~ 2,
    rr >= 9  && rr <= 11 ~ 1,
    rr >= 12 && rr <= 20 ~ 0,
    TRUE                 ~ NA_real_
  )
  
  pulse_ox <- case_when(
    sa02 <= 91               ~ 3,
    sa02 >= 92 && sa02 <= 93 ~ 2,
    sa02 >= 94 && sa02 <= 95 ~ 1,
    sa02 >= 96               ~ 0,
    TRUE                     ~ NA_real_
  )

  pulse_rate <- case_when(
    pr <= 40  || pr >= 131 ~ 3,
    pr >= 111 && pr <= 130 ~ 2,
    pr >= 41  && pr <= 50  ~ 1,
    pr >= 91  && pr <= 110 ~ 1,
    pr >= 51  && pr <= 90  ~ 0,
    TRUE                   ~ NA_real_
  )
  
  bp <- case_when(
    sys_bp <= 90  || sys_bp >= 220  ~ 3,
    sys_bp <= 91  && sys_bp <= 100  ~ 2,
    sys_bp >= 101 && sys_bp <= 110  ~ 1,
    sys_bp >= 111 && sys_bp <= 219  ~ 0,
    TRUE                            ~ NA_real_
  )

  temperature <- case_when(
    temp <= 35                     ~ 3,
    temp >= 39.1                   ~ 2,
    temp >  35.1 && temp <= 36.0   ~ 1,
    temp >  38.1 && temp <= 39.0   ~ 1,
    temp >= 36.1 && temp <= 38.0   ~ 0,
    TRUE                           ~ NA_real_
  )

  neuro <- case_when(
    tolower(avpu) %in% c("confusion", "voice", "pain", "unresponsive") ~ 3,
    tolower(avpu) == "a"                    ~ 0,
    TRUE                                    ~ NA_real_
  )

  air02 <- case_when(
    air_oxygen == TRUE  ~ 2,
    air_oxygen == FALSE ~ 0,
    TRUE                ~ NA_real_
  )
  
  return(respiratory + pulse_ox + pulse_rate + bp + temperature + neuro + air02)
  
}





# Test data -------------------------------

# df %>%
#   mutate(
#     crb65 = pmap_dbl(list(avpu, gcs_v, gcs_tot, rr, sys_bp, dia_bp, age), crb65),
#     pmews = pmap_dbl(list(rr, sa02, pr, sys_bp, temp, avpu, gcs_v, gcs_tot), pmews),
#     news2 = pmap_dbl(list(rr, sa02, pr, sys_bp, temp, avpu, air_oxygen), news2)
#   )





