# Script: 02_read_data.R
#
# NeurOptimal PRL project.
#
# The input data are the Matlab data for each block and each subject.
# This script, together with preliminary_clean_up.R, is used for writing
# two RDS files:
# - prl_eating_edi26.rds: all the raw data with EDI26 information
# - prl_eating_edi26_bmi.rds": all the raw data with EDI26 information and BMI; 
#   information about BMI is missing for some subjects
#
# The RDS files contain all the useful information that has been collected, 
# and they are merged with the EDI26 information (there are subjects who completed
# the PRL task, but did not complete the EDI26 questionnaire).
#
# On each trial, two images are shown. One image depicts food-realted materials, the other
# is a neutral image.
# When the subject chooses the food-related image, choice has value of 1; when the neutral
# image is chosen, choice has the value of 2. 
# If is_food_rewarded_first == 1, the food-related image is rewared with 70% probability 
# in the first epoch. In the second epoch, the food-related image is rewared with 30% probability,
# and so on.
# If is_food_rewarded_first == 0, the food-related image is rewared with 30% probability 
# in the first epoch. In the second epoch, the food-related image is rewared with 70% probability,
# and so on.
#
# Corrado Caudek
# This script was last modified on "Sat Oct 12 08:01:04 2019"
#-------------------------------------------------------------------- 

# GRP_ABI: patients (n = 39, T0, T1, T2)
# GRP_CO: controls (n = 13, T0, T2)
# T0, T1, T2: administration times
# The PRL task was performed twice by each participant, in each time point.


library("here")

source(
  here::here(
    "code", "functions", "funs_neuroptimal.R"
  )
)

source(here::here("code", "scripts", "01_prelims.R"))



# DESTINATION_FOLDER <- "GRP_ABI_SP_T0"
# DESTINATION_FOLDER <- "GRP_ABI_SP_T1"
# DESTINATION_FOLDER <- "GRP_ABI_SP_T2"

# DESTINATION_FOLDER <- "GRP_ABI_CO_T0"
DESTINATION_FOLDER <- "GRP_ABI_CO_T2"

save_rds_raw_prl_data(DESTINATION_FOLDER)


# E N D 


