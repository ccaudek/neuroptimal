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



save_rds_raw_prl_data <- function(DESTINATION_FOLDER) {
  
  dir <- here("data", "raw", "PRL", "matlab_prl", "data", DESTINATION_FOLDER)
  file_names <- as.character(list.files(path=dir))
  n_files <- length(file_names)
  n_files
  
  d_list <- list()
  
  for (i in 1:n_files) {
    
    d  <- readMat(here("data", "raw", "PRL", "matlab_prl", "data", DESTINATION_FOLDER, file_names[i]))
    
    # subj_name <- as.character(d$data[, , 1]$subject.code)
    subj_tag <- as.character(d$data[, , 1]$subTag)
    block <- as.numeric(d$data[, , 1]$block)
    is_food_reward_first <- 
      as.numeric(d$data[, , 1]$is.food.rewarded.first)
    # img_pair <- as.numeric(d$data[, , 1]$img.pair)
    # date <- as.character(d$data[, , 1]$today)
    total_reward <- as.numeric(d$data[, , 1]$totalReward)
    outcome <- as.numeric(d$data[, , 1]$outcome)
    choice <- as.numeric(d$data[, , 1]$choice)
    rt <- as.numeric(d$data[, , 1]$RT)
    fdb_prob <- as.numeric(d$data[, , 1]$img.pair[, , 1]$prob)
    subj_id <- as.numeric(d$data[, , 1]$sID)
    ntrials <- as.numeric(d$data[, , 1]$img.pair[, , 1]$nt)
    trial <- 1:ntrials
    task_version <- as.numeric(d$data[, , 1]$img.pair[, , 1]$taskVersion)
    volatility <- as.character(d$data[, , 1]$img.pair[, , 1]$volatility)
    
    if (is_food_reward_first == 1) {
      principled_corr_choices = c(rep(1, 40), rep(2, 40), rep(1, 40), rep(2, 40))
    } else{
      principled_corr_choices = c(rep(2, 40), rep(1, 40), rep(2, 40), rep(1, 40))
    }
    
    is_correct_choice <- ifelse(
      choice == principled_corr_choices, 0, 1
    )
    
    # mean(is_correct_choice)
    # total_reward / 160
    
    # add epoch vector for a block of 160 trials
    epoch <- rep(1:4, each = 40)
    
    d_list[[i]] <- data.frame(
      subj_id,
      subj_tag,
      volatility = "low",
      trial,
      ntrials,
      block,
      epoch,
      is_food_reward_first,
      total_reward,
      outcome,
      choice,
      rt
    )
  }
  
  
  # convert list into data.frame
  df <- do.call(rbind.data.frame, d_list)
  
  # the numbers should be the same
  # length(unique(df$subj_id))
  # length(unique(df$subj_tag))
  
  name_file <- paste0(DESTINATION_FOLDER, ".rds")
  
  # save data in RDS files
  saveRDS(df, here("data", "processed", "PRL", name_file))
  
}





# Prelims
library("here")
source(here("scripts", "PRL", "01_prelims.R"))
source(here("libraries", "func_eating.R"))


# DESTINATION_FOLDER <- "GRP_ABI_SP_T0"
# DESTINATION_FOLDER <- "GRP_ABI_SP_T1"
# DESTINATION_FOLDER <- "GRP_ABI_SP_T2"
# DESTINATION_FOLDER <- "GRP_ABI_CO_T0"
DESTINATION_FOLDER <- "GRP_ABI_CO_T2"


save_rds_raw_prl_data(DESTINATION_FOLDER)


# E N D 


