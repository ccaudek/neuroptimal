# Script name: 06_write_input_for_hddmrl_all_data.R
# Project: Neuroptimal
# Script purpose: write input data for HDDMrl.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Sat Jun  4 08:36:10 2022
# Last Modified Date: Sat Jun  4 08:36:10 2022
#
# 👉 

library("here")
library("tidyverse")

options(max.print = 999999)


# Read patients' data -----------------------------------------------------

# DESTINATION_FOLDER <- "GRP_ABI_SP_T0"
# DESTINATION_FOLDER <- "GRP_ABI_SP_T1"
# DESTINATION_FOLDER <- "GRP_ABI_SP_T2"
# DESTINATION_FOLDER <- "GRP_ABI_CO_T0"
# DESTINATION_FOLDER <- "GRP_ABI_CO_T2"

s0 <- readRDS(here::here("data", "processed", "PRL", "GRP_ABI_SP_T0.rds"))
s0$is_neuroptimal <- 1
s0$time <- 0
s1 <- readRDS(here::here("data", "processed", "PRL", "GRP_ABI_SP_T1.rds"))
s1$is_neuroptimal <- 1
s1$time <- 1
s2 <- readRDS(here::here("data", "processed", "PRL", "GRP_ABI_SP_T2.rds"))
s2$is_neuroptimal <- 1
s2$time <- 2
c0 <- readRDS(here::here("data", "processed", "PRL", "GRP_ABI_CO_T0.rds"))
c0$is_neuroptimal <- 0
c0$time <- 0
c2 <- readRDS(here::here("data", "processed", "PRL", "GRP_ABI_CO_T2.rds"))
c2$is_neuroptimal <- 0
c2$time <- 2

prl_dat <- bind_rows(s0, s1, s2, c0, c2)

# prl_dat %>% 
#   group_by(is_neuroptimal, block, time) %>% 
#   summarise(
#     avg_tot_rew = mean(total_reward),
#     avg_rt = mean(rt, trim = 0.2, na.rm = TRUE)
#   )
# 
# if (0) {
#   bad_patients <- prl_dat %>% 
#     dplyr::filter(total_reward < 90)
#   sort(unique(bad_patients$subj_id))
#   
#   good_patients <- prl_dat %>% 
#     dplyr::filter(total_reward >= 110)
#   sort(unique(good_patients$subj_id))
# }


DESIGN <- "incomplete" # only times t0 and t2

# Remove the time point t1, so as to have a balanced design.
if (DESIGN == "complete") {
  data_for_hddm <- tibble(
    response = prl_dat$choice - 1,
    rt = prl_dat$rt,
    trial = prl_dat$trial,
    feedback = prl_dat$outcome,
    subj_idx = prl_dat$subj_id,
    subj_code = prl_dat$subj_tag,
    is_neuroptimal = prl_dat$is_neuroptimal,
    is_first = prl_dat$is_food_reward_first,
    epoch = prl_dat$epoch,
    block = prl_dat$block,
    q_init = 0.5,
    split_by = ifelse(prl_dat$time == 0, 0, 1),
    time = prl_dat$time
  ) %>% 
    dplyr::filter(
      time != 1
    )
} else {
  data_for_hddm <- tibble(
    response = prl_dat$choice - 1,
    rt = prl_dat$rt,
    trial = prl_dat$trial,
    feedback = prl_dat$outcome,
    subj_idx = prl_dat$subj_id,
    subj_code = prl_dat$subj_tag,
    is_neuroptimal = prl_dat$is_neuroptimal,
    is_first = prl_dat$is_food_reward_first,
    epoch = prl_dat$epoch,
    block = prl_dat$block,
    q_init = 0.5,
    split_by = ifelse(prl_dat$time == 0, 0, 1),
    time = prl_dat$time
  ) 
}

unique(data_for_hddm$time)

# foo <- prl_dat
# foo$spli_by <- as.integer(factor(as.character(paste0(prl_dat$time, prl_dat$is_neuroptimal))))
# foo$grp <-  factor(as.character(paste0(prl_dat$time, prl_dat$is_neuroptimal)))
# 
# table(
#   foo$spit_by, foo$grp
# )
# 
# split_by == 1 -> 00: T0, is_neuroptimal == 0 CO
# split_by == 2 -> 01: T0, is_neuroptimal == 1 SP
# split_by == 3 -> 11: T1, is_neuroptimal == 1 SP
# split_by == 4 -> 20: T2, is_neuroptimal == 0 CO
# split_by == 5 -> 21: T2, is_neuroptimal == 1 SP


# Multiple imputation on NAs.
temp <- data.frame(
  subj_code      = data_for_hddm$subj_code,
  is_neuroptimal = data_for_hddm$is_neuroptimal,
  rt             = data_for_hddm$rt, 
  block          = data_for_hddm$block,
  trial          = data_for_hddm$trial,
  feedback       = data_for_hddm$feedback, 
  response       = data_for_hddm$response,
  epoch          = data_for_hddm$epoch,
  is_first       = data_for_hddm$is_first,
  time           = data_for_hddm$time
)

# Remove wrong 'feedback'
temp$feedback <- ifelse(temp$rt > 7 | temp$rt < 0.2, NA, temp$feedback)

# Remove wrong 'response'
temp$response <- ifelse(temp$rt > 7 | temp$rt < 0.2, NA, temp$response)

# Remove wrong RTs
temp$rt <- ifelse(temp$rt > 7 | temp$rt < 0.2, NA, temp$rt)

# Imputation
set.seed(123)
miceObj <- miceRanger::miceRanger(
  temp
  , m = 1
  , returnModels = TRUE
  , verbose = TRUE
)

dataList <- miceRanger::completeData(miceObj)

# head(dataList[[1]], 10)
imputed_data <- dataList[[1]]

imputed_data$subj_code <- data_for_hddm$subj_code

data_for_hddm <- imputed_data

table(data_for_hddm$is_neuroptimal)
table(data_for_hddm$time)

unique(data_for_hddm$response)
# [1] 0 1

unique(data_for_hddm$feedback)
# [1] 1 0  

summary(data_for_hddm$rt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2002  0.5637  0.8617  1.1678  1.3977  6.9933 

hist(data_for_hddm$rt)

# Generata an index from 1 to 51, with a single value for each participant.
data_for_hddm$subj_id <- 
  as.numeric(factor(as.character(data_for_hddm$subj_code)))
unique(data_for_hddm$subj_id)

grpid = function(x) match(x, unique(x))

data_for_hddm_2 <- data_for_hddm %>% 
  mutate(subj_idx = group_indices(., subj_id) %>% grpid)

# unique(data_for_hddm_2$subj_idx)
data_for_hddm_2$subj_id <- NULL

data_for_hddm_2$subj_code <- factor(data_for_hddm_2$subj_code)


# Sort by subj_idx, block, time, trial ------------------------------------

data_for_hddm_3 <- arrange(data_for_hddm_2, subj_idx, time, block, trial)

# unique(data_for_hddm_3$subj_idx)
# unique(data_for_hddm_3$block)
# unique(data_for_hddm_3$time)

foo <- data.frame(
  subj_idx = data_for_hddm_3$subj_idx,
  time     = data_for_hddm_3$time,  
  block    = data_for_hddm_3$block, 
  trial    = data_for_hddm_3$trial
)

foo1 <- foo[foo$subj_idx == 51, ]
foo1[c(1:2, 159:161, 319:321, 479:481, 639:640), ] 


# Now this does not work, because treatment and control subjects
# have a different number of trials.  

# Maybe I have to split the dataframe in two....

data_trt <- 
  data_for_hddm_3 %>% 
  dplyr::filter(is_neuroptimal == 1)

n_subj_trt <- length(unique(data_trt$subj_code))
n_subj_trt

data_cnt <- 
  data_for_hddm_3 %>% 
  dplyr::filter(is_neuroptimal == 0)

n_subj_cnt <- length(unique(data_cnt$subj_code))
n_subj_cnt

# Treatment: add block_new,  with values 1:320 for each time point
data_trt$trial_new <- rep(c(1:320, 1:320, 1:320), n_subj_trt)

# Control: add block_new,  with values 1:320 for each time point
data_cnt$trial_new <- rep(c(1:320, 1:320), n_subj_cnt)

# Join
data_for_hddm_3 <- rbind(data_trt, data_cnt)


#####################################################################

# # I add block_new, with values 1:320 for each time point.
# data_for_hddm_3$trial_new <- rep(c(1:320, 1:320), 51)
# 
# # Check again.
# foo <- data.frame(
#   subj_idx = data_for_hddm_3$subj_idx,
#   time     = data_for_hddm_3$time,  
#   block    = data_for_hddm_3$block, 
#   trial    = data_for_hddm_3$trial_new  # <--
# )
# 
# foo1 <- foo[foo$subj_idx == 51, ]
# foo1[c(1:2, 159:161, 319:321, 479:481, 639:640), ] 


# Order according to  -----------------------------------------------------

data_for_hddm_4 <- arrange(
  data_for_hddm_3, 
  subj_idx, trial_new, time
)

head(data_for_hddm_4, 20)
tail(data_for_hddm_4, 20)


# Correct sorting for hddmrl ----------------------------------------------

data_for_hddm_5 <- data_for_hddm_4 %>% 
  dplyr::select(
    subj_code, is_neuroptimal, rt, trial_new, feedback, response, time, 
    subj_idx
  )

data_for_hddm_5 <- data_for_hddm_5 %>% 
  dplyr::rename(
    trial = trial_new
  )

unique(data_for_hddm_5$time)

data_for_hddm_5$split_by <- data_for_hddm_5$time

data_for_hddm_5 <- data_for_hddm_5 %>%
  mutate(
    Time = case_when(
      time == 0 ~ "t0",
      time == 1 ~ "t1",
      time == 2 ~ "t2"
    )
  )
summary(factor(data_for_hddm_5$Time))

# head(data_for_hddm_5)

data_for_hddm_5$time <- NULL

data_for_hddm_5$q_init <- 0.5

rio::export(
  data_for_hddm_5,
  here::here(
    "python", "data", "three_time_points", "input_for_hddmrl_3times.csv"
  )
)


# Create look-up table with subj_idx and subj_code.
lookup_table <- data_for_hddm_5 %>% 
  dplyr::select(subj_code, subj_idx) %>% 
  distinct()

rio::export(
  lookup_table,
  here::here(
    "python", "data", "three_time_points", "lookup_table.csv"
  )
)


# End of file -------------------------------------------------------------



