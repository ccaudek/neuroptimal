# Script: 05_loop_ewa_eating.R
#
# This script generates the estimates of the parameters of the EWA model.
# The data are in the file prl_eating_edi26.rds, with contains
# the raw data with the EDI26 information.
# This script estimates the EWA parameters by looping through each 
# subject. Estimates of the parameters are provided for each block
# of trials.
#
# Issues:
# - not all blocks have 160 trials;
# - not all subjects run 8 blocks of trials;
# - some subjects run all the experiment twice;
# - the coding of 'block' is wrong, in some case, in the raw data;
# - the coding of 'img_pair' is wrong, in some case, in the raw data.
#
# TODO: volatility is coded wrong in 6 cases. Must be fixed!
#
# This script was last modified on "Mon Oct 14 06:29:40 2019"
# Corrado Caudek
#-------------------------------------------------------------------- 


# Meaning of the parameters

# The data were fit with a simplified version of the Experience-Weighted 
# Attraction Model of Camerer and Ho (1999) as described by Ouden et al. 
# (2013). In such model, the meaning of the parameters can be described as 
# follows:
#   
# - $\rho$ = 0, where predictions are always driven by the most recent 
# experiences,  
# - $\rho$ = 1, that weights all trials in the experiment equally, causing 
# all the experience accumulated during the acquisition phase to produce 
# sluggish reversal.
#   
# - $\phi$ is the decay factor for the previous payoffs, equivalent to the 
# learning rate in the Rescorla-Wagner model. The Rescorla-Wagner learning 
# rate, usually denoted $\alpha$, is here equivalent to $(1 – \phi)$.
#   
# - $\beta$: the softmax inverse temperature $\beta$ is equivalent to the 
# product $\beta \times \alpha$ in Rescorla-Wagner model.



# Prelims -----------------------------------------------------------------


library("here")
source(here("scripts", "01_prelims.R"))
source(here("libraries", "func_eating.R"))

library("bayesplot")
theme_update(text = element_text(family = "sans"))

library("hBayesDM")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_update(text = element_text(family = "sans"))

options(max.print=999999)



# Import PRL data ---------------------------------------------------------


destfile <- here("data", "processed", "prl_eating_edi26.rds")
df <- readRDS(destfile)
length(unique(df$subj_name))



# Add variable for new subject names --------------------------------------


# create a new variable that will be used to distinguish among the first
# and the second run of subject with double number of trials. Those subjects 
# will have different modalities on the new variable 'subj_name_2runs'
df$subj_name_2runs <- df$subj_name
# in the following, I will change subj_name_2runs that will be used in the
# loop for computing the EWA parameters

# the variable 'session' has value of 1 for all subjects that have run the
# experiment only once (8 blocks of 160 trials each). Some subjects have
# run the experiment twice (16 blocks of 160 trials each). These subjects
# have been split into two, with session = 1 for the first 8 blocks, and
# sessione = 2 for the second 8 blocks.
df$session <- 1


# Remove problematic blocks -----------------------------------------------


hist(df$total_reward)
# remove blocks in which reward is too low
df <- df[df$total_reward > 50, ]
# all raw data are here!

hist(df$total_reward)

bysub_n <- df %>% 
  group_by(subj_name) %>% 
  summarise(
    n = n()
  )

# create the new variable subj_name_2runs. On this variable, the subjects
# who run the experiment twice had been renamed and are considered as 
# separate subjects for the purpose of the EWA estimation.
source(here("libraries", "fix_subjects_run_twice.R"))
# the data.frame that is created is called new_dat

length(unique(new_dat$subj_name_2runs))



# Remove subjects with too short average RTs ------------------------------


bysub_rt <- new_dat %>% 
  group_by(subj_name_2runs) %>% 
  summarise(
    mrt = median(rt, na.rm = TRUE)
  )

hist(bysub_rt$mrt)

bad_subjects2 <- bysub_rt %>% 
  dplyr::filter(mrt < 0.10)

bad_ids2 <- unique(bad_subjects2$subj_name_2runs)

new_dat2 <- new_dat[!(new_dat$subj_name_2runs %in% bad_ids2), ]


# mean(new_dat2$total_reward)
# 
# length(unique(new_dat2$subj_name_2runs))
# length(unique(new_dat2$subj_name))
# 
# # how many blocks per subject?
# bysubj_nblocks <- new_dat2 %>% 
#   group_by(subj_name_2runs) %>% 
#   summarise(
#     n_blocks = length(unique(block)),
#     n_img_pair = length(unique(img_pair))
#   )
# 
# table(bysubj_nblocks$n_blocks, bysubj_nblocks$n_img_pair)


# I don't remove subjects with less than 8 blocks!



# Fix coding for 'block' and 'img_pair' -----------------------------------


# total number of subjects: those subjects that have been run twice 
# are counted as two
n_sub <- length(unique(new_dat2$subj_name_2runs))

# add variable 'subj_id': progressive integer from 1 to n_sub
new_dat2$subj_id <- as.numeric(factor(new_dat2$subj_name_2runs))

# fix coding for 'block' and 'img_pair'
mydat <- fix_coding_block_imgpair_fnc(new_dat2)

# fix wrong trial number for subject 'gl_ve_1999_09_19_225_f';
# the variable 'trial' in first block start from 160, 1, 2, ...;
# then in one of the following blocks, trial goes from 1 to 159.
# Given that the number of blocks is determined by considering 
# the trial counters, this coding produces an error. To fix it, 
# I change the coding of 'trial' as follows:
mydat$trial <- ifelse(
  mydat$subj_name == "gl_ve_1999_09_19_225_f" & 
    mydat$block == 1 &
    mydat$trial == 160 &
    mydat$rt < 0.5, 
  # in the last trial of the block, trial == 160, rt = 0.7569462
  0, # change trial == 160 to trial == 0 in the first trial of block 1.
  mydat$trial
)

# mydat contains the cleaned-up raw data to be used in the loop 
# for the prl_ewa() estimation.

# remove unnecessary data.frames
rm(new_dat, new_dat2, alldat_without_subjs_twice_data)

# length(unique(mydat$subj_name_2runs))
# 268

# fix subject subj_id == 259


n_sub <- length(unique(mydat$subj_name_2runs))
n_sub



# Fit the EWA model -------------------------------------------------------


# Create empty lists where to save the results.
mu <- list() 
phi <- list() 
rho <- list()  
beta <- list() 
subject_number_list <- list()
loop_subj_name <- list() 
loop_subj_name_2runs <- list() 
loop_session <- list()
loop_block <- list() 
loop_img_pair <- list()
loop_volatility <- list()
loop_is_food_reward_first <- list()
loop_total_reward <- list()
loop_gender <- list()
loop_md_rt <- list()

loop_eat26_dieting <- list()
loop_eat26_bulimia <- list()
loop_eat26_oral_control <- list()


# CHANGE!!
# n_sub <- 2

# foo <- mydat %>% 
#   group_by(subj_id) %>% 
#   summarise(
#     n = n()
#   )
# data.frame(foo)

# less than 8 blocks
# use for programming!
# subj_index <- 15



# Loop for estimating the parameters of all subjects.

## for (subj_index in 131:n_sub) { #### TEST!!!

for (subj_index in 259:n_sub) {
  
  # data.frame with all variables for one subject
  one_subj_tot <- mydat %>% 
    dplyr::filter(subj_id == subj_index) %>% 
    arrange(block)
  
  # fix subject 259: block 2 is repeated twice
  if (one_subj_tot$subj_id == 259) {
    temp <- one_subj_tot[-c(321:480), ]
    one_subj_tot <- temp
  }
  
  one_subj_dt <- data.frame(
    subjID = one_subj_tot$block, 
    choice = one_subj_tot$choice,
    outcome = ifelse(one_subj_tot$outcome == 1, 1, -1)
  )
  
  # prl_ewa() needs a txt file as input.
  write.table(
    one_subj_dt,
    here("scripts", "mydata.txt"), 
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  
  # Fit the Experience-Weighted Attraction Model (Ouden et al., 2013).
  output <- prl_rp(
    data = here("scripts", "mydata.txt"),
    niter = 5000,
    nwarmup = 1000,
    nchain = 4,
    ncore = 2
  )
  
  res <- summary(output$fit)
  
  # mu is a vector with 3 estimated params
  mu[[subj_index]] <- res[[1]][1:3, 1]
  
  # how many block for the subject at hand?
  # n_blocks <- length(unique(one_subj_dt$subjID)) 
  # the above statement could be incorrect if the same integer
  # is used for two different blocks!
  # 
  n_blocks <- sum(diff(one_subj_tot$trial) != 1) + 1
  # in this statement, I assume that the trials are in sequence, 
  # within each block. Then diff() is the difference between
  # trial i+1 and trial i. When the integers of 'trial' are growing,
  # this difference is 1. When we go from 160 to 1, the difference
  # is -159.  This works also when there are less than 160 trials
  # in a block: the difference will be negative, unless the last
  # ingeger for 'trial' in a block is x and 'trial' in the next
  # block starts from x+1, which is very unlikely -- blocks always
  # start with trial == 1 and, in case, are truncated before 
  # trial == 160.
  
  # phi estimates for each block.
  n1_init <- 7
  n1_end <- n1_init + n_blocks - 1
  phi[[subj_index]] <- res[[1]][n1_init:n1_end, 1]
  
  # rho estimates for each block.
  n2_init <- n1_end + 1
  n2_end <- n2_init + n_blocks - 1
  rho[[subj_index]] <- res[[1]][n2_init:n2_end, 1]
  
  # beta estimates for each block.
  n3_init <- n2_end + 1
  n3_end <- n3_init + n_blocks - 1
  beta[[subj_index]] <- res[[1]][n3_init:n3_end, 1]
  
  # subject number for each iteration of the for loop, in a vector
  # with each element repeated n_blocks times
  subject_number_list[[subj_index]] <- rep(subj_index, n_blocks)
  
  # subject name
  loop_subj_name[[subj_index]] <- rep(
    unique(one_subj_tot$subj_name), n_blocks
  )
  
  # subj_name_2runs
  loop_subj_name_2runs[[subj_index]] <- rep(
    unique(one_subj_tot$subj_name_2runs), n_blocks
  )
  
  # block
  loop_block[[subj_index]] <- unique(one_subj_tot$block)
  
  # session
  loop_session[[subj_index]] <- rep(
    unique(one_subj_tot$session), n_blocks
    )
  
  # image pair
  loop_img_pair[[subj_index]] <- 
    unique(one_subj_tot$img_pair)
  
  # median rt by block
  byblock_md_rt <- one_subj_tot %>% 
    group_by(block) %>% 
    summarise(
      md_rt = median(rt, na.rm = TRUE)
    )
  loop_md_rt[[subj_index]] <- byblock_md_rt$md_rt
  
  # volatility
  # recover volatility from 'subj_tag'
  one_subj_tot$vol1 <- one_subj_tot$subj_tag
  one_subj_tot$vol2 <- factor(base::substr(one_subj_tot$vol1, 25, 28))
  one_subj_tot$vol3 <- forcats::fct_recode(one_subj_tot$vol2, low = "low_")
  # summary(one_subj_tot$vol3)
  
  vol_table <- table(
    one_subj_tot$block, 
    one_subj_tot$vol3 # modalities recovered from 'subj_tag'
  )
  
  vol <- rep(NA, n_blocks)
  for (i in 1:n_blocks) {
    if (vol_table[i, 1] > 10 & vol_table[i, 2] == 0)
      vol[i] <- "high"
    else if (vol_table[i, 1] == 0 & vol_table[i, 2] > 10)
      vol[i] <- "low"
    else
      vol[i] <- "error"
  }
  loop_volatility[[subj_index]] <-  vol
  
  # is food image rewarded in the first epoch of the block?
  food_rew_first_table <- table(
    one_subj_tot$block, 
    one_subj_tot$is_food_reward_first
  )
  
  food_rewarded_first <- rep(NA, n_blocks)
  for (i in 1:n_blocks) {
    if (food_rew_first_table[i, 1] > 10 & food_rew_first_table[i, 2] == 0)
      food_rewarded_first[i] <- "no"
    else if (food_rew_first_table[i, 1] == 0 & food_rew_first_table[i, 2] > 10)
      food_rewarded_first[i] <- "yes"
    else
      food_rewarded_first[i] <- "error"
  }
  loop_is_food_reward_first[[subj_index]] <- food_rewarded_first
  
  # total reward
  tot_rew_table <- one_subj_tot %>% 
    group_by(block) %>% 
    summarise(
      tr = mean(total_reward)
    )
  loop_total_reward[[subj_index]] <- tot_rew_table$tr
  
  # gender
  loop_gender[[subj_index]] <- rep(
    as.character(unique(one_subj_tot$gender)), n_blocks
  )
  
  # EAT26 subscales
  loop_eat26_dieting[[subj_index]] <- rep(
    unique(one_subj_tot$dieting), n_blocks
  )
  
  loop_eat26_bulimia[[subj_index]] <- rep(
    unique(one_subj_tot$bulimia_food_preoccupation), n_blocks
  )
    
  loop_eat26_oral_control[[subj_index]] <- rep(
    unique(one_subj_tot$oral_control), n_blocks
  )
  
  
  # clean up
  rm(res, vol_table, vol, tot_rew_table)
  
  message("  -- completed iteration: ", subj_index)

} # end of the for loop for all subjects.




# Summary EWA parameters estimates ----------------------------------------


# summary EWA parameters estimates
mu_df <- as.data.frame(do.call(rbind, mu))
mu_df$subject_number <- unique(unlist(subject_number_list, use.names = FALSE))
# mu_df$subject_name <- unique(unlist(loop_subj_name, use.names = FALSE))
mu_df$subject_name_2runs <- unique(unlist(loop_subj_name_2runs, use.names = FALSE))

# save mu results in an external file
saveRDS(mu_df, here("data", "processed", "mu_rp_17_oct_2019.rds"))
saveRDS(mu_df, here("data", "processed", "mu_rp_part2.rds"))



# By-subject and by-block -------------------------------------------------

# RP parameters estimates, with additional information
byblock_params <- data.frame(
  subj_number = unlist(subject_number_list), 
  subj_name = unlist(loop_subj_name),
  subj_name_2runs = unlist(loop_subj_name_2runs),
  gender = unlist(loop_gender),
  session = unlist(loop_session),
  block = unlist(loop_block),
  img_pair = unlist(loop_img_pair),
  phi = unlist(phi),  
  rho = unlist(rho), 
  beta = unlist(beta),
  md_rt = unlist(loop_md_rt),
  volatility = unlist(loop_volatility),
  is_food_rewarded_first = unlist(loop_is_food_reward_first),
  total_reward = unlist(loop_total_reward),
  dieting = unlist(loop_eat26_dieting), 
  bulimia = unlist(loop_eat26_bulimia), 
  oral_control = unlist(loop_eat26_oral_control)
)



saveRDS(
  byblock_params, 
  here("data", "processed", "byblock_rp_eat26_part2.rds")
)



#  E  N  D





# # # recover img_pair
# # aa <- mydat %>% 
# #   group_by(subj_name) %>% 
# #   summarise(
# #     n = n()
# #   )
# # aa[aa$n > 1280, ]
# 
# 
# subject_number_lst <- list()
# subj_name_lst <- list()
# block_lst <- list()
# img_pair_lst <- list()
# 
# # Loop for estimating the parameters of all subjects.
# for (subj_index in 1:n_sub) {
#   
#   one_subj_tot <- mydat %>% 
#     dplyr::filter(subj_id == subj_index)
#   
#   one_subj_sorted_dt <- one_subj_tot %>% 
#     arrange(block)
#   
#   # subject number for each iteration of the for loop.
#   subject_number_lst[[subj_index]] <- subj_index
#   
#   # subject name
#   subj_name_lst[[subj_index]] <- unique(one_subj_sorted_dt$subj_name)
#   
#   # block
#   block_lst[[subj_index]] <- unique(one_subj_sorted_dt$block)
#   
#   # image pair
#   temp <- one_subj_sorted_dt %>% 
#     group_by(block) %>% 
#     summarise(
#       ip = mean(img_pair)
#     )
#   img_pair_lst[[subj_index]] <- temp$ip
#   
#   # clean up
#   rm(temp)
#   # message("  -- completed iteration: ", subj_index)
#   
# } # end of the for loop for all subjects.
# 
# 
# # img_pair info
# byblock_img_pair <- data.frame(
#   subj_name = rep(unlist(subj_name_lst), each = n_blocks),
#   subj_number = rep(unlist(subject_number_lst), each = n_blocks),
#   block = unlist(loop_block),
#   img_pair = unlist(img_pair_lst)
# )
# 
# 
# all_params <- byblock_ewa_params %>% 
#   inner_join(byblock_img_pair)


# # two sessions!
# bad_ids4 <- c(
#   "as_mu_1999_04_24_300_f",
#   "bi_za_1998_09_14_029_f",
#   "ca_mo_1999_04_03_910_f",
#   "co_ro_1999_09_05_550_f",
#   "gl_ve_1999_09_19_225_f",
#   "ma_pa_1997_05_01_392_f",
#   "pa_al_1998_10_15_624_f",
#   "va_pa_1998_01_01_897_f"
# )
# 
# all_params2 <- all_params[!all_params$subj_name %in% bad_ids4, ]
# length(unique(all_params2$subj_name))






# cor(all_params2[, c(1, 2, 3, 9)])
# 
# all_params2 %>% 
#   group_by(img_pair) %>% 
#   summarise(
#     phi_avg = mean(phi),
#     rho_avg = mean(rho),
#     beta_avg = mean(beta),
#     n = n()
#   )
# 
# 
# 
# temp$img_type <- rep(NA, nrow(temp))
# for (i in 1:nrow(temp)) {
#   if (temp$img_pair[i] < 4) {
#     temp$img_type[i] <- "a"
#   } else if (temp$img_pair[i] > 3 & temp$img_pair[i] < 7) {
#     temp$img_type[i] <- "b"
#   } else if (temp$img_pair[i] > 6 & temp$img_pair[i] < 10) {
#     temp$img_type[i] <- "c"
#   } else {
#     temp$img_type[i] <- "error"
#   }
# }
# temp$img_type <- factor(temp$img_type)

# there is coding error for some subjects concerning the
# image number.  

# Corrected below:

# nip <- lengths(simplify2array(loop_img_pair))
# ngood <- lengths(simplify2array(loop_subj_name))
# nip -ngood

# loop_img_pair[[42]] <- c(5, 7, 8, 1, 2, 3, 6)
# loop_img_pair[[49]] <- c(5, 1, 2, 4, 7, 8, 3, 6)
# loop_img_pair[[53]] <- c(6, 2, 3, 8, 1, 4, 7, 5)
# loop_img_pair[[67]] <- c(2, 8, 1, 6, 5, 3, 4, 7)
# loop_img_pair[[106]] <- c(2, 1, 3, 4, 5, 6, 8)
# loop_img_pair[[130]] <- c(4, 5, 3, 6, 1, 7, 2)
# loop_img_pair[[136]] <- c(5, 2, 6, 4, 8, 7, 1, 3)
# loop_img_pair[[139]] <- c(5, 4, 3, 2, 7, 1, 6, 8)
# loop_img_pair[[183]] <- c(6, 8, 4, 5, 1, 7, 3)
# loop_img_pair[[221]] <- c(2, 6, 8, 5, 4, 1, 7)
# loop_img_pair[[227]] <- c(8, 7, 6, 4, 3, 1, 5, 2)
# loop_img_pair[[230]] <- c(7, 4, 1, 2, 8, 3, 6, 5)
# loop_img_pair[[250]] <- c(8, 3, 4, 6, 5, 1, 2)
# loop_img_pair[[264]] <- c(3, 8, 1, 5, 7, 2, 6, 4)

# loop_subj_name[[49]]


