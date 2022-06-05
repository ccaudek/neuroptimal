
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
    
    # The value of block has been recorded incorrectly!
    # block <- as.numeric(d$data[, , 1]$block)
    
    # I assign to block the value of the index i (1:n_files)
    # so as to have an increasing index. Then I will order the trials
    # according to subj_idx, time, and block. Within each time point,
    # the sequence of blocks is irrelevant. What is important is to 
    # distinguish between the two block. The variable time has been recorded
    # correctly.
    block <- i
    
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
    
    print(i)
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







#' This function correcs the coding of 'block' and 'img_pair'
#' in the raw data, before running the loop for estimating the 
#' EWA and RP parameters for each subjec, and each block. 
#' img_pair is also recorded in output of 05_loop_ewa_eating.R
#' and 06_loop_rp_eating.R, so img_pair must be corrected too.
#'
#' This function was last modified on "Mon Oct 14 06:04:30 2019"
#' Corrado Caudek


#' Convert proportion to logit.
#' 
#' @param p a vector with proportions 
#' @return a vector of logits
mylogit <- function(p) {
  p <- ifelse(p == 0, 0.005, p)
  p <- ifelse(p == 1, 0.995, p)
  
  log(p / (1 - p))
}


#' Determine whether, on each trial in a block, the psycho img is rewarded.
#' Function to be used in the 02_read_data.R script.
#' 
#' @param d integer (0 or 1)
#' @param epoch 160 elements vector for a single block of trials 
#' @return 160 elements vector for a single block of trials 
is_food_img_rewarded_fnc <- function(
  is_food_reward_first, volatility, epoch) {
  
  if (length(epoch) > 160) 
    stop("too many trials")
  
  if (volatility == "low") {
    is_food_img_rewarded = case_when(
      is_food_reward_first == 1 & epoch == 1 ~ 1,
      is_food_reward_first == 1 & epoch == 2 ~ 0,
      is_food_reward_first == 1 & epoch == 3 ~ 1,
      is_food_reward_first == 1 & epoch == 4 ~ 0,
      is_food_reward_first == 0 & epoch == 1 ~ 0,
      is_food_reward_first == 0 & epoch == 2 ~ 1,
      is_food_reward_first == 0 & epoch == 3 ~ 0,
      is_food_reward_first == 0 & epoch == 4 ~ 1
    )
  } else {
    is_food_img_rewarded = case_when(
      is_food_reward_first == 1 & epoch == 1 ~ 1,
      is_food_reward_first == 1 & epoch == 2 ~ 0,
      is_food_reward_first == 1 & epoch == 3 ~ 1,
      is_food_reward_first == 1 & epoch == 4 ~ 0,
      is_food_reward_first == 1 & epoch == 5 ~ 1,
      is_food_reward_first == 1 & epoch == 6 ~ 0,
      is_food_reward_first == 1 & epoch == 7 ~ 1,
      is_food_reward_first == 1 & epoch == 8 ~ 0,
      is_food_reward_first == 0 & epoch == 1 ~ 0,
      is_food_reward_first == 0 & epoch == 2 ~ 1,
      is_food_reward_first == 0 & epoch == 3 ~ 0,
      is_food_reward_first == 0 & epoch == 4 ~ 1,
      is_food_reward_first == 0 & epoch == 5 ~ 0,
      is_food_reward_first == 0 & epoch == 6 ~ 1,
      is_food_reward_first == 0 & epoch == 7 ~ 0,
      is_food_reward_first == 0 & epoch == 8 ~ 1
    )
  }
  
  is_food_img_rewarded
}



#' Get win-stay, lose-shift indeces by epoch within block.
#' 
#' @param d a dataframe with all the subjects
#' @param index an integer indentifying one subject
#' @return a dataframe with "block", "epoch", "is_psycho_img_rewarded, 
#' "subj_name"
get_wsls_imgpair <- function(d, index) {
  
  # data of one subject
  d <- d %>% 
    dplyr::filter(
      subj_index == index
    )
  
  d <- d[order(d$block), ] 
  
  # get number of block for that subject -- not all participants
  # completed 9 blocks! -- the block number that is recorded can 
  # be wrong!
  nblocks <- nrow(d) / 160 
  # must be 160!
  ntrials_byblock <- nrow(d) / nblocks
  # add outcome and choice in the previous trial
  d <- d %>%
    group_by(img_pair, is_psycho_img_rewarded) %>%
    mutate(
      outcome_prev1 = lag(outcome, 1),
      outcome_follow1 = lead(outcome, 1),
      choice_prev1 = lag(choice, 1)
    ) %>%
    ungroup()
  # compute win-stay
  # if there was a positive feedback in the previous trial, the image chosen
  # in the current trial is the same as in the previous trial?
  df_after_win <- d %>%
    dplyr::filter(outcome_prev1 == 1) %>%
    mutate(
      win_stay = ifelse(choice == choice_prev1, 1, 0)
    )
  
  df_ws_by_subj_imgpair <- df_after_win %>%
    group_by(img_pair, is_psycho_img_rewarded) %>%
    summarise(
      win_stay = mean(win_stay, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # compute lose-shift
  # if there was a negative feedback in the previous trial,the image chosen
  # in the current trial differs from the image shown in the previous trial?
  df_after_loss <- d %>%
    group_by(img_pair, is_psycho_img_rewarded) %>%
    dplyr::filter(outcome_prev1 == 0) %>%
    mutate(
      lose_shift = ifelse(choice != choice_prev1, 1, 0)
    ) %>%
    ungroup()
  
  df_ls_by_subj_imgpair <- df_after_loss %>%
    group_by(img_pair, is_psycho_img_rewarded) %>%
    summarise(
      lose_shift = mean(lose_shift, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # merge winstay_mat and lose_shift by block
  df <- df_ws_by_subj_imgpair %>%
    inner_join(df_ls_by_subj_imgpair, 
               by = c("img_pair", "is_psycho_img_rewarded"))
  df$subj_name <- unique(d$subj_name)
  
  # message("Win-stay, lose-shift. Done processing subject number: ", 
  # subj_index)
  df
}




get_win_stay_lose_shift <- function(d, index) {
  
  d <- d %>% 
    dplyr::filter(
      subj_id == index
    )
  # add outcome and choice in the previous trial
  d <- d %>%
    group_by(block) %>%
    mutate(
      outcome_prev1 = lag(outcome, 1),
      outcome_follow1 = lead(outcome, 1),
      choice_prev1 = lag(choice, 1)
    ) %>%
    ungroup()
  
  # compute win-stay
  # if there was a positive feedback in the previous trial, the image chosen
  # in the current trial is the same as in the previous trial?
  df_after_win <- d %>%
    dplyr::filter(outcome_prev1 == 1) %>%
    mutate(
      win_stay = ifelse(choice == choice_prev1, 1, 0)
    )
  df_win_stay_byblock <- df_after_win %>%
    group_by(block) %>%
    summarise(
      win_stay = mean(win_stay, na.rm = TRUE)
    ) %>%
    ungroup()
  # compute lose-shift
  # if there was a negative feedback in the previous trial,the image chosen
  # in the current trial differs from the image shown in the previous trial?
  df_after_loss <- d %>%
    group_by(block) %>%
    dplyr::filter(outcome_prev1 == 0) %>%
    mutate(
      lose_shift = ifelse(choice != choice_prev1, 1, 0)
    ) %>%
    ungroup()
  df_lose_shift_byblock <- df_after_loss %>%
    group_by(block) %>%
    summarise(
      lose_shift = mean(lose_shift, na.rm = TRUE)
    ) %>%
    ungroup()
  # merge winstay_mat and lose_shift by block
  df <- df_win_stay_byblock %>%
    inner_join(df_lose_shift_byblock, by = "block")
  df$subj_id <- unique(d$subj_id)
  # return
  df
}



get_pers_err <- function(dd) {
  
  # get the lengths of the sequences of rewarded trials
  fdbk_prob <- dd$fdb_prob
  info_seq <- rle(as.numeric(fdbk_prob))
  # choices in the second sequence of trials
  ch_1 = dd$choice[
    (info_seq$lengths[1] + 1):
      (info_seq$lengths[1] + info_seq$lengths[2])
    ]
  # choices in the third sequence of trials
  ch_2 = dd$choice[
    (info_seq$lengths[1] + info_seq$lengths[2] + 1):
      (info_seq$lengths[1] + info_seq$lengths[2] + info_seq$lengths[3])
    ]
  # choices in the forth sequence of trials
  ch_3 = dd$choice[
    (info_seq$lengths[1] + info_seq$lengths[2] + info_seq$lengths[3] + 1):
      (info_seq$lengths[1] + info_seq$lengths[2] + info_seq$lengths[3] + info_seq$lengths[4])
    ]
  cons_seq1 <- rle(ch_1)
  p_err1 <- ifelse(
    cons_seq1$values[1] == 1 & ch_1[1] == 1 & cons_seq1$lengths[1] > 1, 
    # ch_1[1] == 1 means that the first choice after the sequence of
    # 35 trials (in the example) is again 1: img1 was chosen (img1 is
    # the image that is rewared in the first sequence of trials). This
    # is a perseverative error. 
    # cons_seq1$lengths[1] > 1 means that there are at least two sequential
    # perseverative errors.
    # cons_seq1$values[1] == 1 means that the output of rle() lists the
    # number of consecutive '1', the number of consecutive '2', the number
    # of consecutive '1', in this order.
    cons_seq1$lengths[1], 0
  )
  # cons_seq1$lengths[1] means the number of consecutive perseverative
  # errors (greater than 1), in the case in which the first trial in which 
  # the reward rule has changed, the response is a perseverative error.
  cons_seq2 <- rle(ch_2)
  p_err2 <- ifelse(
    cons_seq2$values[1] == 2 & ch_2[1] == 2 & cons_seq2$lengths[1] > 1,
    cons_seq2$lengths[1], 0
  )
  cons_seq3 <- rle(ch_3)
  p_err3 <- ifelse(
    cons_seq3$values[1] == 1 & ch_3[1] == 1 & cons_seq3$lengths[1] > 1, 
    cons_seq3$lengths[1], 0
  )
  prop_pers_err <- (p_err1 + p_err2 + p_err3) / 
    (info_seq$lengths[2] + info_seq$lengths[3] + info_seq$lengths[4])
  
  data.frame(
    subj_name = as.character(unique(dd$subj_name)),
    block = unique(dd$block),
    prop_pers_err
  )
  
}


#' Proportion of errors in the second, third, and forth epochs. 
#' From this proportion it is necessary to subtract the proportion of 
#' perseverative errors to get the chance errors.
get_prop_errors <- function(dd) {
  
  # get the lengths of the sequences of rewarded trials
  fdbk_prob <- dd$fdb_prob
  info_seq <- rle(as.numeric(fdbk_prob))
  # choices in the second sequence of trials
  ch_1 = dd$choice[
    (info_seq$lengths[1] + 1):(info_seq$lengths[1] + info_seq$lengths[2])
    ]
  # choices in the third sequence of trials
  ch_2 <- dd$choice[
    (info_seq$lengths[1] + info_seq$lengths[2] + 1):
      (info_seq$lengths[1] + info_seq$lengths[2] + info_seq$lengths[3])
    ]
  # choices in the forth sequence of trials
  ch_3 <- dd$choice[
    (info_seq$lengths[1] + info_seq$lengths[2] + info_seq$lengths[3] + 1):
      (info_seq$lengths[1] + info_seq$lengths[2] + info_seq$lengths[3] +
         info_seq$lengths[4])
    ]
  # In the first sequence the correct response is 1, in the second is 2, so on.
  tot_err1 <- sum(ch_1 == 1)
  # choice == 1 in the second sequence is an error
  tot_err2 <- sum(ch_2 == 2)
  # choice == 2 in the third sequence is an error
  tot_err3 <- sum(ch_3 == 1)
  # choice == 1 in the forth sequence is an error
  
  prop_err <- (tot_err1 + tot_err2 + tot_err3) /
    (length(ch_1) + length(ch_2) + length(ch_3))
  
  data.frame(
    subj_name = as.character(unique(dd$subj_name)),
    block = unique(dd$block),
    prop_err
  )
  
}










