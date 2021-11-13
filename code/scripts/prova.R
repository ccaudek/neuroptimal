## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------


# # fix subject 259: block 2 is repeated twice
# if (subj_id == 259) {
#   temp <- one_subj_tot[-c(321:480), ]
#   one_subj_tot <- temp
# }

all_subj_dt <- data.frame(
  subjID = mydat$subj_id, 
  choice = mydat$choice,
  outcome = ifelse(mydat$outcome == 1, 1, -1)
)



# prl_ewa() needs a txt file as input.
write.table(
  all_subj_dt,
  here("scripts", "PRL", "mydata.txt"), 
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)


# Fit the Experience-Weighted Attraction Model (Ouden et al., 2013).
output <- prl_ewa(
  data = here("scripts", "PRL", "mydata.txt"), 
  niter = 5000,
  nwarmup = 1000,
  nchain = 4,
  ncore = 4
)

res <- output$allIndPars

res <- rename(res, 
              "subj_id" = "subjID")

descr_dt <- mydat %>% 
  group_by(subj_id) %>% 
  summarize(
    medrt = median(rt),
    tot_rew = mean(total_reward)
  ) 

dat <- left_join(res, descr_dt, by = "subj_id")

round(cor(d), 2)


n_sub <- length(unique(mydat$subj_id))
ws_list <- list()

for (subj_index in 1:n_sub) {
  
  d <- mydat %>% 
    dplyr::filter(subj_id == subj_index)
  
  out <- get_win_stay_lose_shift(mydat, subj_index)
  
  ws_list[[subj_index]] <- out
  
}

ws_df <- as.data.frame(do.call(rbind, ws_list))

d1 <- left_join(dat, ws_df, by = "subj_id")
round(cor(d1), 2)



