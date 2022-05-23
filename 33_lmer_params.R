library("here")
library("tidyverse")
library("brms")
library("rio")
library("lme4")
library("cmdstanr")

d <- rio::import(
  here::here("data", "processed", "PRL", "hddm", "hddm_params_clean.txt")
) %>% 
  dplyr::filter(time != 1)


alpha_pos_df <- d %>% 
  dplyr::filter(param == "alpha") %>% 
  dplyr::select(param, mean, time, is_neuroptimal, sub_idx)


hist(alpha_pos_df$mean)

fm <- lmer(
  mean ~ is_neuroptimal * time + (time | sub_idx),
  data = alpha_pos_df
)

summary(fm)


alpha_df <- d %>% 
  dplyr::filter(param == "alpha" | param == "pos_alpha" | param == "v") %>% 
  dplyr::select(param, mean, time, is_neuroptimal, sub_idx)

alpha_w_df <- alpha_df %>%
  pivot_wider(names_from = param, values_from = mean)
# alpha_w_df$is_neuroptimal <- factor(alpha_w_df$is_neuroptimal)
# alpha_w_df$time <- factor(alpha_w_df$time)


hist(1/alpha_w_df$v / 100000)

alpha_w_df$slow <- 1/alpha_w_df$v / 100000

bform0 <- bf(pos_alpha ~ is_neuroptimal * time + (time | sub_idx)) 
# bform0 <- bf(alpha ~ is_neuroptimal * time + (time | sub_idx)) 

fit0 <- brm(bform0, data = alpha_w_df, chains = 4, cores = 4, backend = "cmdstan")
summary(fit0)






bform1 <- bf(
  mvbind(alpha, pos_alpha, slow) ~ 1 + is_neuroptimal + time + is_neuroptimal:time + (time | sub_idx)
  ) +
  set_rescor(TRUE)

fit1 <- brm(bform1, data = alpha_w_df, chains = 4, cores = 4, backend = "cmdstan")

pp_check(fit1, resp = "alpha")
pp_check(fit1, resp = "posalpha")
pp_check(fit1, resp = "slow")

summary(fit1)

conditional_effects(fit0)






summary(fit1)


