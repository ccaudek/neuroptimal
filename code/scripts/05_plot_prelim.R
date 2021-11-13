# Preliminary analyses Neuroptimal


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


# foo <- prl_dat %>% 
#   dplyr::filter(is_neuroptimal == 0)
# 
# table(foo$time, foo$subj_id, foo$block)

# plot(out$trial, out$mc, type = 'l')


hist(prl_dat$total_reward)


prl_dat %>% 
  group_by(is_neuroptimal, time) %>% 
  summarise(
    avg_tot_rew = mean(total_reward),
    avg_rt = mean(rt, trim = 0.2, na.rm = TRUE)
  )


# response	stim	rt	trial	feedback	subj_code	block	q_init	split_by	subj_idx

data_for_hddm <- tibble(
  response = prl_dat$choice - 1,
  rt = prl_dat$rt,
  trial = prl_dat$trial,
  feedback = prl_dat$outcome,
  subj_idx = prl_dat$subj_id,
  is_neuroptimal = prl_dat$is_neuroptimal,
  block = prl_dat$block,
  q_init = 0.5,
  split_by = as.integer(factor(as.character(paste0(prl_dat$block, prl_dat$is_neuroptimal)))),
  # split_by = prl_dat$block,
  time = prl_dat$time
) 

# Multiple imputation on NAs.
temp <- data.frame(
  rt         = data_for_hddm$rt, 
  block      = data_for_hddm$block,
  trial       = data_for_hddm$trial,
  feedback    = data_for_hddm$feedback, 
  response    = data_for_hddm$response - 1
)

temp$rt <- ifelse(temp$rt > 8 | temp$rt < 0.05, NA, temp$rt)

# Imputes the "best value" according to the linear regression model, also 
# known as regression imputation.
imp <- mice::mice(temp, method = "norm.predict", m = 1) 
temp <- complete(imp)

# d$feedback <- ifelse(temp$feedback > 0.5, 1, 0)
data_for_hddm$rt <- temp$rt
hist(data_for_hddm[data_for_hddm$rt < 1, ]$rt)

data_for_hddm$subj_id <- data_for_hddm$subj_idx
data_for_hddm$subj_idx <- NULL


grpid = function(x) match(x, unique(x))
# then
data_for_hddm_2 <- data_for_hddm %>% 
  mutate(subj_idx = group_indices(., subj_id) %>% grpid)

unique(data_for_hddm_2$subj_idx)

data_for_hddm_2$subj_id <- NULL

summary(data_for_hddm_2)



rio::export(
  data_for_hddm_2,
  here::here(
    "python", "input_for_hddm.csv"
  )
)


only_time2 <- data_for_hddm_2 %>% 
  dplyr::filter(time == 2)
only_time2$subj_id <- only_time2$subj_idx
only_time2$subj_idx <- NULL


grpid = function(x) match(x, unique(x))
# then
last <- only_time2 %>% 
  mutate(subj_idx = group_indices(., subj_id) %>% grpid)

unique(last$subj_idx)

rio::export(
  last,
  here::here(
    "python", "input_for_hddm.csv"
  )
)


summary(last)






