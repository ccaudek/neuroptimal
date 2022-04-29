



thedat %>% 
  group_by(time, group, condition) %>% 
  summarise(
    avg_Panas_P = mean(Panas_P), 
    avg_Panas_N = mean(Panas_N),
    avg_Dass_D = mean(Dass_DEP), 
    avg_Dass_A = mean(Dass_ANX),
    avg_Dass_S = mean(Dass_STRESS),
    avg_SDMT_GR = mean(SDMT_GR, na.rm = TRUE),
    avg_PSQI = mean(PSQI),
    avg_SWLS = mean(SWLS),
    avg_BMQP_GR = mean(BMQP_GR),
    avg_BREQP_GR = mean(BREQP_GR),
    avg_BSCQP_GR = mean(BSCQP_GR), 
    avg_BDQP_GR = mean(BDQP_GR),
    avg_BIQF_GR = mean(as.numeric(as.character(BIQF_GR)))
  )


time_02 <- thedat %>% 
  dplyr::filter(time != 1) %>% 
  dplyr::select(
    Panas_P ,
    Panas_N ,
    Dass_DEP,
    Dass_ANX,
    Dass_STRESS,
    # SDMT_GR ,
    PSQI,
    SWLS,
    time
  )

time_0 <- time_02  %>% 
  dplyr::filter(time == 0)
time_0$time <- NULL
time_0m <- as.matrix(time_0)

time_2 <- time_02  %>% 
  dplyr::filter(time == 2)
time_2$time <- NULL
time_2m <- as.matrix(time_2)

time_diff <- time_2m - time_0m


Y <- thedat %>% 
  dplyr::filter(time != 1) %>% 
  dplyr::select(
    Panas_P ,
    Panas_N ,
    Dass_DEP,
    Dass_ANX,
    Dass_STRESS,
    # SDMT_GR ,
    PSQI,
    SWLS
  )


S <- cov(Y)
S


e <- eigen(S)

sum(e$val[1:2]) / sum(e$val)
