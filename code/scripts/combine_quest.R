library("tidyverse")
library("here")
library("rio")
library("lme4")

library("tidybayes")
library("brms")
library("cmdstanr")
library("performance")
library("robcor")



abicot0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABI_CO_T0.xlsx"
  )
)
abicot0$group <- "ABI"
abicot0$condition <- "co"
abicot0$time <- 0
dim(abicot0)
names(abicot0)


abicot2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABI_CO_T2.xlsx"
  )
)
abicot2$group <- "ABI"
abicot2$condition <- "co"
abicot2$time <- 2
dim(abicot2)


# setdiff(
#   names(abicot2),
#   names(abispt2)
# )

abispt0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABI_SP_T0.xlsx"
  )
)
abispt0$group <- "ABI"
abispt0$condition <- "sp"
abispt0$time <- 0
dim(abispt0)
abispt0$BIQP_CR <- as.numeric(abispt0$BIQP_CR)
abispt0$BIQF_CR <- as.numeric(abispt0$BIQF_CR)


abispt1 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABI_SP_T1.xlsx"
  )
)
abispt1$group <- "ABI"
abispt1$condition <- "sp"
abispt1$time <- 1
dim(abispt1)


abispt2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "ABI_SP_T2.xlsx"
  )
)
abispt2$group <- "ABI"
abispt2$condition <- "sp"
abispt2$time <- 2
dim(abispt2)



cgcot0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CG_CO_T0.xlsx"
  )
)
cgcot0$group <- "CG"
cgcot0$condition <- "co"
cgcot0$time <- 0
dim(cgcot0)



cgcot1 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CG_CO_T1.xlsx"
  )
)
cgcot1$group <- "CG"
cgcot1$condition <- "co"
cgcot1$time <- 1
dim(cgcot1)


cgcot2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CG_CO_T2.xlsx"
  )
)
cgcot2$group <- "CG"
cgcot2$condition <- "co"
cgcot2$time <- 2
dim(cgcot2)


cgspt0 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CG_SP_T0.xlsx"
  )
)
cgspt0$group <- "CG"
cgspt0$condition <- "sp"
cgspt0$time <- 0
dim(cgspt0)



cgspt1 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CG_SP_T1.xlsx"
  )
)
cgspt1$group <- "CG"
cgspt1$condition <- "sp"
cgspt1$time <- 1
dim(cgspt1)


cgspt2 <- rio::import(
  here(
    "data", "raw", "quest", "excel", "CG_SP_T2.xlsx"
  )
)
cgspt2$group <- "CG"
cgspt2$condition <- "sp"
cgspt2$time <- 2
dim(cgspt2)


thedat <- rbind(
  abicot0, abicot2, 
  abispt0, abispt1, abispt2,
  cgcot0, cgcot1, cgcot2,
  cgspt0, cgspt2, cgspt2
)
thedat$group <- factor(thedat$group)
thedat$condition <- factor(thedat$condition)






# Read hddmrl params
params <- rio::import(
  here("data", "processed", "prl", "hddm", "hddm_params.txt")
)

# get the first two characters from params_prl_ddm:
params$param <- substr(params$ii, start = 1, stop = 2)
# unique(params$param)

# get integers
params$id_param_string <- gsub(".*\\).", "", params$ii) 
# params_prl_ddm$id_param_string <- stringr::str_remove(params_prl_ddm$ii, "[).]")
params$subj_idx <- as.integer(readr::parse_number(params$id_param_string))
# unique(params$subj_idx)

params$p <- params$param %>% 
  dplyr::recode(
    "a_" = "a",
    "al" = "alpha_neg",
    "po" = "alpha_pos",
    "t_" = "t",
    "v_" = "v",
    "z_" = "z"
  )
# summary(factor(params$p))

params_clean <- params %>% 
  dplyr::select(
    subj_idx, p, mean
  ) %>% 
  dplyr::rename(
    value = mean
  ) %>% 
  group_by(subj_idx, p) %>% 
  summarise(
    y = mean(value)
  ) %>% 
  ungroup()

head(params_clean)

# create a wide data.frame with one column for each PRL parameter
params_wide <- params_clean %>%
  pivot_wider(names_from = p, values_from = y)

thedat <- thedat %>% 
  dplyr::rename(subj_idx = NUMERO)

dat <- full_join(params_wide, thedat, by = "subj_idx")

dat$lesion <- factor(dat$LESIONE)

hist(dat$alpha_neg)
m1 <- lm(
  alpha_neg ~ (Dass_DEP + Dass_ANX + Dass_STRESS) + 
    SWLS + Panas_P + Panas_N + ADL + IADL +
    lesion + ETA + SESSO + SCOLARITA + SDMT_CR,
  data = dat
)
car::Anova(m1)
summary(m1)

hist(dat$alpha_pos)
m2 <- lm(
  alpha_pos ~ (Dass_DEP + Dass_ANX + Dass_STRESS) + 
    SWLS + Panas_P + Panas_N + ADL + IADL +
    lesion + ETA + SESSO + SCOLARITA + SDMT_CR,
  data = dat
)
car::Anova(m2)
summary(m2)

hist(dat$v)
m3 <- lm(
  v ~ (Dass_DEP + Dass_ANX + Dass_STRESS) + 
    SWLS + Panas_P + Panas_N + ADL + IADL +
    lesion + ETA + SESSO + SCOLARITA + SDMT_CR,
  data = dat
)
car::Anova(m3)

hist(dat$t)
m4 <- lm(
  t ~ (Dass_DEP + Dass_ANX + Dass_STRESS) + 
    SWLS + Panas_P + Panas_N + ADL + IADL +
    lesion + ETA + SESSO + SCOLARITA + SDMT_CR,
  data = dat
)
car::Anova(m4)


hist(dat$z)
m5 <- lm(
  z ~ (Dass_DEP + Dass_ANX + Dass_STRESS) + 
    SWLS + Panas_P + Panas_N + ADL + IADL +
    lesion + ETA + SESSO + SCOLARITA + SDMT_CR,
  data = dat
)
car::Anova(m5)





thedat %>% 
  group_by(group, condition, time) %>% 
  summarise(
    SDMT = mean(SDMT_CR, na.rm = TRUE),
    PSQI = mean(PSQI, na.rm = TRUE),
    SWLS = mean(SWLS, na.rm = TRUE),
    dass_d = mean(Dass_DEP, na.rm = TRUE),
    dass_a = mean(Dass_ANX, na.rm = TRUE),
    dass_s = mean(Dass_STRESS, na.rm = TRUE),
    BMQP_GR = mean(BMQP_GR, na.rm = TRUE)
  )

thedat <- thedat %>% 
  mutate(
    dass_tot = Dass_DEP + Dass_ANX + Dass_STRESS + 1
  )

thedat$time <- factor(thedat$time)

abi_dat <- thedat %>% 
  dplyr::filter(group == "ABI")
cg_dat <- thedat %>% 
  dplyr::filter(group == "CG")


mydata <- abi_dat %>% 
  dplyr::select(
    NUMERO, group, condition, time, BMQP_GR, Panas_P, Panas_N, BMQF_GR
  ) %>% 
  na.omit()

mydata$t <- ifelse(mydata$time == "0", 0, 1)

mydata %>% 
  group_by(group, condition, time) %>% 
  summarise(
    BMQP_GR = mean(BMQP_GR, na.rm = TRUE),
    BMQF_GR = mean(BMQF_GR, na.rm = TRUE),
    Panas_P = mean(Panas_P, na.rm = TRUE),
    Panas_N = mean(Panas_N, na.rm = TRUE),
    n = n()
  )


m <- lmer(
  BMQF_GR ~ condition * time  +
    (1 | NUMERO),
  mydata
)
summary(m)


brmf <- bf(
  BMQF_GR ~ time * condition +
    (1 | NUMERO)
    
)


m <- brm(
  brmf,
  data = mydata, 
  # prior = priors,
  family = gaussian(),
  # control = list(adapt_delta = 0.99, max_treedepth = 25),
  iter = 10000,
  cores = 12,
  # inits = list_of_inits,
  backend = "cmdstan",
  seed = 3534534
)


pp_check(m, ndraws = 100) #+ xlim(-15, 15)
loo1 <- loo(m)
plot(loo1)

summary(m)

bayes_R2(m2)
plot(
  conditional_effects(
    m, 
    effects = "t:condition"
  ),
  points = FALSE, 
  rug = FALSE
)

hypothesis(m2, "m:d > 0")




