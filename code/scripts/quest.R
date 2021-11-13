library(tidyverse)
library(here)
library(rio)

library("brms")
library("cmdstanr")
set_cmdstan_path("/Users/corrado/cmdstan")


d0 <- rio::import(
  here("data", "raw", "data0.xlsx")
)

d1 <- rio::import(
  here("data", "raw", "data1.xlsx")
)

d2 <- rio::import(
  here("data", "raw", "data2.xlsx")
)

d0$dass_anx_0 <- d0$`Dass-ANX`
d1$dass_anx_1 <- d1$`Dass-ANX`
d2$dass_anx_2 <- d2$`Dass-ANX`

mean(d0$dass_anx_0, trim = 0.1, na.rm = TRUE)
mean(d1$dass_anx_1, trim = 0.1, na.rm = TRUE)
mean(d2$dass_anx_2, trim = 0.1, na.rm = TRUE)

d0$dass_dep_0 <- d0$`Dass-DEP`
d1$dass_dep_1 <- d1$`Dass-DEP`
d2$dass_dep_2 <- d2$`Dass-DEP`

mean(d0$dass_dep_0, trim = 0.1, na.rm = TRUE)
mean(d1$dass_dep_1, trim = 0.1, na.rm = TRUE)
mean(d2$dass_dep_2, trim = 0.1, na.rm = TRUE)


d0$dass_str_0 <- d0$`Dass-STRESS`
d1$dass_str_1 <- d1$`Dass-STRESS`
d2$dass_str_2 <- d2$`Dass-STRESS`

mean(d0$dass_str_0, trim = 0.1, na.rm = TRUE)
mean(d1$dass_str_1, trim = 0.1, na.rm = TRUE)
mean(d2$dass_str_2, trim = 0.1, na.rm = TRUE)

# Control

c0 <- rio::import(
  here("data", "raw", "control", "control0.xlsx")
)

c2 <- rio::import(
  here("data", "raw", "control", "control2.xlsx")
)



c0$dass_anx_0 <- c0$`Dass-ANX`
c2$dass_anx_2 <- c2$`Dass-ANX`

mean(c0$dass_anx_0, trim = 0.1, na.rm = TRUE)
mean(c2$dass_anx_2, trim = 0.1, na.rm = TRUE)



c0$dass_dep_0 <- c0$`Dass-DEP`
c2$dass_dep_2 <- c2$`Dass-DEP`

mean(c0$dass_dep_0, trim = 0.1, na.rm = TRUE)
mean(c2$dass_dep_2, trim = 0.1, na.rm = TRUE)




c0$dass_str_0 <- c0$`Dass-STRESS`
c2$dass_str_2 <- c2$`Dass-STRESS`

mean(c0$dass_str_0, trim = 0.1, na.rm = TRUE)
mean(c2$dass_str_2, trim = 0.1, na.rm = TRUE)


y <- c(
  c0$dass_str_0,
  c2$dass_str_2
)

gr <- rep(c(0, 1), each = 10)


y <- c(
  d0$dass_anx_0, d1$dass_anx_1, d2$dass_anx_2,
  c0$dass_anx_0, c2$dass_anx_2 
)

grp <- c(
  rep(
    1, length(d0$dass_anx_0) + length(d1$dass_anx_1) + length(d2$dass_anx_2)
  ),
  rep(
    0, length(c0$dass_anx_0) + length(c2$dass_anx_2)
  )
)

t <- c(
  rep(0, length(d0$dass_anx_0)),
  rep(1, length(d1$dass_anx_1)),
  rep(2, length(d2$dass_anx_2)),
  rep(0, length(c0$dass_anx_0)),
  rep(2, length(c2$dass_anx_2))
)

dat <- data.frame(grp, t, y)
dat$y <- dat$y + 0.1
# dat$grp <- factor(dat$grp)
# dat$t <- dat$t + 1


dat %>% 
  group_by(grp, t) %>% 
  summarise(
    m = mean(y, trim = 0.1)
  )


priors <- prior(normal(0, 2), class = "b") + 
  prior(normal(0, 5), class = "Intercept")

m1 <- brm(
  y ~ mo(t) * grp, 
  data = dat, 
  # prior = priors,
  family = lognormal(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 4
  #backend = "cmdstan"
)

pp_check(m1) + xlim(0, 50)
summary(m1)

plot(conditional_effects(m1, "t:grp"))

m0 <- brm(
  y ~ grp, 
  data = dat, 
  # prior = priors,
  family = lognormal(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 4
  #backend = "cmdstan"
)

pp_check(m0) + xlim(0, 50)
summary(m0)


dd <- dat %>% 
  dplyr::filter(grp == "1")


m2 <- brm(
  y ~ mo(t), 
  data = dd, 
  prior = priors,
  family = lognormal(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 4
  #backend = "cmdstan"
)

pp_check(m2) + xlim(0, 50)
summary(m2)

plot(conditional_effects(m2, "t"))

bayes_R2(m1)
bayes_R2(m2)



# sequential difference coding of factors
sdif_coding <- function(x) {
  x <- as.factor(x)
  contrasts(x) <- MASS::contr.sdif(levels(x))
  x
}


dat$tf <- sdif_coding(dat$t)

m3 <- brm(
  y ~ tf * grp, 
  data = dat, 
  # prior = priors,
  family = lognormal(),
  control = list(adapt_delta = 0.98),
  iter = 10000,
  cores = 4,
  backend = "cmdstan"
)

pp_check(m3) + xlim(0, 50)
summary(m3)
plot(conditional_effects(m3, "t:grp"))
bayes_R2(m3)
