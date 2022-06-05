
Packages <- c("tidyverse", "rio", "here", "brms", "emmeans", "tidybayes")

lapply(Packages, library, character.only = TRUE)

params_long <- rio::import(
  here::here("data", "processed", "PRL", "hddm", "hddm_params.txt")
)

names(params_long)

d_long <- params_long %>% 
  dplyr::select("param", "is_neuroptimal", "time", "idx", "mean")

d_wide <- d_long %>% 
  tidyr::pivot_wider(
  names_from = c("param", "time"),
  values_from = mean
)

d_wide <- d_wide %>% 
  mutate(
    pos_alpha_diff = pos_alpha_2 - pos_alpha_0
  )

hist(d_wide$pos_alpha_diff)

d <- d_wide %>% 
  dplyr::select(pos_alpha_diff, is_neuroptimal, idx) %>% 
  na.omit()

d %>% 
  group_by(is_neuroptimal) %>% 
  summarise(
    m = mean(pos_alpha_diff)
  )

df <- d_long[d_long$param == "pos_alpha", ] 

hist(df$time)
hist(df$is_neuroptimal)

table(df$is_neuroptimal, df$time)

df1 <- df %>% 
  dplyr::filter(time != 1)

df$time <- factor(df$time)

bf_pos_alpha <- bf(mean ~ is_neuroptimal * time + (1 + time | idx)) +
  # lf(sigma ~ 0 + time) + 
  gaussian()

mod <- brm(
  bf_pos_alpha,
  data   = df, 
  family = gaussian(),
  warmup = 2000, 
  iter   = 4000, 
  chains = 4, 
  backend = "cmdstan"
)

summary(mod)

pp_check(mod)


# https://gist.github.com/jebyrnes/d1bdea4ad4c8736c4b9e7ac290e8c940

# summarize via 'emmeans'
rg <- ref_grid(mod)
em <- emmeans(rg, "is_neuroptimal")
summary(em, point.est = mean)

#get the adjusted means
warp_em <- emmeans(mod,  ~ time | is_neuroptimal)
warp_em

#get all possible contrasts
cont <- contrast(warp_em, "tukey")
cont


#get the posterior draws from the contrasts
cont_posterior <- gather_emmeans_draws(cont)

#plot
ggplot(cont_posterior,
       aes(y = contrast, x = .value)) +
  geom_halfeyeh() +
  facet_wrap(~wool) +
  geom_vline(xintercept = 0, color = "red", lty = 2)


ggplot(cont_posterior,
       aes(y = contrast, x = .value, fill = wool, group = wool)) +
  geom_halfeyeh(alpha = 0.5)+
  geom_vline(xintercept = 0, color = "red", lty = 2)

#need to figure out how to make this look all posterior-y
emmip(warp.brms,  ~ tension | wool, CIs = TRUE, cov.reduce = range)




mod %>%
  emmeans( ~ time | is_neuroptimal) %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = time, y = .value)) +
  geom_eye() +
  stat_summary(aes(group = NA), fun.y = mean, geom = "line") +
  facet_grid(~ is_neuroptimal) 


mod %>%
  emmeans( ~ time | is_neuroptimal) %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = time, y = .value)) +
  stat_lineribbon(fill = "gray25", color = "gray25", size = .5, alpha = 1/41, .width = ppoints(40)) +
  stat_pointinterval() +
  facet_grid(~ is_neuroptimal) 

dat <- d_long[d_long$param == "alpha", ] 
dat1 <- dat %>% 
  dplyr::filter(time != 1)

mod2 <- brm(
  mean ~ is_neuroptimal * time + (1 + time | idx),
  data   = dat1, 
  family = gaussian(),
  warmup = 2000, 
  iter   = 4000, 
  chains = 4, 
  inits  = "random",
  backend = "cmdstan"
)

summary(mod2)

mod2 %>%
  emmeans( ~ time | is_neuroptimal) %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = time, y = .value)) +
  stat_lineribbon(fill = "gray25", color = "gray25", size = .5, alpha = 1/41, .width = ppoints(40)) +
  stat_pointinterval() +
  facet_grid(~ is_neuroptimal) 


