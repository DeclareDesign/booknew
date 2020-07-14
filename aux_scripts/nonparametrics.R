

library(tidyverse)
library(DeclareDesign)
source("scripts/vayr.R")


inquiry <- function(x) {
  case_when(x <= 1 / 3 ~ 0,
            x > 1 / 3 & x <= 2 / 3 ~ (x * 3 - 1),
            x > 2 / 3 ~ 1)
}

design <-
  declare_population(
    N = 100,
    X_star = seq(0, 1, length.out = N),
    prob_Y = inquiry(X_star),
    Y_star = rbinom(N, 1, prob_Y)
  ) +
  # data strategy has measurement error in Y
  declare_measurement(
    Y_obs = rbinom(N, 1, prob = Y_star * 0.95 + (1 - Y_star) * 0.05)
  ) + 
  # Two alternative measurement strategies for X
  declare_measurement(
    X5 = as.numeric(cut(X_star, breaks = seq(0, 1, length.out = 6), include.lowest = TRUE))) +
  declare_measurement(
    X9 = as.numeric(cut(X_star, breaks = seq(0, 1, length.out = 10), include.lowest = TRUE)))

dat <- draw_data(design)

# Truth
ggplot(dat, aes(X_star, prob_Y)) +
  geom_path(stat = "function", fun = inquiry, size = 4, alpha = 0.5) +
  geom_point() +
  theme_bw()
  
# A1 + D1

ggplot(dat, aes(X5, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 5)}, size = 4, alpha = 0.5) +
  geom_point(position = position_jitter_ellipse(width = 0.1, height = 0)) +
  stat_smooth(method = "lm_robust", se = TRUE) +
  theme_bw()
  

# A1 + D2

ggplot(dat, aes(X9, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 9)}, size = 4, alpha = 0.5) +
  geom_point(position = position_jitter_ellipse(width = 0.1, height = 0)) +
  stat_smooth(method = "lm_robust", se = TRUE) +
  theme_bw()

# A2 + D1

a2_d1 <-
  dat %>%
  group_by(X5) %>%
  do(tidy(lm_robust(Y_obs ~ 1, data = .))) %>%
  mutate(Y_obs = estimate)


ggplot(a2_d1, aes(X5, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 5)}, size = 4, alpha = 0.5) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_line() + 
  geom_point(data = dat, position = position_jitter_ellipse(width = 0.1, height = 0)) +
  theme_bw() +
  coord_cartesian(xlim = c(1, 5))





a2_d2 <-
  dat %>%
  group_by(X9) %>%
  do(tidy(lm_robust(Y_obs ~ 1, data = .))) %>%
  mutate(Y_obs = estimate)


ggplot(a2_d2, aes(X9, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 9)}, size = 4, alpha = 0.5) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_line() + 
  geom_point(data = dat, position = position_jitter_ellipse(width = 0.1, height = 0)) +
  theme_bw() +
  coord_cartesian(xlim = c(1, 9))






