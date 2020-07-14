# ---
# The Fundamental 2x2 of Research Design
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

inquiry <- function(x) {
  case_when(x <= 1 / 3 ~ 0,
            x > 1 / 3 & x <= 2 / 3 ~ (x * 3 - 1),
            x > 2 / 3 ~ 1)
}

design <-
  declare_population(
    N = 1000,
    X_star = seq(0, 1, length.out = N),
    error = rnorm(N, 0, 0.05),
    prob_Y = inquiry(X_star + error),
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
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "Latent treatment variable X", 
       y = "latent outcome variable Y",
       title = "The inquiry under the model")

ggplot(dat, aes(X5, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 5)}, size = 4, alpha = 0.5) +
  geom_point(alpha = 0.5, position = position_jitter_ellipse(width = 0.1, height = 0.05)) +
  stat_smooth(method = "lm", se = TRUE) +
  theme_bw() +
  labs(x = "Measured treatment variable X (1 - 5 Likert Scale)", 
       y = "Measured outcome variable Y (0 - 1 binary scale)",
       title = "Answer strategy 1 under Data strategy 1")

a2_d1 <-
  dat %>%
  group_by(X5) %>%
  do(tidy(lm_robust(Y_obs ~ 1, data = .))) %>%
  mutate(Y_obs = estimate)


ggplot(a2_d1, aes(X5, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 5)}, size = 4, alpha = 0.5) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.1) +
  geom_point(data = dat, alpha = 0.5, position = position_jitter_ellipse(width = 0.2, height = 0.1)) +
  theme_bw()+
  labs(x = "Measured treatment variable X (1 - 5 Likert Scale)", 
       y = "Measured outcome variable Y (0 - 1 binary scale)",
       title = "Answer strategy 2 under Data strategy 1")

a2_d2 <-
  dat %>%
  group_by(X9) %>%
  do(tidy(lm_robust(Y_obs ~ 1, data = .))) %>%
  mutate(Y_obs = estimate)


ggplot(a2_d2, aes(X9, Y_obs)) +
  geom_path(stat = "function", fun = function(x){inquiry((x - 0.5) / 9)}, size = 4, alpha = 0.5) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.1) +
  geom_point(data = dat, alpha = 0.5, position = position_jitter_ellipse(width = 0.2, height = 0.05)) +
  theme_bw()+
  labs(x = "Measured treatment variable X (1 - 9 Likert Scale)", 
       y = "Measured outcome variable Y (0 - 1 binary scale)",
       title = "Answer strategy 2 under Data strategy 2")
