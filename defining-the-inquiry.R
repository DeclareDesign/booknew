# ---
# Defining the inquiry
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)

set.seed(1)
dat <- fabricate(N = 500, X = rnorm(N), Y = 0.5 *X + -0.5 * X^2 + -0.5* X^3 + 0.05 * X^4 + rnorm(N)) %>%
  mutate(Y = Y - mean(Y))

fit_1 <- lm(Y ~ 1, data = dat)
fit_2 <- lm(Y ~ X, data = dat)
fit_3 <- lm(Y ~ X + I(X^2), data = dat)
fit_4 <- lm(Y ~ X + I(X^2) + I(X^3), data = dat)

newdata = tibble(X = seq(-3, 3, length.out = 1000))
dat1 <- newdata %>% mutate(pred = predict(fit_1, newdata))
dat2 <- newdata %>% mutate(pred = predict(fit_2, newdata))
dat3 <- newdata %>% mutate(pred = predict(fit_3, newdata))
dat4 <- newdata %>% mutate(pred = predict(fit_4, newdata))

gg_df <- bind_rows( `1` = dat1, `2` = dat2, `3` = dat3, `4` = dat4,.id = "complexity") %>%
  mutate(complexity = factor(
    complexity,
    levels = 1:4,
    labels = c(
      paste0('One number summary:\n(',paste(round(coef(fit_1), 2)), ")"),
      paste0('Two number summary:\n(', paste(round(coef(fit_2), 2), collapse = ", "), ")"),
      paste0('Three number summary:\n(', paste(round(coef(fit_3), 2), collapse = ", "), ")"),
      paste0('Four number summary:\n(', paste(round(coef(fit_4), 2), collapse = ", "), ")")
    )
  ))

ggplot(gg_df, aes(X, pred)) +
  geom_line(color = "blue") +
  facet_wrap(~complexity, nrow = 1) +
  geom_point(data = dat, aes(X, Y), stroke = 0, alpha = 0.1) +
  coord_cartesian(ylim = c(-10, 10)) + 
  theme_void()

model <-
  declare_model(
    N = 100,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * D + U, 
                       conditions = list(D = c(0, 1))),
    D = rbinom(N, 1, .5)
  ) +
  declare_measurement(Y = reveal_outcomes(Y ~ D))

inquiry <-
  declare_inquiry(
    treatment_group_mean = mean(Y[D == 1]),
    ATE = mean(Y_D_1 - Y_D_0),
    probability_of_causation = mean((Y_D_0 < 0)[D == 1 & Y_D_1 > 0])
  )

draw_inquiries(model + inquiry)

draw_inquiries(model + inquiry) %>% 
  kable(caption = "One model three estimands.", digits = 3, booktabs = TRUE)
