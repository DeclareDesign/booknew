# ---
# Piloting
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

full_dist <- 
  tibble(
    effect_size = seq(0.1, 0.51, by = 0.01),
    N_required = sapply(effect_size, FUN = function(x) power.t.test(delta = x, sd = 1, power = 0.8)$n *2)
  )
ggplot(full_dist, aes(effect_size, N_required)) +
  geom_line() +
  coord_cartesian(xlim = c(0, 0.5)) +
  dd_theme()

prior <- 0.3
prior_sd <- 0.1

calculate_n_required <- function(delta, sd = 1, power = 0.8) {
  sapply(
    delta,
    FUN = function(x)
      power.t.test(
        delta = x,
        sd = sd,
        power = power
      )$n * 2
  )
  
}

pilot <-
  declare_model(N = 100,
                U = rnorm(N),
                potential_outcomes(Y ~ 0.1 * Z + U)) +
  declare_inquiry(SATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N), legacy = FALSE) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "SATE")





pilot_sims <-
  pilot_sims %>%
  filter(N_required <= 7500, !is.na(N_required))
choices_df <-
  tibble(
    xintercept = c(calculate_n_required(qnorm(0.1, prior, prior_sd)),
                   calculate_n_required(0.1),
                   mean(pilot_sims$N_required)),
    label = c("Choice of N without pilot",
              "Best choice of N",
              "Average choice of N with pilot"),
    x = xintercept + 100,
    y = c(130, 110, 95)
  )

ggplot(pilot_sims, aes(N_required)) + 
  geom_histogram(bins = 40) +
  geom_segment(data = choices_df, aes(x = xintercept, xend = xintercept, y = 0, yend = y - 5, color = label)) +
  geom_text(data = choices_df, aes(x = x, y = y, label = label, color = label), hjust = 0.2) +
  dd_theme() +
  theme(legend.position = "none") +
  labs(x = "Distribution of choice of N, depending on results of pilot")
