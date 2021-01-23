# ---
# Pivoting
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

## design <-
##   declare_population(N = 744,
##                      U = rnorm(N),
##                      potential_outcomes(Y ~ 0.1 * Z + U, conditions = list(
##                        Z = c(
##                          "backups",
##                          "pure_control",
##                          "film_placebo_text_placebo",
##                          "film_placebo_text_treat",
##                          "film_treat_text_placebo",
##                          "film_treat_text_treat"
##                        )
##                      ))) +
##   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
##   declare_sampling(n = 215) +
##   declare_assignment(Z = complete_ra(
##     N,
##     m_each = c(15, 40, 40, 40, 40, 40),
##     conditions = c(
##       "backups",
##       "pure_control",
##       "film_placebo_text_placebo",
##       "film_placebo_text_treat",
##       "film_treat_text_placebo",
##       "film_treat_text_treat"
##     )
##   ), handler = fabricate) +
##   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
##   declare_estimator(Y ~ Z, model = lm_robust)
## 
## draw_data(design)
## 

## design <-
##   declare_population(
##     towers = add_level(
##       N = 744,
##       U_tower = rnorm(N)
##     ),
##     citizens = add_level(
##       N = 14,
##       U = rnorm(N),
##       potential_outcomes(Y ~ 0.1 * Z + U + U_tower)
##     )
##   ) +
##   declare_sampling(clusters = towers) +
##   declare_sampling(n = 109, order_by = U_tower, handler = slice_max) +
##   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
##   declare_assignment(Z_film = cluster_ra(N, clusters = towers, prob = 0.5), handler = fabricate) +
##   declare_assignment(Z_sms = cluster_ra(N, clusters = towers, conditions = 7:14), handler = fabricate) +
##   declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
##   declare_estimator(Y ~ Z, clusters = towers, model = lm_robust)
## 
## draw_data(design)
