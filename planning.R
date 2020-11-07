# ---
# Planning
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

rescale <- function(x) (x - min(x)) / (max(x) - min(x))
likert_cut <- function(x)  as.numeric(cut(x, breaks = c(-100, 0.1, 0.3, 0.6, 0.8, 100), labels = 1:5))

model <- 
  declare_population(
    N = 800,
    female = rbinom(N, 1, prob = 0.51),
    lgbtq = rbinom(N, 1, prob = 0.05),
    linked_fate = sample(1:5, N, replace = TRUE, prob = c(0.05, 0.05, 0.15, 0.25, 0.5)),
    age = sample(18:80, N, replace = TRUE),
    religiosity = sample(1:6, N, replace = TRUE),
    income = sample(1:12, N, replace = TRUE),
    college = rbinom(N, 1, prob = 0.5),
    blm_familiarity = sample(1:4, N, replace = TRUE),
    U = runif(N),
    blm_support_latent = rescale(
      U + 0.1 * blm_familiarity + 0.45 * linked_fate + 0.001 * age + 
        0.25 * lgbtq + 0.01 * income + 0.1 * college + -0.1 * religiosity)
  ) + 
  declare_potential_outcomes(
    blm_support_Z_general = 
      likert_cut(blm_support_latent),
    blm_support_Z_nationalism = 
      likert_cut(blm_support_latent + 0.01 + 0.01 * linked_fate + 0.01 * blm_familiarity),
    blm_support_Z_feminism = 
      likert_cut(blm_support_latent - 0.02 + 0.07 * female + 0.01 * blm_familiarity),
    blm_support_Z_intersectional = 
      likert_cut(blm_support_latent  - 0.05 + 0.15 * lgbtq + 0.01 * blm_familiarity)
  )

inquiry <-  
  declare_estimands(
    
    # Average effects
    ATE_nationalism = mean(blm_support_Z_nationalism - blm_support_Z_general),
    ATE_feminism = mean(blm_support_Z_feminism - blm_support_Z_general),
    ATE_intersectional = mean(blm_support_Z_intersectional - blm_support_Z_general),
    
    # Overall heterogeneity w.r.t. blm_familiarity
    DID_nationalism_familiarity = 
      cov(blm_support_Z_nationalism - blm_support_Z_general, blm_familiarity)/var(blm_familiarity),
    DID_feminism_familiarity = 
      cov(blm_support_Z_feminism - blm_support_Z_general, blm_familiarity)/var(blm_familiarity),
    DID_intersectional_familiarity = 
      cov(blm_support_Z_intersectional - blm_support_Z_general, blm_familiarity)/var(blm_familiarity),
    
    # Treatment-specific heterogeneity
    DID_nationalism_linked_fate = 
      cov(blm_support_Z_nationalism - blm_support_Z_general, linked_fate)/var(linked_fate),
    DID_feminism_gender = 
      cov(blm_support_Z_feminism - blm_support_Z_general, female)/var(female),
    DID_intersectional_lgbtq = 
      cov(blm_support_Z_intersectional - blm_support_Z_general, lgbtq)/var(lgbtq)
  )

data_strategy <- 
  declare_assignment(conditions = c("general", "nationalism", "feminism", "intersectional"), simple = TRUE) + 
  declare_reveal(blm_support, Z) 

answer_strategy <-
  declare_estimator(
    blm_support ~ Z,
    term = c("Znationalism", "Zfeminism", "Zintersectional"),
    model = lm_robust,
    estimand = c("ATE_nationalism", "ATE_feminism", "ATE_intersectional"),
    label = "OLS"
  ) +
  declare_estimator(
    blm_support ~ Z + age + female + as.factor(linked_fate) + lgbtq,
    term = c("Znationalism", "Zfeminism", "Zintersectional"),
    estimand = c("ATE_nationalism", "ATE_feminism", "ATE_intersectional"),
    model = lm_robust,
    label = "OLS with controls"
  ) +
    declare_estimator(
    blm_support ~ Z*blm_familiarity,
    term = c("Znationalism:blm_familiarity", "Zfeminism:blm_familiarity", "Zintersectional:blm_familiarity"),
    model = lm_robust,
    estimand = c("DID_nationalism_familiarity", "DID_feminism_familiarity", "DID_intersectional_familiarity"),
    label = "DID_familiarity"
  ) +
  declare_estimator(
    blm_support ~ Z * linked_fate,
    term = "Zfeminism:linked_fate",
    model = lm_robust,
    estimand = "DID_nationalism_linked_fate",
    label = "DID_nationalism_linked_fate"
  ) +
  declare_estimator(
    blm_support ~ Z * female,
    term = "Zfeminism:female",
    model = lm_robust,
    estimand = "DID_feminism_gender",
    label = "DID_feminism_gender"
  ) +
  declare_estimator(
    blm_support ~ Z * lgbtq,
    term = "Zintersectional:lgbtq",
    model = lm_robust,
    estimand = "DID_intersectional_lgbtq",
    label = "DID_intersectional_lgbtq"
  )

design <- model + inquiry + data_strategy + answer_strategy
mock_data <- draw_data(design)

mock_data %>% head %>% kable(digits = 3, caption = "Mock analysis from Bonilla and Tillery design.")

fit_1 <- lm_robust(blm_support ~ Z, data = mock_data)
fit_2 <- lm_robust(blm_support ~ Z + female + lgbtq + age + religiosity + income + college + linked_fate + blm_familiarity, data = mock_data)
fit_3 <- lm_robust(blm_support ~ Z*linked_fate, data = mock_data)
fit_4 <- lm_robust(blm_support ~ Z*blm_familiarity, data = mock_data)

bookreg(l = list(fit_1, fit_2, fit_3, fit_4), include.ci = FALSE, digits = 3, caption = "Mock regression table from Bonilla and Tillery design.")

female_df <-
  mock_data %>%
  group_by(female) %>%
  do(tidy(lm_robust(blm_support ~ Z, data = .))) %>%
  filter(term != "(Intercept)")

female_int <- lm_robust(blm_support  ~ Z*female, data = mock_data) %>% 
  tidy() %>% filter(str_detect(term, ":")) %>%
  mutate(female = 2,
         term = str_remove(term, ":female")) 

gg_df_1 <-
  female_df %>% 
  bind_rows(female_int) %>%
  mutate(facet = factor(female, 0:2, c("Men", "Women", "Difference")),
         term = str_to_sentence(str_remove(term, "Z")))

lgbtq_df <-
  mock_data %>%
  group_by(lgbtq) %>%
  do(tidy(lm_robust(blm_support ~ Z, data = .))) %>%
  filter(term != "(Intercept)")

lgbtq_int <- lm_robust(blm_support ~ Z*lgbtq, data = mock_data) %>% 
  tidy() %>% 
  filter(str_detect(term, ":")) %>%
  mutate(lgbtq = 2,
         term = str_remove(term, ":lgbtq")) 

gg_df_2 <-
  lgbtq_df %>% 
  bind_rows(lgbtq_int) %>%
  mutate(facet = factor(lgbtq, 0:2, c("LGTBQ", "Non-LGBTQ", "Difference")),
         term = str_to_sentence(str_remove(term, "Z")))


g <- 
ggplot(data = NULL, aes(estimate, term)) +
  geom_point() +
  geom_vline(xintercept = 0, color = gray(0.5), linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~facet, ncol = 1) +
  theme(axis.title.y = element_blank())

(g %+% gg_df_1) +
  (g %+% gg_df_2) 





diagnosis %>% 
  get_simulations %>% 
  group_by(estimand_label, estimator_label) %>% 
  summarize(
    bias = mean(estimate - estimand, na.rm = TRUE),
    power = mean(p.value <= 0.05, na.rm = TRUE)
  ) %>% 
  mutate(estimand_label = str_replace_all(estimand_label, "_", " "),
         estimator_label = str_replace_all(estimator_label, "_", " ")) %>% 
  kable(digits = 3, col.names = c("Estimand", "Estimator", "Bias", "Power"), caption = "Design diagnosis for Bonilla and Tillery design.")
