# ---
# Populated Preanalysis Plan
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

dat <- read_dta("data/bonilla_tillery_clean.dta")

dat <-
  dat %>%
  transmute(
    female = (1 - male),
    lgbtq = lgbt_mem,
    age = ageR,
    religiosity = relfreqR,
    income = incomeR,
    college = college,
    blm_familiarity = familiarity,
    linked_fate = linkedfateR,
    Z = factor(treat, 0:3, c("general", "nationalism", "feminism", "intersectional")),
    blm_support = supportR,
  )

fit_1 <- lm_robust(blm_support ~ Z, data = dat)
fit_2 <- lm_robust(blm_support ~ Z + female + lgbtq + age + religiosity + income + college + linked_fate + blm_familiarity, data = dat)
fit_3 <- lm_robust(blm_support ~ Z*linked_fate, data = dat)
fit_4 <- lm_robust(blm_support ~ Z*blm_familiarity, data = dat)

bookreg(l = list(fit_1, fit_2, fit_3, fit_4), include.ci = FALSE)

female_df <-
  dat %>%
  group_by(female) %>%
  do(tidy(lm_robust(blm_support ~ Z, data = .))) %>%
  filter(term != "(Intercept)")

female_int <- lm_robust(blm_support  ~ Z*female, data = dat) %>% 
  tidy() %>% filter(str_detect(term, ":")) %>%
  mutate(female = 2,
         term = str_remove(term, ":female")) 

gg_df_1 <-
  female_df %>% 
  bind_rows(female_int) %>%
  mutate(facet = factor(female, 0:2, c("Men", "Women", "Difference")),
         term = str_to_sentence(str_remove(term, "Z")))

g1 <- 
  ggplot(gg_df_1, aes(estimate, term)) +
  geom_point() +
  geom_vline(xintercept = 0, color = gray(0.5), linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~facet, ncol = 1) +
  theme(axis.title.y = element_blank())

# LGBTQ difference in intersectional treatment

lgbtq_df <-
  dat %>%
  group_by(lgbtq) %>%
  do(tidy(lm_robust(blm_support ~ Z, data = .))) %>%
  filter(term != "(Intercept)")

lgbtq_int <- lm_robust(blm_support ~ Z*lgbtq, data = dat) %>% 
  tidy() %>% 
  filter(str_detect(term, ":")) %>%
  mutate(lgbtq = 2,
         term = str_remove(term, ":lgbtq")) 

gg_df_2 <-
  lgbtq_df %>% 
  bind_rows(lgbtq_int) %>%
  mutate(facet = factor(lgbtq, 0:2, c("LGTBQ", "Non-LGBTQ", "Difference")),
         term = str_to_sentence(str_remove(term, "Z")))

g2 <- 
  ggplot(gg_df_2, aes(estimate, term)) +
  geom_point() +
  geom_vline(xintercept = 0, color = gray(0.5), linetype = "dashed") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~facet, ncol = 1) +
  theme(axis.title.y = element_blank())



g1 + g2
