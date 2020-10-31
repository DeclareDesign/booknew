# ---
# Ethical Review
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

dat <- 
  tibble(
    X = seq(1, 10, 0.001),
    cost =  4* X + 6 ,
    high = log(X, base = 1.05),
    med = log(X, base = 1.06),
    low = log(X, base = 1.10)
  ) 

dat_long <-
  dat %>% 
  pivot_longer(c(high, med, low)) %>%
  mutate(cost_benefit = if_else(value > cost, "A", "B"))

label_df <-
  tibble(
    X = c(9.9, 9.9, 9.9),
    value = c(22, 36, 48),
    label = c("Low",
              "Medium",
              "Inquiry importance: High"),
    cost_benefit = c("B", "B", "A")
  )


ggplot(dat_long, aes(X)) +
  geom_ribbon(data = dat, aes(ymax = cost, x = X, ymin = 0), fill = gray(0.8, alpha = 0.3)) + 
  geom_ribbon(data = dat, aes(ymin = cost, x = X, ymax = 50), fill = "#72B4F344") + 
  geom_line(aes(y = value, color = cost_benefit, group = name)) +
  geom_line(data = dat, aes(y = cost), color = dd_pink) +
  geom_text(data = label_df, aes(y = value, label = label, color = cost_benefit), hjust = 1) +
  scale_color_manual("", values = c(dd_dark_blue, gray(0.5))) + 
  annotate("text", x = 1.5, y = 42, label = "Scientific benefits exceed ethical costs", hjust = 0, color = dd_dark_blue) + 
  annotate("text", x = 4, y = 7, label = "Ethical costs exceed scientific benefits", hjust = 0, color = dd_gray) + 
  labs(x = "Sample size", y = "Costs and benefits") + 
  dd_theme() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          panel.grid.major = element_blank())
  
