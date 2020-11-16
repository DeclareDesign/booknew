
source("scripts/package_list.R")

lapply(bookwide_packages, function(x)
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "https://cran.r-project.org")
    library(x, character.only = TRUE)
  })

set.seed(42)

pro_con_colors <- c("#C67800", "#205C8A")

source("scripts/ggplot_dd_theme.R")
source("scripts/custom_scripts.R")
source("scripts/make_dag_df.R")

# FLAGS
show_flags <- TRUE
source("scripts/flagit.R")

theme_set(dd_theme())
