# this is a list of packages we will load for every chapter
# let's try to keep this to a minimum
# for many chapters, you will load special packages for them -- like if there's a section on matching it will do library(Matching) in the code
#   don't add those chapter-specific packages here

bookwide_packages <-
  c(
    # bookdown and knitr related packages
    "bookdown",
    "knitr",
    "kableExtra",
    "gridExtra",
    "texreg",
    
    # CausalQueries
    "CausalQueries",
    "Rcpp",
    
    # DeclareDesign packages
    "estimatr",
    "fabricatr",
    "randomizr",
    "DeclareDesign",
    "DesignLibrary",
    
    # tidyverse packages
    "ggplot2",
    "patchwork",
    "dplyr",
    "tidyr",
    "readr",
    "purrr",
    "tibble",
    "stringr",
    "forcats",
    "haven",
    # DAGs packages
    "dagitty",
    "ggdag",
    "ggraph",
    # ggplot packages
    "ggforce",
    "ggtext",
    "ggrepel",
    # extras
    "dddag",
    "latex2exp"
  )