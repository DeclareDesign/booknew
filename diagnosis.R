# ---
# Diagnosis
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

knitr::include_app('https://ushintsho.shinyapps.io/diagnosis/', height = '400px')
