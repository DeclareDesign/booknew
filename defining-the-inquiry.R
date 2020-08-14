# ---
# Defining the inquiry
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)


plot(dagitty::graphLayout(dagitty("dag{ 
  X -> Y ;
  X -> M ;
  M -> Y
 }")))
