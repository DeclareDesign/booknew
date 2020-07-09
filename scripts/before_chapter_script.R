
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

theme_set(dd_theme())

get_dropbox_path <- function(section){
  if(.Platform$OS.type == "unix"){
    path <- file.path("~", "Dropbox", "DeclareDesign_book_rfiles", section)
  } else if (.Platform$OS.type == "windows") {
    path <- file.path("C:", "Dropbox", "DeclareDesign_book_rfiles", section)
  }
  if(grepl("jasper",getwd(),TRUE)){
    path <- file.path("~","Dropbox", "09_Software_Development",
                      "DeclareDesign_", "__book",
                      "DeclareDesign_book_rfiles", section)
  }
  dir.create(path, showWarnings = FALSE)
  return(path)
}

