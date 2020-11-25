library(stringr)

rmds <- list.files(pattern = c('.Rmd', '.rmd'), recursive = TRUE)

rmds <- rmds[!rmds %in% c("book.Rmd", "index.Rmd", "999_References/00_references.Rmd")]
rmds <- rmds[!str_detect(rmds, "/000_")]

for (file in rmds){
  # file_r <- paste0('_book/', basename(gsub('Rmd', 'R', file)))
  yaml_title <- rmarkdown::yaml_front_matter(file, encoding = 'UTF-8')$title
  if(!is.null(yaml_title) && yaml_title != "") {
    file_r <- paste0('_book/', tolower(gsub(" ", "-", yaml_title)), '.R')
    if(file.exists(file_r)){
      file.remove(file_r)
    }
    knitr::purl(file, documentation = 0, output = file_r)
    
    
    fileConn <- file(file_r)
    line1 <- 'packages <- c("tidyverse", "DeclareDesign")'
    line2 <- 'lapply(packages, require, character.only = TRUE)'
    writeLines(c("# ---", paste0("# ", yaml_title), "# --- \n", line1, line2, "", readLines(fileConn)), fileConn)
    close(fileConn)
  }
}
