add_parens <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

make_se_entry <- function(est, se, digits = 2){
  paste0(format_num(est, digits = digits)," ", add_parens(se, digits = digits))
}

make_interval_entry <-
  function(conf.low, conf.high, digits = 2) {
    paste0(
      "[",
      format_num(conf.low, digits = digits),
      ", ",
      format_num(conf.high, digits = digits),
      "]"
    )
  }

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

bookreg <- function(...) {
  if(knitr:::kable_format() == "html") {
    do.call(htmlreg, args = list(...))
  } else if(knitr:::kable_format() == "latex") {
    do.call(texreg, args = list(...))
  } else {
    stop(paste0("bookreg only supports html and tex", knitr:::kable_format()))
  }
}

