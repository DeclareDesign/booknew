flagit <- function() if(exists("show_flags")) if(show_flags) {
    if (knitr::is_latex_output()) {
      sprintf("\\textcolor{%s}{%s}", "red", "FLAG! \\")
    } else if (knitr::is_html_output()) {
      sprintf("<span style='color: %s;'>%s</span>", "red", "<small>FLAG</small>")
    } 
}

# devtools::install_github("ropenscilabs/icon")
# flagit <- function(colour = "#1FA67A") if(exists("show_flags")) if(show_flags) {
#  icon::fa_rocket(colour = colour)
#  }

