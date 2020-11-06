dd_theme <-
  function() {
    theme_minimal() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        strip.background = element_blank()
      )
  }

dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_orange <- "#F38672"
dd_purple <- "#7E43B6"
dd_gray <- gray(0.2)
dd_pink <- "#C6227F"
dd_light_gray <- gray(0.8)
dd_dark_blue_alpha <- "#3564EDA0"
dd_light_blue_alpha <- "#72B4F3A0"
