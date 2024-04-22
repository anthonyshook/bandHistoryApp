
library(bslib)

# Themes as list, instead of json
# Why not work within R structures instead of having to load a file?
# theme_list <- jsonlite::read_json('./www/themes.json', simplifyVector = TRUE)
# Switch from JSON to just an in-R structure to avoid unnecessary I/O
theme_list <- list(
  dark = list(
    col_navbar_bg = "#222831",
    col_navbar_text = "#EEEEEE",
    col_body_bg = "#31363F",
    col_card_bg = "#3E444F",
    col_text = "#EEEEEE",
    col_dropdown_highlight_text = "#222831",
    col_accent = "#00A9FF",
    col_grad_low = "#00A9FF",
    col_grad_high = "#EEEEEE"
  ),
  light = list(
    col_navbar_bg =  "#FdFdFd",
    col_navbar_text =  "#000000",
    col_body_bg =  "#FbFbFb",
    col_card_bg =  "#FdFdFd",
    col_text =  "#000000",
    col_dropdown_highlight_text =  "#FdFdFd",
    col_accent =  "#FF6701",
    col_grad_low =  "#9c3f00",
    col_grad_high =  "#FF6701"
  )
)


# Active Theme
active_theme <- theme_list$dark
