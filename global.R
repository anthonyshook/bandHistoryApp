
library(bslib)

# Themes as list, instead of json
# Why not work within R structures instead of having to load a file?
theme_list <- jsonlite::read_json('./www/themes.json', simplifyVector = TRUE)

# Active Theme
active_theme <- theme_list$dark_blue
