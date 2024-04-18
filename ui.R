#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
library(plotly)
library(bslib)
library(waiter)

# Setup Colors ------------------------------------------------------------
dark_list <- list(
  col_accent_pop = '#76ABAE',
  col_light = '#EEEEEE',
  col_accent_focus = '#222831',
  col_main_body = '#3E444F',
  col_bg = '#31363F',
  col_fg = '#eeeeee',
  col_navbar = '#222831'
)

# light theme
light <- bs_theme()

# Dark theme
dark <- bs_theme(bg = dark_list$col_bg, fg = dark_list$col_light) |> 
  bs_add_rules(sass::as_sass(dark_list)) |>
  bs_add_rules(sass::sass_file("./www/dash.scss"))


# SHINY CODE --------------------------------------------------------------
page_navbar( 
  lang='en',
  window_title='Live Show Dashboard',
  inverse = 'auto',
  bg=dark_list$col_navbar,
  fillable = TRUE, 
  
  # WAITER LOADING OPTIONS --------------------------------------------------
  # Placed quietly in footer -- currently not working, because it just creates a weird effect.
  footer = tagList(
    div(class = 'dont-show',
        waiting_screen <- tagList(
          spin_wave()
        ),
        #autoWaiter(html = waiting_screen, color = 'rgba(84, 89, 95, .75)'),
        use_waiter(),
        waiter_preloader(html = waiting_screen, color = dark_list$col_navbar)
    )
  ),
  # THEME -------------------------------------------------------------------
  ## Build a theme to include, perhaps
  # base_font = bslib::font_collection(bslib::font_google('Nunito', local=TRUE), "sans-serif")
  theme = dark,
  
  # FRONT-END CODE START ----------------------------------------------------
  # checkboxInput('dark_mode', 'Dark Mode'),
  nav_spacer(),
  nav_panel('Summary',
            layout_columns(
              value_box(title = NULL, value = textOutput('num_bands'), h5('Unique Bands'), class = 'summaryCard'),
              value_box(title = NULL, value = textOutput('num_shows'), h5("Unique Shows*"), class = 'summaryCard'),
              value_box(title = NULL, value = textOutput('num_venues'), h5("Unique Venues*"), class = 'summaryCard')
            ),
            layout_columns(
              card(plotOutput('top10_bandGraph'), class='graphs'),
              card(plotOutput('top10_venuesGraph'), class='graphs')
            ),
            layout_columns(
              card(plotlyOutput('timeline'), class='graphs')
            )
  ),
  nav_panel('Explore By Band',
            layout_sidebar(border = TRUE, 
                           sidebar = sidebar(width = '20%',
                                             id='band_select_sidebar',
                                             open=list(desktop='always', 
                                                       mobile='closed'),
                                             uiOutput('band_select_ui')
                           ), 
                           card(class='tables',
                                h3('Shows'),
                                reactableOutput('band_summary'),
                                hr(),
                                h3('Seen with...'),
                                reactableOutput('played_with')
                           )
                           
            )
  ), 
  nav_panel('Explore By Show', 
            card(class='tables', reactableOutput('show_list'))),
  nav_panel('Totally Raw Data', 
            card(class='tables', reactableOutput('raw_data'))),
  nav_item(
    tagList(
      tags$a(bsicons::bs_icon('github'), "Get the Source Code!",
             href='https://github.com/anthonyshook/bandHistoryApp',
             target='_blank', style = 'color:white;')))
)
