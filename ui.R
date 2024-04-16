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

page_navbar( 
  # title = tagList(
  #   tags$a(bsicons::bs_icon('github'), "Get the Source Code!",
  #          href='https://github.com/anthonyshook/bandHistoryApp',
  #          target='_blank', style = 'color:white;')),
  inverse = TRUE,
  fillable = TRUE, 
  footer = tagList(
    
    waiting_screen <- tagList(
      spin_wave()
    ),
    autoWaiter(html = waiting_screen, color = 'rgba(84, 89, 95, .75)'),
    waiter_preloader(html = waiting_screen, color = '#54595F')
  ),
  # sidebar = nav,
  # THEME -------------------------------------------------------------------
  ## Build a theme to include, perhaps
  theme = bslib::bs_theme(base_font = bslib::font_collection(bslib::font_google('Roboto', local=TRUE), "sans-serif")),
  
  # WAITER LOADING OPTIONS --------------------------------------------------
  # This is used to include "Loading" options to 
  
  
  # FRONT-END CODE START ----------------------------------------------------
  
  nav_spacer(),
  
  nav_panel('Summary',
            layout_columns(
              value_box("Unique Bands", value = textOutput('num_bands')),
              value_box("Unique Shows*", value = textOutput('num_shows')),
              value_box("Unique Venues*", value = textOutput('num_venues'))
            ),
            layout_columns(
              card(plotOutput('top10_bandGraph')),
              card(plotOutput('top10_venuesGraph'))
            ),
            layout_columns(
              card(plotlyOutput('timeline'))
            )
  ),
  nav_panel('By Band',
            layout_sidebar(border = TRUE, 
                           sidebar = sidebar(width = '20%',
                                             id='band_select_sidebar',
                                             uiOutput('band_select_ui')
                           ), 
                           card(
                             h3('Shows'),
                             reactableOutput('band_summary'),
                             hr(),
                             h3('Seen with...'),
                             reactableOutput('played_with')
                           )
                           
            )
  ), 
  nav_panel('By Show', 
            reactableOutput('show_list')),
  nav_panel('Totally Raw Data', 
            reactableOutput('raw_data')),
  nav_item(
    tagList(
      tags$a(bsicons::bs_icon('github'), "Get the Source Code!",
             href='https://github.com/anthonyshook/bandHistoryApp',
             target='_blank', style = 'color:white;')))
)
