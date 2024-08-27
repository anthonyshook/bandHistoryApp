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


# SHINY CODE --------------------------------------------------------------
page_navbar( 
  lang='en',
  window_title='Live Show Dashboard',
  inverse = FALSE,
  #bg=active_theme$col_navbar_bg,
  fillable = TRUE, 
  collapsible = TRUE,
  
  # WAITER LOADING OPTIONS --------------------------------------------------
  # Placed quietly in footer -- currently not working, because it just creates a weird effect.
  header = tagList(
    div(class = 'dont-show',
        waiting_screen <- tagList(
          spin_wave()
        ),
        #autoWaiter(html = waiting_screen, color = 'rgba(84, 89, 95, .75)'),
        use_waiter(),
        waiter_preloader(html = waiting_screen, color = active_theme$col_navbar_bg)
    )
  ),
  # footer = (
  #   shinyWidgets::switchInput(inputId = 'dark_mode', label=NA, onLabel = 'Dark', offLabel='Light', size='mini')
  # ),
  # THEME -------------------------------------------------------------------
  ## Build a theme to include, perhaps
  # base_font = bslib::font_collection(bslib::font_google('Nunito', local=TRUE), "sans-serif")
  theme = bs_theme(
    bg = active_theme$col_body_bg, fg = active_theme$col_text
  ) |> 
    bs_add_rules(sass::as_sass(active_theme)) |>
    bs_add_rules(sass::sass_file("./www/dash.scss")),
  
  # FRONT-END CODE START ----------------------------------------------------
  nav_spacer(),
  nav_panel('Summary',
            layout_columns(
              value_box(title = NULL, value = textOutput('num_bands'), h5('Unique Bands'), textOutput('bands_ytd'), class = 'summaryCard'),
              value_box(title = NULL, value = textOutput('num_shows'), h5("Unique Shows*"), textOutput('shows_ytd'), class = 'summaryCard'),
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
                                h5('Shows With:'),
                                uiOutput('selected_band'),
                                reactableOutput('band_summary')
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
             target='_blank', 
             class = 'source_link')
    )
  ),
  nav_item(shinyWidgets::switchInput(inputId = 'dark_mode', label=NA, onLabel = 'Dark', offLabel='Light', size='mini', value = TRUE))
)
