#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(semantic.dashboard)
library(reactable)
library(plotly)

dashboardPage(
  dashboardHeader(inverted = TRUE, 
                  div(style='color:white; margin:10px;', semantic.dashboard::icon('github'), HTML('<a href="https://github.com/anthonyshook/bandHistoryApp" style="color:#ffffff;" target=”_blank”>Get The Source Code Here!</a>'))),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "Summary", "Summary"),
      menuItem(tabName = "byBand", "Explore by Band"),
      menuItem(tabName = "byShow", "Explore by Show"),
      menuItem(tabName = 'rawData', "Totally Raw Data")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    tabItems(
      tabItem(tabName = 'Summary',
              fluidRow(
                valueBox(subtitle = 'Unique Bands', value=textOutput('num_bands'), width = 4, size = 'huge'),
                valueBox(subtitle = 'Unique Shows*', value=textOutput('num_shows'), width = 4, size = 'huge'),
                valueBox(subtitle = 'Unique Venues*', value=textOutput('num_venues'), width = 4, size = 'huge')
              ),
              fluidRow(
                box(width=6, plotOutput('top10_bandGraph')),
                box(width=6, plotOutput('top10_venuesGraph'))
              ),
              fluidRow(
                box(width=12, plotlyOutput('timeline'),
                    title = "Timeline of Shows", ribbon = T, title_side = 'top left', collapsible = F, color = 'violet')
              )
              # fluidRow(
              #   valueBox(subtitle = 'Busiest Month', value='test', width = 4, size = 'mini'),
              #   valueBox(subtitle = 'Busiest Month', value='test', width = 4, size = 'huge'),
              #   valueBox(subtitle = 'Busiest Month', value='test', width = 4, size = 'huge')
              # )
      ),
      # shiny.semantic::card(textOutput('num_venues'))  # An alternative to Boxes which might be interesting to approach.
      tabItem(tabName = 'byBand',
              fluidRow(
                box(width=4, uiOutput('band_select_ui')),
                # box(width=6, reactableOutput('raw_data'))
                box(width=12,
                    h3('Shows'),
                    reactableOutput('band_summary'),
                    hr(),
                    h3('Seen with...'),
                    reactableOutput('played_with')
                )
              )
      ),
      tabItem(tabName='byShow', box(width = 12, reactableOutput('show_list'))),
      tabItem(tabName='rawData', box(width = 12, reactableOutput('raw_data')))
    )
  )
)

