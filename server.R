#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
library(data.table)
library(ggplot2)
library(plotly)
library(bslib)
library(ggtext)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  dat <- fetch_band_info()
  
  # Summary Data
  num_bands  <- uniqueN(dat$Band)
  num_venues <- uniqueN(dat[Venue != 'Unknown' & !is.na(Venue)]$Venue)
  num_shows  <- uniqueN(dat[!is.na(Date)]$ShowID)
  
  output$num_bands <- renderText(num_bands)
  output$num_venues <- renderText(num_venues)
  output$num_shows <- renderText(num_shows)
  
  # Creating a display table
  band_summary  <- dat[Band %in% unique(dat[!is.na(Date)]$Band), list(`Number of Shows` = .N, most_recent_date = max(Date, na.rm=T)), by = Band][order(Band)]
  venue_summary <- dat[, list(`Number of Shows` = uniqueN(ShowID)), by = Venue][order(Venue)]
  
  
  ##### UI ELEMENTS #####
  output$band_select_ui <- renderUI({
    shinyWidgets::virtualSelectInput(
      inputId = "band_select",
      label = "Pick a Band / Artist!",
      choices = sort(unique(dat$Band)),
      selected = sample(unique(dat$Band), size = 1),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = FALSE
    )
  })
  outputOptions(output, "band_select_ui", suspendWhenHidden = FALSE)
  
  ##### OBSERVE #####
  # a way to close the sidebar when the band is selected
  # observeEvent(input$band_select, {
  #   sidebar_toggle(
  #     id = "band_select_sidebar"
  #   )
  #   }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Set theme as reactive
  ract_theme_var <- reactiveVal(active_theme)
  observeEvent(input$dark_mode, {
    if (input$dark_mode) {
      active_theme <- theme_list$dark_blue
    } else {
      active_theme <- theme_list$light
    }
    # Set the new theme object for ggplot purposes
    ract_theme_var(active_theme)
    session$setCurrentTheme(
      bs_theme(
        bg = active_theme$col_body_bg, fg = active_theme$col_text
      ) |>
        bs_add_rules(sass::as_sass(active_theme)) |>
        bs_add_rules(sass::sass_file("./www/dash.scss"))
    )
  })

  
  ##### REACTIVES #####
  dat_band_summary <- reactive({
    x <- dat[Band %in% input$band_select][, list(ShowID, BandID, Date, Venue, Band, PlayedWith=1, Notes)]
    for (i in 1:nrow(x)) {
      x$PlayedWith[i] <- dat[ShowID == x$ShowID[i] & Band != input$band_select, paste0(Band, collapse=',<br>')]
    }
    return(x)
  })
  
  dat_band_played_with <- reactive({
    other_acts <- dat[!grepl('00000-', ShowID)][ShowID %in% unique(dat[Band %in% input$band_select]$ShowID)]
    other_acts <- other_acts[!(Band %in% input$band_select)]
    return(other_acts)
  })
  
  dat_show_list <- reactive({
    showlist <- dat[!grepl('00000-', ShowID), 
                    list(
                      Bands = .N,
                      Bill = paste(Band, collapse=', '),
                      Notes = paste0(unique(Notes), collapse="")
                    ), 
                    by = list(Date, Venue)]
  })
  
  
  ##### DATA ELEMENTS #####  
  # Selected Band Summary
  output$selected_band <- renderUI({
    h1(input$band_select)
  })
  output$band_summary <- renderReactable({
    reactable(dat_band_summary(),
              class = 'tables',
              columns = list(
                Band = colDef(show = FALSE),
                ShowID = colDef(show  = FALSE),
                BandID = colDef(show = FALSE),
                Venue = colDef(minWidth=125),
                PlayedWith= colDef(name = 'Played With...', html=TRUE, minWidth = 200)
              ),
              defaultSorted = list('Date' = 'desc'),
              filterable = F, searchable = F, fullWidth = TRUE, compact = F, pagination = F)
  })

  # List of Shows, somewhat Raw  
  output$show_list <- renderReactable({
    reactable(dat_show_list(),
              class = 'tables',
              columns = list(
                Date = colDef(maxWidth = 100),
                Bands = colDef(maxWidth = 100),
                Venue = colDef(maxWidth = 500)
              ),
              defaultSorted = list('Date' = 'asc'),
              filterable=F, searchable = T, fullWidth = T, compact = T, pagination = F, striped=T)
  })
  
  # Totally Raw Data
  output$raw_data <- renderReactable({
    rtable <- reactable(dat, 
                        class = 'tables',
                        defaultSorted = list('ShowID' = 'desc'),
                        filterable = FALSE, 
                        searchable = TRUE,
                        fullWidth =  TRUE,
                        defaultPageSize = 25,
                        minRows=25,
                        showPageSizeOptions = TRUE,
                        pagination = FALSE,
                        compact = TRUE,
                        rowStyle = function(index) {
                          if (dat[index, "ShowID"] < -1) {
                            list(background = "rgba(0, 0, 0, 0.05)")
                          }
                        }
    )
    return(rtable)
  })
  
  ##### GRAPHS #####
  
  # Top 10 Bands Graph
  output$top10_bandGraph <- renderPlot({
    plotdata <- band_summary[order(-`Number of Shows`, -most_recent_date)][1:10]
    newPlot <- rbindlist(lapply(1:10, function(z) {data.table(Band = rep(plotdata$Band[z], times = plotdata$`Number of Shows`[z]))}))
    newPlot[, Shows := 1:.N, by=Band]
    newPlot[, height := 1]
    newPlot$Band <- factor(x = newPlot$Band, levels = plotdata$Band, ordered = TRUE)
    
    ggplot(newPlot, aes(x = Shows, y = Band)) +
      geom_tile(aes(fill=Shows, width=0.9, height=0.9)) +
      scale_y_discrete(limits = rev) +
      # scale_fill_manual(values=colours) +
      labs(title = "Most Seen Bands") +
      scale_fill_gradient(low=ract_theme_var()$col_grad_low, high=ract_theme_var()$col_grad_high) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(colour=ract_theme_var()$col_text, size=16),
            axis.text = element_text(colour=ract_theme_var()$col_text, size = 14),
            panel.grid = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill=ract_theme_var()$col_card_bg), #transparent panel bg
            plot.background = element_rect(fill=ract_theme_var()$col_card_bg, color=NA) #transparent plot bg
      )
    
  })
  
  # Top 10 Venue Graph
  output$top10_venuesGraph <- renderPlot({
    plotdata <- venue_summary[order(-`Number of Shows`)][1:10]
    newPlot <- rbindlist(lapply(1:10, function(z) {data.table(Venue = rep(plotdata$Venue[z], times = plotdata$`Number of Shows`[z]))}))
    newPlot[, Shows := 1:.N, by=Venue]
    newPlot$Venue <- factor(x = newPlot$Venue, levels = plotdata$Venue, ordered = TRUE)
    
    ggplot(newPlot, aes(x = Shows, y = Venue)) + 
      geom_tile(aes(fill=Shows, width=0.9, height=0.9)) + 
      scale_y_discrete(limits = rev) + 
      labs(title = "Most Visited Venues") + 
      # scale_fill_gradient(low="darkblue", high="darkred") +
      scale_fill_gradient(low=ract_theme_var()$col_grad_low, high=ract_theme_var()$col_grad_high) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(colour=ract_theme_var()$col_text, size=16),
            axis.text = element_text(colour=ract_theme_var()$col_text, size=14),
            panel.grid = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill=ract_theme_var()$col_card_bg), #transparent panel bg
            plot.background = element_rect(fill=ract_theme_var()$col_card_bg, color=NA) #transparent plot bg
      )
  })
  
  
  # Timeline Graph, for fun
  output$timeline <- renderPlotly({
    plotdata <- dat[!is.na(Date), list(Shows = uniqueN(ShowID)), by = list(Year = factor(Year), Month = month.abb[month(Date)])]
    plotdata$Month <- factor(plotdata$Month, levels = month.abb, ordered=TRUE)
    
    ggplotly(
      ggplot(plotdata, aes(x = Year, y = Month)) +
        geom_tile(aes(fill = Shows, width=.8, height=.8, color='transparent')) + 
        scale_y_discrete(limits = rev) + 
        # scale_fill_gradient(low="black", high="purple") +
        scale_fill_gradient(low=ract_theme_var()$col_grad_low, high=ract_theme_var()$col_grad_high) +
        theme_bw(14) +
        theme(axis.title.x = element_blank(), 
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              legend.position = "none",
              text = element_text(colour=ract_theme_var()$col_text),
              axis.text = element_text(colour=ract_theme_var()$col_text),
              panel.background = element_rect(fill='transparent'), #transparent panel bg
              plot.background = element_rect(fill='transparent', color=NA) #transparent plot bg
        ),
      tooltip = c('x', 'y', 'fill')
    ) |> layout(xaxis = list(fixedrange = TRUE),  
                yaxis = list(fixedrange = TRUE)) |> 
      config(displayModeBar = FALSE)
  })
  
  
}
