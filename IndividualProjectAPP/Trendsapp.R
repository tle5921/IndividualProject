library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(quantmod)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyquant)
library(shinyWidgets)
library(dashboardthemes)
library(fpp3)
library(ggthemes)


# Path where data is
file_path <- "~/BAS 475 Spring T TH/multiTimeline3.0.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- ymd(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)

ui <- dashboardPage( skin = "yellow", 
                     
                     dashboardHeader( title = "Interest in \"Wildfires\"", titleWidth = 500),
                     
                     dashboardSidebar(width = 300,
                       
                       
                       # Making menu tabs
                       sidebarMenu( #id = "tabs",
                                    menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
                                    menuItem("Full-Time Series", tabName = "tab2", icon = icon("fas fa-chart-bar")),
                                    menuItem("Plot Choice", tabName = "tab3", icon = icon("fas fa-chart-bar")),
                                    menuItem("My Feature", tabName = "tab4", icon = icon("fa-light fa-block-question")), 

                                    
                                    # User can choose date range for the graphic
                                    dateRangeInput(
                                      "chooseDate",
                                      label = h3("Choose a Date")
                                    )
                                  )
                                ),
                     
                     dashboardBody(
                       shinyDashboardThemes( theme = "grey_dark" ),
                       
                       
                       # Setting content for tabs
                       tabItems(
                         tabItem(tabName = "intro", 
                                 #Displays an introduction/instructions on how to use the app
                                 h1("Introduction"), 
                                 
                                 hr(),
                                 
                                 tags$div(
                                   tags$h3("This application analyzes the interest in 
                                      \"Wildfires\" from data collected by 
                                      GoogleTrends."),
                                   
                                   tags$head(tags$style('h3 {color:#FF8200;}')),
                                   
                                   tags$br(),
                                   
                                   tags$h3("The second tab displays the Full-Time Series graphic for the interest in \"Wildfires\" from _____ 20__ to ___ 20__."),
                                   
                                   tags$br(),
                                   
                                   tags$h3("The third tab displays your choice in one of three types of graphics: (1) seasonality, (2) autocorrelation, and (3) decomposition. "),
                                   
                                   tags$br(),
                                   
                                   tags$h3("The fourth tab displays _______."),
                                   
                                   tags$br(),
                                 ),
                         
                         #Displays a plot of the full-time series
                         tabItem(tabName = "tab2", 
                                 h1("Full-Time Series Graph"),
                                 
                                 hr(),
                                 
                                 basicPage(
                                   plotlyOutput("fulltimeseries")
                                 ),
                                 
                                 hr(),
                                 
                                 h3("Interpretation"),
                                 
                                 h4("The full-time series shows a trend that was 
                                relatively increasing from 20__-20__.The trend
                                then appears to be decreasing from about
                                20__-20__. The trend then appears to increase 
                                from 20__-20__. There appears to be strong
                                seasonality throughtout the plot. This is 
                                likely due to ______ season.")
                                 
                         ),   
                                 
                        #Displays another plot of the user's choosing
                         tabItem(tabName = "tab3", 
                                 h1("Graphic of Your Choice"), 
                                 
                                 hr(),
                                 
                                 radioButtons("plot_type", 
                                              label = h2("Which plot do you want to see?"),
                                              choices = c("Seasonality", 
                                                          "Autocorrelation", 
                                                          "Decomposition")),
                                 
                                 hr(),
                                 
                                 plotOutput("myplot"),
                                 
                                 hr(),
                                 
                                 h3("Interpretation"),
                                 
                                 textOutput("myplotint")
                                
                         ), 
                         
                        #Displays another plot of the user's choosing
                        tabItem(tabName = "tab4", 
                                h1("My Feature"),
                       )
                     )
                  )
                )
              )

# tabnames <- c("intro, tab2, tab3, tab4") 

server <- function(input, output, session) {
  #fulltimeseries
  output$fulltimeseries <- renderPlotly({
    p <- ggplot(g_trends, aes(Month, Interest)) + 
      geom_line(color = "red") + 
      theme_fivethirtyeight()+
      labs(title = "The Interest of \"Wildfires\"", y = "Interest") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "#ff8200") +
      theme(plot.background = element_rect(fill = "grey"), 
            panel.background = element_rect(fill = "grey"))
    ggplotly(p)
  })
  
  #graph of choices
  output$myplot <- renderPlotly({
    if (input$plot_type == "Seasonality") {
      g_trends %>% gg_season(Interest)+
        theme_fivethirtyeight()+
        labs(title = "The Interest of \"Wildfires\"", y = "Interest") +
        ggeasy::easy_center_title() +
        ggeasy::easy_all_text_color(color = "#ff8200") +
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    } 
    else if (input$plot_type == "Autocorrelation") {
      g_trends %>% ACF() %>% 
        autoplot()+
        labs(title = "Interest of Wildfires")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#FF8200")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
    else if (input$plot_type == "Decomposition") {
      x11_dcmp <- g_trends %>%
        model(x11 = X_13ARIMA_SEATS(Interest ~ x11())) %>%
        components()
      autoplot(x11_dcmp) +
        labs(title = "Decomposition of Interest of \"Wildfires\" using X-11.")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#FF8200")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
  })
    
 #interpretation of graph of choice
  output$myplotint <- renderText({
    if (input$plot_type == "Seasonality") {
      noquote(paste(c("The seasonality plot shows that the interest 
      in \"Wildfires\" peaks from ___until ____ and 
      again from ___ to ____. This makes logical sense 
      because this coincides with ___ season. Also of note is 
      that ___ is particularly high for most of the summer months.
      This is likely due to the conclusion of _____ season. The
      highest amount of interest seems to coincide with Wildfires in ____ and 
      _____. ", 
                      collapse = " ")))
    } 
    else if (input$plot_type == "Autocorrelation") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in \"Wildfires\" is extremely seasonal. This
      is likely due to _____ season and _____ season. 
      This is especially the case for seasons that wildfires seem
      be excessive.", collapse = " ")))
    }
    else if (input$plot_type == "Decomposition") {
      noquote(paste(c("The X11 decomposition plot shows that the trend
      peaked in about ______. The plot also shows a consistent amount
      of seasonality.", collapse = " ")))
    }
  })
}

shinyApp(ui, server)
