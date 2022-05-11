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
                                        menuItem("Full-Time Series", tabName = "tab2", icon = icon("fa-solid fa-chart-line")),
                                        menuItem("Plot Choice", tabName = "tab3", icon = icon("fas fa-chart-bar")),
                                        menuItem("My Feature", tabName = "tab4", icon = icon("expand"))
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
                                   
                                   tags$h3("The second tab displays the Full-Time Series graphic for the interest in \"Wildfires\" from March 2017 to March 2022."),
                                   
                                   tags$br(),
                                   
                                   tags$h3("The third tab displays your choice in one of three types of graphics: (1) seasonality, (2) autocorrelation, and (3) decomposition. "),
                                   
                                   tags$br(),
                                   
                                   tags$h3("The fourth tab displays _______."),
                                   
                                   tags$br(),
                                 ),
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
                                relatively slow increases from 2017-2022.There appears to be strong
                                seasonality throughtout the plot. This is 
                                likely due to summer and fall season, transition.")
                                 
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
                                 
                                 plotlyOutput("myplot"),
                                 
                                 hr(),
                                 
                                 h3("Interpretation"),
                                 
                                 textOutput("myplotint")
                                 
                         ), 
                         
                         #Displays another plot of the user's choosing
                         tabItem(tabName = "tab4", 
                                 h1("My Feature"),
                             
                                 
                                 hr(),
                                 
                                 radioButtons("Newplot_type", 
                                              label = h2("Which new plot do you want to see?"),
                                              choices = c("Classical Decomposition", 
                                                          "Transforming", 
                                                          "Forecast")),
                                 
                                 hr(),
                                 
                                 plotlyOutput("mynewplot"),
                                 
                                 hr()
                         )
                       )
                     )
)


tabnames <- c("intro, tab2, tab3, tab4") 

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
      g_trends %>% ACF(Interest, lag_max = 70)+  
        theme_fivethirtyeight()+
        labs(title = "Interest of Wildfires")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#FF8200")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
    else if (input$plot_type == "Decomposition") {
      dcmp <- g_trends %>%
        model(STL(Interest ~ trend(window = 7) + season(window = "periodic"), robust = TRUE)) %>%
        components() %>%
        autoplot()+
        theme_fivethirtyeight()+
        labs(title = "STL Decomposition of Interest of \"Wildfires\"")+
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
      in \"Wildfires\" peaks from AUGUST until SEPTEMBER. This makes logical sense 
      because this coincides with SUMMER season transition into FALL season. The
      highest amount of interest seems to coincide with Wildfires in AUGUST and 
      SEPTEMBER ", 
                      collapse = " ")))
    } 
    else if (input$plot_type == "Autocorrelation") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in \"Wildfires\" is semi seasonal. This
      is likely due to summer season and transition into fall season, where winds likely picked up more wildfires. 
      This is especially the case for seasons that wildfires seem
      be excessive.", collapse = " ")))
    }
    else if (input$plot_type == "Decomposition") {
      noquote(paste(c("The STL decomposition plot shows that the trend
      peaked in about 2020. The plot also shows a consistent amount
      of seasonality.", collapse = " ")))
    }
  })
  
  
  #newPlots
  output$mynewplot <- renderPlotly({
    if (input$Newplot_type == "Classical Decomposition") {
      g_trends %>% model(
        classical_decomposition(Interest, type = "multiplicative")
      ) 
    } else if (input$plot_type == "Transforming") {
      g_trends %>% features(g_trends$Interest , features = guerrero) %>%
        pull(lambda_guerrero) -> lambda 
      DATA %>% (box_cox(Interest, lambda)) 
    }
  
})
}

shinyApp(ui, server)

