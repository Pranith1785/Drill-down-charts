
library(shiny)
library(shinydashboard)
library(shinyjs)

library(dplyr)
library(plotly)

library(nycflights13)

### Data
airlines <- airlines
airports <- airports
flights <- flights


ui  <- dashboardPage(
            header = dashboardHeader(
                            title = "Drill down plot"
                        ),
            sidebar = dashboardSidebar(disable = TRUE),
            body = dashboardBody(
                      useShinyjs(),
                      plotlyOutput("flightsCount"),
                      actionButton("back","back",icon = icon("arrow-left"))
                  
                )
    )

server <- function(input, output, session){
  
  ## Create reactive values object to hold the different views the user is in 
  ## depending on how many times they have drilled down on the facility plot
  drillDown_levels <- reactiveValues(
                        levelOne = NULL
                    )

  data <- reactive({
    
        flights %>% 
                 left_join(airlines, by= "carrier") %>% 
                 rename(airline = name) %>%
                 left_join(airports,by = c("origin" = "faa")) %>%
                 rename(origin_name = name ) %>%
                 left_join(airports, by = c("dest" = "faa")) %>%
                 rename(dest_name = name)  %>%
                 ## to simplify, select only certain data     
                 select("year", "month","day","carrier","origin_name","airline","dest_name")
        
    
  })
  
  
  #Reactive dataset that will change based on which view we are in  
  drill_data <- reactive({
    
    
      df <- data() %>% group_by(month,day) %>% 
              tally() %>% ungroup() %>% as.data.frame()
    
      if(!length(drillDown_levels$one)){
          
          df
        
      }else{
        
        df <- df %>% filter(month = drillDown_levels$one) 
        
      }
    
    
  })
  
  #Facility drill down plot    
  output$flightsCount <- plotly::renderPlotly({
   
    
    #plot_ly(drill_data, x = ~month, y = ~n, source = "drillDownPlot",type = 'bar',color = ~day)
    p <- plot_ly(drill_data(), source = "drillDownPlot")
    
    obs$resume()
    #Bar chart by facility
    if(!length(drillDown_levels$levelOne)) {
      add_bars(p, 
               x = ~month, y = ~n,
               color = ~day,
               showlegend = FALSE)
        
    }else{
        
      add_bars(p, x = ~day, y= ~n)
      
        
    }
    
  })
  
    #Set the x axis to be the chosen view based on clicking on the plot

    obs <-  observeEvent(event_data("plotly_click", source = "drillDownPlot"),
                         suspended = TRUE, {
                           x <- event_data("plotly_click", source = "drillDownPlot")$x
                           print(x)
                           if (!length(x)) return()

                           if (!length(drillDown_levels$levelOne)) {
                             drillDown_levels$levelOne <- x
                           } 

                         })
    
    # hide the back button
    observe({
      shinyjs::hide("back")

      if(length(drillDown_levels$levelOne) > 0)
        shinyjs::show("back")
    })
    
    ## update drilldown levels after clicking back button
    observeEvent(input$back,
                 if(length(drillDown_levels$levelOne) > 0) {
                        drillDown_levels$levelOne <- NULL
                 })
    

}



shinyApp(ui, server)