######## APP TEST

ui <- fluidPage(titlePanel("Hydrologic Comparison Tool"),
                
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput(
                      inputId = "aspect_select",
                      label = "Select Aspect(s):",
                      choices = c("North" = "north", "South" = "south")
                    ),
                    
                    dateRangeInput(
                      inputId = "date_range",
                      label = "Select Date Range:",
                      start = Sys.Date() - 30,
                      end   = Sys.Date()
                    ),
                    
                    checkboxGroupInput(
                      inputId = "variable_select",
                      label = "Select Variable:",
                      choices = c(
                        "Streamflow" = "pressure1_Q",
                        "Wind Speed Avg" = "WS_ms_Avg",
                        "Wind Speed Max" = "WS_ms_Max",
                        "Wind Direction" = "WindDir"
                      ),
                      selected = "pressure1_Q"
                    )
                  ),
                  
                  mainPanel(
                    uiOutput("dynamic_plots")   # <- layout controlled by server
                  )
                ))

server <- function(input, output, session) {
  # ---- Reactive filtering ----
  
  north_filtered <- reactive({
    req(input$date_range)
    
    streamflowNorth %>%
      filter(datetime >= input$date_range[1],
             datetime <= input$date_range[2])
  })
  
  
  south_filtered <- reactive({
    req(input$date_range)
    
    streamflowSouth %>%
      filter(datetime >= input$date_range[1],
             datetime <= input$date_range[2])
  })
  
  wind_filtered <- reactive({
    req(input$date_range)
    
    wind %>%
      filter(datetime >= input$date_range[1],
             datetime <= input$date_range[2])
  })
  
  
  # ---- Dynamic Layout ----
  
  output$dynamic_plots <- renderUI({
    aspects <- input$aspect_select
    
    if (length(aspects) == 2) {
      # Two columns for each aspect
      fluidRow(
        column(
          6,
          plotOutput("streamflow_plot_north"),
          plotOutput("wind_speed_plot_north"),
          plotOutput("wind_dir_plot_north")
        ),
        column(
          6,
          plotOutput("streamflow_plot_south"),
          plotOutput("wind_speed_plot_south"),
          plotOutput("wind_dir_plot_south")
        )
      )
    } 
    
    else if (length(aspects) == 1) {
      if ("north" %in% aspects) {
        fluidRow(column(
          12,
          plotOutput("streamflow_plot_north"),
          plotOutput("wind_speed_plot_north"),
          plotOutput("wind_dir_plot_north")
        ))
      } 
      
      else {
        fluidRow(column(
          12,
          plotOutput("streamflow_plot_south"),
          plotOutput("wind_speed_plot_south"),
          plotOutput("wind_dir_plot_south")
        ))
      }
    }
  })
  
  
  # ---- North Streamflow Plot ----
  
  output$streamflow_plot_north <- renderPlot({
    if (!"pressure1_Q" %in% input$variable_select)
      return(NULL)
    
    ggplot(north_filtered(), aes(datetime, pressure1_Q)) +
      geom_line(color = "blue") +
      labs(title = "North Aspect Streamflow", x = "Date", y = "Streamflow") +
      theme_classic()
  })
  
  
  # ---- South Streamflow Plot ----
  
  output$streamflow_plot_south <- renderPlot({
    if (!"pressure1_Q" %in% input$variable_select)
      return(NULL)
    
    ggplot(south_filtered(), aes(datetime, pressure1_Q)) +
      geom_line(color = "blue") +
      labs(title = "South Aspect Streamflow", x = "Date", y = "Streamflow") +
      theme_classic()
  })
  
  
  # ---- North & South Wind Speed Avg & Max Plot ----
  
  output$wind_speed_plot_north <- renderPlot({
    if (!any(c("WS_ms_Avg", "WS_ms_Max") %in% input$variable_select))
      return(NULL)
    
    p <- ggplot(wind_filtered(), aes(datetime)) +
      theme_classic() +
      labs(
        title = "North Aspect Wind Speed",
        x = "Date",
        y = "Wind Speed (m/s)",
        color = "Legend"
      )
    
    if ("WS_ms_Avg" %in% input$variable_select) {
      p <- p + geom_line(aes(y = WS_ms_Avg, color = "Avg"))
    }
    if ("WS_ms_Max" %in% input$variable_select) {
      p <- p + geom_line(aes(y = WS_ms_Max, color = "Max"))
    }
    
    p + scale_color_manual(values = c("Avg" = "darkgreen", "Max" = "red"))
  })
  
  output$wind_speed_plot_south <- renderPlot({
    if (!any(c("WS_ms_Avg", "WS_ms_Max") %in% input$variable_select))
      return(NULL)
    
    p <- ggplot(wind_filtered(), aes(datetime)) +
      theme_classic() +
      labs(
        title = "South Aspect Wind Speed",
        x = "Date",
        y = "Wind Speed (m/s)",
        color = "Legend"
      )
    
    if ("WS_ms_Avg" %in% input$variable_select) {
      p <- p + geom_line(aes(y = WS_ms_Avg, color = "Avg"))
    }
    if ("WS_ms_Max" %in% input$variable_select) {
      p <- p + geom_line(aes(y = WS_ms_Max, color = "Max"))
    }
    
    p + scale_color_manual(values = c("Avg" = "darkgreen", "Max" = "red"))
  })
  
  
  # ---- North & South Wind Direction Plot ----
  
  output$wind_dir_plot_north <- renderPlot({
    if (!"WindDir" %in% input$variable_select)
      return(NULL)
    
    ggplot(wind_filtered(), aes(datetime, WindDir)) +
      geom_line(color = "purple") +
      scale_y_continuous(
        name = "Wind Direction",
        breaks = seq(0, 360, by = 45),
        labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
      ) +
      labs(title = "North Aspect Wind Direction", x = "Date") +
      theme_classic()
  })
  
  output$wind_dir_plot_south <- renderPlot({
    if (!"WindDir" %in% input$variable_select)
      return(NULL)
    
    ggplot(wind_filtered(), aes(datetime, WindDir)) +
      geom_line(color = "purple") +
      scale_y_continuous(
        name = "Wind Direction",
        breaks = seq(0, 360, by = 45),
        labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
      ) +
      labs(title = "South Aspect Wind Direction", x = "Date") +
      theme_classic()
  })
}

shinyApp(ui = ui, server = server)
