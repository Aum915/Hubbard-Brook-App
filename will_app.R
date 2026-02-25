
### This doesnt really work rn ### 

ui <- fluidPage(
  
  titlePanel("Hydrologic Monitoring Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(
        "aspect_select",
        "Select Aspect(s):",
        choices = c("North" = "north",
                    "South" = "south"),
        selected = "north"   # default
      ),
      
      checkboxGroupInput(
        "variable_select",
        "Select Variable(s):",
        choices = c(
          "Streamflow" = "streamflow",
          "Precipitation" = "precip",
          "Average Temp" = "temp",
          "Average & Max Wind Speed" = "wind",
          "Soil Moisture" = "soil"
        ),
        selected = c("streamflow","precip","temp","wind","soil")  # default ALL
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = global_max_date - 14,
        end   = global_max_date,
        min   = global_min_date,
        max   = global_max_date
      )
      
    ),
    
    mainPanel(
      uiOutput("variable_plots")
    )
  )
)


server <- function(input, output, session) {
  
  # ---- Helper: add aspect label + filter date ----
  
  prep_data <- function(df, aspect_label) {
    df %>%
      mutate(aspect = aspect_label) %>%
      filter(datetime >= input$date_range[1],
             datetime <= input$date_range[2])
  }
  
  
  # ---- Streamflow Reactive ----
  
  streamflow_data <- reactive({
    
    req(input$aspect_select)
    
    data_list <- list()
    
    if ("north" %in% input$aspect_select) {
      data_list[[length(data_list)+1]] <- 
        prep_data(streamflowNorth, "North")
    }
    
    if ("south" %in% input$aspect_select) {
      data_list[[length(data_list)+1]] <- 
        prep_data(streamflowSouth, "South")
    }
    
    bind_rows(data_list)
  })
  
  
  # ---- Similar reactives for other variables ----
  
  precip_data <- reactive({
    
    data_list <- list()
    
    if ("north" %in% input$aspect_select) {
      data_list[[length(data_list)+1]] <- 
        prep_data(precipNorth, "North")
    }
    
    if ("south" %in% input$aspect_select) {
      data_list[[length(data_list)+1]] <- 
        prep_data(precipSouth, "South")
    }
    
    bind_rows(data_list)
  })
  
  
  temp_data <- reactive({
    
    data_list <- list()
    
    if ("north" %in% input$aspect_select) {
      data_list[[length(data_list)+1]] <- 
        prep_data(tempNorth, "North")
    }
    
    if ("south" %in% input$aspect_select) {
      data_list[[length(data_list)+1]] <- 
        prep_data(tempSouth, "South")
    }
    
    bind_rows(data_list)
  })
  
  
  # Wind & soil default to North per your instruction
  
  wind_data <- reactive({
    prep_data(wind, "North")
  })
  
  soil_data <- reactive({
    prep_data(soilmoisture, "North")
  })
  
  
  # ---- Dynamic stacked plots ----
  
  output$variable_plots <- renderUI({
    
    req(input$variable_select)
    
    plot_list <- lapply(input$variable_select, function(v) {
      plotlyOutput(paste0("plot_", v), height = "350px")
    })
    
    tagList(plot_list)
  })
  
  
  # ---- Generic plot function ----
  
  make_plot <- function(data, yvar, title, ylab) {
    
    p <- ggplot(data, aes(x = datetime,
                          y = .data[[yvar]],
                          color = aspect)) +
      geom_line() +
      labs(title = title,
           x = "Date",
           y = ylab,
           color = "Aspect") +
      theme_minimal()
    
    plotly::ggplotly(p)
  }
  
  
  # ---- Render each variable plot ----
  
  observe({
    
    for (v in input$variable_select) {
      
      local({
        
        var <- v
        plot_id <- paste0("plot_", var)
        
        output[[plot_id]] <- plotly::renderPlotly({
          
          if (var == "streamflow") {
            make_plot(streamflow_data(),
                      "pressure1_Q",
                      "Streamflow",
                      "Flow")
          }
          
          if (var == "precip") {
            make_plot(precip_data(),
                      "ReportPCP",
                      "Precipitation",
                      "Precip")
          }
          
          if (var == "temp") {
            make_plot(temp_data(),
                      "temp_avg",
                      "Average Temperature",
                      "Temperature")
          }
          
          if (var == "wind") {
            make_plot(wind_data(),
                      "WS_ms_Avg",
                      "Wind Speed (Avg)",
                      "m/s")
          }
          
          if (var == "soil") {
            make_plot(soil_data(),
                      "value",
                      "Soil Moisture",
                      "VWC")
          }
          
        })
        
      })
    }
    
  })
  
}

shinyApp(ui, server)