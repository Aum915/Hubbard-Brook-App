library(tidyverse)
library(lubridate)
library(shiny)


######## NORTH ASPECT STREAM FLOW READ IN ##############

f <- "weir3_weir_3.dat"

header_lines <- readLines(f, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


streamflowNorth <- read.table(
  
  f,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\"") |>
  select(TIMESTAMP, pressure1_Q) |>
  mutate(datetime = ymd_hms(TIMESTAMP)) |>
  quote = "\"") |> 
  select(TIMESTAMP, pressure1_Q) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) > 2025)


########### SOUTH ASPECT STREAM FLOW READ IN #########################

g <- "weir9_weir_9.dat"

header_lines <- readLines(f, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


streamflowSouth <- read.table(
  
  g,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\"") |>
  select(TIMESTAMP, pressure1_Q) |>
  mutate(datetime = ymd_hms(TIMESTAMP)) |>
  quote = "\"") |> 
  select(TIMESTAMP, pressure1_Q) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) > 2025)


########### PRECIP READ IN ##############################


### NORTH

h <- "wxsta1_Wx_1_rain.dat"

header_lines <- readLines(h, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


precipNorth <- read.table(
  
  h,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\"") |>
  select(TIMESTAMP, ReportPCP) |>
  mutate(datetime = ymd_hms(TIMESTAMP)) |>
  quote = "\""
) |> 
  select(TIMESTAMP, ReportPCP) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) >= 2023)  ## Static file only goes to 2024

### SOUTH

z <- "wxsta23_Wx_23_rain.dat"

header_lines <- readLines(z, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


precipSouth <- read.table(
  
  z,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\""
) |> 
  select(TIMESTAMP, ReportPCP) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) >= 2023)  ## Static file only goes to 2024

######## WIND READ IN  ####################################

j <- "Kineo_Tower_Kineo-Aum.dat"

header_lines <- readLines(j, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


wind <- read.table(
  
  j,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\""
) |> 
  select(TIMESTAMP, WS_ms_Avg, WS_ms_Max, WindDir) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) == 2026)


###### SOIL MOISTURE READ IN #######################

k <- "Snowcourse_19_SS19_soildat.dat"

header_lines <- readLines(k, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


soilmoisture <- read.table(
  
  k,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\""
) |> 
  select(TIMESTAMP, TDR_10typ_vwc, TDR_30typ_vwc, TDR_50typ_vwc) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) == 2026) |> 
  pivot_longer(cols = c(TDR_10typ_vwc : TDR_50typ_vwc))



######## SNOW DATA READ IN #######################

l <- "Snowcourse_2_SS2-snowdat-Aum.dat"

header_lines <- readLines(l, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


snow <- read.table(
  
  l,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\""
) |> 
  select(TIMESTAMP, SWE, Depthraw, Depthscaled) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) == 2026)


############ TEMP READ IN #################


### North

x <- "wxsta1_SF_Wx1_Temp_15min.dat"

header_lines <- readLines(x, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


tempNorth <- read.table(
  
  x,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\""
) |> 
  select(TIMESTAMP, ST110_1 : St110_3, RH) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) == 2026) |> 
  group_by(datetime) |> 
  mutate(temp_avg = mean(c(ST110_1, St110_2, St110_3)))

### SOUTH

c <- "wxsta23_Wx_23_Temp_15_min.dat"

header_lines <- readLines(c, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


tempSouth <- read.table(
  
  c,
  sep = ",",
  header = FALSE,
  skip = 4,
  col.names = col_names,
  fill = TRUE,
  stringsAsFactors = FALSE,
  quote = "\""
) |> 
  select(TIMESTAMP, ST110_1 : St110_3, RH) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) == 2026) |> 
  group_by(datetime) |> 
  mutate(temp_avg = mean(c(ST110_1, St110_2, St110_3)))




#soilmoisture |> 
#  ggplot(aes(datetime, value, color = name))+
#  geom_line()



######## APP TEST

ui <- fluidPage(
  
  titlePanel("Hydrologic Comparison Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput(
        inputId = "aspect_select",
        label = "Select Aspect(s):",
        choices = c("North" = "north",
                    "South" = "south")
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
        choices = c("Streamflow" = "pressure1_Q"),
        selected = "pressure1_Q"
      )
      
    ),
    
    mainPanel(
      uiOutput("dynamic_plots")   # <- layout controlled by server
    )
  )
)

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
  
  
  
  # ---- Dynamic Layout ----
  
  output$dynamic_plots <- renderUI({
    
    aspects <- input$aspect_select
    
    # Both selected → side by side
    if (length(aspects) == 2) {
      
      fluidRow(
        column(6, plotOutput("north_plot")),
        column(6, plotOutput("south_plot"))
      )
      
    } 
    # One selected → full width
    else if (length(aspects) == 1) {
      
      fluidRow(
        column(12,
               if ("north" %in% aspects) {
                 plotOutput("north_plot")
               } else {
                 plotOutput("south_plot")
               }
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
  
  output$north_plot <- renderPlot({
    
    req("north" %in% input$aspect_select)
    req("pressure1_Q" %in% input$variable_select)
    
    ggplot(north_filtered(),
           aes(x = datetime, y = pressure1_Q)) +
      geom_line(color = "blue") +
      labs(
        title = "North Aspect Streamflow",
        x = "Date",
        y = "Streamflow"
      ) +
      theme_classic()
  })
  
  
  # ---- South Streamflow Plot ----
  
  output$south_plot <- renderPlot({
    
    req("south" %in% input$aspect_select)
    req("pressure1_Q" %in% input$variable_select)
    
    ggplot(south_filtered(),
           aes(x = datetime, y = pressure1_Q)) +
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
        title = "South Aspect Streamflow",
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
        y = "Streamflow"
      ) +
      theme_classic()
  })
}

shinyApp(ui = ui, server = server)
