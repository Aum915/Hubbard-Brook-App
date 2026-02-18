
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
  quote = "\""
) |> 
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
  quote = "\""
) |> 
  select(TIMESTAMP, pressure1_Q) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) |> 
  filter(year(datetime) > 2025)


###########

h <- "wxsta1_Wx_1_rain.dat"

header_lines <- readLines(h, n = 4)
col_names <- gsub('"', "", header_lines[2])
col_names <- strsplit(col_names, ",")[[1]]


precip <- read.table(
  
  h,
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

precip |> 
  ggplot(aes(datetime, ReportPCP))+
  geom_col()+
  scale_y_continuous(limits = c(0, .15))







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
  })
  
  
  # ---- North Plot ----
  
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
  
  
  # ---- South Plot ----
  
  output$south_plot <- renderPlot({
    
    req("south" %in% input$aspect_select)
    req("pressure1_Q" %in% input$variable_select)
    
    ggplot(south_filtered(),
           aes(x = datetime, y = pressure1_Q)) +
      geom_line(color = "blue") +
      labs(
        title = "South Aspect Streamflow",
        x = "Date",
        y = "Streamflow"
      ) +
      theme_classic()
  })
  
}

shinyApp(ui = ui, server = server)






