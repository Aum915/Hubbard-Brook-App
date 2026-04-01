# ---- Simplified Live Hubbard Brook Viewer (Enhanced) ----

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(curl)
library(memoise)

# -----------------------------
# GLOBAL URL LIST
# -----------------------------
URLS <- c(
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/Kineo_Tower_Kineo.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/Snowcourse_2_SS2-snowdat.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/Snowcourse_19_SS19_soildat.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/weir3_weir_3.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/weir9_weir_9.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta1_SF_Wx1_Temp_15min.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta1_Wx_1_rain.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta23_Wx_23_rain.dat",
  "https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta23_Wx_23_Temp_15_min.dat"
)

# Basin areas for mm/day conversion
WEIR_AREA_KM2 <- c("3" = 0.424, "9" = 0.684)

# -----------------------------
# READ FUNCTION (cached)
# -----------------------------
readHBdat <- memoise(function(url, tz = "America/New_York") {
  lines <- readr::read_lines(curl::curl(
    url,
    handle = curl::new_handle(username = "capstone", password = "data2025")
  ))
  
  header_idx <- which(grepl("^\"?TIMESTAMP\"?,", lines, ignore.case = TRUE))[1]
  if (is.na(header_idx)) return(NULL)
  
  col_names <- gsub('"', "", lines[header_idx]) |> strsplit(",") |> unlist()
  
  df <- readr::read_csv(
    I(lines[(header_idx + 2):length(lines)]),
    col_names = col_names,
    na = c("NaN", "NA", "")
  )
  
  df |> mutate(datetime = ymd_hms(TIMESTAMP, tz = tz, quiet = TRUE)) |> filter(!is.na(datetime))
})

# -----------------------------
# LOAD + STANDARDIZE
# -----------------------------
load_all_data <- function() {
  map_df(URLS, function(u) {
    df <- tryCatch(readHBdat(u), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    
    name <- tolower(basename(u))
    
    df$aspect <- case_when(
      str_detect(name, "weir3|wxsta1|snowcourse_2") ~ "South",
      str_detect(name, "weir9|wxsta23|snowcourse_19") ~ "North",
      TRUE ~ "Other"
    )
    
    # ---- Standardize variables ----
    df <- df |> mutate(
      discharge_cfs = as.numeric(coalesce(pressure1_Q, Q, NA)),
      stage_m = as.numeric(coalesce(float_stage, stage, NA)) * 0.3048,
      precip_mm = as.numeric(coalesce(Rain, Precip, NA)),
      air_temp_c = as.numeric(coalesce(Air_TempC_Avg, AirTC, NA)),
      snow_depth_cm = as.numeric(coalesce(Depthscaled, SnowDepth, NA)),
      swe_cm = as.numeric(coalesce(SWE, NA)),
      vwc_10 = as.numeric(coalesce(TDR_10typ_vwc, NA)),
      vwc_30 = as.numeric(coalesce(TDR_30typ_vwc, NA)),
      vwc_50 = as.numeric(coalesce(TDR_50typ_vwc, NA)),
      wind_speed_avg = as.numeric(coalesce(WS_ms_Avg, NA)),
      wind_speed_max = as.numeric(coalesce(WS_ms_Max, NA)),
      wind_dir_deg = as.numeric(coalesce(WindDir, NA))
    )
    
    # mm/day conversion
    if (str_detect(name, "weir")) {
      id <- str_extract(name, "\\d+")
      area <- WEIR_AREA_KM2[[id]]
      if (!is.null(area)) {
        df <- df |> mutate(
          discharge_mm_day = (discharge_cfs * 0.0283168) / (area * 1e6) * 86400 * 1000
        )
      }
    }
    
    df
  })
}

# -----------------------------
# DAILY CUM PRECIP
# -----------------------------
add_cum_precip <- function(df) {
  df |> mutate(day = as.Date(datetime)) |>
    arrange(aspect, day, datetime) |>
    group_by(aspect, day) |>
    mutate(precip_cum = cumsum(replace_na(precip_mm, 0))) |>
    ungroup()
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Hubbard Brook Live Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("vars", "Variables",
                         choices = c(
                           "Discharge (cfs)" = "discharge_cfs",
                           "Discharge (mm/day)" = "discharge_mm_day",
                           "Stage" = "stage_m",
                           "Precip" = "precip_mm",
                           "Cumulative Precip" = "precip_cum",
                           "Air Temp" = "air_temp_c",
                           "Snow Depth" = "snow_depth_cm",
                           "Wind Speed" = "wind_speed_avg",
                           "Wind Dir" = "wind_dir_deg",
                           "Soil Moisture" = "soil"
                         ), selected = "discharge_cfs"
      ),
      
      checkboxGroupInput("aspect", "Aspect", choices = c("South", "North"), selected = "South"),
      
      checkboxGroupInput("soil_depths", "Soil Depths", choices = c("10" = "vwc_10", "30" = "vwc_30", "50" = "vwc_50"), selected = c("vwc_10")),
      
      dateRangeInput("dates", "Date Range", start = Sys.Date() - 14, end = Sys.Date())
    ),
    
    mainPanel(uiOutput("plots"))
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  data_all <- reactive({
    invalidateLater(60000, session)
    load_all_data()
  })
  
  filtered <- reactive({
    df <- data_all()
    df <- df |> filter(aspect %in% input$aspect, datetime >= input$dates[1], datetime <= input$dates[2] + 1)
    df <- add_cum_precip(df)
    df
  })
  
  output$plots <- renderUI({
    tagList(lapply(input$vars, function(v) plotlyOutput(paste0("plot_", v))))
  })
  
  observe({
    df <- filtered()
    
    for (v in input$vars) {
      local({
        var <- v
        output[[paste0("plot_", var)]] <- renderPlotly({
          
          sub <- df
          
          p <- plot_ly(source = "sync")
          
          for (asp in unique(sub$aspect)) {
            s <- sub |> filter(aspect == asp)
            
            if (var == "soil") {
              for (d in input$soil_depths) {
                p <- add_lines(p, data = s, x = ~datetime, y = s[[d]], name = paste(asp, d))
              }
            } else {
              p <- add_lines(p, data = s, x = ~datetime, y = s[[var]], name = asp)
            }
          }
          
          # assign back to p
          p <- p |> layout(
            title = var,
            xaxis = list(rangeslider = list(visible = FALSE))
          )
          
          p <- event_register(p, "plotly_relayout")
          
          return(p)
        })
        })
      })
    }
  })
  
  # linked zoom
  observeEvent(event_data("plotly_relayout", source = "sync"), {
    ev <- event_data("plotly_relayout", source = "sync")
    if (!is.null(ev[["xaxis.range[0]"]])) {
      xr <- c(ev[["xaxis.range[0]"]], ev[["xaxis.range[1]"]])
      for (v in input$vars) {
        plotlyProxy(paste0("plot_", v), session) |> plotlyProxyInvoke("relayout", list(xaxis = list(range = xr)))
      }
    }
  })
}

shinyApp(ui, server)
