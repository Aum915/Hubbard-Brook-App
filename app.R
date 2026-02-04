# app.R
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(plotly)

# ----------------------------
# 1) TOA5 reader (robust)
# ----------------------------
read_toa5 <- function(path, tz = "America/New_York") {
  raw_lines <- readLines(path, n = 50, warn = FALSE)
  header_idx <- which(grepl("^\"?TIMESTAMP\"?,", raw_lines, ignore.case = TRUE))[1]
  if (is.na(header_idx)) stop("Could not find TIMESTAMP header row in TOA5 file: ", path)
  
  df <- read_csv(path, skip = header_idx - 1, show_col_types = FALSE, progress = FALSE)
  
  # Convert TIMESTAMP + parse numbers safely
  df <- df |>
    mutate(
      TIMESTAMP = as.POSIXct(TIMESTAMP, tz = tz),
      across(-TIMESTAMP, ~ suppressWarnings(parse_number(as.character(.x))))
    ) |>
    filter(!is.na(TIMESTAMP))
  
  df
}

# ----------------------------
# 2) Cleaning / standard schema
# ----------------------------
clean_weir <- function(df, watershed_id, aspect = NA_character_) {
  df |>
    transmute(
      ts = TIMESTAMP,
      watershed_id = watershed_id,
      aspect = aspect,
      
      stage_ft = coalesce(float_stage, pressure1_stage, pressure1_rawdepth),
      stage_m  = stage_ft * 0.3048,
      
      discharge_cfs = pressure1_Q,        # ft^3/sec
      well_temp_c   = welltemp1,
      stream_temp_c = streamtemp,
      batt_volt     = batt_volt
    ) |>
    filter(!is.na(ts))
}

clean_precip <- function(df, watershed_id, aspect = NA_character_, reportpcp_in_inches = FALSE) {
  p <- if (reportpcp_in_inches) df$ReportPCP * 25.4 else df$ReportPCP
  
  tibble(
    ts = df$TIMESTAMP,
    watershed_id = watershed_id,
    aspect = aspect,
    precip_mm = p,
    gage_temp_c = df$ActTemp,
    blocked_sec = df$blockedSec
  ) |>
    filter(!is.na(ts))
}

# ----------------------------
# 3) Precip helpers
# ----------------------------
precip_last_15min_mm <- function(df_precip) {
  if (is.null(df_precip) || nrow(df_precip) == 0) return(NA_real_)
  end_t <- max(df_precip$ts, na.rm = TRUE)
  start_t <- end_t - 15 * 60
  df_precip |>
    filter(ts >= start_t, ts <= end_t) |>
    summarise(total = sum(precip_mm, na.rm = TRUE)) |>
    pull(total)
}

tag_precip_events <- function(df_precip, gap_hours = 4) {
  # Works best when df_precip is one watershed at a time
  df <- df_precip |>
    arrange(ts) |>
    mutate(is_wet = !is.na(precip_mm) & precip_mm > 0)
  
  # A new event starts when wet begins after a long dry gap.
  # We detect dry gap length between wet points.
  wet_times <- df$ts[df$is_wet]
  if (length(wet_times) == 0) {
    df$event_id <- 0L
    return(df)
  }
  
  df <- df |>
    mutate(
      prev_wet_ts = ifelse(is_wet, lag(ts), as.POSIXct(NA)),
      prev_wet_ts = zoo::na.locf(prev_wet_ts, na.rm = FALSE),
      dry_hours = ifelse(is_wet,
                         as.numeric(difftime(ts, lag(ts[is_wet], default = first(wet_times)), units = "hours")),
                         NA_real_)
    )
  
  # simpler robust approach: compute new_event by looking at time since last wet row
  last_wet <- as.POSIXct(rep(NA, nrow(df)), origin = "1970-01-01", tz = attr(df$ts, "tzone"))
  last_seen <- as.POSIXct(NA, tz = attr(df$ts, "tzone"))
  for (i in seq_len(nrow(df))) {
    last_wet[i] <- last_seen
    if (isTRUE(df$is_wet[i])) last_seen <- df$ts[i]
  }
  
  df <- df |>
    mutate(
      last_wet_ts = last_wet,
      hrs_since_last_wet = as.numeric(difftime(ts, last_wet_ts, units = "hours")),
      new_event = is_wet & (is.na(last_wet_ts) | hrs_since_last_wet >= gap_hours),
      event_id = cumsum(new_event) * as.integer(is_wet)
    )
  
  df
}

summarise_events <- function(df_events) {
  df_events |>
    filter(event_id > 0) |>
    group_by(watershed_id, event_id) |>
    summarise(
      start = min(ts),
      end = max(ts),
      duration_hr = as.numeric(difftime(end, start, units = "hours")),
      total_mm = sum(precip_mm, na.rm = TRUE),
      peak_15min_mm = max(precip_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(start))
}

# ----------------------------
# 4) Static "refresh" (file mtime)
# ----------------------------
file_mtime <- function(path) {
  info <- file.info(path)
  if (is.na(info$mtime)) as.POSIXct(0, origin = "1970-01-01") else info$mtime
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Hubbard Brook – Real-Time (Static CSV Prototype)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("weir_path", "Weir CSV path", value = "data/weir_3.csv"),
      textInput("precip_path", "Precip CSV path", value = "data/precip_weir_3.csv"),
      
      textInput("watershed_id", "Watershed ID label", value = "weir_3"),
      selectInput("aspect", "Aspect", choices = c("North", "South", "Unknown"), selected = "Unknown"),
      
      checkboxInput("precip_inches", "ReportPCP is in inches (convert to mm)", value = FALSE),
      
      hr(),
      selectInput(
        "var",
        "Output variable",
        choices = c(
          "Stage (m)" = "stage_m",
          "Discharge (cfs)" = "discharge_cfs",
          "Stream temp (°C)" = "stream_temp_c",
          "Well temp (°C)" = "well_temp_c",
          "Precip (mm per interval)" = "precip_mm"
        ),
        selected = "discharge_cfs"
      ),
      
      dateRangeInput("daterange", "Date range", start = Sys.Date() - 7, end = Sys.Date()),
      checkboxInput("show_events", "Show precip events (precip only)", value = TRUE),
      numericInput("gap_hours", "Event gap (hours)", value = 4, min = 1, step = 1),
      
      hr(),
      verbatimTextOutput("status", placeholder = TRUE)
    ),
    
    mainPanel(
      fluidRow(
        column(4, uiOutput("card_updated")),
        column(4, uiOutput("card_last15")),
        column(4, uiOutput("card_now"))
      ),
      tabsetPanel(
        tabPanel("Time Series", plotlyOutput("ts_plot", height = "520px")),
        tabPanel("Events", tableOutput("events_tbl"))
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  # simulate “live updates” by repolling when files change
  weir_raw <- reactivePoll(
    intervalMillis = 5 * 1000,
    session = session,
    checkFunc = function() file_mtime(input$weir_path),
    valueFunc = function() {
      read_toa5(input$weir_path)
    }
  )
  
  precip_raw <- reactivePoll(
    intervalMillis = 5 * 1000,
    session = session,
    checkFunc = function() file_mtime(input$precip_path),
    valueFunc = function() {
      read_toa5(input$precip_path)
    }
  )
  
  weir_std <- reactive({
    req(weir_raw())
    clean_weir(
      weir_raw(),
      watershed_id = input$watershed_id,
      aspect = if (input$aspect == "Unknown") NA_character_ else input$aspect
    )
  })
  
  precip_std <- reactive({
    req(precip_raw())
    clean_precip(
      precip_raw(),
      watershed_id = input$watershed_id,
      aspect = if (input$aspect == "Unknown") NA_character_ else input$aspect,
      reportpcp_in_inches = isTRUE(input$precip_inches)
    )
  })
  
  # unified series for plotting (choose based on variable)
  plot_df <- reactive({
    var <- input$var
    dr <- input$daterange
    req(dr)
    
    if (var == "precip_mm") {
      df <- precip_std()
      df <- df |> filter(as.Date(ts) >= dr[1], as.Date(ts) <= dr[2])
      if (isTRUE(input$show_events)) df <- tag_precip_events(df, gap_hours = input$gap_hours)
      df
    } else {
      df <- weir_std()
      df |> filter(as.Date(ts) >= dr[1], as.Date(ts) <= dr[2])
    }
  })
  
  output$card_updated <- renderUI({
    # show whichever dataset is relevant to selected var
    if (input$var == "precip_mm") {
      df <- precip_std()
      t <- if (nrow(df) == 0) NA else max(df$ts, na.rm = TRUE)
    } else {
      df <- weir_std()
      t <- if (nrow(df) == 0) NA else max(df$ts, na.rm = TRUE)
    }
    
    bslib::card(
      bslib::card_header("Last update"),
      bslib::card_body(tags$b(format(t, "%Y-%m-%d %H:%M:%S")))
    )
  })
  
  output$card_last15 <- renderUI({
    mm <- precip_last_15min_mm(precip_std())
    bslib::card(
      bslib::card_header("Precip last 15 min"),
      bslib::card_body(tags$b(ifelse(is.na(mm), "NA", sprintf("%.2f mm", mm))))
    )
  })
  
  output$card_now <- renderUI({
    dfw <- weir_std()
    if (nrow(dfw) == 0) {
      val <- NA
    } else {
      last <- dfw |> arrange(desc(ts)) |> slice(1)
      val <- last[[input$var]]
      # if chosen var isn't in weir df (e.g., precip), show discharge as a default
      if (is.null(val)) val <- last$discharge_cfs
    }
    
    bslib::card(
      bslib::card_header("Latest value"),
      bslib::card_body(tags$b(ifelse(is.na(val), "NA", format(round(val, 4), nsmall = 2))))
    )
  })
  
  output$status <- renderText({
    paste0(
      "Weir file mtime:   ", as.character(file_mtime(input$weir_path)), "\n",
      "Precip file mtime: ", as.character(file_mtime(input$precip_path)), "\n",
      "Watershed:         ", input$watershed_id, " (", input$aspect, ")"
    )
  })
  
  output$ts_plot <- renderPlotly({
    df <- plot_df()
    req(nrow(df) > 0)
    
    var <- input$var
    y <- df[[var]]
    
    p <- plot_ly(
      data = df,
      x = ~ts,
      y = y,
      type = if (var == "precip_mm") "bar" else "scatter",
      mode = if (var == "precip_mm") NULL else "lines",
      hovertemplate = paste0("%{x}<br>", var, ": %{y}<extra></extra>")
    ) |>
      layout(
        xaxis = list(title = "", rangeslider = list(visible = TRUE)),
        yaxis = list(title = var),
        margin = list(l = 60, r = 20, b = 50, t = 10)
      )
    
    # Optional event shading for precip
    if (var == "precip_mm" && isTRUE(input$show_events) && "event_id" %in% names(df)) {
      ev <- df |>
        filter(event_id > 0) |>
        group_by(event_id) |>
        summarise(x0 = min(ts), x1 = max(ts), .groups = "drop")
      
      if (nrow(ev) > 0) {
        shapes <- lapply(seq_len(nrow(ev)), function(i) {
          list(
            type = "rect",
            xref = "x", yref = "paper",
            x0 = ev$x0[i], x1 = ev$x1[i],
            y0 = 0, y1 = 1,
            line = list(width = 0)
          )
        })
        p <- p |> layout(shapes = shapes)
      }
    }
    
    # Remove export/download controls from plotly modebar
    config(p, displaylogo = FALSE, modeBarButtonsToRemove = c("toImage", "sendDataToCloud"))
  })
  
  output$events_tbl <- renderTable({
    dfp <- precip_std()
    req(nrow(dfp) > 0)
    dfp2 <- tag_precip_events(
      dfp |> filter(as.Date(ts) >= input$daterange[1], as.Date(ts) <= input$daterange[2]),
      gap_hours = input$gap_hours
    )
    summarise_events(dfp2)
  })
}

shinyApp(ui, server)
