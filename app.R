# app.R ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)

DATA_DIR <- "."

# -----------------------------
# Aspect mapping (override headers)
# -----------------------------
ASPECT_LOOKUP <- tibble::tribble(
  ~site_type, ~site_id, ~aspect,
  "weir", "3", "North",
  "weir", "9", "South"
)

infer_aspect_vec <- function(site_type_vec, site_id_vec) {
  key <- paste0(site_type_vec, ":", site_id_vec)
  map <- ASPECT_LOOKUP %>%
    mutate(key = paste0(site_type, ":", site_id)) %>%
    select(key, aspect)
  out <- map$aspect[match(key, map$key)]
  out[is.na(out)] <- "Unknown"
  out
}

# -----------------------------
# Pretty labels
# -----------------------------
SITE_TYPE_CHOICES <- c(
  "Weir" = "weir",
  "Water station" = "wxsta",
  "Snowcourse" = "snowcourse",
  "Kineo Tower" = "kineo"
)

product_label <- function(site_type, product) {
  if (site_type == "weir" && product == "stream") return("Stream discharge & stage")
  if (site_type == "wxsta" && product == "precip") return("Precipitation")
  if (site_type == "wxsta" && product == "air_temp_15min") return("Air temperature (15-min)")
  if (site_type == "wxsta" && product == "air_temp") return("Air temperature")
  if (site_type == "snowcourse" && product == "snowpack") return("Snow depth & SWE")
  if (site_type == "snowcourse" && product == "soil") return("Soil moisture (Typical)")
  if (site_type == "kineo" && product == "wind") return("Wind")
  str_to_sentence(product)
}

# -----------------------------
# File index
# -----------------------------
empty_file_index <- function() {
  tibble(
    path = character(),
    file = character(),
    file_l = character(),
    site_type = character(),
    site_id = character(),
    site_key = character(),
    aspect = character(),
    site_display = character(),
    product = character(),
    product_display = character()
  )
}

build_file_index <- function(data_dir) {
  files <- list.files(
    data_dir,
    pattern = "\\.(dat|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  idx <- tibble(path = files, file = basename(files)) %>%
    mutate(file_l = str_to_lower(file)) %>%
    mutate(
      site_type = case_when(
        str_detect(file_l, "^weir\\d+") ~ "weir",
        str_detect(file_l, "^wxsta\\d+") ~ "wxsta",
        str_detect(file_l, "^snowcourse[_\\-]\\d+") ~ "snowcourse",
        str_detect(file_l, "^kineo") ~ "kineo",
        TRUE ~ NA_character_
      ),
      site_id = case_when(
        site_type %in% c("weir", "wxsta") ~ str_match(file_l, "^(weir|wxsta)(\\d+)")[, 3],
        site_type == "snowcourse" ~ str_match(file_l, "^snowcourse[_\\-](\\d+)")[, 2],
        site_type == "kineo" ~ NA_character_,
        TRUE ~ NA_character_
      ),
      site_key = case_when(
        site_type == "kineo" ~ "kineo_tower",
        !is.na(site_id) ~ paste0(site_type, site_id),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(site_type), !is.na(site_key)) %>%
    mutate(
      aspect = if_else(site_type == "kineo", "Unknown",
                       infer_aspect_vec(site_type, site_id)),
      site_base = case_when(
        site_type == "weir" ~ paste0("Weir ", site_id),
        site_type == "wxsta" ~ paste0("Water station ", site_id),
        site_type == "snowcourse" ~ paste0("Snowcourse ", site_id),
        site_type == "kineo" ~ "Kineo Tower",
        TRUE ~ site_key
      ),
      site_display = case_when(
        site_type == "kineo" ~ "Kineo Tower",
        !is.na(aspect) & aspect != "Unknown" & aspect != "" ~ paste0(site_base, " — ", aspect, "-facing"),
        TRUE ~ site_base
      ),
      product = case_when(
        site_type == "weir" ~ "stream",
        site_type == "kineo" ~ "wind",
        site_type == "wxsta" & str_detect(file_l, "rain") ~ "precip",
        site_type == "wxsta" & str_detect(file_l, "temp") &
          str_detect(file_l, "15\\s*min|15min|15_min") ~ "air_temp_15min",
        site_type == "wxsta" & str_detect(file_l, "temp") ~ "air_temp",
        site_type == "snowcourse" & str_detect(file_l, "snowdat") ~ "snowpack",
        site_type == "snowcourse" & str_detect(file_l, "soildat") ~ "soil",
        TRUE ~ "unknown"
      ),
      product_display = mapply(product_label, site_type, product, USE.NAMES = FALSE)
    ) %>%
    filter(product != "unknown") %>%
    arrange(site_type, suppressWarnings(as.numeric(site_id)), product_display, file) %>%
    select(path, file, file_l, site_type, site_id, site_key, aspect, site_display, product, product_display)
  
  if (nrow(idx) == 0) empty_file_index() else idx
}

# -----------------------------
# TOA5 reader
# -----------------------------
read_toa5_table <- function(path, tz = "America/New_York") {
  header_lines <- readLines(path, n = 6, warn = FALSE)
  col_names <- gsub('"', "", header_lines[2])
  col_names <- strsplit(col_names, ",")[[1]]
  skip_n <- 4
  
  if (!any(toupper(col_names) == "TIMESTAMP")) {
    lines <- readLines(path, n = 80, warn = FALSE)
    header_idx <- which(grepl("^\"?TIMESTAMP\"?,", lines, ignore.case = TRUE))[1]
    if (is.na(header_idx)) stop("Could not find TIMESTAMP header row in: ", path)
    col_names <- gsub('"', "", lines[header_idx])
    col_names <- strsplit(col_names, ",")[[1]]
    skip_n <- header_idx + 2
  }
  
  df <- read.table(
    path,
    sep = ",",
    header = FALSE,
    skip = skip_n,
    col.names = col_names,
    fill = TRUE,
    stringsAsFactors = FALSE,
    quote = "\""
  ) %>% as_tibble()
  
  if (!("TIMESTAMP" %in% names(df))) stop("TIMESTAMP missing after read: ", path)
  
  df %>%
    mutate(datetime = ymd_hms(TIMESTAMP, tz = tz, quiet = TRUE)) %>%
    filter(!is.na(datetime))
}

# -----------------------------
# Soil (LIMITED): Typical VWC only at 10/30/50 cm
# -----------------------------
make_soil_vwc_typ_choices <- function(df) {
  choices <- c()
  add <- function(col, label) {
    if (col %in% names(df)) choices <<- c(choices, setNames(col, label))
  }
  add("TDR_10typ_vwc", "Soil moisture (VWC %) — 10 cm (Typical)")
  add("TDR_30typ_vwc", "Soil moisture (VWC %) — 30 cm (Typical)")
  add("TDR_50typ_vwc", "Soil moisture (VWC %) — 50 cm (Typical)")
  choices
}

# -----------------------------
# Standardization
# -----------------------------
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

standardize_dataset <- function(df, site_type, product) {
  out <- df %>% select(datetime, everything())
  
  if (site_type == "weir" && product == "stream") {
    out <- out %>%
      mutate(
        discharge_cfs = suppressWarnings(as.numeric(pressure1_Q)),
        stage_ft = coalesce(
          suppressWarnings(as.numeric(float_stage)),
          suppressWarnings(as.numeric(pressure1_stage)),
          suppressWarnings(as.numeric(pressure1_rawdepth))
        ),
        stage_m = stage_ft * 0.3048
      )
  }
  
  if (site_type == "wxsta" && product == "precip") {
    out <- out %>% mutate(precip_mm = suppressWarnings(as.numeric(ReportPCP)))
  }
  
  if (site_type == "wxsta" && product %in% c("air_temp_15min", "air_temp")) {
    air_candidates <- c("RH_airtemp", "Air_TempC_Avg", "AirTC", "Ta", "air_temp", "ActTemp")
    air_col <- pick_first_existing(out, air_candidates)
    out <- out %>%
      mutate(air_temp_c = if (!is.na(air_col)) suppressWarnings(as.numeric(.data[[air_col]])) else NA_real_)
  }
  
  if (site_type == "snowcourse" && product == "snowpack") {
    out <- out %>%
      mutate(
        snow_depth_cm = if ("Depthscaled" %in% names(out)) suppressWarnings(as.numeric(Depthscaled)) else NA_real_,
        swe_cm = if ("SWE" %in% names(out)) suppressWarnings(as.numeric(SWE)) else NA_real_
      )
  }
  
  # ---- Kineo wind: auto-detect common columns and standardize ----
  if (site_type == "kineo" && product == "wind") {
    # common possibilities across Campbell/TOA5 exports
    spd_candidates <- c("WindSpd", "Wind_Spd", "WS", "WS_ms", "WS_mps", "WindSpeed", "Wind_Speed",
                        "WS_Avg", "WindSpd_Avg", "WindSpd_ms", "WindSpd_mps")
    dir_candidates <- c("WindDir", "Wind_Dir", "WD", "WD_deg", "WindDirection", "Wind_Direction",
                        "WD_Avg", "WindDir_Avg")
    
    spd_col <- pick_first_existing(out, spd_candidates)
    dir_col <- pick_first_existing(out, dir_candidates)
    
    out <- out %>%
      mutate(
        wind_speed = if (!is.na(spd_col)) suppressWarnings(as.numeric(.data[[spd_col]])) else NA_real_,
        wind_dir   = if (!is.na(dir_col)) suppressWarnings(as.numeric(.data[[dir_col]])) else NA_real_
      )
  }
  
  out
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Hubbard Brook Viewer (Static TOA5 Prototype)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh file list"),
      hr(),
      checkboxInput("compare", "Compare two sites (side-by-side)", value = FALSE),
      
      h4("Site A"),
      selectInput("site_type_a", "Site type", choices = SITE_TYPE_CHOICES, selected = "weir"),
      selectInput("site_a", "Site", choices = character(0)),
      selectInput("product_a", "Data product", choices = character(0)),
      
      hr(),
      conditionalPanel(
        condition = "input.compare == true",
        h4("Site B"),
        selectInput("site_type_b", "Site type", choices = SITE_TYPE_CHOICES, selected = "weir"),
        selectInput("site_b", "Site", choices = character(0)),
        selectInput("product_b", "Data product", choices = character(0))
      ),
      
      hr(),
      dateRangeInput("date_range", "Date range", start = Sys.Date() - 30, end = Sys.Date()),
      uiOutput("var_ui_mode"),
      hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(uiOutput("dynamic_plots"))
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  file_index <- reactiveVal(empty_file_index())
  
  refresh_index <- function() file_index(build_file_index(DATA_DIR))
  observe({ refresh_index() })
  observeEvent(input$refresh, { refresh_index() })
  
  # strict dropdown builders (base subsetting)
  site_choices <- function(site_type) {
    idx <- file_index()
    idx <- idx[idx$site_type == site_type, , drop = FALSE]
    if (nrow(idx) == 0) return(setNames(character(0), character(0)))
    sites <- idx %>% distinct(site_key, site_display) %>% arrange(site_display)
    setNames(sites$site_key, sites$site_display)
  }
  
  product_choices <- function(site_type, site_key) {
    idx <- file_index()
    idx <- idx[idx$site_type == site_type & idx$site_key == site_key, , drop = FALSE]
    if (nrow(idx) == 0) return(setNames(character(0), character(0)))
    prods <- idx %>% distinct(product, product_display) %>% arrange(product_display)
    setNames(prods$product, prods$product_display)
  }
  
  # Site A dropdown updates
  observeEvent(list(file_index(), input$site_type_a), {
    updateSelectInput(session, "site_a", choices = character(0), selected = character(0))
    updateSelectInput(session, "product_a", choices = character(0), selected = character(0))
    ch <- site_choices(input$site_type_a)
    req(length(ch) > 0)
    updateSelectInput(session, "site_a", choices = ch, selected = ch[[1]])
  }, ignoreInit = FALSE)
  
  observeEvent(list(file_index(), input$site_type_a, input$site_a), {
    req(nzchar(input$site_a))
    updateSelectInput(session, "product_a", choices = character(0), selected = character(0))
    ch <- product_choices(input$site_type_a, input$site_a)
    req(length(ch) > 0)
    updateSelectInput(session, "product_a", choices = ch, selected = ch[[1]])
  }, ignoreInit = FALSE)
  
  # Site B dropdown updates
  observeEvent(list(file_index(), input$site_type_b), {
    updateSelectInput(session, "site_b", choices = character(0), selected = character(0))
    updateSelectInput(session, "product_b", choices = character(0), selected = character(0))
    ch <- site_choices(input$site_type_b)
    req(length(ch) > 0)
    updateSelectInput(session, "site_b", choices = ch, selected = ch[[1]])
  }, ignoreInit = FALSE)
  
  observeEvent(list(file_index(), input$site_type_b, input$site_b), {
    req(nzchar(input$site_b))
    updateSelectInput(session, "product_b", choices = character(0), selected = character(0))
    ch <- product_choices(input$site_type_b, input$site_b)
    req(length(ch) > 0)
    updateSelectInput(session, "product_b", choices = ch, selected = ch[[1]])
  }, ignoreInit = FALSE)
  
  selected_path <- function(site_type_arg, site_key_arg, product_arg) {
    idx <- file_index()
    idx <- idx[idx$site_type == site_type_arg &
                 idx$site_key == site_key_arg &
                 idx$product == product_arg, , drop = FALSE]
    if (nrow(idx) == 0) return(character(0))
    idx$path[[1]]
  }
  
  load_site <- function(site_type_arg, site_key_arg, product_arg) {
    p <- selected_path(site_type_arg, site_key_arg, product_arg)
    req(length(p) == 1, file.exists(p))
    
    df <- read_toa5_table(p)
    df2 <- standardize_dataset(df, site_type_arg, product_arg)
    
    meta <- file_index()
    meta <- meta[meta$site_type == site_type_arg &
                   meta$site_key == site_key_arg &
                   meta$product == product_arg, , drop = FALSE]
    meta <- meta[1, , drop = FALSE]
    
    df2 %>%
      mutate(
        site_display = meta$site_display,
        product_display = meta$product_display,
        site_type = site_type_arg,
        product = product_arg
      )
  }
  
  data_a <- reactive({
    req(input$site_type_a, input$site_a, input$product_a)
    load_site(input$site_type_a, input$site_a, input$product_a)
  })
  
  data_b <- reactive({
    req(input$compare, input$site_type_b, input$site_b, input$product_b)
    load_site(input$site_type_b, input$site_b, input$product_b)
  })
  
  # ---- Variable UI: compare=single select, single-site=checkboxes ----
  output$var_ui_mode <- renderUI({
    df <- data_a()
    st <- isolate(input$site_type_a)
    pr <- isolate(input$product_a)
    
    # Soil: only Typical VWC at 10/30/50
    if (st == "snowcourse" && pr == "soil") {
      var_choices <- make_soil_vwc_typ_choices(df)
      if (length(var_choices) == 0) return(helpText("No typical VWC columns found (expected TDR_10typ_vwc etc.)."))
    } else {
      var_choices <- c()
      if ("discharge_cfs" %in% names(df)) var_choices <- c(var_choices, "Discharge (cfs)" = "discharge_cfs")
      if ("stage_m" %in% names(df))      var_choices <- c(var_choices, "Stage height (m)" = "stage_m")
      if ("precip_mm" %in% names(df))    var_choices <- c(var_choices, "Precipitation (mm per interval)" = "precip_mm")
      if ("air_temp_c" %in% names(df))   var_choices <- c(var_choices, "Air temperature (°C)" = "air_temp_c")
      if ("snow_depth_cm" %in% names(df)) var_choices <- c(var_choices, "Snow depth (cm)" = "snow_depth_cm")
      if ("swe_cm" %in% names(df))        var_choices <- c(var_choices, "Snow water equivalent (cm)" = "swe_cm")
      
      # Kineo wind
      if ("wind_speed" %in% names(df)) var_choices <- c(var_choices, "Wind speed" = "wind_speed")
      if ("wind_dir" %in% names(df))   var_choices <- c(var_choices, "Wind direction (deg)" = "wind_dir")
    }
    
    if (length(var_choices) == 0) return(helpText("No plottable variables found for this dataset."))
    
    if (isTRUE(input$compare)) {
      selectInput("var_select", "Variable", choices = var_choices, selected = var_choices[[1]])
    } else {
      checkboxGroupInput(
        "vars_single",
        "Variables (stacked plots)",
        choices = var_choices,
        selected = var_choices[[1]]
      )
    }
  })
  
  # date filtering
  filtered_a <- reactive({
    req(input$date_range)
    data_a() %>%
      filter(datetime >= as.POSIXct(input$date_range[1]),
             datetime <= as.POSIXct(input$date_range[2] + 1))
  })
  
  filtered_b <- reactive({
    req(input$compare, input$date_range)
    data_b() %>%
      filter(datetime >= as.POSIXct(input$date_range[1]),
             datetime <= as.POSIXct(input$date_range[2] + 1))
  })
  
  # plot factory
  make_plot <- function(df, var, title) {
    req(nrow(df) > 0, var %in% names(df))
    type <- if (var == "precip_mm") "bar" else "scatter"
    mode <- if (var == "precip_mm") NULL else "lines"
    
    plot_ly(df, x = ~datetime, y = df[[var]], type = type, mode = mode) %>%
      layout(
        title = list(text = title),
        xaxis = list(title = "", rangeslider = list(visible = TRUE)),
        yaxis = list(title = ""),
        margin = list(l = 60, r = 20, b = 50, t = 60)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("toImage", "sendDataToCloud"))
  }
  
  # UI for plots (compare vs single stacked)
  output$dynamic_plots <- renderUI({
    if (isTRUE(input$compare)) {
      fluidRow(
        column(6, plotlyOutput("plot_a", height = "520px")),
        column(6, plotlyOutput("plot_b", height = "520px"))
      )
    } else {
      req(input$vars_single)
      tagList(lapply(seq_along(input$vars_single), function(i) {
        plotlyOutput(paste0("plot_single_", i), height = "320px")
      }))
    }
  })
  
  # Compare plots
  output$plot_a <- renderPlotly({
    req(isTRUE(input$compare), input$var_select)
    df <- filtered_a()
    ttl <- paste0(unique(df$site_display), " | ", unique(df$product_display))
    make_plot(df, input$var_select, ttl)
  })
  
  output$plot_b <- renderPlotly({
    req(isTRUE(input$compare), input$var_select)
    df <- filtered_b()
    ttl <- paste0(unique(df$site_display), " | ", unique(df$product_display))
    
    if (!(input$var_select %in% names(df))) {
      plot_ly() %>%
        layout(
          title = list(text = ttl),
          annotations = list(
            list(
              text = paste0("Variable '", input$var_select, "' is not available for this dataset."),
              x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
            )
          )
        )
    } else {
      make_plot(df, input$var_select, ttl)
    }
  })
  
  # Single-site stacked plots
  observe({
    req(!isTRUE(input$compare))
    req(input$vars_single)
    
    df <- filtered_a()
    vars <- input$vars_single
    
    lapply(seq_along(vars), function(i) {
      local({
        ii <- i
        v <- vars[[ii]]
        
        output[[paste0("plot_single_", ii)]] <- renderPlotly({
          req(v %in% names(df))
          base_ttl <- paste0(unique(df$site_display), " | ", unique(df$product_display))
          make_plot(df, v, paste0(base_ttl, " | ", v))
        })
      })
    })
  })
  
  output$status <- renderText({
    idx <- file_index()
    paste0(
      "Data dir: ", normalizePath(DATA_DIR, winslash = "/", mustWork = FALSE), "\n",
      "Indexed files: ", nrow(idx)
    )
  })
}

shinyApp(ui, server)
