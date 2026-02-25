# app.R ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)

DATA_DIR <- "."

# ---- Weir basin areas (km^2) from hectares ----
# 1 ha = 0.01 km^2
WEIR_AREA_KM2 <- c(
  "3" = 0.424,  # 42.4 ha
  "9" = 0.684   # 68.4 ha
)

# -----------------------------
# File indexing from naming conventions
# -----------------------------
empty_file_index <- function() {
  tibble(
    path = character(),
    file = character(),
    file_l = character(),
    site_type = character(),
    site_id = character(),
    site_key = character(),
    site_display = character(),
    product = character(),
    product_display = character()
  )
}

product_label <- function(site_type, product) {
  if (site_type == "weir"       && product == "stream")      return("Stream discharge & stage")
  if (site_type == "wxsta"      && product == "precip")      return("Precipitation")
  if (site_type == "wxsta"      && product == "air_temp_15") return("Air temperature (15-min)")
  if (site_type == "snowcourse" && product == "snowpack")    return("Snow depth & SWE")
  if (site_type == "snowcourse" && product == "soil")        return("Soil moisture (Typical)")
  if (site_type == "kineo"      && product == "wind")        return("Wind")
  str_to_sentence(product)
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
      ),
      site_display = case_when(
        site_type == "weir" ~ paste0("Weir ", site_id),
        site_type == "wxsta" ~ paste0("Weather station ", site_id),
        site_type == "snowcourse" ~ paste0("Snowcourse ", site_id),
        site_type == "kineo" ~ "Kineo Tower",
        TRUE ~ site_key
      ),
      product = case_when(
        site_type == "weir" ~ "stream",
        site_type == "kineo" ~ "wind",
        
        # ---- more robust filename matching for precip ----
        site_type == "wxsta" & str_detect(file_l, "rain|pcp|precip|ppt") ~ "precip",
        
        # air-temp rule (filename-based)
        site_type == "wxsta" & str_detect(file_l, "temp") &
          str_detect(file_l, "15\\s*min|15min|15_min") ~ "air_temp_15",
        
        site_type == "snowcourse" & str_detect(file_l, "snowdat") ~ "snowpack",
        site_type == "snowcourse" & str_detect(file_l, "soildat") ~ "soil",
        TRUE ~ "unknown"
      ),
      product_display = mapply(product_label, site_type, product, USE.NAMES = FALSE)
    ) %>%
    filter(!is.na(site_type), !is.na(site_key), product != "unknown") %>%
    arrange(site_type, suppressWarnings(as.numeric(site_id)), product_display, file) %>%
    select(path, file, file_l, site_type, site_id, site_key, site_display, product, product_display)
  
  if (nrow(idx) == 0) empty_file_index() else idx
}

# -----------------------------
# TOA5 reader (supports your 4-line TOA5 headers)
# -----------------------------
read_toa5_table <- function(path, tz = "America/New_York") {
  header_lines <- readLines(path, n = 6, warn = FALSE)
  col_names <- gsub('"', "", header_lines[2])
  col_names <- strsplit(col_names, ",")[[1]]
  skip_n <- 4
  
  if (!any(toupper(col_names) == "TIMESTAMP")) {
    lines <- readLines(path, n = 120, warn = FALSE)
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
# Header-only column scan (FAST) for fallback file selection
# -----------------------------
toa5_colnames_fast <- function(path) {
  lines <- readLines(path, n = 6, warn = FALSE)
  if (length(lines) < 2) return(character(0))
  raw <- gsub('"', "", lines[2])
  cols <- strsplit(raw, ",")[[1]]
  cols <- trimws(cols)
  
  if (!any(toupper(cols) == "TIMESTAMP")) {
    lines2 <- readLines(path, n = 120, warn = FALSE)
    header_idx <- which(grepl("^\"?TIMESTAMP\"?,", lines2, ignore.case = TRUE))[1]
    if (is.na(header_idx)) return(character(0))
    raw2 <- gsub('"', "", lines2[header_idx])
    cols2 <- strsplit(raw2, ",")[[1]]
    cols2 <- trimws(cols2)
    return(cols2)
  }
  
  cols
}

file_has_any_cols <- function(path, candidates) {
  cols <- toa5_colnames_fast(path)
  if (length(cols) == 0) return(FALSE)
  any(candidates %in% cols)
}

# -----------------------------
# Robust column picking
# -----------------------------
pick_first <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# -----------------------------
# Standardize datasets into common columns (NO CRASH IF MISSING)
# -----------------------------
standardize_dataset <- function(df, site_type, product, site_id = NA_character_) {
  out <- df %>% select(datetime, everything())
  
  if (site_type == "weir" && product == "stream") {
    q_col <- pick_first(out, c("pressure1_Q", "Q", "Discharge", "discharge", "discharge_cfs"))
    stage_col <- pick_first(out, c("float_stage", "pressure1_stage", "pressure1_rawdepth", "stage", "Stage"))
    
    out <- out %>%
      mutate(
        discharge_cfs = if (!is.na(q_col)) suppressWarnings(as.numeric(.data[[q_col]])) else NA_real_,
        stage_ft = if (!is.na(stage_col)) suppressWarnings(as.numeric(.data[[stage_col]])) else NA_real_,
        stage_m = stage_ft * 0.3048
      )
    
    area_km2 <- WEIR_AREA_KM2[[as.character(site_id)]]
    
    out <- out %>%
      mutate(
        discharge_mm_day = {
          area_m2 <- area_km2 * 1e6
          q_m3s <- discharge_cfs * 0.028316846592
          (q_m3s / area_m2) * 86400 * 1000
        }
      )
  }
  
  if (site_type == "wxsta" && product == "precip") {
    p_col <- pick_first(out, c("ReportPCP", "Precip", "precip", "Rain", "rain"))
    out <- out %>% mutate(precip_mm = if (!is.na(p_col)) suppressWarnings(as.numeric(.data[[p_col]])) else NA_real_)
  }
  
  if (site_type == "wxsta" && product == "air_temp_15") {
    t_col <- pick_first(out, c("Air_TempC_Avg", "RH_airtemp", "ActTemp", "AirTC", "Ta"))
    out <- out %>% mutate(air_temp_c = if (!is.na(t_col)) suppressWarnings(as.numeric(.data[[t_col]])) else NA_real_)
  }
  
  if (site_type == "snowcourse" && product == "snowpack") {
    d_col <- pick_first(out, c("Depthscaled", "SnowDepth", "snow_depth"))
    s_col <- pick_first(out, c("SWE", "swe"))
    out <- out %>%
      mutate(
        snow_depth_cm = if (!is.na(d_col)) suppressWarnings(as.numeric(.data[[d_col]])) else NA_real_,
        swe_cm = if (!is.na(s_col)) suppressWarnings(as.numeric(.data[[s_col]])) else NA_real_
      )
  }
  
  if (site_type == "snowcourse" && product == "soil") {
    out <- out %>%
      mutate(
        vwc_10 = if ("TDR_10typ_vwc" %in% names(out)) suppressWarnings(as.numeric(TDR_10typ_vwc)) else NA_real_,
        vwc_30 = if ("TDR_30typ_vwc" %in% names(out)) suppressWarnings(as.numeric(TDR_30typ_vwc)) else NA_real_,
        vwc_50 = if ("TDR_50typ_vwc" %in% names(out)) suppressWarnings(as.numeric(TDR_50typ_vwc)) else NA_real_
      )
  }
  
  if (site_type == "kineo" && product == "wind") {
    out <- out %>%
      mutate(
        wind_speed_avg = if ("WS_ms_Avg" %in% names(out)) suppressWarnings(as.numeric(WS_ms_Avg)) else NA_real_,
        wind_speed_max = if ("WS_ms_Max" %in% names(out)) suppressWarnings(as.numeric(WS_ms_Max)) else NA_real_,
        wind_dir_deg   = if ("WindDir" %in% names(out)) suppressWarnings(as.numeric(WindDir)) else NA_real_
      )
  }
  
  out
}

# -----------------------------
# Keep only columns needed for plotting (prevents bind_rows type errors)
# -----------------------------
keep_plot_cols <- function(df, site_type, product) {
  if (is.null(df)) return(df)
  
  cols <- c("datetime")
  
  if (site_type == "weir" && product == "stream")
    cols <- c(cols, "discharge_cfs", "discharge_mm_day", "stage_m")
  
  if (site_type == "wxsta" && product == "precip")
    cols <- c(cols, "precip_mm")
  
  if (site_type == "wxsta" && product == "air_temp_15")
    cols <- c(cols, "air_temp_c")
  
  if (site_type == "snowcourse" && product == "snowpack")
    cols <- c(cols, "snow_depth_cm", "swe_cm")
  
  if (site_type == "snowcourse" && product == "soil")
    cols <- c(cols, "vwc_10", "vwc_30", "vwc_50")
  
  if (site_type == "kineo" && product == "wind")
    cols <- c(cols, "wind_speed_avg", "wind_speed_max", "wind_dir_deg")
  
  df %>% select(any_of(cols))
}

# -----------------------------
# Aspect selection logic
#   South = lower numeric ID
#   North = higher numeric ID
# -----------------------------
pick_site_for_aspect <- function(idx, site_type, aspect) {
  sub <- idx %>% filter(site_type == !!site_type, !is.na(site_id))
  if (nrow(sub) == 0) return(character(0))
  
  ids_num <- suppressWarnings(as.numeric(sub$site_id))
  if (all(is.na(ids_num))) return(character(0))
  
  if (aspect == "South") {
    target <- min(ids_num, na.rm = TRUE)
    sub %>% filter(suppressWarnings(as.numeric(site_id)) == target) %>% slice(1) %>% pull(site_key)
  } else if (aspect == "North") {
    target <- max(ids_num, na.rm = TRUE)
    sub %>% filter(suppressWarnings(as.numeric(site_id)) == target) %>% slice(1) %>% pull(site_key)
  } else {
    character(0)
  }
}

# -----------------------------
# Plot helpers (no rangeslider)
# -----------------------------
base_plot_cfg <- function(p) {
  p %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "toImage"))
}

plot_lines_multi <- function(df, y, title, ylab) {
  req(nrow(df) > 0, y %in% names(df))
  p <- plot_ly() %>%
    layout(
      title = list(text = title),
      xaxis = list(title = "", rangeslider = list(visible = FALSE)),   # removed label
      yaxis = list(title = ylab),
      legend = list(orientation = "h", x = 0, y = -0.25),              # legend under plot
      margin = list(l = 70, r = 20, b = 95, t = 60)                    # extra bottom room
    )
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    p <- add_lines(p, data = sub, x = ~datetime, y = sub[[y]], name = lab)
  }
  base_plot_cfg(p)
}

plot_bars_multi <- function(df, y, title, ylab) {
  req(nrow(df) > 0, y %in% names(df))
  p <- plot_ly() %>%
    layout(
      title = list(text = title),
      xaxis = list(title = "", rangeslider = list(visible = FALSE)),   # removed label
      yaxis = list(title = ylab),
      barmode = "overlay",
      legend = list(orientation = "h", x = 0, y = -0.25),              # legend under plot
      margin = list(l = 70, r = 20, b = 95, t = 60)                    # extra bottom room
    )
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    p <- add_bars(p, data = sub, x = ~datetime, y = sub[[y]], name = lab, opacity = 0.6)
  }
  base_plot_cfg(p)
}

plot_soil_multi <- function(df, depths_on = c("10","30","50"), title = "Soil moisture (Typical)") {
  req(nrow(df) > 0)
  p <- plot_ly() %>%
    layout(
      title = list(text = title),
      xaxis = list(title = "", rangeslider = list(visible = FALSE)),   # removed label
      yaxis = list(title = "Volumetric Water Content (VWC %)"),
      legend = list(orientation = "h", x = 0, y = -0.25),              # legend under plot
      margin = list(l = 70, r = 20, b = 95, t = 60)                    # extra bottom room
    )
  
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    
    if ("10" %in% depths_on && "vwc_10" %in% names(sub))
      p <- add_lines(p, data = sub, x = ~datetime, y = ~vwc_10, name = paste0(lab, " — 10 cm"))
    if ("30" %in% depths_on && "vwc_30" %in% names(sub))
      p <- add_lines(p, data = sub, x = ~datetime, y = ~vwc_30, name = paste0(lab, " — 30 cm"))
    if ("50" %in% depths_on && "vwc_50" %in% names(sub))
      p <- add_lines(p, data = sub, x = ~datetime, y = ~vwc_50, name = paste0(lab, " — 50 cm"))
  }
  
  base_plot_cfg(p)
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
      selectInput(
        "aspect",
        "Aspect",
        choices = c("South-facing" = "South", "North-facing" = "North", "Show both" = "Both"),
        selected = "South"
      ),
      dateRangeInput("date_range", "Date range", start = Sys.Date() - 30, end = Sys.Date()),
      hr(),
      checkboxGroupInput(
        "graphs_on",
        "Graphs to show",
        choices = c(
          "Stream discharge (cfs)" = "discharge",
          "Stream discharge (mm/day)" = "discharge_mmday",
          "Stage height" = "stage",
          "Precipitation" = "precip",
          "Air temperature" = "airtemp",
          "Snow depth" = "snowdepth",
          "Wind speed (avg/max)" = "wind_speed",
          "Wind direction" = "wind_dir",
          "Soil moisture (10/30/50 cm)" = "soil"
        ),
        selected = c("discharge", "stage", "precip", "airtemp")
      ),
      conditionalPanel(
        condition = "input.graphs_on.indexOf('soil') >= 0",
        checkboxGroupInput(
          "soil_depths",
          "Soil depths",
          choices = c("10 cm" = "10", "30 cm" = "30", "50 cm" = "50"),
          selected = c("10", "30", "50")
        )
      ),
      hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(uiOutput("plots_ui"))
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
  
  # which site keys for selected aspect(s)
  aspect_sites <- reactive({
    idx <- file_index()
    
    if (input$aspect == "Both") {
      list(
        weir = c(South = pick_site_for_aspect(idx, "weir", "South"),
                 North = pick_site_for_aspect(idx, "weir", "North")),
        wx   = c(South = pick_site_for_aspect(idx, "wxsta", "South"),
                 North = pick_site_for_aspect(idx, "wxsta", "North")),
        snow = c(South = pick_site_for_aspect(idx, "snowcourse", "South"),
                 North = pick_site_for_aspect(idx, "snowcourse", "North")),
        kineo = "kineo_tower"
      )
    } else {
      list(
        weir = c(Selected = pick_site_for_aspect(idx, "weir", input$aspect)),
        wx   = c(Selected = pick_site_for_aspect(idx, "wxsta", input$aspect)),
        snow = c(Selected = pick_site_for_aspect(idx, "snowcourse", input$aspect)),
        kineo = "kineo_tower"
      )
    }
  })
  
  # fallback path selection for wxsta precip based on file contents
  selected_path <- function(site_type, site_key, product) {
    idx <- file_index()
    sub <- idx %>% filter(site_type == !!site_type, site_key == !!site_key, product == !!product)
    if (nrow(sub) > 0) return(sub$path[[1]])
    
    if (site_type == "wxsta" && product == "precip") {
      candidates <- idx %>% filter(site_type == "wxsta", site_key == !!site_key)
      if (nrow(candidates) == 0) return(character(0))
      
      precip_cols <- c("ReportPCP", "Precip", "precip", "Rain", "rain")
      hits <- candidates$path[vapply(candidates$path, file_has_any_cols, logical(1), candidates = precip_cols)]
      if (length(hits) > 0) return(hits[[1]])
    }
    
    character(0)
  }
  
  site_id_from_key <- function(site_key) {
    if (is.na(site_key) || !nzchar(site_key)) return(NA_character_)
    if (site_key == "kineo_tower") return(NA_character_)
    str_match(site_key, "^(weir|wxsta|snowcourse)(\\d+)$")[, 3]
  }
  
  load_one <- function(site_type, site_key, product, label) {
    p <- selected_path(site_type, site_key, product)
    if (length(p) != 1 || !file.exists(p)) return(NULL)
    
    df <- read_toa5_table(p)
    sid <- site_id_from_key(site_key)
    
    df <- standardize_dataset(
      df,
      site_type = site_type,
      product = product,
      site_id = sid
    )
    
    # ✅ prevents bind_rows type mismatch errors in "Show both"
    df <- keep_plot_cols(df, site_type, product)
    
    df$series_label <- label
    df
  }
  
  datasets <- reactive({
    s <- aspect_sites()
    
    kineo <- load_one("kineo", s$kineo, "wind", "Kineo Tower")
    
    if (input$aspect == "Both") {
      list(
        weir = bind_rows(
          load_one("weir", s$weir[["South"]], "stream", "South-facing"),
          load_one("weir", s$weir[["North"]], "stream", "North-facing")
        ),
        precip = bind_rows(
          load_one("wxsta", s$wx[["South"]], "precip", "South-facing"),
          load_one("wxsta", s$wx[["North"]], "precip", "North-facing")
        ),
        airtemp = bind_rows(
          load_one("wxsta", s$wx[["South"]], "air_temp_15", "South-facing"),
          load_one("wxsta", s$wx[["North"]], "air_temp_15", "North-facing")
        ),
        snowpack = bind_rows(
          load_one("snowcourse", s$snow[["South"]], "snowpack", "South-facing"),
          load_one("snowcourse", s$snow[["North"]], "snowpack", "North-facing")
        ),
        soil = bind_rows(
          load_one("snowcourse", s$snow[["South"]], "soil", "South-facing"),
          load_one("snowcourse", s$snow[["North"]], "soil", "North-facing")
        ),
        kineo = kineo
      )
    } else {
      list(
        weir = load_one("weir", s$weir[["Selected"]], "stream", paste0(input$aspect, "-facing")),
        precip = load_one("wxsta", s$wx[["Selected"]], "precip", paste0(input$aspect, "-facing")),
        airtemp = load_one("wxsta", s$wx[["Selected"]], "air_temp_15", paste0(input$aspect, "-facing")),
        snowpack = load_one("snowcourse", s$snow[["Selected"]], "snowpack", paste0(input$aspect, "-facing")),
        soil = load_one("snowcourse", s$snow[["Selected"]], "soil", paste0(input$aspect, "-facing")),
        kineo = kineo
      )
    }
  })
  
  filter_by_date <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    req(input$date_range)
    df %>%
      filter(datetime >= as.POSIXct(input$date_range[1]),
             datetime <= as.POSIXct(input$date_range[2] + 1))
  }
  
  output$plots_ui <- renderUI({
    req(input$graphs_on)
    tagList(lapply(input$graphs_on, function(id) {
      plotlyOutput(paste0("plot_", id), height = "320px")
    }))
  })
  
  # ---- discharge cfs ----
  output$plot_discharge <- renderPlotly({
    req("discharge" %in% input$graphs_on)
    df <- filter_by_date(datasets()$weir)
    req(!is.null(df), nrow(df) > 0)
    validate(need("discharge_cfs" %in% names(df) && any(!is.na(df$discharge_cfs)),
                  "No discharge column found in this weir file."))
    plot_lines_multi(df, "discharge_cfs", "Stream discharge", "Discharge (cfs)")
  })
  
  # ---- discharge mm/day ----
  output$plot_discharge_mmday <- renderPlotly({
    req("discharge_mmday" %in% input$graphs_on)
    df <- filter_by_date(datasets()$weir)
    req(!is.null(df), nrow(df) > 0)
    validate(need("discharge_mm_day" %in% names(df) && any(!is.na(df$discharge_mm_day)),
                  "No discharge/mm-day available for this weir file."))
    plot_lines_multi(
      df, "discharge_mm_day",
      "Stream discharge (mm/day)",
      "Discharge equivalent (mm/day)"
    )
  })
  
  # ---- stage ----
  output$plot_stage <- renderPlotly({
    req("stage" %in% input$graphs_on)
    df <- filter_by_date(datasets()$weir)
    req(!is.null(df), nrow(df) > 0)
    validate(need("stage_m" %in% names(df) && any(!is.na(df$stage_m)),
                  "No stage column found in this weir file."))
    plot_lines_multi(df, "stage_m", "Stage height", "Stage height (m)")
  })
  
  # ---- precip ----
  output$plot_precip <- renderPlotly({
    req("precip" %in% input$graphs_on)
    df <- filter_by_date(datasets()$precip)
    req(!is.null(df), nrow(df) > 0)
    validate(need("precip_mm" %in% names(df) && any(!is.na(df$precip_mm)),
                  "No precipitation column found in this weather file."))
    plot_bars_multi(df, "precip_mm", "Precipitation", "Precipitation (mm per interval)")
  })
  
  # ---- air temp ----
  output$plot_airtemp <- renderPlotly({
    req("airtemp" %in% input$graphs_on)
    df <- filter_by_date(datasets()$airtemp)
    req(!is.null(df), nrow(df) > 0)
    validate(need("air_temp_c" %in% names(df) && any(!is.na(df$air_temp_c)),
                  "No air temperature column found in this weather file."))
    plot_lines_multi(df, "air_temp_c", "Air temperature", "Air temperature (°C)")
  })
  
  # ---- snow depth ----
  output$plot_snowdepth <- renderPlotly({
    req("snowdepth" %in% input$graphs_on)
    df <- filter_by_date(datasets()$snowpack)
    req(!is.null(df), nrow(df) > 0)
    validate(need("snow_depth_cm" %in% names(df) && any(!is.na(df$snow_depth_cm)),
                  "No snow depth column found in this snowcourse file."))
    plot_lines_multi(df, "snow_depth_cm", "Snow depth", "Snow depth (cm)")
  })
  
  # ---- wind speed ----
  output$plot_wind_speed <- renderPlotly({
    req("wind_speed" %in% input$graphs_on)
    df <- filter_by_date(datasets()$kineo)
    req(!is.null(df), nrow(df) > 0)
    validate(need("wind_speed_avg" %in% names(df) && any(!is.na(df$wind_speed_avg)),
                  "No wind speed avg column found for Kineo."))
    p <- plot_ly() %>%
      add_lines(data = df, x = ~datetime, y = ~wind_speed_avg, name = "Avg wind speed") %>%
      add_lines(data = df, x = ~datetime, y = ~wind_speed_max, name = "Max wind speed") %>%
      layout(
        title = list(text = "Wind speed (Kineo Tower)"),
        xaxis = list(title = "", rangeslider = list(visible = FALSE)),  # removed label
        yaxis = list(title = "Wind speed (m/s)"),
        legend = list(orientation = "h", x = 0, y = -0.25),             # legend under plot
        margin = list(l = 70, r = 20, b = 95, t = 60)                   # extra bottom room
      )
    base_plot_cfg(p)
  })
  
  # ---- wind direction ----
  output$plot_wind_dir <- renderPlotly({
    req("wind_dir" %in% input$graphs_on)
    df <- filter_by_date(datasets()$kineo)
    req(!is.null(df), nrow(df) > 0)
    validate(need("wind_dir_deg" %in% names(df) && any(!is.na(df$wind_dir_deg)),
                  "No wind direction column found for Kineo."))
    
    p <- plot_ly(
      data = df,
      x = ~datetime,
      y = ~wind_dir_deg,
      type = "scatter",
      mode = "markers",
      name = "Kineo Tower",
      marker = list(size = 4)
    ) %>%
      layout(
        title = list(text = "Wind direction (Kineo Tower)"),
        xaxis = list(title = "", rangeslider = list(visible = FALSE)),
        yaxis = list(title = "Wind direction (degrees)"),
        legend = list(orientation = "h", x = 0, y = -0.25),
        margin = list(l = 70, r = 20, b = 95, t = 60)
      )
    
    base_plot_cfg(p)
  })
  
  # ---- soil moisture (single plot w/ depth toggles) ----
  output$plot_soil <- renderPlotly({
    req("soil" %in% input$graphs_on)
    req(input$soil_depths)
    df <- filter_by_date(datasets()$soil)
    req(!is.null(df), nrow(df) > 0)
    plot_soil_multi(df, depths_on = input$soil_depths)
  })
  
  output$status <- renderText({
    idx <- file_index()
    paste0(
      "Data dir: ", normalizePath(DATA_DIR, winslash = "/", mustWork = FALSE), "\n",
      "Indexed files: ", nrow(idx), "\n",
      "Weir areas used (km²): weir3=0.424, weir9=0.684"
    )
  })
}

shinyApp(ui, server)