# app.R ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
library(httr)
library(shinycssloaders)
library(bslib)
library(thematic)
library(later)

# -----------------------------
# Config
# -----------------------------
SNOW_SPIKE_CUTOFF_CM <- 125
DEFAULT_LOOKBACK_DAYS <- 90

# -----------------------------
# Accessing credentials
# -----------------------------
station_keys <- c(
  "kineo", "snow19", "southsnow", "southsoil",
  "weir3", "weir9", "temp1", "temp23", "rain1", "rain23"
)

get_station_config <- function() {
  cfg <- lapply(station_keys, function(key) {
    key_upper <- toupper(key)
    list(
      user = Sys.getenv(paste0(key_upper, "_USER")),
      pass = Sys.getenv(paste0(key_upper, "_PW")),
      url  = Sys.getenv(paste0(key_upper, "_URL"))
    )
  })
  names(cfg) <- station_keys
  cfg
}

station_config <- get_station_config()

missing_cfg <- sapply(station_config, function(x)
  x$user == "" || x$pass == "" || x$url == "")

if (any(missing_cfg)) {
  stop(paste("Missing credentials for:", paste(names(missing_cfg)[missing_cfg], collapse = ", ")))
}

DATA_DIR <- "."

# -----------------------------
# Weir basin areas (km^2)
# -----------------------------
WEIR_AREA_KM2 <- c(
  "3" = 0.424,
  "9" = 0.684
)

# -----------------------------
# Downloading / caching live files
# -----------------------------
download_live_file <- function(station_key, file_name) {
  cfg <- get_station_config()[[station_key]]
  if (is.null(cfg)) return(NA_character_)
  
  tmp <- tempfile(fileext = ".dat")
  res <- tryCatch(
    httr::GET(cfg$url, httr::authenticate(cfg$user, cfg$pass, type = "basic")),
    error = function(e) {
      message("Request failed for ", station_key, ": ", e$message)
      NULL
    }
  )
  if (is.null(res)) return(NA_character_)
  if (httr::status_code(res) != 200) {
    message("Failed to download ", station_key, " (status ", httr::status_code(res), ")")
    return(NA_character_)
  }
  writeBin(httr::content(res, "raw"), tmp)
  tmp
}

get_live_file <- function(station_key, file_name, live_file_cache = NULL) {
  key <- paste(station_key, file_name, sep = "_")
  if (!is.null(live_file_cache) && !is.null(live_file_cache[[key]])) {
    return(live_file_cache[[key]])
  }
  tmp <- download_live_file(station_key, file_name)
  if (!is.null(live_file_cache)) live_file_cache[[key]] <- tmp
  tmp
}

# -----------------------------
# File index
# -----------------------------
empty_file_index <- function() {
  tibble(
    path = character(), file = character(), file_l = character(),
    station_key = character(), site_type = character(), site_id = character(),
    site_key = character(), site_display = character(),
    product = character(), product_display = character()
  )
}

product_label <- function(site_type, product) {
  if (site_type == "weir"       && product == "stream")         return("Stream discharge")
  if (site_type == "wxsta"      && product == "precip")         return("Precipitation")
  if (site_type == "wxsta"      && product == "air_temp_15min") return("Air temperature (15-min)")
  if (site_type == "snowcourse" && product == "snowpack")       return("Snow depth")
  if (site_type == "snowcourse" && product == "soil")           return("Soil moisture and temperature")
  if (site_type == "kineo"      && product == "wind")           return("Wind")
  str_to_sentence(product)
}

build_file_index <- function() {
  tibble(
    station_key = c("kineo","snow19","southsnow","southsoil","weir3","weir9","temp1","temp23","rain1","rain23"),
    site_type   = c("kineo","snowcourse","snowcourse","snowcourse","weir","weir","wxsta","wxsta","wxsta","wxsta"),
    site_id     = c(NA,"19","3","3","3","9","1","23","1","23"),
    product     = c("wind","soil","snowpack","soil","stream","stream",
                    "air_temp_15min","air_temp_15min","precip","precip"),
    file_name   = c(
      "Kineo_Tower_Kineo.dat",
      "Snowcourse_19_SS19_soildat.dat",
      "Water_table_WS3upper_WS_3Up_snowdat_15min.dat",
      "Water_table_WS3upper_WS_3up_soildat.dat",
      "weir3_weir_3.dat",
      "weir9_weir_9.dat",
      "wxsta1_SF_Wx1_Temp_15min.dat",
      "wxsta23_Wx_23_Temp_15_min.dat",
      "wxsta1_precip_Wx_1_rain.dat",
      "wxsta23_Wx_23_rain.dat"
    )
  ) %>%
    rowwise() %>%
    mutate(
      path = NA_character_,
      site_key = case_when(
        station_key == "southsnow" ~ "southsnow",
        station_key == "southsoil" ~ "southsoil",
        site_type == "kineo" ~ "kineo_tower",
        TRUE ~ paste0(site_type, site_id)
      ),
      site_display = case_when(
        site_type == "kineo" & station_key == "kineo" ~ "Kineo Tower",
        station_key == "southsnow" ~ "South-facing snow sensor (WS3 upper)",
        station_key == "southsoil" ~ "South-facing soil sensor (WS3 upper)",
        site_type == "snowcourse" ~ paste0("Snowcourse ", site_id),
        site_type == "weir" ~ paste0("Weir ", site_id),
        site_type == "wxsta" ~ paste0("Weather station ", site_id)
      ),
      product_display = product_label(site_type, product)
    ) %>%
    ungroup() %>%
    select(path, file = file_name, file_l = file_name, station_key,
           site_type, site_id, site_key, site_display, product, product_display)
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
    lines <- readLines(path, n = 120, warn = FALSE)
    header_idx <- which(grepl("^\"?TIMESTAMP\"?,", lines, ignore.case = TRUE))[1]
    if (is.na(header_idx)) stop("Could not find TIMESTAMP header row in: ", path)
    col_names <- gsub('"', "", lines[header_idx])
    col_names <- strsplit(col_names, ",")[[1]]
    skip_n <- header_idx + 2
  }
  
  df <- read.table(path, sep = ",", header = FALSE, skip = skip_n,
                   col.names = col_names, fill = TRUE,
                   stringsAsFactors = FALSE, quote = "\"") %>% as_tibble()
  
  if (!("TIMESTAMP" %in% names(df))) stop("TIMESTAMP missing after read: ", path)
  
  df %>%
    mutate(datetime = ymd_hms(TIMESTAMP, tz = tz, quiet = TRUE)) %>%
    filter(!is.na(datetime))
}

# -----------------------------
# Column picking helper
# -----------------------------
pick_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# -----------------------------
# Standardization
# -----------------------------
standardize_dataset <- function(df, site_type, product, site_id = NA_character_) {
  out <- df %>% select(datetime, everything())
  
  if (site_type == "weir" && product == "stream") {
    q_col <- pick_first_existing(out, c("pressure1_Q","Q","Discharge","discharge_cfs"))
    stage_col <- pick_first_existing(out, c("float_stage","pressure1_stage","pressure1_rawdepth","stage","Stage"))
    out <- out %>%
      mutate(
        discharge_cfs = if (!is.na(q_col)) suppressWarnings(as.numeric(.data[[q_col]])) else NA_real_,
        stage_ft      = if (!is.na(stage_col)) suppressWarnings(as.numeric(.data[[stage_col]])) else NA_real_,
        stage_m       = stage_ft * 0.3048
      )
    area_km2 <- WEIR_AREA_KM2[[as.character(site_id)]]
    if (is.null(area_km2) || is.na(area_km2)) area_km2 <- NA_real_
    out <- out %>%
      mutate(
        discharge_mm_day = if (is.na(area_km2)) NA_real_ else {
          area_m2 <- area_km2 * 1e6
          q_m3s   <- discharge_cfs * 0.028316846592
          (q_m3s / area_m2) * 86400 * 1000
        }
      )
  }
  
  if (site_type == "wxsta" && product == "precip") {
    p_col <- pick_first_existing(out, c("ReportPCP","Precip","precip","Rain","rain"))
    out <- out %>%
      mutate(precip_mm = if (!is.na(p_col)) suppressWarnings(as.numeric(.data[[p_col]])) else NA_real_)
  }
  
  if (site_type == "wxsta" && product %in% c("air_temp_15min","air_temp")) {
    t_col <- pick_first_existing(out, c("Air_TempC_Avg","RH_airtemp","ActTemp","AirTC","Ta"))
    out <- out %>%
      mutate(air_temp_c = if (!is.na(t_col)) suppressWarnings(as.numeric(.data[[t_col]])) else NA_real_)
  }
  
  if (site_type == "snowcourse" && product == "snowpack") {
    d_col <- pick_first_existing(out, c("Depthscaled","SnowDepth","snow_depth"))
    s_col <- pick_first_existing(out, c("SWE","swe"))
    out <- out %>%
      mutate(
        snow_depth_cm = if (!is.na(d_col)) suppressWarnings(as.numeric(.data[[d_col]])) else NA_real_,
        swe_cm        = if (!is.na(s_col)) suppressWarnings(as.numeric(.data[[s_col]])) else NA_real_
      ) %>%
      mutate(
        snow_depth_cm = ifelse(snow_depth_cm > SNOW_SPIKE_CUTOFF_CM, NA_real_, snow_depth_cm)
      )
  }
  
  if (site_type == "snowcourse" && product == "soil") {
    t10_col <- pick_first_existing(out, c("TDR_10typ_t", "Terros_10typ_t"))
    t30_col <- pick_first_existing(out, c("TDR_30typ_t", "Terros_30typ_t"))
    t50_col <- pick_first_existing(out, c("TDR_50typ_t", "Terros_50typ_t"))
    
    out <- out %>%
      mutate(
        vwc_10 = if ("TDR_10typ_vwc" %in% names(out)) suppressWarnings(as.numeric(TDR_10typ_vwc)) else NA_real_,
        vwc_30 = if ("TDR_30typ_vwc" %in% names(out)) suppressWarnings(as.numeric(TDR_30typ_vwc)) else NA_real_,
        vwc_50 = if ("TDR_50typ_vwc" %in% names(out)) suppressWarnings(as.numeric(TDR_50typ_vwc)) else NA_real_,
        
        soil_temp_10_c = if (!is.na(t10_col)) suppressWarnings(as.numeric(.data[[t10_col]])) else NA_real_,
        soil_temp_30_c = if (!is.na(t30_col)) suppressWarnings(as.numeric(.data[[t30_col]])) else NA_real_,
        soil_temp_50_c = if (!is.na(t50_col)) suppressWarnings(as.numeric(.data[[t50_col]])) else NA_real_
      )
  }
  
  if (site_type == "kineo" && product == "wind") {
    avg_col <- pick_first_existing(out, c("WS_ms_Avg","WS_Avg","WindSpd_Avg","WindSpd","WS"))
    max_col <- pick_first_existing(out, c("WS_ms_Max","WS_Max","WindSpd_Max"))
    dir_col <- pick_first_existing(out, c("WindDir","Wind_Dir","WD","WD_deg",
                                          "WindDirection","Wind_Direction","WD_Avg","WindDir_Avg"))
    out <- out %>%
      mutate(
        wind_speed_avg = if (!is.na(avg_col)) suppressWarnings(as.numeric(.data[[avg_col]])) else NA_real_,
        wind_speed_max = if (!is.na(max_col)) suppressWarnings(as.numeric(.data[[max_col]])) else NA_real_,
        wind_dir_deg   = if (!is.na(dir_col)) suppressWarnings(as.numeric(.data[[dir_col]])) else NA_real_
      )
  }
  
  out
}

# -----------------------------
# Keep only plot-relevant columns
# -----------------------------
keep_plot_cols <- function(df, site_type, product) {
  if (is.null(df)) return(df)
  cols <- "datetime"
  if (site_type == "weir" && product == "stream")
    cols <- c(cols, "discharge_cfs", "discharge_mm_day", "stage_m")
  if (site_type == "wxsta" && product == "precip")
    cols <- c(cols, "precip_mm")
  if (site_type == "wxsta" && product %in% c("air_temp_15min","air_temp"))
    cols <- c(cols, "air_temp_c")
  if (site_type == "snowcourse" && product == "snowpack")
    cols <- c(cols, "snow_depth_cm", "swe_cm")
  if (site_type == "snowcourse" && product == "soil")
    cols <- c(cols, "vwc_10", "vwc_30", "vwc_50",
              "soil_temp_10_c", "soil_temp_30_c", "soil_temp_50_c")
  if (site_type == "kineo" && product == "wind")
    cols <- c(cols, "wind_speed_avg", "wind_speed_max", "wind_dir_deg")
  df %>% select(any_of(cols))
}

# -----------------------------
# Precip helpers
# -----------------------------
make_daily_cum_precip <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>%
    mutate(day = as.Date(datetime)) %>%
    arrange(series_label, day, datetime) %>%
    group_by(series_label, day) %>%
    mutate(
      precip_mm_clean   = replace_na(precip_mm, 0),
      precip_cum_day_mm = cumsum(precip_mm_clean)
    ) %>%
    ungroup()
}

make_event_cum_precip <- function(df, dry_gap_hours = 4) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  df %>%
    arrange(series_label, datetime) %>%
    group_by(series_label) %>%
    group_modify(~{
      x <- .x %>%
        arrange(datetime) %>%
        mutate(
          precip_mm_clean = replace_na(precip_mm, 0),
          is_wet = precip_mm_clean > 0
        )
      
      n <- nrow(x)
      event_id <- rep(NA_integer_, n)
      cum_vals <- rep(0, n)
      
      current_event <- 0L
      running_total <- 0
      in_event <- FALSE
      last_wet_index <- NA_integer_
      
      i <- 1
      while (i <= n) {
        if (x$is_wet[i]) {
          if (!in_event) {
            current_event <- current_event + 1L
            running_total <- 0
            in_event <- TRUE
          }
          
          running_total <- running_total + x$precip_mm_clean[i]
          event_id[i] <- current_event
          cum_vals[i] <- running_total
          last_wet_index <- i
          i <- i + 1
          
        } else {
          if (!in_event) {
            cum_vals[i] <- 0
            i <- i + 1
          } else {
            dry_start <- i
            j <- i
            
            while (j <= n && !x$is_wet[j]) {
              j <- j + 1
            }
            
            dry_end <- j - 1
            
            if (dry_start <= n && !is.na(last_wet_index)) {
              dry_gap <- as.numeric(difftime(x$datetime[dry_end], x$datetime[last_wet_index], units = "hours"))
            } else {
              dry_gap <- 0
            }
            
            if (dry_gap >= dry_gap_hours) {
              cum_vals[dry_start:dry_end] <- 0
              in_event <- FALSE
              running_total <- 0
            } else {
              event_id[dry_start:dry_end] <- current_event
              cum_vals[dry_start:dry_end] <- running_total
            }
            
            i <- j
          }
        }
      }
      
      x %>%
        mutate(
          event_id = event_id,
          precip_event_cum_mm = cum_vals
        )
    }) %>%
    ungroup()
}

# -----------------------------
# Plot helpers
# -----------------------------
base_plot_cfg <- function(p) {
  p %>% plotly::config(displaylogo = FALSE,
                       modeBarButtonsToRemove = c("sendDataToCloud","toImage"))
}

plot_lines_multi <- function(df, y, title, ylab, source_id) {
  req(nrow(df) > 0, y %in% names(df))
  p <- plotly::plot_ly(source = source_id) %>%
    plotly::layout(
      title  = list(text = title),
      xaxis  = list(title = "", rangeslider = list(visible = FALSE)),
      yaxis  = list(title = ylab),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 70, r = 20, b = 95, t = 60)
    )
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    p <- plotly::add_lines(
      p, data = sub, x = ~datetime, y = sub[[y]],
      name = lab, showlegend = TRUE
    )
  }
  base_plot_cfg(p)
}

plot_bars_multi <- function(df, y, title, ylab, source_id) {
  req(nrow(df) > 0, y %in% names(df))
  p <- plotly::plot_ly(source = source_id) %>%
    plotly::layout(
      title   = list(text = title),
      xaxis   = list(title = "", rangeslider = list(visible = FALSE)),
      yaxis   = list(title = ylab),
      barmode = "overlay",
      showlegend = TRUE,
      legend  = list(orientation = "h", x = 0, y = -0.25),
      margin  = list(l = 70, r = 20, b = 95, t = 60)
    )
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    p <- plotly::add_bars(
      p, data = sub, x = ~datetime, y = sub[[y]],
      name = lab, opacity = 0.6, showlegend = TRUE
    )
  }
  base_plot_cfg(p)
}

plot_soil_multi <- function(df, depths_on = c("10","30","50"),
                            title = "Soil moisture (Typical)", source_id) {
  req(nrow(df) > 0)
  p <- plotly::plot_ly(source = source_id) %>%
    plotly::layout(
      title  = list(text = title),
      xaxis  = list(title = "", rangeslider = list(visible = FALSE)),
      yaxis  = list(title = "Volumetric Water Content (VWC %)"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 70, r = 20, b = 95, t = 60)
    )
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    if ("10" %in% depths_on && "vwc_10" %in% names(sub))
      p <- plotly::add_lines(p, data = sub, x = ~datetime, y = ~vwc_10,
                             name = paste0(lab, " — 10 cm"), showlegend = TRUE)
    if ("30" %in% depths_on && "vwc_30" %in% names(sub))
      p <- plotly::add_lines(p, data = sub, x = ~datetime, y = ~vwc_30,
                             name = paste0(lab, " — 30 cm"), showlegend = TRUE)
    if ("50" %in% depths_on && "vwc_50" %in% names(sub))
      p <- plotly::add_lines(p, data = sub, x = ~datetime, y = ~vwc_50,
                             name = paste0(lab, " — 50 cm"), showlegend = TRUE)
  }
  base_plot_cfg(p)
}

plot_soil_temp_multi <- function(df, depths_on = c("10","30","50"),
                                 title = "Soil temperature", source_id) {
  req(nrow(df) > 0)
  p <- plotly::plot_ly(source = source_id) %>%
    plotly::layout(
      title  = list(text = title),
      xaxis  = list(title = "", rangeslider = list(visible = FALSE)),
      yaxis  = list(title = "Soil temperature (°C)"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = -0.25),
      margin = list(l = 70, r = 20, b = 95, t = 60)
    )
  
  for (lab in unique(df$series_label)) {
    sub <- df %>% filter(series_label == lab)
    if ("10" %in% depths_on && "soil_temp_10_c" %in% names(sub))
      p <- plotly::add_lines(p, data = sub, x = ~datetime, y = ~soil_temp_10_c,
                             name = paste0(lab, " — 10 cm"), showlegend = TRUE)
    if ("30" %in% depths_on && "soil_temp_30_c" %in% names(sub))
      p <- plotly::add_lines(p, data = sub, x = ~datetime, y = ~soil_temp_30_c,
                             name = paste0(lab, " — 30 cm"), showlegend = TRUE)
    if ("50" %in% depths_on && "soil_temp_50_c" %in% names(sub))
      p <- plotly::add_lines(p, data = sub, x = ~datetime, y = ~soil_temp_50_c,
                             name = paste0(lab, " — 50 cm"), showlegend = TRUE)
  }
  
  base_plot_cfg(p)
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$script(HTML("
  Shiny.addCustomMessageHandler('hideOverlay', function(msg) {
    document.getElementById('startup_overlay').style.display = 'none';
  });
  Shiny.addCustomMessageHandler('showOverlay', function(msg) {
    document.getElementById('startup_overlay').style.display = 'flex';
  });

  $(document).on('shiny:value', function(event) {
    if (event.name && event.name.startsWith('plot_')) {
      setTimeout(function() {
        Shiny.setInputValue('plot_rendered', event.name, {priority: 'event'});
      }, 100);
    }
  });
")),
  
  tags$div(
    id = "startup_overlay",
    style = "position:fixed; top:0; left:0; width:100%; height:100%;
           background:rgba(255,255,255,0.95); z-index:9999;
           display:flex; flex-direction:column;
           align-items:center; justify-content:center;",
    
    tags$style(HTML("
    .tree-container {
      position: relative;
      width: 120px;
      height: 160px;
      margin-bottom: 20px;
    }
    .tree-trunk {
      position: absolute;
      bottom: 0;
      left: 50%;
      transform: translateX(-50%);
      width: 16px;
      height: 45px;
      background: #6b3d0f;
      border-radius: 3px;
    }
    .tree-top {
      position: absolute;
      bottom: 38px;
      left: 50%;
      transform: translateX(-50%);
      width: 0;
      height: 0;
      border-left: 50px solid transparent;
      border-right: 50px solid transparent;
      border-bottom: 80px solid #2d6a2d;
    }
    .tree-mid {
      position: absolute;
      bottom: 68px;
      left: 50%;
      transform: translateX(-50%);
      width: 0;
      height: 0;
      border-left: 40px solid transparent;
      border-right: 40px solid transparent;
      border-bottom: 65px solid #357a35;
    }
    .tree-tip {
      position: absolute;
      bottom: 105px;
      left: 50%;
      transform: translateX(-50%);
      width: 0;
      height: 0;
      border-left: 28px solid transparent;
      border-right: 28px solid transparent;
      border-bottom: 50px solid #3d8f3d;
    }
    .loading-text {
      display: flex;
      gap: 1px;
      font-size: 22px;
      color: #357a35;
      letter-spacing: 2px;
      margin-bottom: 8px;
    }
    .loading-dot {
      display: inline-block;
      animation: wave 1.4s ease-in-out infinite;
    }
    .loading-dot:nth-child(1) { animation-delay: 0.0s; }
    .loading-dot:nth-child(2) { animation-delay: 0.1s; }
    .loading-dot:nth-child(3) { animation-delay: 0.2s; }
    .loading-dot:nth-child(4) { animation-delay: 0.3s; }
    .loading-dot:nth-child(5) { animation-delay: 0.4s; }
    .loading-dot:nth-child(6) { animation-delay: 0.5s; }
    .loading-dot:nth-child(7) { animation-delay: 0.6s; }
    .loading-dot:nth-child(8) { animation-delay: 0.7s; }
    .loading-dot:nth-child(9) { animation-delay: 0.8s; }
    .loading-dot:nth-child(10) { animation-delay: 0.9s; }
    @keyframes wave {
      0%, 60%, 100% { transform: translateY(0); }
      30% { transform: translateY(-10px); }
    }
    .loading-subtext {
      font-size: 11px;
      color: #bbb;
      font-weight: 300;
      margin-top: 4px;
      letter-spacing: 0.5px;
    }")),
    
    tags$div(
      class = "tree-container",
      tags$div(class = "tree-tip"),
      tags$div(class = "tree-mid"),
      tags$div(class = "tree-top"),
      tags$div(class = "tree-trunk")
    ),
    
    tags$div(class = "loading-text",
             tags$span(class = "loading-dot", "L"),
             tags$span(class = "loading-dot", "o"),
             tags$span(class = "loading-dot", "a"),
             tags$span(class = "loading-dot", "d"),
             tags$span(class = "loading-dot", "i"),
             tags$span(class = "loading-dot", "n"),
             tags$span(class = "loading-dot", "g"),
             tags$span(class = "loading-dot", "."),
             tags$span(class = "loading-dot", "."),
             tags$span(class = "loading-dot", ".")),
    tags$div(class = "loading-subtext",
             "Downloading live data now!")
  ),
  
  theme = bs_theme(preset = "flatly"),
  titlePanel("Hubbard Brook Live Viewer"),
  
  tabsetPanel(
    tabPanel(
      "Viewer",
      sidebarLayout(
        sidebarPanel(
          actionButton("refresh", "Refresh data"),
          tags$p("Most recent data is downloaded at app start. If you want to redownload the data, press refresh.",
                 style = "font-size: 11px; color: #888; margin-top: 5px"),
          hr(),
          selectInput(
            "aspect", "Select Location of Real Time Sites",
            choices  = c("South-facing Sites" = "South", "North-facing Sites" = "North", "Show both" = "Both"),
            selected = "South"
          ),
          dateRangeInput("date_range", "Date range"),
          hr(),
          checkboxGroupInput(
            "graphs_on", "Graph Variables",
            choices = c(
              "Stream discharge (cfs)"         = "discharge",
              "Stream discharge (mm/day)"      = "discharge_mmday",
              "Precipitation"                  = "precip",
              "Daily cumulative precipitation" = "precip_cum_day",
              "Event cumulative precipitation" = "precip_cum_event",
              "Air temperature"                = "airtemp",
              "Snow depth"                     = "snowdepth",
              "Wind speed (avg/max)"           = "wind_speed",
              "Wind direction"                 = "wind_dir",
              "Soil moisture (10/30/50 cm)"    = "soil",
              "Soil temperature (10/30/50 cm)" = "soil_temp"
            ),
            selected = c("discharge_mmday", "precip", "airtemp")
          ),
          conditionalPanel(
            condition = "input.graphs_on.indexOf('soil') >= 0 || input.graphs_on.indexOf('soil_temp') >= 0",
            checkboxGroupInput(
              "soil_depths", "Soil depths",
              choices  = c("10 cm" = "10", "30 cm" = "30", "50 cm" = "50"),
              selected = c("10","30","50")
            )
          ),
          hr(),
          verbatimTextOutput("status")
        ),
        mainPanel(uiOutput("plots_ui"))
      )
    ),
    
    tabPanel(
      "Metadata",
      fluidRow(
        column(
          12,
          h3("Metadata"),
          p("This tab is intended for data source descriptions, caveats on interpretation, and references to published datasets."),
          p("All displayed data are live data feeds. QC'd and fully published datasets are available through December 31, 2025."),
          tags$ul(
            tags$li("South-facing precipitation: Weather station 1"),
            tags$li("North-facing precipitation: Weather station 23"),
            tags$li("South-facing streamflow: Weir 3"),
            tags$li("North-facing streamflow: Weir 9"),
            tags$li("South-facing air temperature: Weather station 1"),
            tags$li("North-facing air temperature: Weather station 23"),
            tags$li("South-facing snow: WS3 upper snow sensor"),
            tags$li("South-facing soil: WS3 upper soil sensor"),
            tags$li("North-facing soil: Snowcourse 19"),
            tags$li("Wind: Kineo Tower")
          ),
          h4("Notes"),
          p("This tab can also be used to describe data sources, station pairings for North/South selections, interpretation caveats, and references to published datasets."),
          p("You can replace this placeholder metadata text with final wording later.")
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  live_file_cache <- reactiveValues()
  file_index <- reactiveVal(empty_file_index())
  
  refresh_index <- function() {
    keys <- names(reactiveValuesToList(live_file_cache))
    for (k in keys) live_file_cache[[k]] <- NULL
    
    idx <- build_file_index()
    idx$path <- vapply(seq_len(nrow(idx)), function(i) {
      get_live_file(idx$station_key[i], idx$file[i], live_file_cache)
    }, character(1))
    
    file_index(idx)
  }
  
  selected_path <- function(site_type, site_key, product) {
    idx <- file_index()
    sub <- idx %>% filter(site_type == !!site_type,
                          site_key  == !!site_key,
                          product   == !!product)
    if (nrow(sub) > 0) return(sub$path[[1]])
    character(0)
  }
  
  site_id_from_key <- function(site_key) {
    if (is.na(site_key) || !nzchar(site_key) || site_key %in% c("kineo_tower", "southsnow", "southsoil"))
      return(NA_character_)
    str_match(site_key, "^(weir|wxsta|snowcourse)(\\d+)$")[, 3]
  }
  
  load_one <- function(site_type, site_key, product, label) {
    p <- selected_path(site_type, site_key, product)
    if (length(p) != 1 || is.na(p) || !file.exists(p)) return(NULL)
    df <- read_toa5_table(p)
    sid <- site_id_from_key(site_key)
    df <- standardize_dataset(df, site_type, product, site_id = sid)
    df <- keep_plot_cols(df, site_type, product)
    df$series_label <- label
    df
  }
  
  all_data <- reactive({
    idx <- file_index()
    req(nrow(idx) > 0, any(!is.na(idx$path)))
    list(
      weir3      = load_one("weir",       "weir3",        "stream",        "Weir 3"),
      weir9      = load_one("weir",       "weir9",        "stream",        "Weir 9"),
      precip1    = load_one("wxsta",      "wxsta1",       "precip",        "Weather station 1"),
      precip23   = load_one("wxsta",      "wxsta23",      "precip",        "Weather station 23"),
      temp1      = load_one("wxsta",      "wxsta1",       "air_temp_15min","Weather station 1"),
      temp23     = load_one("wxsta",      "wxsta23",      "air_temp_15min","Weather station 23"),
      southsnow  = load_one("snowcourse", "southsnow",    "snowpack",      "South snow sensor"),
      southsoil  = load_one("snowcourse", "southsoil",    "soil",          "South soil sensor"),
      northsoil  = load_one("snowcourse", "snowcourse19", "soil",          "Snowcourse 19"),
      kineo      = load_one("kineo",      "kineo_tower",  "wind",          "Kineo Tower")
    )
  })
  
  observeEvent(TRUE, {
    refresh_index()
    later::later(function() {
      session$sendCustomMessage("hideOverlay", list())
    }, delay = 2)
  }, once = TRUE)
  
  observeEvent(input$refresh, {
    session$sendCustomMessage("showOverlay", list())
    refresh_index()
    later::later(function() {
      session$sendCustomMessage("hideOverlay", list())
    }, delay = 0.5)
  })
  
  observe({
    max_date <- Sys.Date()
    min_date <- max_date - DEFAULT_LOOKBACK_DAYS
    
    updateDateRangeInput(
      session, "date_range",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date
    )
  })
  
  datasets <- reactive({
    d <- all_data()
    
    if (input$aspect == "Both") {
      list(
        weir     = bind_rows(d$weir3, d$weir9),
        precip   = bind_rows(d$precip1, d$precip23),
        airtemp  = bind_rows(d$temp1, d$temp23),
        snowpack = d$southsnow,
        soil     = bind_rows(d$southsoil, d$northsoil),
        kineo    = d$kineo
      )
    } else if (input$aspect == "South") {
      list(
        weir     = d$weir3,
        precip   = d$precip1,
        airtemp  = d$temp1,
        snowpack = d$southsnow,
        soil     = d$southsoil,
        kineo    = d$kineo
      )
    } else {
      list(
        weir     = d$weir9,
        precip   = d$precip23,
        airtemp  = d$temp23,
        snowpack = NULL,
        soil     = d$northsoil,
        kineo    = d$kineo
      )
    }
  })
  
  filter_by_date <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    req(input$date_range)
    
    max_allowed <- Sys.Date()
    min_allowed <- max_allowed - DEFAULT_LOOKBACK_DAYS
    
    start <- max(as.Date(input$date_range[1]), min_allowed)
    end   <- min(as.Date(input$date_range[2]), max_allowed)
    
    df %>%
      filter(
        datetime >= as.POSIXct(start),
        datetime < as.POSIXct(end + 1)
      )
  }
  
  output$plots_ui <- renderUI({
    req(input$graphs_on)
    tagList(lapply(input$graphs_on, function(id) {
      plotlyOutput(paste0("plot_", id), height = "320px")
    }))
  })
  
  observers <- reactiveValues()
  is_syncing <- reactiveVal(FALSE)
  
  observeEvent(input$graphs_on, {
    req(input$graphs_on)
    for (id in input$graphs_on) {
      if (!is.null(observers[[id]])) next
      local({
        my_id <- id
        source_name <- paste0("plot_", my_id)
        observers[[my_id]] <- observeEvent(
          plotly::event_data("plotly_relayout", source = source_name),
          {
            if (is_syncing()) return()
            ev <- plotly::event_data("plotly_relayout", source = source_name)
            if (is.null(ev)) return()
            if (!is.null(ev[["xaxis.range[0]"]]) && !is.null(ev[["xaxis.range[1]"]])) {
              xr <- c(ev[["xaxis.range[0]"]], ev[["xaxis.range[1]"]])
              is_syncing(TRUE)
              on.exit(is_syncing(FALSE), add = TRUE)
              for (other in setdiff(input$graphs_on, my_id)) {
                plotly::plotlyProxy(paste0("plot_", other), session) %>%
                  plotly::plotlyProxyInvoke("relayout", list(xaxis = list(range = xr)))
              }
            }
            if (!is.null(ev[["xaxis.autorange"]]) && isTRUE(ev[["xaxis.autorange"]])) {
              is_syncing(TRUE)
              on.exit(is_syncing(FALSE), add = TRUE)
              for (other in setdiff(input$graphs_on, my_id)) {
                plotly::plotlyProxy(paste0("plot_", other), session) %>%
                  plotly::plotlyProxyInvoke("relayout", list(xaxis = list(autorange = TRUE)))
              }
            }
          },
          ignoreInit = TRUE
        )
      })
    }
  }, ignoreInit = FALSE)
  
  output$plot_discharge <- renderPlotly({
    req("discharge" %in% input$graphs_on)
    df <- filter_by_date(datasets()$weir)
    req(!is.null(df), nrow(df) > 0)
    validate(need("discharge_cfs" %in% names(df) && any(!is.na(df$discharge_cfs)),
                  "No discharge column found in this weir file."))
    plot_lines_multi(df, "discharge_cfs", "Stream discharge", "Discharge (cfs)", "plot_discharge")
  })
  
  output$plot_discharge_mmday <- renderPlotly({
    req("discharge_mmday" %in% input$graphs_on)
    df <- filter_by_date(datasets()$weir)
    req(!is.null(df), nrow(df) > 0)
    validate(need("discharge_mm_day" %in% names(df) && any(!is.na(df$discharge_mm_day)),
                  "No discharge mm/day available."))
    plot_lines_multi(df, "discharge_mm_day", "Stream discharge (mm/day)",
                     "Discharge equivalent (mm/day)", "plot_discharge_mmday")
  })
  
  output$plot_precip <- renderPlotly({
    req("precip" %in% input$graphs_on)
    df <- filter_by_date(datasets()$precip)
    req(!is.null(df), nrow(df) > 0)
    validate(need("precip_mm" %in% names(df) && any(!is.na(df$precip_mm)),
                  "No precipitation column found in this weather file."))
    plot_bars_multi(df, "precip_mm", "Precipitation", "Precipitation (mm per interval)", "plot_precip")
  })
  
  output$plot_precip_cum_day <- renderPlotly({
    req("precip_cum_day" %in% input$graphs_on)
    df <- filter_by_date(datasets()$precip)
    req(!is.null(df), nrow(df) > 0)
    validate(need("precip_mm" %in% names(df) && any(!is.na(df$precip_mm)),
                  "No precipitation column found in this weather file."))
    df2 <- make_daily_cum_precip(df)
    plot_lines_multi(df2, "precip_cum_day_mm", "Daily cumulative precipitation",
                     "Cumulative precipitation (mm)", "plot_precip_cum_day")
  })
  
  output$plot_precip_cum_event <- renderPlotly({
    req("precip_cum_event" %in% input$graphs_on)
    df <- filter_by_date(datasets()$precip)
    req(!is.null(df), nrow(df) > 0)
    validate(need("precip_mm" %in% names(df) && any(!is.na(df$precip_mm)),
                  "No precipitation column found in this weather file."))
    df2 <- make_event_cum_precip(df, dry_gap_hours = 4)
    plot_lines_multi(df2, "precip_event_cum_mm", "Event cumulative precipitation",
                     "Event cumulative precipitation (mm)", "plot_precip_cum_event")
  })
  
  output$plot_airtemp <- renderPlotly({
    req("airtemp" %in% input$graphs_on)
    df <- filter_by_date(datasets()$airtemp)
    req(!is.null(df), nrow(df) > 0)
    validate(need("air_temp_c" %in% names(df) && any(!is.na(df$air_temp_c)),
                  "No air temperature column found in this weather file."))
    plot_lines_multi(df, "air_temp_c", "Air temperature", "Air temperature (°C)", "plot_airtemp")
  })
  
  output$plot_snowdepth <- renderPlotly({
    req("snowdepth" %in% input$graphs_on)
    df <- filter_by_date(datasets()$snowpack)
    req(!is.null(df), nrow(df) > 0)
    validate(need("snow_depth_cm" %in% names(df) && any(!is.na(df$snow_depth_cm)),
                  "No snow depth column found in this snow file."))
    plot_lines_multi(df, "snow_depth_cm", "Snow depth", "Snow depth (cm)", "plot_snowdepth")
  })
  
  output$plot_wind_speed <- renderPlotly({
    req("wind_speed" %in% input$graphs_on)
    df <- filter_by_date(datasets()$kineo)
    req(!is.null(df), nrow(df) > 0)
    validate(need("wind_speed_avg" %in% names(df) && any(!is.na(df$wind_speed_avg)),
                  "No wind speed column found for Kineo."))
    p <- plotly::plot_ly(source = "plot_wind_speed") %>%
      plotly::add_lines(data = df, x = ~datetime, y = ~wind_speed_avg, name = "Avg wind speed", showlegend = TRUE) %>%
      plotly::add_lines(data = df, x = ~datetime, y = ~wind_speed_max, name = "Max wind speed", showlegend = TRUE) %>%
      plotly::layout(
        title  = list(text = "Wind speed (Kineo Tower)"),
        xaxis  = list(title = "", rangeslider = list(visible = FALSE)),
        yaxis  = list(title = "Wind speed (m/s)"),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.25),
        margin = list(l = 70, r = 20, b = 95, t = 60)
      )
    base_plot_cfg(p)
  })
  
  output$plot_wind_dir <- renderPlotly({
    req("wind_dir" %in% input$graphs_on)
    df <- filter_by_date(datasets()$kineo)
    req(!is.null(df), nrow(df) > 0)
    validate(need("wind_dir_deg" %in% names(df) && any(!is.na(df$wind_dir_deg)),
                  "No wind direction column found for Kineo."))
    p <- plotly::plot_ly(
      source = "plot_wind_dir", data = df,
      x = ~datetime, y = ~wind_dir_deg,
      type = "scatter", mode = "markers",
      name = "Kineo Tower", marker = list(size = 4),
      showlegend = TRUE
    ) %>%
      plotly::layout(
        title  = list(text = "Wind direction (Kineo Tower)"),
        xaxis  = list(title = "", rangeslider = list(visible = FALSE)),
        yaxis  = list(title = "Wind direction (degrees)"),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.25),
        margin = list(l = 70, r = 20, b = 95, t = 60)
      )
    base_plot_cfg(p)
  })
  
  output$plot_soil <- renderPlotly({
    req("soil" %in% input$graphs_on, input$soil_depths)
    df <- filter_by_date(datasets()$soil)
    req(!is.null(df), nrow(df) > 0)
    plot_soil_multi(df, depths_on = input$soil_depths, source_id = "plot_soil")
  })
  
  output$plot_soil_temp <- renderPlotly({
    req("soil_temp" %in% input$graphs_on, input$soil_depths)
    df <- filter_by_date(datasets()$soil)
    req(!is.null(df), nrow(df) > 0)
    plot_soil_temp_multi(df, depths_on = input$soil_depths, source_id = "plot_soil_temp")
  })
  
  output$status <- renderText({
    idx <- file_index()
    n_ok <- sum(!is.na(idx$path))
    paste0(
      "Indexed files: ", nrow(idx), " | Downloaded: ", n_ok, "\n",
      "Snow spike cutoff: ", SNOW_SPIKE_CUTOFF_CM, " cm\n",
      "Weir areas (km²): weir3=0.424, weir9=0.684"
    )
  })
}

shinyApp(ui, server)