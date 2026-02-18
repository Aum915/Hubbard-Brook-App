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
  if (site_type == "snowcourse" && product == "soil") return("Soil conditions")
  if (site_type == "kineo" && product == "wind") return("Wind")
  str_to_sentence(product)
}

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

read_toa5_table <- function(path, tz = "America/New_York") {
  header_lines <- readLines(path, n = 6, warn = FALSE)
  col_names <- gsub('"', "", header_lines[2])
  col_names <- strsplit(col_names, ",")[[1]]
  skip_n <- 4
  
  if (!any(toupper(col_names) == "TIMESTAMP")) {
    lines <- readLines(path, n = 60, warn = FALSE)
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

make_soil_var_choices <- function(df) {
  choices <- c()
  add_if_exists <- function(col, label) {
    if (col %in% names(df)) choices <<- c(choices, setNames(col, label))
  }
  for (loc in c("typ", "bhs")) {
    loc_label <- if (loc == "typ") "Typical" else "BHS"
    for (d in c(10, 30, 50)) {
      add_if_exists(paste0("TDR_", d, loc, "_vwc"),
                    paste0("Soil moisture (VWC %) — ", d, " cm (", loc_label, ")"))
      add_if_exists(paste0("Terros_", d, loc, "_wp"),
                    paste0("Water potential (kPa) — ", d, " cm (", loc_label, ")"))
      add_if_exists(paste0("Terros_", d, loc, "_t"),
                    paste0("Soil temperature (°C) — ", d, " cm (", loc_label, ")"))
      add_if_exists(paste0("TDR_", d, loc, "_t"),
                    paste0("Soil temperature (°C, TDR) — ", d, " cm (", loc_label, ")"))
    }
  }
  choices
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
    air_candidates <- c("RH_airtemp", "Air_TempC_Avg", "AirTC", "Ta", "air_temp")
    air_col <- air_candidates[air_candidates %in% names(out)]
    air_col <- if (length(air_col) == 0) NA_character_ else air_col[1]
    out <- out %>% mutate(
      air_temp_c = if (!is.na(air_col)) suppressWarnings(as.numeric(.data[[air_col]])) else NA_real_
    )
  }
  
  if (site_type == "snowcourse" && product == "snowpack") {
    out <- out %>% mutate(
      snow_depth_cm = if ("Depthscaled" %in% names(out)) suppressWarnings(as.numeric(Depthscaled)) else NA_real_,
      swe_cm = if ("SWE" %in% names(out)) suppressWarnings(as.numeric(SWE)) else NA_real_
    )
  }
  
  out
}

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
      uiOutput("var_ui"),
      hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(uiOutput("dynamic_plots"))
  )
)

server <- function(input, output, session) {
  
  file_index <- reactiveVal(empty_file_index())
  
  refresh_index <- function() file_index(build_file_index(DATA_DIR))
  observe({ refresh_index() })
  observeEvent(input$refresh, { refresh_index() })
  
  # ---- HARD FILTERED dropdown builders (base R subsetting) ----
  site_choices <- function(site_type) {
    idx <- file_index()
    idx <- idx[idx$site_type == site_type, , drop = FALSE]
    if (nrow(idx) == 0) return(setNames(character(0), character(0)))
    sites <- idx %>%
      distinct(site_key, site_display) %>%
      arrange(site_display)
    setNames(sites$site_key, sites$site_display)  # names=labels, values=keys
  }
  
  product_choices <- function(site_type, site_key) {
    idx <- file_index()
    idx <- idx[idx$site_type == site_type & idx$site_key == site_key, , drop = FALSE]
    if (nrow(idx) == 0) return(setNames(character(0), character(0)))
    prods <- idx %>%
      distinct(product, product_display) %>%
      arrange(product_display)
    setNames(prods$product, prods$product_display) # names=labels, values=product codes
  }
  
  # ---- Site A updates ----
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
  
  # ---- Site B updates ----
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
    idx <- idx[idx$site_type == site_type_arg & idx$site_key == site_key_arg & idx$product == product_arg, , drop = FALSE]
    if (nrow(idx) == 0) return(character(0))
    idx$path[[1]]
  }
  
  load_site <- function(site_type_arg, site_key_arg, product_arg) {
    p <- selected_path(site_type_arg, site_key_arg, product_arg)
    req(length(p) == 1, file.exists(p))
    
    df <- read_toa5_table(p)
    df2 <- standardize_dataset(df, site_type_arg, product_arg)
    
    meta <- file_index()
    meta <- meta[meta$site_type == site_type_arg & meta$site_key == site_key_arg & meta$product == product_arg, , drop = FALSE]
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
  
  output$var_ui <- renderUI({
    df <- data_a()
    st <- isolate(input$site_type_a)
    pr <- isolate(input$product_a)
    
    if (st == "snowcourse" && pr == "soil") {
      soil_choices <- make_soil_var_choices(df)
      if (length(soil_choices) == 0) return(helpText("No recognized soil sensor columns found."))
      return(selectInput("var_select", "Variable", choices = soil_choices, selected = soil_choices[[1]]))
    }
    
    choices <- c()
    if ("discharge_cfs" %in% names(df)) choices <- c(choices, "Discharge (cfs)" = "discharge_cfs")
    if ("stage_m" %in% names(df)) choices <- c(choices, "Stage height (m)" = "stage_m")
    if ("precip_mm" %in% names(df)) choices <- c(choices, "Precipitation (mm per interval)" = "precip_mm")
    if ("air_temp_c" %in% names(df)) choices <- c(choices, "Air temperature (°C)" = "air_temp_c")
    if ("snow_depth_cm" %in% names(df)) choices <- c(choices, "Snow depth (cm)" = "snow_depth_cm")
    if ("swe_cm" %in% names(df)) choices <- c(choices, "Snow water equivalent (cm)" = "swe_cm")
    
    if (length(choices) == 0) helpText("No standardized variables found for this dataset.")
    else selectInput("var_select", "Variable", choices = choices, selected = choices[[1]])
  })
  
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
  
  output$dynamic_plots <- renderUI({
    if (isTRUE(input$compare)) {
      fluidRow(
        column(6, plotlyOutput("plot_a", height = "520px")),
        column(6, plotlyOutput("plot_b", height = "520px"))
      )
    } else {
      fluidRow(column(12, plotlyOutput("plot_a", height = "520px")))
    }
  })
  
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
  
  output$plot_a <- renderPlotly({
    req(input$var_select)
    df <- filtered_a()
    ttl <- paste0(unique(df$site_display), " | ", unique(df$product_display))
    make_plot(df, input$var_select, ttl)
  })
  
  output$plot_b <- renderPlotly({
    req(input$compare, input$var_select)
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
  
  output$status <- renderText({
    idx <- file_index()
    paste0(
      "Data dir: ", normalizePath(DATA_DIR, winslash = "/", mustWork = FALSE), "\n",
      "Indexed files: ", nrow(idx), "\n",
      "Site types found: ", paste(sort(unique(idx$site_type)), collapse = ", ")
    )
  })
}

shinyApp(ui, server)
