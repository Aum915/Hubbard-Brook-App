# Hubbard Brook Live Viewer

The Hubbard Brook Live Viewer is an interactive Shiny dashboard for visualizing environmental sensor data from the Hubbard Brook Experimental Forest.  

The app securely downloads protected live data, standardizes it into a consistent format, and displays interactive visualizations for:
- Stream discharge  
- Precipitation  
- Air temperature  
- Snow depth  
- Wind  
- Soil moisture  
- Soil temperature  

---

## Live App

https://vt-eds.shinyapps.io/HBRealTimeViewer/

---

## Important Branch Information

To view the live-data version of the code:

- Go to the **`live_proto`** branch  
- Open app.R`

---

## Features

### Live download of protected sensor data files
```r
download_live_file <- function(station_key, file_name) {
  cfg <- get_station_config()[[station_key]]
  httr::GET(cfg$url, httr::authenticate(cfg$user, cfg$pass))
}
```

### Interactive Plotly graphs
```r
plotly::plot_ly(data = df, x = ~datetime, y = ~value)
```

### Linked zoom across graphs
```r
plotly::plotlyProxyInvoke("relayout", list(xaxis = list(range = xr)))
```

### South/North/Combined view
```r
selectInput("aspect", choices = c("South","North","Both"))
```

### Stream discharge (cfs + mm/day)
```r
discharge_mm_day = (q_m3s / area_m2) * 86400 * 1000
```

### Daily cumulative precipitation
```r
precip_cum_day_mm = cumsum(replace_na(precip_mm, 0))
```

### Wind direction as points
```r
plotly::plot_ly(type="scatter", mode="markers")
```

### Soil moisture by depth
```r
vwc_10 = as.numeric(TDR_10typ_vwc)
```

### Soil temperature by depth
```r
soil_temp_10_c = as.numeric(TDR_10typ_t)
```

### Manual refresh
```r
observeEvent(input$refresh, { refresh_index() })
```

### 90-day rolling window
```r
min_date <- Sys.Date() - 90
```

---

## Data Sources
- Kineo Tower  
- Snowcourse 2  
- Snowcourse 19  
- Weir 3  
- Weir 9  
- Weather Station 1  
- Weather Station 23  

---

## Known Issues
- Slow loading time  
- No north-facing snow data  

---

## Credential Setup
- This application accesses protected Hubbard Brook data using authentication credentials stored in a .Renviron file.

What is .Renviron
- .Renviron is a local configuration file used to securely store environment variables such as usernames, passwords, and data URLs.
This keeps sensitive information out of the main code.

Required Format
- Create a file named .Renviron in the root project folder (same location as app.R) with the following structure:

```r
KINEO_USER=your_username
KINEO_PW=your_password
KINEO_URL=https://...

SNOW19_USER=your_username
SNOW19_PW=your_password
SNOW19_URL=https://...

SOUTHSNOW_USER=your_username
SOUTHSNOW_PW=your_password
SOUTHSNOW_URL=https://...

SOUTHSOIL_USER=your_username
SOUTHSOIL_PW=your_password
SOUTHSOIL_URL=https://...

WEIR3_USER=your_username
WEIR3_PW=your_password
WEIR3_URL=https://...

WEIR9_USER=your_username
WEIR9_PW=your_password
WEIR9_URL=https://...

TEMP1_USER=your_username
TEMP1_PW=your_password
TEMP1_URL=https://...

TEMP23_USER=your_username
TEMP23_PW=your_password
TEMP23_URL=https://...

RAIN1_USER=your_username
RAIN1_PW=your_password
RAIN1_URL=https://...

RAIN23_USER=your_username
RAIN23_PW=your_password
RAIN23_URL=https://...

Important Notes
- There should be no spaces around "=" signs
- Each variable must be on its own linex
- The file must be name ".Renviron"
- The file must be placed in the same folder as "app.R"
- Do not upload .Renviron to public repositories

## Running Locally
```r
install.packages(c("shiny", "tidyverse", "lubridate", "plotly", "httr", "readr", "shinycssloaders", "bslib", "thematic". "later"))
shiny::runApp()
```
