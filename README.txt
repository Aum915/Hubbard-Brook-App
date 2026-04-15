# Hubbard Brook Live Viewer

The Hubbard Brook Live Viewer is an interactive Shiny dashboard for viewing environmental sensor data from the Hubbard Brook Experimental Forest. The app downloads protected live data files, standardizes them into a consistent format, and displays interactive plots for stream discharge, precipitation, air temperature, snow depth, wind, and soil moisture.

## Shiny App Website

Live app: https://vt-eds.shinyapps.io/HBRealTimeViewer/

## Important Branch Information

To view the live-data version of the code, go to the **`live_proto`** branch and open **`app.R`**.

That branch contains the working prototype for downloading and displaying live sensor data.

## Features

- Live download of protected sensor data files
- Interactive Plotly graphs
- Linked zoom across graphs
- South-facing, North-facing, or combined view
- Stream discharge displayed in both cfs and mm/day
- Daily cumulative precipitation graph
- Wind direction displayed as points
- Soil moisture by depth
- Soil Temperature by depth
- Manual refresh button to redownload current files
- Filtering to the most recent 2 months of data for improved speed

## Data Sources

The app works with live data from the following Hubbard Brook sensor locations:

- Kineo Tower
- Snowcourse 2
- Snowcourse 19
- Weir 3
- Weir 9
- Weather Station 1
- Weather Station 23
- WS3 upper snow sensor
- WS3 upper soil sensor

## Known Issues
- Loading time for the app can be long because of the extent of the data. 
- There is no data for snow depth for the north-facing sites. 

## Future Features

With more time, we would incorporate Hubbard Brook specific branding. We would also, explore
data with all the watersheds that Hubbard Brook has to make an extensive data viewer for the whole
property. 

## Running the Project Locally

To run the project locally, install the required R packages:

```r
install.packages(c("shiny", "tidyverse", "lubridate", "plotly", "httr", "readr", "shinycssloaders", "bslib", "thematic". "later"))
