
############## Date read in/cleaning file ##################

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
  mutate(datetime = ymd_hms(TIMESTAMP)) #|> 
  #filter(year(datetime) > 2025)

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
  mutate(datetime = ymd_hms(TIMESTAMP)) #|> 
 # filter(year(datetime) > 2025)


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
  quote = "\""
) |> 
  select(TIMESTAMP, ReportPCP) |> 
  mutate(datetime = ymd_hms(TIMESTAMP)) #|> 
  #filter(year(datetime) >= 2023)  ## Static file only goes to 2024

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
  mutate(datetime = ymd_hms(TIMESTAMP)) #|> 
 # filter(year(datetime) >= 2023)  ## Static file only goes to 2024

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
  mutate(datetime = ymd_hms(TIMESTAMP)) #|> 
  #filter(year(datetime) == 2026)


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
  #filter(year(datetime) == 2026) |> 
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
  mutate(datetime = ymd_hms(TIMESTAMP)) #|> 
 # filter(year(datetime) == 2026)


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
  #filter(year(datetime) == 2026) |> 
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
  #filter(year(datetime) == 2026) |> 
  group_by(datetime) |> 
  mutate(temp_avg = mean(c(ST110_1, St110_2, St110_3)))


all_dates <- c(
  streamflowNorth$datetime,
  streamflowSouth$datetime,
  precipNorth$datetime,
  precipSouth$datetime,
  wind$datetime,
  soilmoisture$datetime,
  tempNorth$datetime,
  tempSouth$datetime
)

global_min_date <- min(all_dates, na.rm = TRUE)
global_max_date <- max(all_dates, na.rm = TRUE)



###########################################
#########LIVE DATA READ IN ################
##########################################

library(httr)
library(readr)

#### Read Hubbard Brook Data function
readHBdat <- function(url){

url <- url

lines <- read_lines(curl::curl(
  url,
  handle = curl::new_handle(
    username = "capstone",
    password = "data2025"
  )
))

# Keep line 2 (headers) and lines 5 onward (data)
cleaned_lines <- c(
  lines[2],        # header
  lines[5:length(lines)]  # data
)

# Read cleaned text as CSV
df <- read_csv(I(cleaned_lines), na = "NaN") |> 
  drop_na()

return(df)
}


winddat <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/Kineo_Tower_Kineo.dat")
snowdat <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/Snowcourse_2_SS2-snowdat.dat")
soildat <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/Snowcourse_19_SS19_soildat.dat")
streamdatSouth <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/weir3_weir_3.dat")
streamdatNorth <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/weir9_weir_9.dat")
tempdatSouth <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta1_SF_Wx1_Temp_15min.dat")
raindatSouth <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta1_Wx_1_rain.dat")
raindatNorth <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta23_Wx_23_rain.dat")
tempdatNorth <- readHBdat("https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/LoggerNetDir/wxsta23_Wx_23_Temp_15_min.dat")

tempdatNorth <- tempdatNorth |>
  drop_na() |> 
  group_by(TIMESTAMP) |> 
  mutate(temp_avg = mean(c(ST110_1, St110_2, St110_3)))
