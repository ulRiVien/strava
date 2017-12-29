library(rvest)
library(stringr)
library(data.table)
library(dplyr)
library(lubridate)

function.athlete_data <- function(id) {
  
  athl_url <- paste0("https://www.strava.com/athletes/", id)
  #if (!RCurl::url.exists(athl_url)) stop("Athlete does not exist")
  
  doc <- read_html(athl_url)
  doc <- html_node(doc, "#athlete-view")

  # profile
  avatar <- html_node(doc, ".athlete-profile .avatar-img") %>% html_attr("src")
  name <- html_node(doc, ".athlete-profile .bottomless") %>% html_text()
  location <- html_node(doc, ".athlete-profile .location") %>% html_text()
  
  # monthly distance
  intervals <- html_nodes(doc, ".athlete-graph .intervals .interval") %>% html_attr("id")
  months_all_dist_px <- sapply(intervals, function(i) {
    html_nodes(doc, paste0(".athlete-graph #", i[1],  " .fill")) %>% html_attr("style", 0)
  })
  months_all_dist_px <- str_replace_all(months_all_dist_px, "[^\\d\\.]+", "") %>% as.numeric()
  names(months_all_dist_px) <- str_sub(intervals, 10)
  px_100 <- html_nodes(doc, ".y-axis") %>% html_text()
  px_100 <- str_extract(px_100, "[0-9]+$")
  
  # current month stats
  month_all_stats <- html_nodes(doc, ".inline-stats strong") %>% html_text()
  month_all_stats <- str_replace_all(month_all_stats, "\n", "")
  stats_name <- c("dist", "time", "elev")
  names(month_all_stats) <- stats_name
  
  # records
  records_bike_stats <- html_nodes(doc, ".athlete-records td") %>% html_text()
  ytd_bike_stats <- records_bike_stats[1:4]
  names(ytd_bike_stats) <- c(stats_name, "nb")
  ay_bike_stats <- records_bike_stats[5:8]
  names(ay_bike_stats) <- c(stats_name, "nb")
  
  return(list(id = id,
              avatar = avatar,
              name = name,
              location = location,
              months_all_dist_px = months_all_dist_px,
              px_100 = px_100,
              month_all_stats = month_all_stats,
              ytd_bike_stats = ytd_bike_stats,
              ay_bike_stats = ay_bike_stats))
  
}

function.athlete_data(2216191)

# Id strava members club St-Ger (id 28259)
ids <- c(1078226, 12497128, 21175337, 17127600, 6534080,
         1064065, 5092692, 1636898, 21748556, 3613818,
         13206164, 2225253, 3351268, 11450803, 8767660,
         580149, 11433122, 21097239, 7097243, 1633448,
         5411671, 3440131, 2216191, 16978434, 11375165,
         18308066, 17722512, 11715453)

system.time({
lst_dt <- lapply(ids, function(id) {
  lst <- function.athlete_data(id)
  if (all(lst$months_all_dist_px == "0")) lst$months_all_dist_px <- NULL
  dt <- data.table(t(unlist(lst)))
})
})

dt <- rbindlist(lst_dt, fill = TRUE)




# save
saveRDS(dt, file = "./Robject/athl_data.rds")

# load
dt <- readRDS("./Robject/athl_data.rds")



# rename columns
names(dt) <- str_replace(names(dt), "stats.", "")

# reset incorrect monthly stats (previous month)
cols_months_all_dist_px <- str_subset(names(dt), "^months_all_dist_px.")
dt[get(last(cols_months_all_dist_px)) == "0", ':='(month_all_dist = "0km",
                                                   month_all_time = "0h 0m",
                                                   month_all_elev = "0m")]

# extract and convert lengths (distance and elevation in km/m)
function.length <- function(x) {
  v <- as.numeric(str_replace_all(x, "[^\\d\\.]+", ""))
  v[(str_detect(x, "mi$"))] <- v[(str_detect(x, "mi$"))] * 1.609344
  v[(str_detect(x, "ft$"))] <- v[(str_detect(x, "ft$"))] * 0.3048
  return(v)
}
cols_length <- str_subset(names(dt), "(_dist$)|(_elev$)")
dt[, c(cols_length) := lapply(.SD, function.length), .SDcols = cols_length]

# convert time
cols_time <- str_subset(names(dt), "_time$")
dt[, c(cols_time) := lapply(.SD, function(x) as.duration(hm(x))), .SDcols = cols_time]

# transform (approx.) monthly distances from px to km
cols_px <- c(cols_months_all_dist_px, "px_100") 
dt[, c(cols_px) := lapply(.SD, as.numeric), .SDcols = cols_px]
dt[, c(cols_px) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols_px]
months <-  str_sub(cols_months_all_dist_px, -6)
cols_months_all_dist = paste0("month-", 12:0, "_all_dist")
dt[, c(cols_months_all_dist) := lapply(.SD, function(x) x * px_100 / 100), .SDcols = cols_months_all_dist_px]
dt[, c(cols_px) := NULL]
