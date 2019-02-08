# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

install.packages("lubridate")

# Load in your data
evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)
View(evictions)
# Compute some values of interest and store them in variables for the report

# How many evictions were there?
num_evictions <- nrow(evictions)
num_features <- ncol(evictions)
# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- evictions %>% 
  group_by(Eviction.Notice.Source.Zipcode) %>%
  count() %>%
  arrange(-n) %>%
  ungroup() %>%
  top_n(10, wt = n)

# Create a plot of the number of evictions each month in the dataset
by_month <- evictions %>%
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(month) %>% 
  count()

# Store plot in a variable
ggplot(data = by_month) +
  geom_line(mapping = aes(x = month, y = n), color = "blue", alpha = .5) +
  labs(x = "Dtae", y = "Number of Evictions", title = "Evictions over time in SF")

# Map evictions in 2017 

# Format the lat/long variables, filter to 2017
evictions_2017 <- evictions %>% 
  mutate(date = as.Date(File.Date, format="%m/%d/%y")) %>% 
  filter(format(date, "%Y") == "2017") %>%
  separate(Location, c("lat", "long"), ", ") %>% # split the column at the comma
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)), # remove starting parentheses
    long = as.numeric(gsub("\\)", "", long)) # remove closing parentheses
  ) 

# Create a maptile background
base_plot <- qmplot(
  data = evictions_2017,        # name of the data frame
  x = long,                     # data feature for longitude
  y = lat,                      # data feature for latitude
  geom = "blank",               # don't display data points (yet)
  maptype = "toner-background", # map tiles to query
  darken = .7,                  # darken the map tiles
  legend = "topleft"            # location of legend on page
)

# Add a layer of points on top of the map tiles
evictions_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = .3) +
  labs(title = "Evictions in San Francisco, 2017") +
  theme(plot.margin = margin(.3, 0, 0, 0, "cm")) # adjust spacing around the map
