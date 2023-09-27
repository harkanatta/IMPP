#Innlestur
Sys.setlocale("LC_ALL", "Icelandic") # `Sys.setlocale` er hér fyrir íslenskar dagsetningar.
library(tidyverse)
library(readr)


library(readxl)
MAN03101_20230913_155520 <- read_excel("data/MAN03101_20230913-155520.xlsx", skip = 2)
mannfj_island <- MAN03101_20230913_155520 [-1,-2]
names(mannfj_island)[names(mannfj_island) == '...1'] <- 'Place'





# Load required libraries
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

# Read geopackage layers
mannvirki_flakar <- st_read("C:/Users/valty/Documents/vinna/GIS/lmi/IS50V/IS_50V_MANNVIRKI_170062020/IS_50V_MANNVIRKI_170062020.gpkg", layer = "mannvirki_flakar")
mannvirki_punktar <- st_read("C:/Users/valty/Documents/vinna/GIS/lmi/IS50V/IS_50V_MANNVIRKI_170062020/IS_50V_MANNVIRKI_170062020.gpkg", layer = "mannvirki_punktar")

names(mannvirki_flakar)[names(mannvirki_flakar) == 'nafnfitju'] <- 'Place'

# Join the data
mannvirki_flakar_joined <- left_join(mannvirki_flakar, mannfj_island, by = "Place")

# Drop NA or empty geometries if needed
mannvirki_flakar_joined <- mannvirki_flakar_joined %>% 
  filter(!is.na(geom))

# Convert the 2023 column to numeric if it isn't
mannvirki_flakar_joined$`2023` <- as.numeric(mannvirki_flakar_joined$`2023`)

# Create the plot
ggplot() +
  geom_sf(data = mannvirki_flakar_joined, aes(size = `2023`, fill = `2023`), alpha = 0.7) +
  scale_size_continuous(range = c(3, 20)) +
  scale_fill_continuous() +
  labs(title = "Population Distribution in Iceland (2023)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()



df <- mannvirki_flakar_joined


# Assuming 'Place' is the column in mannvirki_flakar that matches with 'Place' in mannfj_island
# Initial Join
joined_data <- left_join(mannfj_island, mannvirki_flakar, by = "Place")
# Filter places that are in both data frames
common_places <- mannvirki_flakar %>% 
  filter(Place %in% mannvirki_punktar$place)

# Now common_places will have only the rows where the Place exists in both data frames

# Filter data to include only values > 100
# Filter entire data frame to include rows where '2023' column has values > 100
filtered_joined_data <- joined_data[joined_data$`2023` > 100, ]
filtered_joined_data <- filtered_joined_data[,-c(2:23)]
filtered_joined_data <-  filtered_joined_data %>% select(-contains("ibuarfj_"))
filtered_joined_data <- filtered_joined_data[,-c(3:16)]

# Plot
ggplot() +
  geom_sf(data = mannvirki_flakar, fill = "white", color = "black") +
  geom_sf(data = filtered_joined_data, aes(size = as.numeric(`2023`), color = as.numeric(`2023`)), alpha = 0.7) +
  scale_size_continuous(range = c(3, 20)) +
  labs(title = "Population Distribution in Iceland (2023)",
       x = "Longitude",
       y = "Latitude")



library(ggplot2)
library(sf)

# Read the GeoPackage file into an sf object
your_data <- st_read("data/centroids.gpkg")
your_data$X2023 <- as.numeric(your_data$X2023)
ggplot(data = your_data) +
  geom_sf(aes(size = X2023)) +  # Size points based on "2023"
  scale_size_continuous(range = c(1, 20)) +  # Adjust the size range as needed
  ggtitle("Map with Points Sized by X2023")



df <- mannvirki_flakar_joined

# Calculate the trend as the difference between the last and the first available numbers for each row
df$Trend <- apply(df[,35:57], 1, function(row) {
  # Convert the list to a numeric vector
  row_vector <- as.numeric(as.vector(row))
  
  # Remove NA values
  row_clean <- na.omit(row_vector)
  
  # Check if at least two numbers are available
  if (length(row_clean) >= 2) {
    last_value <- tail(row_clean, n=1)
    first_value <- head(row_clean, n=1)
    return(last_value - first_value)
  } else {
    return(NA)
  }
})





iceland.df <- iceland.sp %>% fortify()
glimpse(iceland.df)

your_data.df <- your_data %>% fortify() %>% st_transform( 4326)
#glimpse(your_data.df)

coords <- st_coordinates(your_data.df)

# Add coordinates as new columns to your_data
your_data.df$lon <- coords[, "X"]
your_data.df$lat <- coords[, "Y"]

ggplot() +
  geom_polygon(data = iceland.df, aes(long, lat, group = group)) +
  geom_point(data = your_data.df[,c("X2023", "lon","lat")], aes(lon, lat), col = "red") #+
  #coord_map(xlim = c(-22.8, -21.7), ylim = c(63.95, 64.4))




xlim <- c(-28, -10)
ylim <- c(62.5, 67.5)
depth <- 
marmap::getNOAA.bathy(lon1 = xlim[1], lon2 = xlim[2],
                lat1 = ylim[1], lat2 = ylim[2],
                resolution = 1) %>% 
  fortify()  # turn the object into a data.frame

m <- ggplot() +
  theme_bw() +
  geom_contour(data = depth, aes(x, y, z = z),
               breaks=c(-25, -50, -100, -200, -400),
               colour="black", size=0.1) +
  geom_polygon(data = iceland.df, aes(long, lat, group = group), fill = "grey") +
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(x = NULL, y = NULL)

m + geom_point(data = your_data.df[,c("X2023", "lon","lat")], aes(lon, lat, size = X2023), colour = "red")
