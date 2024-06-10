#INSTALLING PACKAGES (previously installed packages commented - please uncomment if necessary)
if(!require("sf")) {install.packages("sf"); library(sf)}
if(!require("sp")) {install.packages("sp"); library(sp)}
if(!require("spdep")) {install.packages("spdep"); library(spdep)}
if(!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
#if(!require("spatialEco")) {install.packages("spatialEco"); library(spatialEco)}
#if(!require("plotKML")) {install.packages("plotKML"); library(plotKML)}
if(!require("spatialreg")) {install.packages("spatialreg"); library(spatialreg)}
if(!require("stargazer")) {install.packages("stargazer"); library(stargazer)}
if(!require("ggplot2")) {install.packages("ggplot2"); library(ggplot2)}

setwd("/Users/weronikakieliszek/Desktop/Uni/Masters/3rd Semester/Ekonometria Przestrzenna/Dane/")

rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

subfolders <- list.dirs("Script4/", full.names = TRUE, recursive = FALSE)
mapa <- data.frame()
for (subfolder in subfolders) {
  mapa_ <- read_sf(subfolder, "gis_osm_pois_a_free_1")
  mapa_ <- st_transform(mapa_, "+proj=longlat")
  mapa <- rbind(mapa, mapa_)
}

selected_classes <- c("school", "hospital", "university", "kindergarten", "doctors", "playground")
mapa <- mapa[mapa$fclass %in% selected_classes, ]

custom_palette <- c("school" = "#457b9d", "hospital" = "#eab69f", "university" = "#e07a5f", "kindergarten" = "#3d405b", "doctors" = "#81b29a", "playground" = "#f2cc8f")

# Convert the data frame to sf object
mapa_sf <- st_as_sf(mapa)

# Plot with ggplot
gg <- ggplot() +
  geom_sf(data = mapa_sf, aes(color = fclass)) + # Adjust size and alpha here
  scale_color_manual(values = custom_palette, name = "Kategoria punktu") + 
  theme_void() + 
  labs(title = "Wybrane punkty wizualizowane na mapie Polski")

gg

output_file_path <- "Points_on_map.png"
png(filename = output_file_path)
ggplot() +
  geom_sf(data = mapa_sf, aes(color = fclass)) + # Adjust size and alpha here
  scale_color_manual(values = custom_palette, name = "Category") + 
  theme_void() + 
  labs(title = "Selected points visualised on the map")
dev.off()

unique(mapa$fclass)

punkty <- mapa[mapa$fclass == "kindergarten", c("osm_id", "geometry")]

mapa <- read_sf(".", "powiaty")
mapa <- st_transform(mapa, "+proj=longlat")
mapa$Kod <- as.numeric(as.character(mapa$jpt_kod_je))
plot(st_geometry(mapa), main = "Szkoły w Polsce")
sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
plot(punkty, pch = 1, col = sgh_green, add = TRUE)

st_crs(punkty) <- st_crs(mapa)
mapa$key <- 1:nrow(mapa)
punkty.w.powiatach <- st_intersects(punkty, mapa) %>% 
  unlist() %>% 
  table %>% 
  as.data.frame()
colnames(punkty.w.powiatach) <- c("powiat", "punkty")
punkty.w.powiatach$key <- as.numeric(as.character(punkty.w.powiatach$powiat))
punkty.przestrzenne <- merge(x = mapa, y = punkty.w.powiatach, by = "key", all = TRUE)
rm(punkty.w.powiatach, mapa)
punkty.przestrzenne$punkty[is.na(punkty.przestrzenne$punkty)] <- 0

dane <- read.csv("EP_dane.csv", header = TRUE, sep = ";", dec = ",")
dane$Kod <- dane$Kod/1000
spatial_data <- merge(y = dane, x = punkty.przestrzenne, by.y = "Kod", by.x = "Kod")
rm(mapa_, dane, punkty.przestrzenne, punkty)

spatial_data$punkty <- spatial_data$punkty / spatial_data$Ludnosc
model.liniowy <- lm(spatial_data$Urodzenia ~ spatial_data$Urbanizacja + spatial_data$Unemp + spatial_data$Wydatki + spatial_data$Fem + spatial_data$Malzenstwa + spatial_data$punkty)
summary(model.liniowy)

#GRAPHICAL EVALUATION
spatial_data$res <- model.liniowy$residuals
green_area <- rgb(40, 54, 24, 80, names = NULL, maxColorValue = 255)
red_area <- rgb(188, 71, 73, 80, names = NULL, maxColorValue = 255)
beige_area <- rgb(254, 250, 224, 80, names = NULL, maxColorValue = 255)
pal <- colorRampPalette(c(red_area, beige_area, green_area), bias = 1.7)
#Put 3 colors into the map, "white" in the middle of the palette, play around with the bias other than 1 so as to plot the near-zero residuals as white
plot(spatial_data[,"res"], key.pos = 4, pal = pal, nbreaks = 20,
     main = "Residuals from linear model")

pal2 <- colorRampPalette(c("white", green_area), bias = 1.7)
plot(spatial_data["punkty"], key.pos = 4, pal = pal2, nbreaks = 20,
     main = "Przedszkola na liczbę ludności")

output_file_path <- "kindergartens_per_capita.png"
png(filename = output_file_path)
plot(spatial_data["punkty"], key.pos = 4, pal = pal2, nbreaks = 20,
     main = "Kindergartens per capita")
dev.off()
