#INSTALLING PACKAGES (previously installed packages commented - please uncomment if necessary)
if(!require("sf")) {install.packages("sf"); library(sf)}
if(!require("sp")) {install.packages("sp"); library(sp)}
if(!require("spdep")) {install.packages("spdep"); library(spdep)}
if(!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
if(!require("spatialreg")) {install.packages("spatialreg"); library(spatialreg)}
if(!require("stargazer")) {install.packages("stargazer"); library(stargazer)}

#SET WORKING DIRECTORY
setwd("/Users/weronikakieliszek/Desktop/Uni/Masters/3rd Semester/Ekonometria Przestrzenna/Dane/")

# clear the workspace, plots and console
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

green_area <- rgb(40, 54, 24, 80, names = NULL, maxColorValue = 255)
red_area <- rgb(188, 71, 73, 80, names = NULL, maxColorValue = 255)
beige_area <- rgb(254, 250, 224, 80, names = NULL, maxColorValue = 255)
pal1 <- colorRampPalette(c(red_area, beige_area, green_area), bias = 1.7)

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
dane.przestrzenne <- merge(y = dane, x = punkty.przestrzenne, by.y = "Kod", by.x = "Kod")
rm(mapa_, dane, punkty.przestrzenne, punkty)

N <- nrow(dane.przestrzenne)

#Many markets in Warsaw? No rocket science. Maybe better the number of markets per 10000 residents?
dane.przestrzenne$przedszkola <- (dane.przestrzenne$punkty / dane.przestrzenne$Ludnosc) * 1000
attach(dane.przestrzenne)


cont <- poly2nb(dane.przestrzenne, queen = T)
W_list <- nb2listw(cont, style = "W")

cont2 <- poly2nb(dane.przestrzenne, queen = FALSE)   # Użyj queen = FALSE dla sąsiadów pierwszego rzędu
W1_list <- nb2listw(cont2, style = "B")         # Macierz wag binarnych bez normalizacji
W1 <- listw2mat(W1_list)                        # Zamień na macierz
eigenvalues <- eigen(W1)$values                 # Oblicz wartości własne
max_eigenvalue <- max(eigenvalues)              # Znajdź najwyższą wartość własną
W1_normalized <- W1 / max_eigenvalue            # Normalizuj macierz wag
# Konwertuj macierz z powrotem na obiekt wag przestrzennych
W1_normalized_list <- mat2listw(W1_normalized, style = "W")

centroids <- st_centroid(dane.przestrzenne$geometry)
distance <- st_distance(centroids)                                  # Oblicz macierz odległości w kilometrach
units(distance) <- "km"
threshold <- 200                                                    # Zdefiniuj próg
# Utwórz obiekt za pomocą dnearneigh, ograniczając się do 200 km
cont2 <- dnearneigh(centroids, 0, threshold, row.names = dane.przestrzenne$jpt_kod_je)
cont2_dists <- nbdists(cont2, centroids)                            # Użyj nbdists, aby uzyskać odległości dla sąsiadów w promieniu 200 km
cont2_weights <- lapply(cont2_dists, function(x) 1 / (x^2))         # Odwróć kwadrat odległości, aby utworzyć wagi
W2_list <- nb2listw(cont2, glist = cont2_weights, style = "W")      # Utwórz obiekt listw z wagami
W2 <- listw2mat(W2_list)                                            # Konwertuj obiekt listw na macierz

library(spatialreg)
model2a <- lagsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia, listw = W1_normalized_list)
model2b <- lagsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia, listw = W2_list)
summary(model2a)
summary(model2b)

residuals2a <- residuals(model2a)
residuals2b <- residuals(model2b)

moran.test(residuals2a, listw=W1_normalized_list)
moran.test(residuals2b, listw=W2_list)

AIC(model2a)
AIC(model2b)

impacts_model <- impacts(model2a, listw = W1_normalized_list)
print(impacts_model)

index <- which(dane.przestrzenne$Nazwa == "Powiat stalowowolski")
specific_weights <- W2[index, ]
dane.przestrzenne$weights <- specific_weights * 2.9738596056

pal <- colorRampPalette(c("white", "darkred"), bias = 1)
plot(dane.przestrzenne[, "weights"], key.pos = 4, pal = pal, main = "Impact of the number of kindergartens in a county on births in neighboring counties")

output_file_path <- "impacts.png"
png(filename = output_file_path)
plot(dane.przestrzenne[, "weights"], key.pos = 4, pal = pal, main = "Impact of the number of kindergartens in a county on births in neighboring counties")
dev.off()