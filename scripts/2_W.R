#INSTALLING PACKAGES (previously installed packages commented - please uncomment if necessary)
if(!require("sf")) {install.packages("sf"); library(sf)}
if(!require("sp")) {install.packages("sp"); library(sp)}
if(!require("spdep")) {install.packages("spdep"); library(spdep)}
if(!require("geosphere")) {install.packages("geosphere"); library(geosphere)}  # function distm()

#SET WORKING DIRECTORY
setwd("/Users/weronikakieliszek/Desktop/Uni/Masters/3rd Semester/Ekonometria Przestrzenna/Dane")

# clear the workspace, plots and console
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

#Re-running code from previous class
mapa <- read_sf(".", "powiaty")
mapa <- st_transform(mapa, "+proj=longlat")
dane <- read.csv("LUDN_3428_CTAB_20240208163006.csv", header = TRUE, sep = ";", dec = ",")
dane$Kod <- dane$Kod/1000
mapa$Kod <- as.numeric(as.character(mapa$jpt_kod_je))
spatial_data <- merge(y = dane, x = mapa, by.y = "Kod", by.x = "Kod")
N <- nrow(spatial_data)
rm(mapa)
rm(dane)

# W1
cont2 <- poly2nb(spatial_data, queen = FALSE)   # Użyj queen = FALSE dla sąsiadów pierwszego rzędu
W1_list <- nb2listw(cont2, style = "B")         # Macierz wag binarnych bez normalizacji
W1 <- listw2mat(W1_list)                        # Zamień na macierz
eigenvalues <- eigen(W1)$values                 # Oblicz wartości własne
max_eigenvalue <- max(eigenvalues)              # Znajdź najwyższą wartość własną
W1_normalized <- W1 / max_eigenvalue            # Normalizuj macierz wag
# Konwertuj macierz z powrotem na obiekt wag przestrzennych
W1_normalized_list <- mat2listw(W1_normalized, style = "W")

#W2
centroids <- st_centroid(spatial_data$geometry)
distance <- st_distance(centroids)                                  # Oblicz macierz odległości w kilometrach
units(distance) <- "km"
threshold <- 200                                                    # Zdefiniuj próg
# Utwórz obiekt za pomocą dnearneigh, ograniczając się do 200 km
cont2 <- dnearneigh(centroids, 0, threshold, row.names = spatial_data$jpt_kod_je)
cont2_dists <- nbdists(cont2, centroids)                            # Użyj nbdists, aby uzyskać odległości dla sąsiadów w promieniu 200 km
cont2_weights <- lapply(cont2_dists, function(x) 1 / (x^2))         # Odwróć kwadrat odległości, aby utworzyć wagi
W2_list <- nb2listw(cont2, glist = cont2_weights, style = "W")      # Utwórz obiekt listw z wagami
W2 <- listw2mat(W2_list)                                            # Konwertuj obiekt listw na macierz

#W3
distance2 <- matrix(0, nrow = N, ncol = N)      
for(ii in 1:N) {                                # Oblicz odległości pomiędzy każdą parą punktów
  for(jj in 1:N) {                              # Oblicz odległość euklidesową pomiędzy punktami ii i jj
    distance2[ii, jj] <- sqrt(
      (spatial_data$urodzenia.żywe.na.1000.ludności.2022....[ii] - spatial_data$urodzenia.żywe.na.1000.ludności.2022....[jj])^2 +
        (spatial_data$urodzenia.żywe.na.1000.ludności.2022....[ii] - spatial_data$urodzenia.żywe.na.1000.ludności.2022....[jj])^2 +
        (spatial_data$urodzenia.żywe.na.1000.ludności.2022....[ii] - spatial_data$urodzenia.żywe.na.1000.ludności.2022....[jj])^2
    )
  }
}
gamma <- 1                                      # Oblicz ciężary, korzystając z odwróconej odległości
W3 <- 1 / (distance2 ^ gamma)
W3[distance2==0] <- 1/min(distance2[distance2>0]) # Ustaw wagi dla odległości zerowych, aby uniknąć dzielenia przez zero
diag(W3) <- 0                                   # Ustaw elementy przekątne na zero
# Normalizuj macierz wag
W3 <- W3 / (as.matrix(rowSums(W3)) %*% matrix(1, nrow = 1, ncol = N))
rm(ii, jj)                                      # Wyczyść zmienne
W3_list <- mat2listw(W3, style = "W")           # Konwertuj macierz wag na przestrzenną listę wag
