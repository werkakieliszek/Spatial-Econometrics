#INSTALLING PACKAGES (previously installed packages commented - please uncomment if necessary)
if(!require("sf")) {install.packages("sf"); library(sf)}
if(!require("sp")) {install.packages("sp"); library(sp)}
if(!require("spdep")) {install.packages("spdep"); library(spdep)}

#SET WORKING DIRECTORY
setwd("/Users/weronikakieliszek/Desktop/Uni/Masters/3rd Semester/Ekonometria Przestrzenna/Dane")

# clear the workspace, plots and console
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

#Re-run the code from the previous class
mapa <- read_sf(".", "powiaty")
mapa <- st_transform(mapa, "+proj=longlat")
dane <- read.csv("EP_dane.csv", header = TRUE, sep = ";", dec = ",")
dane$Kod <- dane$Kod/1000
mapa$Kod <- as.numeric(as.character(mapa$jpt_kod_je))
spatial_data <- merge(y = dane, x = mapa, by.y = "Kod", by.x = "Kod")
rm(mapa)
rm(dane)
sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)

#LINEAR MODEL ESTIMATED VIA OLS
spatial_data$Wydatki_percap <- spatial_data$Wydatki/spatial_data$Ludnosc
model.liniowy <- lm(spatial_data$Urodzenia ~ spatial_data$Urbanizacja + spatial_data$Unemp + spatial_data$Wydatki + spatial_data$Fem + spatial_data$Malzenstwa)
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

output_file_path <- "residuals_from_linear_model.png"
png(filename = output_file_path)
plot(spatial_data[,"res"], key.pos = 4, pal = pal, nbreaks = 20,
     main = "Residuals from linear model")
dev.off()

#GLOBAL MORAN'S TEST FOR RESIDUALS
cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "W")
lm.morantest(model.liniowy, W1_list, alternative = "greater")
res <- model.liniowy$residuals
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = green_area)

output_file_path <- "morans_plot.png"
png(filename = output_file_path)
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = green_area)
dev.off()

#The same vor a variable (not necessarily for residuals)
moran(res, W1_list, length(W1_list$neighbours), length(W1_list$neighbours)) #Only I statistic and kurtosis of residuals
moran.test(res, W1_list) #Test statistic with p-value

#Geary's C test
geary.test(res, W1_list)

# #Local Moran's tests
# LM <- localmoran(res, W1_list)
# LM[,5] <- p.adjustSP(LM[,5], cont1, method = "bonferroni")
# LM
# 
# #Join count tests
# JC <- joincount.test(as.factor(res > 0), listw = W1_list)
# 
# #Tests with specified alternative hypotheses
# lm.LMtests(model.liniowy, listw = W1_list, test = "all")

# Wybór progu podziału
threshold <- 8.07 # Próg podziału - wartość dla Polski

# Podział wartości zmiennej na dwie klasy
spatial_data$binary <- ifelse(spatial_data$Urodzenia > threshold, 1, 0)

# Przeprowadzenie testu liczby połączeń dla zmiennej binarnej
JC_binary <- joincount.test(as.factor(spatial_data$binary), listw = W1_list)

# Wyświetlenie wyników
print(JC_binary)

