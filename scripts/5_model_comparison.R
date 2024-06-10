#INSTALLING PACKAGES (previously installed packages commented - please uncomment if necessary)
if(!require("sf")) {install.packages("sf"); library(sf)}
if(!require("sp")) {install.packages("sp"); library(sp)}
if(!require("spdep")) {install.packages("spdep"); library(spdep)}
if(!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
#if(!require("spatialEco")) {install.packages("spatialEco"); library(spatialEco)}
#if(!require("plotKML")) {install.packages("plotKML"); library(plotKML)}
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

#Visualise the number of markets in poviats
pal <- colorRampPalette(c("white", sgh_green), bias = 1)
plot(dane.przestrzenne[, "punkty"], key.pos = 4, pal = pal, main = "Przedszkola per powiat")

output_file_path <- "Kindergartens_per_poviat.png"
png(filename = output_file_path)
plot(dane.przestrzenne[, "punkty"], key.pos = 4, pal = pal, main = "Kindergartens per poviat")
dev.off()

#Many markets in Warsaw? No rocket science. Maybe better the number of markets per 10000 residents?
dane.przestrzenne$przedszkola <- dane.przestrzenne$punkty / dane.przestrzenne$Ludnosc * 1000
plot(dane.przestrzenne[, "przedszkola"], key.pos = 4, pal = pal,
     main = "Number of kindergartens in poviat per 1000 residents")

output_file_path <- "Kindergartens_per_poviat_per_1000_residents.png"
png(filename = output_file_path)
plot(dane.przestrzenne[, "przedszkola"], key.pos = 4, pal = pal,
     main = "Number of kindergartens in poviat per 1000 residents")
dev.off()

attach(dane.przestrzenne)
cont <- poly2nb(dane.przestrzenne, queen = T)
W_list <- nb2listw(cont, style = "W")

#Linear model
model1 <- lm(dane.przestrzenne$Urodzenia ~ dane.przestrzenne$Urbanizacja + dane.przestrzenne$Unemp + dane.przestrzenne$Fem + dane.przestrzenne$Malzenstwa + dane.przestrzenne$przedszkola + dane.przestrzenne$Wynagrodzenia)
summary(model1)
lm.morantest(model1, listw = W_list)
lm.LMtests(model1, listw = W_list, test = "all")
dane.przestrzenne$reslm <- model1$residuals
plot(dane.przestrzenne[,"reslm"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from linear model")

#SAR: Spatial Lag - ML and TSLS
library(spatialreg)
model2 <- lagsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia, listw = W_list)
summary(model2)
res2a <- model2$residuals
moran.test(res2a, listw = W_list)

dane.przestrzenne$ressar <- model2$residuals
plot(dane.przestrzenne[,"ressar"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SAR model")

output_file_path <- "residuals_from_SAR_model.png"
png(filename = output_file_path)
plot(dane.przestrzenne[,"ressar"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SAR model")
dev.off()

#SEM: Spatial Error - ML and GLS
model3 <- errorsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia, listw = W_list)
summary(model3)
res3a <- model3$residuals
moran.test(res3a, listw = W_list)

dane.przestrzenne$ressem <- model3$residuals
plot(dane.przestrzenne[,"ressem"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SEM model")

output_file_path <- "residuals_from_SEM_model.png"
png(filename = output_file_path)
plot(dane.przestrzenne[,"ressem"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SEM model")
dev.off()

#SLX
W <- listw2mat(W_list)
X <- cbind(Urbanizacja, przedszkola, Unemp, Fem, Malzenstwa, Wynagrodzenia)
WX <- W %*% X

lag.Urbanizacja <- WX [, 1]
lag.przedszkola <- WX [, 2]
lag.Unemp <- WX [, 3]
lag.Fem <- WX [, 4]
lag.Malzenstwa <- WX [, 5]
lag.Wynagrodzenia <- WX [, 6]

model4 <- lm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia + lag.Urbanizacja + lag.przedszkola + lag.Unemp + lag.Fem + lag.Malzenstwa + lag.Wynagrodzenia)
summary(model4)

lm.morantest(model4, listw = W_list)
lm.LMtests(model4, listw = W_list, test = "all")

dane.przestrzenne$resslx <- model4$residuals
plot(dane.przestrzenne[,"resslx"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SLX model")

output_file_path <- "residuals_from_SLX_model.png"
png(filename = output_file_path)
plot(dane.przestrzenne[,"resslx"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SLX model")
dev.off()

#SARAR - ML and GS2SLS
model5 <- sacsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia, listw = W_list)
summary(model5)
res5a <- model5$residuals
moran.test(res5a, listw = W_list)

AIC(model5)

dane.przestrzenne$ressarar <- model5$residuals
plot(dane.przestrzenne[,"ressarar"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SARAR model")

output_file_path <- "residuals_from_SARAR_model.png"
png(filename = output_file_path)
plot(dane.przestrzenne[,"ressarar"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SARAR model")
dev.off()

#SDM
model6 <- lagsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia + lag.Urbanizacja + lag.przedszkola + lag.Unemp + lag.Fem + lag.Malzenstwa + lag.Wynagrodzenia, listw = W_list)
summary(model6)
res6a <- model6$residuals
moran.test(res6a, listw = W_list)

dane.przestrzenne$ressdm <- model6$residuals
plot(dane.przestrzenne[,"ressdm"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SDM model")

output_file_path <- "residuals_from_SDM_model.png"
png(filename = output_file_path)
plot(dane.przestrzenne[,"ressdm"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SDM model")
dev.off()

#SDEM
model7 <- errorsarlm(Urodzenia ~ Urbanizacja + przedszkola + Unemp + Fem + Malzenstwa + Wynagrodzenia + lag.Urbanizacja + lag.przedszkola + lag.Unemp + lag.Fem + lag.Malzenstwa + lag.Wynagrodzenia, listw = W_list)
summary(model7)
res7a <- model7$residuals
moran.test(res7a, listw = W_list)

dane.przestrzenne$ressdem <- model7$residuals
plot(dane.przestrzenne[,"ressdem"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SDEM model")

output_file_path <- "residuals_from_SDEM_model.png"
png(filename = output_file_path)
plot(dane.przestrzenne[,"ressdem"], key.pos = 4, pal = pal1, nbreaks = 20,
     main = "Residuals from SDEM model")
dev.off()

lrtest(model2, model5)

stargazer(model1, model2, model3, model4, model6, model7,
          column.labels = c("linear", "SAR", "SEM", "SLX", "SDM", "SDEM"),
          align = TRUE, type = "text",
          add.lines = list(c("rho", "--", round(model2$rho,4), "--", "--", round(model6$rho,4), "--"),
                           c("lambda", "--", "--", round(model3$lambda,4), "--", "--", round(model7$lambda,4)),
                           c("AIC", round(AIC(model1),2), round(AIC(model2),2), round(AIC(model3),2), round(AIC(model4),2), round(AIC(model6),2), round(AIC(model7),2))))

