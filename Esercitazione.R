# aprendo R, vai su file, ingranaggio e permo su "set as working directory"

# caricare il documento CSV attraverso la funzione read.table e inserendo tra virgolette il nome del file presente nella cartella"
# sep = "" per indicare il separatore
# header = T per indicare che la prima riga della tabella contiene i nomi delle variabili

data <- read.table("CSV_PALOCAMPIONATO.csv", sep = ",", header = T)

#### PULIZIA DATI ####

# qui dico: togli le righe della colonna SEMPLE_1 che contengono un valore "NA"
data_na <- subset(data, SAMPLE_1 != "NA")
nrow(data) - nrow(data_na) # risultano 1882 i valori eliminati perchè contenenti NA

#### ANALISI ####
quota_campionata <- data_na$SAMPLE_1
quota_idw <- data_na$field_3

summary(quota_campionata)
summary(quota_idw)
confronto <- by(quota_idw, quota_campionata, summary)

plot(confronto)


# calcolo i principali indicatori statistici per quaanto riguarda le quote del prodotto CAMPIONATO (SEMPLE_1)

# media
media_quota_CAMP <- mean(data_na$SAMPLE_1, na.rm = TRUE)
media_quota_CAMP

# moda
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
moda_quota_CAMP <- Mode(data_na$SAMPLE_1)
moda_quota_CAMP

# mediana
mediana_quota_CAMP <- median(data_na$SAMPLE_1, na.rm = TRUE)
media_quota_CAMP

# SQM
SQM_quota_CAMP <- sd(data_na$SAMPLE_1, na.rm = TRUE)
SQM_quota_CAMP

# valore massimo e minimo
max_quota_CAMP <- max(data_na$SAMPLE_1, na.rm = TRUE)
max_quota_CAMP 
min_quota_CAMP <- min(data_na$SAMPLE_1, na.rm = TRUE)
min_quota_CAMP

# calcolo i principali indicatori statistici per quanto riguarda le quote del prodotto IDW (field_3)

# media
media_quota_IDW <- mean(data_na$field_3, na.rm = TRUE)
media_quota_IDW

# moda
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
moda_quota_IDW <- Mode(data_na$field_3)
moda_quota_IDW

# mediana
mediana_quota_IDW <- median(data_na$field_3, na.rm = TRUE)
media_quota_IDW

# SQM
SQM_quota_IDW <- sd(data_na$field_3, na.rm = TRUE)
SQM_quota_IDW

# valore massimo e minimo
max_quota_IDW <- max(data_na$field_3, na.rm = TRUE)
max_quota_IDW # coincide con la quota registrata della cima del monte Cornetto
min_quota_IDW <- min(data_na$field_3, na.rm = TRUE)
min_quota_IDW


# calcolo della COVARIANZA e di R-quadro 

covarianza<- cov(data_na$field_3, data_na$SAMPLE_1)
print(covarianza)

R <- covarianza/(SQM_quota_CAMP*SQM_quota_IDW)
print(R)




# calcolo la differenza tra i due prodotti di quota IDW e campionato sulla base di quello originale 
data_na$diff <- data_na$field_3 - data_na$SAMPLE_1 # sottraggo dalla quota IDW la quota campionata 
summary(data_na$diff)

# identifico gli outlayers
Q1 <- quantile(data_na$diff, 0.25, na.rm = TRUE) # identifico il primo quartile
Q3 <- quantile(data_na$diff, 0.75, na.rm = TRUE) # identifico il secondo quartile 
IQR <- Q3 - Q1
outliers_iqr <- data_na$diff[data_na$diff > Q3 + 1.5 * IQR | data_na$diff < Q1 - 1.5 * IQR] # identifico tutti i valori che sono considerati outliers in base al metodo dell'Intervallo Interquartile (IQR) con un fattore di moltiplicazione di 1.5 per determinare i limiti superiore e inferiore della distribuzione dei dati.
str(outliers_iqr)

# ordino gli outlayer in ordine crescente 
outliers_cresc <- sort(outliers_iqr)
outliers_cresc
outliers <- c(outliers_iqr)




#### ESPORTARE I DATI ####

write.csv(data_na$diff, file = "diff_quota_R.csv", row.names = FALSE)
write.csv(data_na, file = "tabella_pulita_R.csv", row.names = FALSE)

#### PLOT ####
par(mfrow = c(1,2))
plot(data_na$field_3, data_na$SAMPLE_1,
     xlab = "z IDW", ylab = "z CAMPIONATA",
     main = "Grafico di correlazione di quote",
     pch = 16)
boxplot(data_na$diff,
        ylab = "m",
        main = "boxplot delle differenze di quota",
        pch = 16)


#### CONFRONTO CON IMMAGINI SATELLITARI SENTINEL 2 ####

# importo i pacchetti necessari
#install.packages("raster")
library(terra)
library(imageRy)
library(viridis)
library(raster)
library(ggplot2)



# lavoro su immagini a dimensione originale

setwd("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE")

# importare le bande
cornetto_rgb <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_TCI.jp2")
b2 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B02.jp2")
b3 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B03.jp2")
b4 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B04.jp2")
b8 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B08.jp2")

# color palette
cl <- colorRampPalette(c("yellow", "orange", "red")) (100)

#multiframe
par(mfrow=c(2,2))
plot(b2)
plot(b3)
plot(b4)
plot(b8)

plot(cornetto_rgb)

stacksent <- stack(b2, b3, b4, b8)
stacksent
plot(stacksent)

# RGB
# stacksent[[1]] = b2 = red
# stacksent[[2]] = b3 = green
# stacksent[[3]] = b4 = blue
# stacksent[[4]] = b8 = nir

im.plotRGB(stacksent, 3, 2, 1) # naturale RGB
im.plotRGB(stacksent, 4, 3, 2) # falso colore, nir on red





# lavoro su immagini RITAGLIATE

setwd("C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/immagini sentinal2")

# importare le bande
cornetto_rgb_rt <- raster("C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/immagini sentinal2/Ritagliato_RGB.tif")
b2_rt <- raster("C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/immagini sentinal2/Ritagliato_B2.tif")
b3_rt <- raster("C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/immagini sentinal2/Ritagliato_B3.tif")
b4_rt <- raster("C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/immagini sentinal2/Ritagliato_B4.tif")
b8_rt <- raster("C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/immagini sentinal2/Ritagliato_B8.tif")

# color palette
cl <- colorRampPalette(c("yellow", "orange", "red")) (100)

#multiframe
par(mfrow=c(2,2))
plot(b2_rt)
plot(b3_rt)
plot(b4_rt)
plot(b8_rt)

plot(cornetto_rgb_rt)

stacksent_rt <- stack(b2_rt, b3_rt, b4_rt, b8_rt)
stacksent_rt
plot(stacksent_rt)

nir_green_bends <- stack(b3_rt, b8_rt, b2_rt)
# RGB
# stacksent[[1]] = b2 = red
# stacksent[[2]] = b3 = green
# stacksent[[3]] = b4 = blue
# stacksent[[4]] = b8 = nir
par(mfrow=c(2,2))
im.plotRGB(stacksent_rt, 3, 2, 1) # naturale RGB
im.plotRGB(stacksent_rt, 4, 3, 2) # falso colore, nir on red
im.plotRGB(stacksent_rt, 2, 4, 1) # nir on green 
im.plotRGB(stacksent_rt, 1, 2, 4) # nir on blue

immagine <- im.plotRGB(stacksent_rt, 2, 4, 1) # nir on green 


writeRaster(immagine, filename = "nir_green", format = "GTiff", overwrite = TRUE)

#Errore in (function (classes, fdef, mtable)  : 
            # non è possibile trovare un metodo ereditato per la funzione ‘writeRaster’ per la firma ‘"NULL", "character"’

