data <- read.table("CSV_PALOCAMPIONATO.csv", sep = ",", header = T)

#### PULIZIA DATI ####

# qui dico: togli le righe della colonna SEMPLE_1 che contengono un valore "NA"
data_na <- subset(data, SAMPLE_1 != "NA")
nrow(data) - nrow(data_na) # risultano 1882 i valori eliminati perchè contenenti NA

#### ANALISI ####

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

# creare un dataframe con i risultati per confrontarli 


# calcolo la differenza tra i due prodotti di quota IDW e campionato sulla base di quello originale 
data_na$diff <- data_na$field_3 - data_na$SAMPLE_1 # sottraggo dalla quota IDW la quota campionata 


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