# Salve, dopo aver lavorato un po' a partire dalle immagini che sono riuscito a visualizzare grazie al suo prezioso aiuto mi trovo a affrontare un ultimo problema. Esiste un modo per esportare da R le 
# immagini visualizzate con la funzione im.plotRGB() in modo da mantenere le informazioni di georeferenziazione? Qui sotto allego il mio codice nella parte interessata e aggiungo 
# nelle ultime due righe l'errore che mi da quando provo ad esportare in formato GeoTiff (ho provato anche dal tasto Export della visualizzazione del plot ma il risultato è lo stesso, riesco ad esportare 
# l'immagine ma una volta caricata su QGis perde le infomrazioni di georeferenziazione). 


library(terra)
library(imageRy)
library(viridis)
library(raster)
library(ggplot2)

################
setwd("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE")

#################

b2 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B02.jp2")
b3 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B03.jp2")
b4 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B04.jp2")
b8 <- raster("//?/C:/Users/chicc/OneDrive/Desktop/Università/MAGISTRALE - GEOGRAFIA E PROCESSI TERRITORIALI/SECONDO SEMESTRE/ANALISI GEOSTATISTICA DEI DATI TERRITORIALI/progetto finale/ALPI/S2B_MSIL1C_20240412T100559_N0510_R022_T32TPR_20240412T121150.SAFE/GRANULE/L1C_T32TPR_A037085_20240412T100818/IMG_DATA/T32TPR_20240412T100559_B08.jp2")

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

immagine <- im.plotRGB(stacksent_rt, 2, 4, 1) # nir on green 


writeRaster(immagine, filename = "nir_green", format = "GTiff", overwrite = TRUE)


# A seguire l'errore che mi da quando provo a mandare l'esportazione del raster con la funzione writeRaster

# Errore in (function (classes, fdef, mtable)  : 
            # non è possibile trovare un metodo ereditato per la funzione ‘writeRaster’ per la firma ‘"NULL", "character"’
