#Contaminación acústica: datos en tiempo real

#Cargamos librerías
library(ggmap)
library(reshape2)

#Importamos datos de ruido de http://datos.madrid.es/
ruido <- read.table('ruido.txt', sep = ',', header=F, fileEncoding = "latin1")

colnames(ruido)[colnames(ruido)=="V1"] <- "Estación"
colnames(ruido)[colnames(ruido)=="V2"] <- "Año"
colnames(ruido)[colnames(ruido)=="V3"] <- "Mes"
colnames(ruido)[colnames(ruido)=="V4"] <- "Día"
colnames(ruido)[colnames(ruido)=="V5"] <- "Periodo"
colnames(ruido)[colnames(ruido)=="V6"] <- "Nivel"

#Damos formato ancho a ruido
ruido<-dcast(ruido, Estación~Periodo, value.var = "Nivel")

#Importamos localización de estaciones
est <- read.csv('coordenadas nmts 2015.csv', sep = ',', header=T, fileEncoding = "latin1")

#Nos quedamos con las columnas de interés
est<-est[,c(1,4,5)]
colnames(est)[colnames(est)=="NÂº"] <- "Estación"
colnames(est)[colnames(est)=="Latitud"] <- "Lat"
colnames(est)[colnames(est)=="Longitud"] <- "Lon"

#Limpiamos los datos

est$Lon<-as.character(est$Lon)
est$Lat<-as.character(est$Lat)

est$Lon<-gsub(",",".",est$Lon)
est$Lat<-gsub(",",".",est$Lat)

est$Lon<-gsub("O"," ",est$Lon)
est$Lat<-gsub("O"," ",est$Lat)

est$Lon<-gsub("º","",est$Lon)
est$Lat<-gsub("º","",est$Lat)

est$Lon<-gsub("°","",est$Lon)
est$Lat<-gsub("°","",est$Lat)

est$Lon<-gsub(" ","",est$Lon)
est$Lat<-gsub(" ","",est$Lat)

#Convertimos las coordenadas de GMS a decimal.

lon<-gsub("([0-9]*).([0-9]*).([0-9]*).*", "\\1-\\2-\\3", est$Lon)
lon
lat<-gsub("([0-9]*).([0-9]*).([0-9]*).*", "\\1-\\2-\\3", est$Lat)
lat

tmp<-gregexpr("[0-9]+",lon)
tmp<-regmatches(lon, tmp)
tmp<-lapply(tmp, as.numeric)
res<-lapply(tmp,function(x) x[1]+x[2]/60+x[3]/3600)
est$Lon<-as.numeric(res)

tmp<-gregexpr("[0-9]+",lat)
tmp<-regmatches(lat, tmp)
tmp<-lapply(tmp, as.numeric)
res<-lapply(tmp,function(x) x[1]+x[2]/60+x[3]/3600)
est$Lat<-as.numeric(res)

head(est)
sapply(est, class)

#Combinamos la tabla de ruido con la de estaciones
ruido<-merge(ruido,est,by="Estación")

colnames(ruido)[colnames(ruido)==" T"] <- "Total"
colnames(ruido)[colnames(ruido)==" D"] <- "Diurno"
colnames(ruido)[colnames(ruido)==" E"] <- "Vespertino"
colnames(ruido)[colnames(ruido)==" N"] <- "Nocturno"

#Extraemos mapa de Madrid
library(ggplot2)
mad <- geocode('Madrid, España', source = "google") # coordenadas de Madrid
map.mad <- get_map(location = as.numeric(mad),
                   color = "color",
                   maptype = "roadmap",
                   scale = 2,
                   zoom = 11)

#Pintamos el nivel total de ruido
ggmap(map.mad) +geom_point(aes(x = -Lon, y = Lat, colour = Total ), #Ojo que hay que poner -Lon por ser dirección Oeste
                           data = ruido) + ggtitle("Ruido Madrid")




