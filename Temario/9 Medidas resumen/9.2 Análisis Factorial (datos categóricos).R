
#--------------------------------------------------------------------------------
# Tema:       Analisis Factorial - Datos categoricos
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      09-02-2023
# Datos:      Latinobarometro 2018
#             https://www.latinobarometro.org/latContents.jsp
#             Descargar: Latinobarómetro 2018 .Rdata
# Github:     
# Notas:       

# Contenido

#     0. Entorno de trabajo
#     1. Cargar librerias
#     2. Importar base
#     3. Seleccion de variables
#     4. Filtrar la base
#     5. Respuestas validas y missings
#     6. Tabulados
#     7. Correlacion policorica
#     8. Ajuste del modelo

#--------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)     


#1. Cargar librerias
library(foreign)
library(ggplot2)
library(psych)
library(dplyr)
library(psych)
library(tidyr)


#2. Importar base
setwd("")
latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")


#3. Seleccion de variables

# P15STGBSC.A --- Confianza en el Ejercito
# P15STGBSC.B --- Confianza en la Policia
# P15STGBSC.C --- Confianza en la Iglesia
# P15STGBSC.D --- Confianza en el Congreso
# P15STGBSC.E --- Confianza en el Gobierno

# Respuestas:
# 1.- Mucha confianza
# 2.- Algo de confianza
# 3.- Poca confianza
# 4.- Ninguna confianza
# -1-.- No sabe
# -2-.- No responde
# -4-.- No preguntada


#4. Filtrar la base

# Filtrar paises: Mexico y Brasil
dat<-latino%>%filter(as.numeric(IDENPA)==862 | as.numeric(IDENPA)==76)


# Filtrar variables
dat<-dat %>% select("P15STGBSC.A","P15STGBSC.B","P15STGBSC.C","P15STGBSC.D", "P15STGBSC.E")

# Renombrar las variables
names(dat)<-c("Ejercito","Policia","Iglesia","Congreso","Gobierno")


#5. Respuestas validas y missings
dat[dat <=0] <- NA
dat<-dat%>%drop_na()


#6. Tabulados
knitr::kable(table(dat$Ejercito))
knitr::kable(table(dat$Iglesia))


#7. Correlacion policorica 
poly_cor = polychoric(dat)
rho = poly_cor$rho
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Correlacion tetracorica", show.legend = FALSE)


#8. Ajuste del modelo
poly_model = fa(dat, nfactor=2, cor="poly", fm="mle", rotate = "none")
poly_model$loadings
fa.diagram(poly_model)


























