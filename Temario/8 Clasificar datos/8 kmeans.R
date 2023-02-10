
#--------------------------------------------------------------------------------
# Tema:       Analisis de conglomerados: k-meadias
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      09-02-2023
# Datos:      Encuesta Nacional de Ocupaci?n y Empleo, INEGI
#             https://www.inegi.org.mx/programas/enoe/15ymas/#Microdatos
# Github:     
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Analisis de conglomerados
#           4.1 Normalizar datos
#           4.2 K-medias
#       5. Interpretacion

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(survey) 
library(car)
library(tidyverse)
library(stats)


#2.  Directorio de trabajo
setwd("")


#3.  Importar datos
sdemt<-data.frame(read.csv("SDEMT120.csv", header = TRUE))


#4. Analisis de conglomerados

# El objetivo de esta tecnica es hacer grupos homogeneos al interior
# y heterogeneos al exterior
ocupados<-filter(sdemt, clase2==1)


ac.base<-filter(ocupados, clase2==1)%>%
  select(anios_esc,hrsocup, ingocup)


# Validar tipo de datos
class(ac.base$anios_esc)
class(ac.base$hrsocup)
class(ac.base$ingocup)


# Cambiar a numerico
ac.base$anios_esc<-as.numeric(ac.base$anios_esc)
ac.base$hrsocup<-as.numeric(ac.base$hrsocup)
ac.base$ingocup<-as.numeric(ac.base$ingocup)


    #4.1 Normalizar datos
    ac.base<-mutate(ac.base, es.nor=(anios_esc-min(anios_esc))/(max(anios_esc)-min(anios_esc)))
    ac.base<-mutate(ac.base, hr.nor=(anios_esc-min(hrsocup))/(max(hrsocup)-min(hrsocup)))
    ac.base<-mutate(ac.base, ing.nor=(anios_esc-min(ingocup))/(max(ingocup)-min(ingocup)))
    
    
    # Generar un subconjunto de datos
    ac.base.nor<-ac.base[, 4:6]
    
    
    #4.2 K-medias
    fit <- kmeans(ac.base.nor, 3)
    
    
    # Ahora se pega la variable de cluster a la base
    ocupados <- data.frame(ocupados, fit$cluster)
    
    table(ocupados$fit.cluster)
    

#5. Interpretacion

# Etiquetar las variables cs_p13_1 y e_con
table(ocupados$cs_p13_1, ocupados$fit.cluster)
table(ocupados$e_con, ocupados$fit.cluster)





