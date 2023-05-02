
#--------------------------------------------------------------------------------
# Tema:       Analisis de Componentes Principales
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
#       4. Analisis de componentes principales
#       5. Ajuste del modelo
#       6. Resumen
#           6.1 Grafica de sedimentacion
#           6.2 Interpretacion 
#       7. Datos de encuestas
#           7.1 Definir el esquema de muestreo
#           7.2 Correr el modelo con el esquema de muestreo

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
setwd("C:/Users/Ciencias de Datos-2/OneDrive - El Colegio de México A.C/1 Proyectos/2023/84 Curso-Colmex-R/2 Temario/9 Medidas resumen/9 Datos")


#3.  Importar datos
sdemt<-data.frame(read.csv("SDEMT120.csv", header = TRUE))


#4. Analisis de componentes principales

#4.1 Filtrar los datos
mujeres<-filter(sdemt, sex==2)
acp.base<-mujeres%>% select(anios_esc,hrsocup, ingocup, n_hij, eda, upm, est_d, fac)


# Validar tipo de datos
class(acp.base$anios_esc)
class(acp.base$hrsocup)
class(acp.base$ingocup)
class(acp.base$n_hij)
class(acp.base$eda)

acp.base$anios_esc<-as.numeric(acp.base$anios_esc)
acp.base$hrsocup<-as.numeric(acp.base$hrsocup)
acp.base$ingocup<-as.numeric(acp.base$ingocup)
acp.base$n_hij<-as.numeric(acp.base$n_hij)
acp.base$eda<-as.numeric(acp.base$eda)


#5. Ajuste del modelo
Acomp<-prcomp(na.omit(acp.base[, 1:5]), scale=TRUE)


#6. Resumen
summary(Acomp)


    #6.1 Grafica de sedimentacion
    plot(Acomp,type="lines")
    
    
    #6.2 Interpretacion 
    Acomp$rotation%*%diag(Acomp$sdev)


#7. Datos de encuestas

    #7.1 Definir el esquema de muestreo
    sv=svydesign(id=~upm, weights=~fac,strata=~est_d, nest=TRUE, survey.lonely.psu = "adjust", data=sdemt)
    
    
    #7.2 Correr el modelo con el esquema de muestreo
    acp=svyprcomp(~anios_esc+hrsocup+ingocup+n_hij+eda, design=sv,scale=TRUE,scores=TRUE)
    acp
