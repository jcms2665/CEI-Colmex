
#--------------------------------------------------------------------------------
# Tema:       Regresion lineal
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      09-02-2023
# Datos:      Latinobarometro 2018
#             https://www.latinobarometro.org/latContents.jsp
#             Descargar: Latinobarómetro 2018 .Rdata
# Github:     
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Cargar base de datos
#       4. Regresion lineal
#           4.1 Variable dependiente/objetivo
#           4.2 Variables independientes/covariables
#           4.3 Normalizacion 
#           4.4 Ajuste del modelo
#       5. Supuestos
#           5.1 Homocedasticidad
#           5.2 Normalidad de residuos
#       6. Datos de encuestas
#           6.1 Definir el esquema de muestreo
#           6.2 Correr el modelo con el esquema de muestreo

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign)                 
library(questionr)              
library(tidyverse) 
library(dplyr) 
library(stats) 
library(survey) 


#2. Directorio de trabajo
setwd("")


#3. Cargar base de datos

latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")


#4. Regresion lineal

# Pregunta guia:
# Cuales son los factores que influyen en la ideologia politica?


    #4.1 Variable dependiente/objetivo
    
    # Ideologia: Escala Izquierda-Derecha (P22ST)
    # 0=Izquierda, 10 Derecha
    
    class(latino$P22ST)
    latino$P22ST<-as.numeric(latino$P22ST)
    class(latino$P22ST)
    
    table(latino$P22ST)
    
    
    # Filtro para los casos validos (0-10)
    l.1<-latino %>% filter(P22ST>=0)
    table(l.1$P22ST)
    
    
    #4.2 Variables independientes/covariables
    
    # Nivel socioeconomico (P10STC.A)
    # 0-Pobre, 10-Rico
    table(l.1$P10STC.A)
    
    # Edad de termino de estudios (S9)
    table(l.1$S9)
    
    # Creamos una nueva base con casos validos
    latino$P10STC.A<-as.numeric(latino$P10STC.A)
    latino$S9<-as.numeric(latino$S9)
    
    # Filtro para los casos validos (0-10)
    l.1<-filter(l.1,P10STC.A>=0 & S9>=0)
    
    
    # Seleccionar las variables
    l.reg<-l.1%>%select(P22ST,P10STC.A, S9, IDENPA, WT)
    
    
    # Con rename se cambia el nombre de las variables
    l.reg<-l.reg%>%rename(ideol=P22ST, pob=P10STC.A, educ=S9)

    
    #4.3 Normalizacion 
    l.reg<-mutate(l.reg, ideol.nor=(ideol-min(ideol))/(max(ideol)-min(ideol)))
    l.reg<-mutate(l.reg, pob.nor=(pob-min(pob))/(max(pob)-min(pob)))
    l.reg<-mutate(l.reg, educ.nor=(educ-min(educ))/(max(educ)-min(educ)))
    
    
    # Selecci?n de variables normalizadas
    l.reg.nor=l.reg %>% select("ideol.nor","pob.nor","educ.nor","IDENPA","WT")
    
    cor(l.reg.nor[,1:3])
    
    
    #4.4 Ajuste del modelo
    regresion <- lm(ideol.nor ~ pob.nor+educ.nor, data = l.reg.nor)
    summary(regresion)


#5. Supuestos

    #5.1 Homocedasticidad
    
    # Homocedasticidad 
    # la varianza del error condicional a las variables explicativas es 
    # constante a lo largo de las observaciones
    
    residuos <- rstandard(regresion)
    valores.ajustados <- fitted(regresion)
    plot(valores.ajustados, residuos)
    
    # Si se observa un patron, por lo que el supuesto de homocedasticidad 
    # puede no cumplirse
    
    #5.2 Normalidad de residuos
     
    qqnorm(residuos)
    qqline(residuos)
    
    # Los puntos estan bastante alineados, 
    # la normalidad parece aceptable.


#6. Datos de encuestas
    
    #6.1 Definir el esquema de muestreo
    sv <- svydesign(id=~1, weights=~WT,strata=~IDENPA, nest=TRUE, survey.lonely.psu = "adjust", data=l.reg.nor)
    
    
    #6.2 Correr el modelo con el esquema de muestreo
    summary(svyglm(ideol.nor~pob.nor+educ.nor, design=sv))



#-------------------------------------------
# RETO-1:
# Ajusta un modelo de regresion para
# algun pais (IDENPA):
# IDENPA  32  Argentina
# IDENPA  76  Brasil
# IDENPA  724 Espana
# IDENPA  858 Uruguay
#-------------------------------------------




