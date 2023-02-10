
#--------------------------------------------------------------------------------
# Tema:       Regresion logistica
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
#       4. Pregunta guia
#       5. Regresion logistica
#           5.1 Variable dependiente/objetivo
#           5.2 Variables independientes/covariables
#           5.3 Ajustar el modelo
#           5.4 Resultados: momios
#       6. Supuestos
#           6.1 Likelihood ratio
#           6.2 clasificaci?n predicha y observaciones
#       7. Datos de encuestas
#           7.1 Definir el esquema de muestreo
#           7.2 Correr el modelo con el esquema de muestreo

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
library(vcd)


#2. Directorio de trabajo
setwd("")


#3. Cargar base de datos
latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")


#4. Pregunta guia

# Cuales son los factores que influyen para que 
# las personas esten satisfechas -o no- con su vida?


#5. Regresion logistica

    #5.1 Variable dependiente/objetivo
    
    # Grado de satisfaccion con la vida (P1STC)
    # 1.- Muy satisfecho
    # 2.- Bastante satisfecho
    # 3.- No muy satisfecho
    # 4.- Para nada satisfecho
    # -1-.- No answer/Refused
    # -4-.- No preguntada
    
    table(latino$P13STGBS.A)
    
    # Cambiar a numerico para crear la variable binaria
    class(latino$P13STGBS.A)
    latino$P13STGBS.A<-as.numeric(as.character(latino$P13STGBS.A))
    class(latino$P13STGBS.A)
    
    # Filtrar solo los casos validos
    s1<-filter(latino,P13STGBS.A>=0)
    table(s1$P13STGBS.A)
    
    # Creacion de la variable binaria (0/1)
    s1$satisfecho=0
    s1$satisfecho[s1$P13STGBS.A==1 |s1$P13STGBS.A==2]<-1
    s1$satisfecho[s1$P13STGBS.A==3 |s1$P13STGBS.A==4]<-0
    
    table(s1$satisfecho)
    
    
    #5.2 Variables independientes/covariables
    
    # Confianza en la Iglesia (P15STGBSC.C)
    table(s1$P15STGBSC.C)
    
    
    # Aprueba al presidente (P20STGBSC)
    table(s1$P20STGBSC)
    
    # SEXO
    table(s1$SEXO)
    
    # EDAD
    table(s1$EDAD)
    
    # Renombrar variables
    s1<-s1%>%rename(igl=P15STGBSC.C, pres=P20STGBSC, sexo=SEXO)
    
    
    # Seleccionar las variables
    s1<-s1%>%select(satisfecho, igl, pres, sexo, EDAD, WT, IDENPA)
    
    
    # Filtrar casos validos
    s1<-filter(s1, igl>0 & pres>0)
    
    # Etiquetar variables categoricas
    s1$igl<-factor(s1$igl, levels =c(1,2,3,4), labels=c("Mucha", "Algo", "Poco", "Nada"))
    s1$sexo<-factor(s1$sexo, levels =c(1,2), labels=c("Hombre", "Mujer"))
    s1$pres<-factor(s1$pres, levels =c(1,2), labels=c("Aprueba", "No aprueba"))
    
    s1$satisfecho<-factor(s1$satisfecho, levels =c(0,1), labels=c("No Satisfecho", "Satisfecho"))
    s1$edad<-as.numeric(s1$EDAD)
    
    
    
    #5.3 Ajustar el modelo
    
    reg.log <- glm(satisfecho ~ igl+pres + sexo+edad, 
                   data = s1, family = "binomial")
    summary(reg.log)
    
    
    #5.4 Resultados: momios
    
    momios<-exp(coefficients(reg.log))%>%data.frame()
    View(momios)


#6. Supuestos

    #6.1 Likelihood ratio
    
    
    # El modelo es ?til si es capaz de explicando las observaciones 
    # respecto al modelo nulo (sin predictores).
    
    anova(reg.log, test ='Chisq')
    
    
    #6.2 clasificaci?n predicha y observaciones
    
    predicciones <- ifelse(test = reg.log$fitted.values > 0.5, yes = 1, no = 0)
    matriz_confusion <- table(reg.log$model$satisfecho, predicciones,
                              dnn = c("observaciones", "predicciones"))
    matriz_confusion
    
    mosaic(matriz_confusion, shade = T, colorize = T,
           gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
    
    
    # El modelo es capaz de clasificar correctamente
    # (13372+106)/(13372+106+4529+89) = 74% de los casos


#7. Datos de encuestas

    #7.1 Definir el esquema de muestreo
    sv <- svydesign(id=~1, weights=~WT,strata=~IDENPA, nest=TRUE, survey.lonely.psu = "adjust", data=s1)
    
    
    #7.2 Correr el modelo con el esquema de muestreo
    summary(svyglm(satisfecho ~ igl+pres + sexo+edad, design=sv, family = quasibinomial))



#-------------------------------------------
# RETO-2:
# Ajusta un modelo de regresion para
# algun pais (IDENPA):
# IDENPA  32  Argentina
# IDENPA  76  Brasil
# IDENPA  724 Espana
# IDENPA  858 Uruguay
#-------------------------------------------

