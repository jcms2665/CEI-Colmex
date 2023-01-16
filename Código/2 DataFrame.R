
#--------------------------------------------------------------------------------
# Tema:       DataFrame
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      enero 2023
# Datos:      I Trimestre ENOE nueva, INEGI.
#             https://www.inegi.org.mx/programas/enoe/15ymas/#Microdatos
# Github:     https://github.com/jcms2665/CEI-Colmex
      

# Contenido             

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Exploracion inicial
#           4.1 Etiquetar variables
#           4.1 Nuevas variables etiquetadas
#       5. Recodificar variables
#           5.1 Validar tipo de dato
#           5.2 Cambiar formato
#           5.3 Crear nueva variable 
#           5.4 Generar rangos 
#       6. Subconjuntos de datos
#           6.1 Variables (columnas)
#           6.2 Casos (filas)
#           6.3 Casos (filas) y variables (columnas)

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)
library(questionr)
library(ggplot2)
library(dplyr)    # aquí esta pipes %>%
library(survey)


#---------------------------------------
# NOTA:
# Si aparece un error, revisa en la
# pestana "packages" si el paquete 
# este instalado.
# 
# install.package()

#---------------------------------------


#2.  Directorio de trabajo

getwd()
setwd("C:/Users/JCMARTINEZ/OneDrive - El Colegio de México A.C/5. Proyectos/2022/74. Flacso-R-2022/FLACSO-R-2022-main/Datos/ENOE")
---------------------------------------
# NOTA:
# Si aparece un error, revisa el tipo
# de diagonal que tienes. 
# Se debe usar "/" o "//"
#---------------------------------------



#3.  Importar datos


    #3.1 Bases en .dbf
    enoe<-read.dbf("ENOEN_SDEMT121.dbf")%>%data.frame()

    #3.1 Datos de SPSS (.sav)
    enoe.2<-read.spss("ENOEN_SDEMT121.sav")%>%data.frame()
    
    #3.3 Datos de Excel (.csv)
    enoe<-read.csv("sdemt120.csv") %>% data.frame()
    
    #3.4 Datos de Stata (.dta)
    enoe.4<-read.dta("ENOEN_SDEMT121.dta")%>%data.frame()
    
            
    #---------------------------------------
    # NOTA:
    # El operador %>% sirve para vincular
    # propiedades y funciones
    #---------------------------------------
    
    
    rm("enoe.2", "enoe.3", "enoe.4")                               # rm sirve para quitar elementos

    
    
#4.  Exploracion inicial

    names(enoe)
    head(enoe,2)

    wtd.table(enoe$sex)

    table(enoe) 


  #4.1  Etiquetar variables
  

    # Para etiquetar una variable se usa la funcion "factor" con
    # tres argumentos: 
    
    #  i.   La variable que se va etiquetar:  enoe$SEX
    #  ii.  Los valores (levels)              c(1,2)
    #  iii. Las etiquetas (labels)            c("Hombre","Mujer")
    
    
    enoe$sex <- factor(enoe$sex,levels = c(1,2),labels = c("Hombre","Mujer"))
    wtd.table(enoe$sex)
    
    
    #-----------------------------------
    # NOTA:
    # Al etiquetar una variable, se 
    # sustituye su contenido original 
    #-----------------------------------



  # 4.2 Etiquetar y crear nuevas variables
    
    # CLASE2 = 1, Poblacion ocupada
    wtd.table(enoe$clase2)
    
    # Codigos: 1=Ocupado, 2=Desocupado, 3=Disponible, 4=No disponible
    enoe$ocup <- factor(enoe$clase2,levels = c(1,2,3,4),labels = c("Ocupado","Desocupado","Disponible","No disponible"))
    
    wtd.table(enoe$ocup, weights = enoe$fac)                      # Con "weights=" se ponderan los datos 
    
    wtd.table(enoe$ocup, weights = enoe$fac)%>%prop.table()       # Frecuencias ponderadas 
    
    

#5. Recodificar variables

    #5.1 Validar tipo de dato
    
    # EDA = Edad
    class(enoe$eda)
    
    #-----------------------------------
    # NOTA:
    # El 80% de los errores ocurren 
    # por no considerar el tipo de 
    # variable que tenemos.
    #-----------------------------------
    

    #5.2 Cambiar formato
    
    enoe$eda <-enoe$eda %>% as.character() %>% as.numeric()
    
    #-------------------------------------------------
    # NOTA:
    # El operador %>% encadena los procesos de 
    # izquierda a derecha:
    # i.    Toma la variable "edad" de la base "enoe"
    # ii.   Luego conviertela en caracter
    # iii.  Despues pasala a numero
    
    #-------------------------------------------------
    
    class(enoe$EDA)
    
  
    #5.3 Crear nueva variable 
    
    length(enoe)                             # numero de variables
    enoe$edad_recod<-0                       # agregar una variable
    length(enoe)
  
  
    #5.4 Generar rangos  
    
    enoe$edad_recod[enoe$eda >= 0 & enoe$eda <=30] <- 1
    enoe$edad_recod[enoe$eda >= 31 & enoe$eda <=59] <- 2
    enoe$edad_recod[enoe$eda >= 60] <- 3
  
    wtd.table(enoe$edad_recod)

    enoe$edad_recod<-factor(enoe$edad_recod,levels = c(1,2,3),labels = c("Menos de 30","31-59","60 y +"))
    wtd.table(enoe$edad_recod)
    wtd.table(enoe$edad_recod)%>%prop.table()

    #-----------------------------------
    # NOTA:
    # Si se corre el proceso nuevamente 
    # se borra el etiquetado.
    #----------------------------------- 
        
       

#6. Subconjuntos 

    #6.1 Seleccionar VARIABLES (columnas)
    enoe.1 <-enoe %>%select("sex", "eda","pos_ocu","fac", "upm")


    #6.2 Seleccionar CASOS (filas)
    #    Solo vamos a considerar mujeres que trabajan
    #    SEX=Sexo, CLASE2=Condicion de ocupacion
    
    enoe.2 <-enoe %>% filter(sex=="Mujer" & clase2==1)       # (Ver el punto 4.1)
    

    #6.3 Seleccionar CASOS (filas) y VARIABLES (columnas)
    enoe.3 <- enoe %>%select("sex", "eda","pos_ocu","fac", "upm", "clase2")%>%filter(sex=="Mujer" & clase2==1)
    
    #------------------------------------------
    # NOTA:
    # Por que ocurre el error? 
    #------------------------------------------
    
    enoe.3 <- enoe%>%filter(sex=="Mujer" & clase2==1)%>%select("sex", "eda","pos_ocu","fac", "upm")

    
    rm("enoe.1","enoe.2")          #Quitamos las bases que no usaremos
    
    
