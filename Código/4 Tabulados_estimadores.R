
#---------------------------------------------------------------------------------------


# Contenido

#0. Preparar entorno de trabajo
#1. Cargar librerias
#2. Directorio de trabajo
#   2.1. Convertir las variables a numéricas
#   2.2. Filtrar los casos

# Cálculo de la varianza de los estimadores poblacionales

#3. Promedio
#	  3.1 Escenario 1: Muestreo aleatorio simple
#	  3.2 Escenario 2: Muestreo por conglomerados
#	  3.3 Escenario 3: Muestreo estratificado
#	  3.4 Escenario 4: Muestreo complejo
#	  3.5 Comparación de los 4 escenarios
#4. Totales
#5. Tasas

# Modelos estadísticos usando muestras complejas

#6. Modelo regresión lineal
#	  6.1 Series de Taylor
#	  6.2 Bootstrap


#---------------------------------------------------------------------------------------


#0. Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(tidyverse) 
library(dplyr) 
library(stats) 
library(survey) 
library(readr)
library(sampling)



#2. Directorio de trabajo

setwd("...")


SDEMT222=read_csv("ENOEN_SDEMT222.csv", show_col_types = FALSE)


# Para iniciar con el análisis conviene identificar las variables que se van a utilizar 
# y adecuar el formato. En este caso la población de referencia es la población ocupada (CLASE2) 
# y los variables que se van a utilizar son sexo (SEX), edad (EDA) y el ingreso (ING7C). 
# Además, para que un registro de la encuesta sea válido, debe de tratarse de un residente 
# habitual (C_RES) con entrevista completa (R_DEF). Para fines prácticos, conviene que estas 
# variables sean numéricas, por lo tanto se adecúan con este formato:

    #2.1. Convertir las variables a numéricas
    
    #Convertir las variables a numéricas
    SDEMT222$r_def=as.numeric(as.character(SDEMT222$r_def))
    SDEMT222$c_res=as.numeric(as.character(SDEMT222$c_res))
    SDEMT222$eda=as.numeric(as.character(SDEMT222$eda))
    SDEMT222$sex=as.numeric(as.character(SDEMT222$sex))
    SDEMT222$clase2=as.numeric(as.character(SDEMT222$clase2))
    SDEMT222$hrsocup=as.numeric(as.character(SDEMT222$hrsocup))


    
    # Para tener a la población de referencia se crea una base únicamente con los registros válidos,
    # es decir, con los ocupados que satisfacen el criterio de ser residentes habituales con entrevista 
    # completa y mayores de 15 años.

    #2.2. Filtrar los casos
    SD=SDEMT222[which(SDEMT222$clase2 == 1 & 
                         SDEMT222$eda>=15 & SDEMT222$eda<=98 &
                         SDEMT222$r_def==0 &
                         (SDEMT222$c_res==1 | SDEMT222$c_res==3)),]


# Estimación de la varianza por linealización por Series de Taylor

#3. Promedio
    
    
    #3.1 Escenario 1: Muestreo aleatorio simple
    
    # Paso 1. Definir y guardar el esquema de muestreo con la función svydesign
    ds_enoe1=svydesign(id=~1,weight=~fac_tri,data=SD)
    
    # Paso 2. Obtener el estimador (promedio de la variable HRSOCUP) y su efecto de diseño con la función svymean
    
    svy1=svymean(~hrsocup, ds_enoe1, deff=TRUE)
    svy1
    
    # Paso 3. Se calcula el coeficiente de variación para determinar la confiabilidad. 
    # De acuerdo al INEGI, si dicho coeficiente es menor a 15%, entonces el dato es de buena calidad.
    
    cv1=cv(svy1)*100
    cv1
    
    
    #3.2 Escenario 2: Muestreo por conglomerados
    
    ds_enoe2=svydesign(id=~upm,weight=~fac_tri,data=SD)
    svy2=svymean(~hrsocup, ds_enoe2, deff=TRUE)
    svy2
    cv2=cv(svy2)*100
    cv2
    
    
    #3.3 Escenario 3: Muestreo estratificado
    
    ds_enoe3=svydesign(id=~1, strata=~est_d_tri, weight=~fac_tri, data=SD, nest=TRUE)
    svy3=svymean(~hrsocup, ds_enoe3, deff=TRUE)
    svy3
    cv3=cv(svy3)*100
    cv3
    
    
    #3.4 Escenario 4: Muestreo complejo
    
    ds_enoe4=svydesign(id=~upm, strata=~est_d_tri, weight=~fac_tri, data=SD, nest=TRUE)
    svy4=svymean(~hrsocup, ds_enoe4, deff=TRUE)
    svy4
    cv4=cv(svy4)*100
    cv4
    
    
    #3.5 Comparación de los 4 escenarios
    
    x1=c(cv1,cv2,cv3, cv4);x2=c("MAS","Congl","Estr","Complex");res = rbind(x1,x2)
    res


#4. Totales

# Para calcular el coeficiente de variación en tabulados, el primer paso es seleccionar 
# la población de referencia. En este ejemplo se analiza el nivel de escolaridad de las mujeres ocupadas.

SD2=SD[which(SD$sex == 2),]

#Definir el esquema de muestreo: Estratificado y por conglomerados
ds_enoe8=svydesign(id=~upm, 
                    strata=~est_d_tri, 
                    weight=~fac_tri, 
                    data=SD2, 
                    nest=TRUE)

#Se define el método para el caso de 1 UPM en los estratos
options(survey.lonely.psu="adjust")

#Obtener la estimación
svy8=svytotal(~factor(ing7c), ds_enoe8, deff=TRUE)

#Calcular el coeficiente de variación
cv8=cv(svy8)*100

#Resultados
svy8



#5. Tasas

# Para hacer este ejercicio, se estima la Tasa de trabajo asalariado, 
# la cual toma como referncia a la población ocupada. 
# Para calcular este indicador, se requiere la variable remune2c


#Se cambia el formato de la variable
SD$remune2c=as.numeric(as.character(SD$remune2c))

#Se crea una variable binaria
SD$Tasa_trabajo_asalariado=0
SD$Tasa_trabajo_asalariado[SD$remune2c==1]=1

#Definir el esquema de muestreo: Estratificado y por conglomerados
ds_enoe6=svydesign(id=~upm, 
                    strata=~est_d_tri, 
                    weight=~fac_tri, 
                    data=SD, 
                    nest=TRUE)

#Obtener la estimación
svy6=svymean(~Tasa_trabajo_asalariado, ds_enoe6, deff=TRUE)

#Calcular el coeficiente de variación
cv6=cv(svy6)*100

#Resultados
svy6


#6. Modelos de regresión lineal

#6.1 Series de Taylor

ds_enoe=svydesign(id=~upm, 
                   strata=~est_d_tri, 
                   weight=~fac_tri, 
                   data=SD, 
                   nest=TRUE)
options(survey.lonely.psu="adjust")

glmsvy1 = svyglm(hrsocup~sex+niv_ins+e_con+pos_ocu+eda, design=ds_enoe)
summary(glmsvy1, df.resid = degf(ds_enoe))

glmsvy2 = glm(hrsocup~sex+niv_ins+e_con+pos_ocu+eda, data=SD)
summary(glmsvy2)


#6.2 Bootstrap
# Puede tardar dependiendo la potencia de la máquina
boot_design=as.svrepdesign (ds_enoe, type="bootstrap",replicates=100)
options(survey.lonely.psu="adjust")

glmsvy3 = svyglm(hrsocup~sex+niv_ins+e_con+pos_ocu+eda, design=boot_design)
summary(glmsvy3, df.resid = degf(boot_design))



