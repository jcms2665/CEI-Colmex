
#--------------------------------------------------------------------------------
# Tema:       Graficas con variables discretas y continuas
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      09-02-2023
# Datos:      Shape de Estados, INEGI.
#             CONACYT (casos diarios):
#             https://datos.covid-19.conacyt.mx/#DownZCSV
      

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Visualizar la base de datos asociada shape (No es el shape)
#       4. Crear la variable discreta 
#           4.1 Numero de casos de covid
#           4.2 Formato
#           4.3 Dividir la muestra en terciles   
#           4.4 Rangos: Bajo, Medio y Alto
#           4.5 Filtrar la base
#           4.6 Merge
#           4.7 Sustituir la base
#       5. Cargar el SHAPE
#       6. Agrega una capa llamada "mapa"
#       7. Mapa con variable discreta
#       8. Mapa con variable continua

#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign); library(ggplot2); library(dplyr); library(tidyverse); library(sf)


#2.  Directorio de trabajo (SHAPE)

ruta="..........."

setwd(paste(ruta,"/6_Shape", sep = ""))


#3.  Visualizar la base de datos asociada shape (No es el shape)
base_mx=read.dbf("00ent.dbf")


base_mx=select(base_mx, -c("rango","SUMA"))


#-----------------------------------
# NOTA:
# Necesitamos identificar una variable 
# que sirva como LLave para unir dos
# tablas.
#-----------------------------------

#4.   Crear la variable a graficar

      #4.1 Buscar la base de datos del numero de casos de covid
      setwd(paste(ruta,"/6_Datos", sep = ""))      
      covid=read.csv("Casos_Diarios_Estado_Nacional_Confirmados_20230209.csv")
      
            #4.1.1 Crear la variable SUMA
            covid=covid%>%mutate(SUMA=rowSums(.[4:1083]))    
            
            #4.1.2 Quitar el total
            
            covid=covid%>%filter(cve_ent>=1)
            
            #4.1.3 Seleccionar la variable de estado y SUMA
            covid=covid%>%select(cve_ent, SUMA)
            
      #4.2 Agregar un 0 para tener el mismo formato
      covid=covid%>%mutate(cve_ent=formatC(cve_ent, width = 2, format = "d", flag = "0"))
      covid=covid%>%data.frame()
      
      
      #4.3 Dividir la muestra en terciles
      quantile(covid$SUMA, probs = c(.33,.66,1))

      
      #4.4 Crear 3 rangos: Bajo, Medio y Alto
      covid$rango=""
      covid$rango[covid$SUMA<=118817]<-"Bajo"
      covid$rango[covid$SUMA>118817 & covid$SUMA<=184605]<-"Medio"
      covid$rango[covid$SUMA>184605]<-"Alto"
      
      #4.5 De la base covid solo nos quedamos con cve_ent y rango
      covid=covid %>% select("cve_ent", "rango", "SUMA")
      

      
      #4.6 Unir tabla de rangos con el dbf del shape
      # Indicar la columna Llave en ambos casos
      base_mx=base_mx%>%left_join(covid,
                  by = c("CVE_ENT" = "cve_ent"))

      #4.7 Guardamos la base base_mx en donde esta el shp
      #-----------------------------------
      # NOTA:
      # Con este procedimiento se sustituye 
      # la base original.
      #-----------------------------------
      
      
      setwd(paste(ruta,"/6_Shape", sep = ""))
      write.dbf(base_mx,"00ent.dbf")
      rm("base_mx","covid")

#5. Cargar el SHAPE


mx=st_read("00ent.shp")

#6. Agrega una capa llamada "mapa"
mapa=ggplot(mx) + geom_sf() 
mapa



#7. Mapa con variable discreta
mx%>%
  ggplot(aes(fill = rango)) +
  geom_sf(colour = "white", size = 0.07) +
  labs(title = "Covid-19 en MX") +
  scale_fill_brewer("Total de casos", palette = "Reds") +
  theme_bw()


#8. Mapa con variable continua
mx%>%
  ggplot(aes(fill = SUMA)) +                                      # En aesthetic fill indicamos la columna de interés: SUMA
  geom_sf(colour = "grey75", size = 0.07) +                       # Ajustar color y grosor de las lineas
  labs(title = "Covid-19 en MX") +                                # Titulos
  scale_fill_gradient("Casos", high = "red", low = "white") +     # Relleno y el titulo de la leyenda
  theme_bw() 
