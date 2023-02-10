
#--------------------------------------------------------------------------------
# Tema:       Repaso de DataFrame
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      15-08-2021
# Datos:      Instituto V-Dem
#             https://www.v-dem.net/es/
#             https://www.v-dem.net/en/data/archive/previous-data/v-dem-dataset/
# Github:     
# Notas:       

# Contenido             

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Exploracion inicial
#       5. Seleccion de variables
#       6. Subconjuntos 
#       7. Formato

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(ggplot2) 
library(car)
library(dplyr)
library(tidyverse)
library(devtools)


# Paquete para hacer tablas con formato
install.packages("kableExtra")
devtools::install_github("haozhu233/kableExtra")
library(kableExtra)


#2.  Directorio de trabajo
setwd("")


#3.  Importar datos
democracia<-read.csv("V-Dem-DS-CY-v7.csv")%>%data.frame()


#4.  Exploracion inicial
dim(democracia)                                   # Dimension de la base 

 
#5.  Seleccion de variable 

# v2x_polyarchy = En que medida se alcanza el ideal de democracia electoral en su sentido mas amplio?
# Es un indice ponderado que considera:
#   - Libertad de asociacion
#   - Elecciones limpias
#   - Libertad de expresion
#   - Funcionarios electos
# Su escala es de 0 a 1 (Mas cercano a 1, mejor)

class(democracia$v2x_polyarchy)
democracia$v2x_polyarchy <-democracia$v2x_polyarchy %>% as.numeric()
class(democracia$v2x_polyarchy)


#5.1 Medidas de tendencia central 
  
    min(democracia$v2x_polyarchy)
    
    # Por que no corre ? 

#5.2 Quitar missings
    demo1<-democracia%>%drop_na(v2x_polyarchy)
    
    min(demo1$v2x_polyarchy)
    
    max(demo1$v2x_polyarchy)
    
    mean(demo1$v2x_polyarchy)
    
    median(demo1$v2x_polyarchy)  

    max(demo1$v2x_polyarchy)
    
    
#6. Subconjuntos 
#   Vamos a explorar 3 paises: Argentina, Brasil y México 
    
    # Total de paises
    table(demo1$country_name)
    
    # Hacer un subconjunto con 3 caracteristicas:
        # i.    Paises: Argentina, Brasil, México
        # ii.   El dato de 2000-2015
        # iii.  Solo la variable v2x_polyarchy

    
    # Primero, cambiar a numericas las variables country_id y year
    # para poder hacer los filtros
    demo1$country_id=demo1$country_id %>% as.numeric()
    demo1$year=demo1$year%>%as.numeric()
    
    
    # Segundo, hacer los filtros
    demo2<-demo1%>%filter(country_id==37 | country_id==19 | country_id==3)%>%
      filter(year>=2000 & year<=2015) %>%
      select("country_name","year","v2x_polyarchy")
    
    # Cambiar nombre de variables
    names(demo2)=c("Pais","Tiempo","Indice")
    
    # Estadisticos por pais
    # Guardamos los tres tabulados para después darle formato
    # aggregate   Sirve para agregar los datos
    # list        Indica la variable de los grupos (paises)
    
    t.1=aggregate(demo2$Indice, by = list(demo2$Pais), min)
    t.2=aggregate(demo2$Indice, by = list(demo2$Pais), mean)
    t.3=aggregate(demo2$Indice, by = list(demo2$Pais), max)
    

#7. Formato 
    
    # Para darle formato usamos tres argumentos:
    #i.     kbl                 Genera el tabulado y asigna un nombre
    #ii.    <center>            Centra el titulo
    #iii.   kable_classic       Da formato a las lineas del cuadro
    
    t.1 %>%
      kbl(caption = "<center>Tabla 1. indice de democracia 2000-2015</center>",
          col.names = c("Pais","Minimo")) %>%
      kable_classic(full_width = F, html_font = "Times New Roman")
    
    
    t.2 %>%
      kbl(caption = "<center>Tabla 2. indice de democracia 2000-2015</center>",
          col.names = c("Pais","Promedio")) %>%
      kable_classic_2(full_width = F, html_font = "Arial")
  
      
    t.3 %>%
      kbl(caption = "<center>Tabla 3. indice de democracia 2000-2015</center>",
          col.names = c("Pais","Maximo")) %>%
      kable_styling(bootstrap_options = c("striped", "hover"))%>%
      kable_classic_2(full_width = F)


