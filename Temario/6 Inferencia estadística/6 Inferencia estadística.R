
#--------------------------------------------------------------------------------
# Tema:       Inferencia estadistica
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      09-02-2023
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
#       5. Subconjunto
#           5.1 Validar el tipo de dato
#           5.2 Quitar missings
#           5.3 Subconjunto
#       6. Estadisticos basicos
#       7. Una muestra
#           7.1 t-Student 
#           7.2 Z o Binomial
#       8. Dos muestras
#           8.1 t-pareada
#           8.2 U de Mann Whitney
#           8.3 Z o Binomial de 2 poblaciones
#       9. Chi-cuadrada

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(car)
library(dplyr)
library(tidyverse)
library(kableExtra)


#2.  Directorio de trabajo
setwd("")

#3.  Importar datos
democracia<-read.csv("V-Dem-DS-CY-v7.csv")%>%data.frame()


#4.  Exploracion inicial
dim(democracia)                                   


#5.  Subconjunto


# VARIABLE 1: "v2xcs_ccsi" = Que tan robusta es la sociedad civil?

# Consideraciones metodol?gicas:
# El indice esta disenado para proporcionar una medida de una sociedad civil, 
# entendida como aquella que disfruta de autonomia del Estado y en la que los 
# ciudadanos persiguen libre y activamente sus objetivos politicos y civicos,
# independientemente de como se conciban.

# VARIABLE 2: "v2xel_elecpres" = Tuvieron elecciones presidenciales este a?o

    #5.1 Validar el tipo de dato
    democracia$v2xcs_ccsi <-democracia$v2xcs_ccsi %>% as.numeric()
    democracia$v2xel_elecpres <-democracia$v2xel_elecpres %>% as.numeric()
    
    democracia$year=democracia$year%>%as.numeric()
    
    #5.2 Quitar missings
    #    drop_na sirve para ubicar y quitar NA
    demo1<-democracia%>%
      drop_na(v2xcs_ccsi)%>%
      drop_na(v2xel_elecpres)
    
    
    #5.3 Subconjunto
    
    # Hacer un subconjunto con 3 caracteristicas:
    # i.   El dato de 2000 y 2010
    # ii.  Solo considerar las variables v2elsuffrage, v2xel_elecpres, country_name, year
    
    #---------------------------------------
    # NOTA:
    # Conviene hacer bases en cada etapa
    # para repetir el proceso en caso 
    # de error
    #---------------------------------------
    
    demo2<-demo1%>%
      filter(year==2000 | year==2010)%>%
      select("country_name","year","v2xcs_ccsi", "v2xel_elecpres")
    
    # Cambiamos el formato de la tabla
    
    demo3<-demo2 %>% 
      reshape(idvar = "country_name", timevar = "year", direction = "wide")
    
    # Ajustar nombre de variables
    names(demo3)=c("Pais","SC_2015", "Elecc_2015","SC_2016", "Elecc_2016")
    
#6. Estadisticos basicos
    
    # Variable "Sociedad_civil"
    min(demo3$SC_2015)
    
    
    # Hay missings
    # Nuevamente, con drop_na quitamos los registros con missings
    demo3<-demo3%>%
      drop_na(SC_2015)%>%
      drop_na(SC_2016)%>%
      drop_na(Elecc_2015)%>%
      drop_na(Elecc_2016)
      
    # Estad?sticos b?sicos
    min(demo3$SC_2015)
    max(demo3$SC_2015)
    mean(demo3$SC_2015)
    median(demo3$SC_2015)  
    max(demo3$SC_2015)  
    
          
#7. Una muestra

    #7.1 t-Student (Dos colas)
        
        # Supuestos:
        # i.  La muestra sigue una distribuci?n normal
        # ii. Tama?o de muestra peque?o
        
    
        # Planteamiento
        # H0: El promedio del indice es igual a 0.40
        # Ha: El promedio del indice es diferente a 0.40
        
            
        set.seed(123)
        
        shapiro.test(demo3$SC_2015)
        t.test(demo3$SC_2015, mu=0.40)
        
        # p-value = 2.2e-16 < 0.05 (Criterio de uso com?n)
        # Con un 95% de confianza, rechazamos la propuesta de 
        # que el ?ndice tiene un valor promedio de 0.40
        
    
    #7.2 Z o Binomial (Dos colas)
        
        # Supuestos:
        # i.  La muestra sigue una distribucion Binomial (Exito, fracaso)
        # ii. Muestra pequena
    
        # Planteamiento
        # H0: La proporci?n es de 0.5
        # Ha: La proporci?n es diferente a 0.5
        
        set.seed(123)
        binom.test(51, 174, p=0.5, alternative=c("two.sided"))
        
        # p-value = 4.779e-08 < 0.05 (Criterio de uso com?n)
        # Con un 95% de confianza, rechazamos la propuesta de 
        # que la proporci?n de paises que tuvieron elecciones
        # presidenciales es igual a 0.5
        
        table(demo3$Elecc_2015)

    
#8. Dos muestras        
    #8.1 t-pareada
        
        # Supuestos:
        # i.  La muestra sigue una distribucion normal
        # ii. Elementos relacionados (Homogeneidad de varianza)
        
        
        # Planteamiento
        # H0: El promedio de ambas muestras es igual
        # Ha: El promedio de ambas muestras es diferente
        
        
        # Distribucion normal
        set.seed(123)
        shapiro.test(demo3$SC_2015)
        shapiro.test(demo3$SC_2016)
        
        # Elementos relacionados
        set.seed(123)
        var.test(demo3$SC_2015, demo3$SC_2016)
        
        t.test(demo3$SC_2015, demo3$SC_2016, var.equal = T)
        
        # p-value = 0.4893 > 0.05 (Criterio de uso com?n)
        
        # LAS DOS MUESTRAS SON GENERADAS POR LA MISMA DISTRIBUCI?N
        
        
    #8.2 U de Mann Whitney
        
        
        # Planteamiento
        # H0: La media en ambas muestras es igual
        # Ha: La media en ambas muestras no es igual
        
        # Supuestos:
        # i.  La muestra no sigue una distribucion normal (no param?trica)
        set.seed(123)
        shapiro.test(demo3$SC_2015)
        shapiro.test(demo3$SC_2016)
        
        
        set.seed(123)
        wilcox.test(demo3$SC_2015, demo3$SC_2016)
        
        # p-value = 0.5056> 0.05
        # Con un 95% de confianza, aceptamos Ha
        
    #8.3 Z o Binomial de 2 poblaciones
        
        # Planteamiento
        # H0: La proporci?n en ambas muestras es igual
        # Ha: La proporci?n en ambas muestras no es igual
        
        
        n<-c(23, 8)
        x<-c(23, 6) 
        set.seed(123)
        prop.test(x,n, correct = TRUE)
        
        # p-value = 0.1002> 0.05
        # Con un 95% de confianza, aceptamos Ha
        
        
#9. Chi-cuadrada
        
        
        # Las hip?tesis que se prueban en una ji cuadrada son :
        #     H0: las dos variables son independientes
        #     HA: las dos variables no son independientes
        
        #     p < 0.05: Las variables tienen una asociaci?n 
        #     estad?sticamente significativa (Rechazar H0)
        
        #     p > 0.05: No se puede concluir que las 
        #     variables est?n asociadas (No se puede rechazar H0)
        
        
        trab.t<-wtd.table(demo3$Elecc_2015, demo3$Elecc_2016)       
        chisq.test(trab.t)        
        
        #     p= 0.01226 < 0.05: Las variables tienen una asociaci?n 
        
        
        
        #-------------------------------------------
        # RETO-1:
        # Repite el proceso para 2009 y 2011
        #-------------------------------------------
        