
#--------------------------------------------------------------------------------
# Tema:       Estructuras basicas en R
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      enero 2023
# Datos:      
# Github:     https://github.com/jcms2665/CEI-Colmex
 

# Contenido             

#       0. Preparar entorno de trabajo
#       1. Librerias
#           1.1 Instalar (solo una vez)
#           1.2 Cargar (siempre)
#       2. Tipos de datos
#           2.1 Asignacion
#           2.2 Validacion
#           2.3 Reasignacion
#       3. Estructuras de datos
#           3.1 Vectores
#           3.2  Matrices
#           3.3  Tabla (Data Frames)


#--------------------------------------------------------------------------------




#0.  Preparar entorno de trabajo

rm(list=ls())                           # Quitar elementos guardados (Environment)
graphics.off()                          # Limpiar el espacio para las graficas (Plots)
options(warn=-1)                        # Evita que aparezcan warnings
# Ctrl + L                              # Limpia la consola (Console)


#1. Librerias

    #1.1 Instalar (Solo una vez)
    
    install.packages("foreign")         # Importar/exportar archivos en diversos formatos
    install.packages("questionr")       # Tablas ponderadas
    install.packages("ggplot2")         # Crear graficas
    install.packages("tidyverse")
    install.packages("dplyr")

    
    
    #1.2 Cargar (Cada vez que se va a usar R)
    
    library(foreign)                 
    library(questionr)              
    library(ggplot2) 
    library(dplyr)  
    library(tidyverse) 


#2.  Tipos de datos

    #2.1 Asignacion
    
    x<-3.1416                           # Numerico
    x=3.1416
    class(x)
    
    y<-"Curso R"                        # Caracter
    class(y)


    #2.2 Validacion
    
    is.numeric(x)                       # checar si es un número y devuelve falso/verd 
    is.numeric(y)
    
    r<-is.numeric(x)                    # Valores logicos (TRUE/FALSE)
    class(r)

    #2.3 Reasignacion
    
    x<-as.character(x)
    class(x)

    
    
#--------------------------------------
# NOTA:
# Un gran porcentaje de los errores al 
# analizar los datos se debe a no saber 
# que tipo de datos tenemos.
#
# Algo "se ve" como numero puede ser
# un string
#--------------------------------------
    


#3. Estructuras de datos
    
    #3.1 Vectores
    
    num_1 <- c(3.14, 1.61, 0, -1)           # Vector caracteres
    num_1[2]                                # Acceder a lo que tiene el vector en la posicion 2
    
    
    caracter<-c("A", "B", "C","D","E") 
    caracter[3:5]                           # los : me indican de qué posición a qué posición



    #3.2  Matrices
    
    v_1 <- c(78, 2, 3)
    v_2 <- c(10, 63, 0)
    
    matriz_1 <- rbind(v_1, v_2)             # rbind sirve para unir vectores (filas)
    matriz_1                                
    dim(matriz_1)
    
    matriz_2 <- cbind(v_1, v_2)             # cbind sirve para unir vectores (columnas)
    matriz_2
    dim(matriz_2)
    
    
    matriz_2[1,]                            # La , (coma) sirve para diferenciar entre columnas y filas
    matriz_2[,1]
    
    matriz_2[2,2]


    #--------------------------------------
    # NOTA:
    # Para crear vectores se debe utilizar
    # los caracteres: c()  
    #--------------------------------------


    #3.3  Tabla (Data Frames)
    
    nombre<-c("Luisa","Ana","Miriam","Pedro")               # Vector de texto
    sexo<-c("M", "M", "M", "H")                             
    edad<-c(23, 45, 19, 65)                                 # Vector de numeros enteros
    ingreso<-c(23.50, 450.01, 58.89, 789.85)                # Vector de numeros con decimales (flotantes/racionales)
    nacion<-c("Mx", "Ur", "Mx", "Pa" )
    
    tabla <- data.frame(nombre, sexo,edad,ingreso,nacion)   # data.frame une vectores con distintas caracteristicas
    
    View(tabla)

    #--------------------------------------
    # NOTA:
    # Que tienen en comun los 5 vectores 
    # que acabamos de unir ?
    #--------------------------------------
    


    

    
  
    
    
    