
#--------------------------------------------------------------------------------
# Tema:       Solucion del Ejercicio - 1
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      agosto 2022
# Datos:      I Trimestre ENOE nueva, INEGI.
#             https://www.inegi.org.mx/programas/enoe/15ymas/#Microdatos
# Github:     https://github.com/jcms2665/CEI-Colmex
# Notas:      Se recomienda seguir el procedimiento paso por paso     


# Objetivo:   Obtener el tabulado ponderado de la variable "Posicion en 
#              la ocupación 

#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign);library(questionr); library(ggplot2); library(dplyr) 


#2.  Directorio de trabajo
setwd(".../Datos")


#3.  Importar datos
enoe<-read.dbf("ENOEN_SDEMT121.dbf")%>%data.frame()


#4. Tabular la variable POS_OCU (sin ponderar)
wtd.table(enoe$pos_ocu)


#5. Crear una nueva base y nombrarla enoe.3
    #   Filtrar: Hombres ocupados (SEX==1 & CLASE2==1) 
    #   Seleccionar variables: "SEX", "EDA","POS_OCU","FAC_TRI", "UPM", "EST_D_TRI","R_DEF","C_RES"
    #   Ayuda: ver punto 6.3
enoe.3 <- enoe%>%filter(sex==1 & clase2==1)%>%select("sex", "eda","pos_ocu","fac_tri", "upm", "est_d_tri","r_def","c_res")


#6. Convertir las variables R_DEF, C_RES y EDA numericas
#   Ayuda: ver punto 7.1.1
enoe.3$R_DEF <-as.numeric(as.character(enoe.3$R_DEF))
enoe.3$C_RES <-as.numeric(as.character(enoe.3$C_RES))
enoe.3$EDA <-as.numeric(as.character(enoe.3$EDA))


#8. Filtrar casos validos para la ENOE y crear la base SD 
#   Ayuda: ver punto 7.1.2
SD<-enoe.3%>%filter(R_DEF==0)%>%filter(C_RES==1 | C_RES==3)%>%filter(EDA>=15 & EDA<=98)


#9. Con la base SD, generar el tabulado ponderado de la variable: CLASE1  
wtd.table(enoe$clase1, weights = enoe$fac_tri)



