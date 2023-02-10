
#--------------------------------------------------------------------------------
# Tema:       Graficas con variables discretas y continuas
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      agosto 2022
# Datos:      Encuesta Telefonica sobre COVID-19 y Mercado Laboral, INEGI.
#             https://www.inegi.org.mx/investigacion/ecovidml/2020/#Microdatos
# Github:     https://github.com/jcms2665/FLACSO-R-2022
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#           3.1 Etiquetar variables
#           3.2 Etiquetar datos
#       4. Graficas (ggplot)
#           4.1 Con una variable discreta
#           4.2 Con dos variables discretas
#           4.3 Con una variable continua
#           4.4 Una variable continua y una discreta

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign); library(questionr); library(ggplot2); library(dplyr) 


#2.  Directorio de trabajo

setwd("C:/Users/Ciencias de Datos-2/OneDrive - El Colegio de México A.C/5. Proyectos/2023/84. Curso-Colmex-R/2. Code/1 Introducción a R/3 Graficas/3_Datos")



#3.  Importar datos


encovid=read.dbf("ECOVID0720.dbf") %>% data.frame()


    # 3.1 Subconjuntos de datos

    # CLASE2=Condicion de ocupacion
    # Codigos: 1=Ocupado, 2=Desocupado, 3=Disponible, 4=No disponible
    
    # Convertir la variable a dato numerico
    encovid$CLASE2=as.numeric(encovid$CLASE2)
    
    # Filtrar la base, necesitamos a los ocupados CLASE2=1
    ocupados=encovid%>%filter(CLASE2==1)

    # 3.2 Etiquetar datos
        
    # PB1=SEXO
    # Codigos: 1=Hombre, 2=Mujer
    ocupados$PB1 <- factor(ocupados$PB1,levels = c(1,2),labels = c("Hombre","Mujer"))
    
    # POS_OCU=Posicion en la ocupacion
    # Codigos: 1=Suborninado, 2=Independiente, 3=Disponible, 0=No aplica
    ocupados$POS_OCU <- factor(ocupados$POS_OCU,levels = c(0,1,2,3),labels = c("No Aplica","Suborninado","Independiente","Disponible"))
    
    #---------------------------------------
    # NOTA:
    # Estamos recodificando en la misma
    # variable, entonces se pierden los 
    # valores originales.
    #---------------------------------------
    
    

#4. Graficas (ggplot)
    
    #4.1 Con 1 variable discreta
    
    # Comentario:
    class(ocupados$PB1)    
    ggplot(ocupados,aes(PB1))+
      geom_bar(fill="blue")
    
    
    # Comentario:
    ggplot(ocupados,aes(PB1))+
      geom_bar(fill="blue")+
        xlab("Sexo")+
        ylab("Personas")+
        ggtitle("Comparacion entre hombres y mujeres")

    
    # Comentario:
    ggplot(ocupados,aes(PB1))+geom_bar(fill="blue")+
      xlab("Sexo")+
      ylab("Personas")+
      ggtitle("Comparacion entre hombres y mujeres")+
      geom_text(aes(label=..count..), stat='count',  
                position=position_dodge(0.9), 
                vjust=-0.5, 
                size=3.5) 

    
    
    
    #4.2 Con 2 variables discretas
    
    g1<-ggplot(ocupados, aes(POS_OCU))            # Base de datos y variable
    
    g1+geom_bar(fill="yellowgreen")+
      facet_wrap(~ PB1)+                          # facet_wrap divide a la pantalla
      ggtitle("Ocupacion por sexo")+
      xlab("Posicion de la ocupacion")+
      ylab("Personas")
    
    # ~ Alt+126  ASCII
    
    

    
    g2<-ggplot(ocupados,aes(x=POS_OCU,fill=PB1))   # Base de datos y variable
    g2+geom_bar(aes(weights=ocupados$FAC_PER))+    # weights pondera la base para tener datos reales
      ggtitle("Ocupacion por sexo")+
      xlab("Posicion de la ocupacion")+
      ylab("Personas")
    
    
    g3<-ggplot(ocupados,aes(x=POS_OCU,fill=PB1))   # Base de datos y variable
    g3+geom_bar(aes(weights=ocupados$FAC_PER),
                position = "dodge")+
      ggtitle("Ocupacion por sexo")+              # position ajusta la posicion de las barras
      xlab("Posicion de la ocupacion")+
      ylab("Personas")+
      scale_fill_brewer(palette = "Blues")
    

    g3+geom_bar(aes(weights=ocupados$FAC_PER),
                position = "dodge")+
      ggtitle("Ocupacion por sexo")+              # position ajusta la posicion de las barras
      xlab("Posicion de la ocupacion")+
      ylab("Personas")+
      scale_fill_brewer(palette = "Blues")+
      scale_y_continuous(labels = scales::comma)
    
 
    g3+geom_bar(aes(weights=ocupados$FAC_PER),
                position = "dodge")+
      ggtitle("Ocupacion por sexo")+              # position ajusta la posicion de las barras
      xlab("Posicion de la ocupacion")+
      ylab("Personas")+
      scale_fill_brewer(palette = "Blues", name = "Sexo")+
      scale_y_continuous(labels = scales::comma)   

    g3+geom_bar(aes(weights=ocupados$FAC_PER),
                position = "dodge")+
      ggtitle("Ocupacion por sexo")+              # position ajusta la posicion de las barras
      xlab("Posicion de la ocupacion")+
      ylab("Personas")+
      scale_fill_brewer(palette = "Blues", name = "Sexo")+
      scale_y_continuous(labels = scales::comma) 
    
    
    
    #4.3 Con 1 variable continua
    
    
    class(ocupados$PB2)                            
    ocupados$PB2<-as.numeric(ocupados$PB2)
    class(ocupados$PB2)
    
    # PB2=Edad
    g4<-ggplot(ocupados, aes(PB2, weight = ocupados$FAC_PER))                 # Base de datos y variable
    
    
    # Agregamos diferentes tipos de graficas
    g4+geom_area(stat="bin")
    
    g4+geom_freqpoly()
    
    g4+geom_histogram(binwidth = 5)
    
    #---------------------------------------
    # NOTA:
    # g4 guarda la base de datos y la variable
    # en un objeto, al cual lo podemos 
    # usar varias veces.
    #---------------------------------------
    

    #4.4 Una variable continua y una discreta
    
    g5<-ggplot(ocupados, aes(PB2, colour=PB1))     # colour indica que hay grupos (2 en este caso)
    
    g5+geom_freqpoly()
    
    g5+geom_freqpoly()+
      ggtitle("Edad por sexo")+
      xlab("Edad")+
      ylab("Personas")+
      theme(plot.title =                           # theme ajusta el centrado
              element_text(hjust = 0.5))
    
    
    # Si los datos vienen de una encuesta, hay que ponderar
    #, weight = FAC_PER                                              


 install.packages(c("packrat", "rsconnect"))   
    
    
    
    
    