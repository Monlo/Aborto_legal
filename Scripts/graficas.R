rm(list=ls())
setwd("~")

library(ggplot2)
library(tidyverse)
library(fmsb)

# 1. Revisión legal del aborto

# Cargo directorios
datos = "/Users/lopez/OneDrive/Documentos/Wordpress/Aborto_legal_seguro/in"
out = "/Users/lopez/OneDrive/Documentos/Wordpress/Aborto_legal_seguro/out"

# Cargo base de datos con revisión legal
aborto<- read.csv(paste(datos, "revision_legal.csv", sep="/"), sep=",", encoding = "UTF-8")

# Cargo el shapefile
capa_estados <-readOGR("C:/Users/lopez/OneDrive/Documentos/Bases de datos/Geográficos/01_Estados_2010", layer="01_Estados_2010")

# Limpio los datos
nuevos<- c("id", "ent", "pena", "pena_years","id_pena")
names(aborto)<- nuevos

# Fortify (GgPlot2)
estados <- fortify(capa_estados, region="CVE_ENT") 
estados$id<-as.numeric(estados$id)

# Las junto
junta <- inner_join(estados, aborto, by="id") #Uno las medias con los polígonos por la variable clave: id.
junta$id_pena<-as.factor(junta$id_pena)

# Mapa
ggplot(junta) +
  geom_polygon(aes(x=long, y=lat, 
                   group=group,      #El argumento group=group arma grupos tanto para para los polígonos como para `fill=`. 
                   fill=id_pena))+
  geom_path(data = junta, aes(x = long, 
                                              y = lat, 
                                              group = group), 
            color = "white") +
  coord_equal() +
  scale_fill_manual(values = c("#009a44", "#6eb87c", "#90c698", "#d1e3d2",
                                 "#c4cdd5", "#99abba", "#6e8aa0", "#004c6d"),name = "Años de prisión",
                    labels=c("Despenalizado antes de las 12 semanas","Menos de 1 año", "Hasta 1 año","Hasta 2 años",
                             "Hasta 3 años","Hasta 4 años","Hasta 5 años","Hasta 6 años"))+
  theme_void()+
  theme(legend.position = "right") +
  labs(x = NULL, 
       y = NULL, 
       title = "Pena máxima en años", 
       subtitle = "Revisión de las legislaciones locales", 
       caption = "Fuente: Códigos Penales estatales")
ggsave(paste(out, "revision_legal.png", sep="/"), width = 7, height = 5, pointsize = 15) 

# 2. Edad de las mujeres que abortan

# Cargo base de datos ILE
aborto<- read.csv(paste(datos, "interrupcion-legal-del-embarazo.csv", sep="/"), sep=";", encoding = "UTF-8")

# Preparo los datos
data<-aborto
data <- group_by(aborto, EDAD)
data$tot <- 1
data <- summarize(data, tot_personas = sum(tot, na.rm=T))
data <- na.omit(data)
data <- filter(data, EDAD !="N/E")
data$id <- seq(1, nrow(data))

# Convierto a factores
data$id<- as.factor(data$id)
data$EDAD<- as.factor(data$EDAD)

# Para hacer las siguientes tres gráficas me basé en el siguiente recurso:
# https://www.r-graph-gallery.com/297-circular-barplot-with-groups
ggplot(data, aes(x=reorder(id, -tot_personas), y=tot_personas)) + 
  geom_bar(stat="identity", fill=alpha("forestgreen", 0.7)) +
  ylim(-400,5000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,200), "cm")      
  ) +
  coord_polar(start = 0) +
  geom_text(data=data, aes(x=reorder(id, -tot_personas), y=tot_personas+200, label=EDAD), color="black", fontface="bold",alpha=0.7, size=1.5, inherit.aes = TRUE )
ggsave(paste(out, "edad.png", sep="/"), width = 4, height = 6, pointsize = 15) 

# 3. Estado civil de las mujeres que abortaron

# Preparo los datos
data<-aborto
data <- group_by(aborto, EDOCIVIL_DESCRIPCION)
data$tot <- 1
data <- summarize(data, tot_personas = sum(tot, na.rm=T))
data <- na.omit(data)
data <- filter(data, EDOCIVIL_DESCRIPCION !="N/E")
data$id <- seq(1, nrow(data))

# Reclasificar
data$EDOCIVIL_DESCRIPCION = gsub("casada", "Casada", data$EDOCIVIL_DESCRIPCION)
data$EDOCIVIL_DESCRIPCION = gsub("divorciada", "Divorciada", data$EDOCIVIL_DESCRIPCION)
data$EDOCIVIL_DESCRIPCION = gsub("separada", "Separada", data$EDOCIVIL_DESCRIPCION)
data$EDOCIVIL_DESCRIPCION = gsub("soltera", "Soltera", data$EDOCIVIL_DESCRIPCION)
data$EDOCIVIL_DESCRIPCION = gsub("unión libre", "Unión libre", data$EDOCIVIL_DESCRIPCION)
data$EDOCIVIL_DESCRIPCION = gsub("viuda", "Viuda", data$EDOCIVIL_DESCRIPCION)

# Convierto a factores
data$id<- as.factor(data$id)
data$EDOCIVIL_DESCRIPCION<- as.factor(data$EDOCIVIL_DESCRIPCION)

# Gráfica
ggplot(data, aes(x=reorder(id, tot_personas), y=tot_personas)) +       
  geom_bar(stat="identity", fill=alpha("steelblue4", 0.7)) +
  ylim(-5000,60000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-7,7), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(data=data, aes(x=reorder(id, tot_personas), y=tot_personas+100, label=EDOCIVIL_DESCRIPCION), color="black", fontface="bold",alpha=0.7, size=2.5, inherit.aes = FALSE )
ggsave(paste(out, "edocivil2.png", sep="/"), width = 4, height = 6, pointsize = 12) 

# 3. Escolaridad de las mujeres que abortaron

# Preparo los datos
data<-aborto

# Reclasifico
data$NIVEL_EDU = gsub("carrera técnica", "Carrera técnica", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("doctorado", "Posgrado", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("no especifica", "No especificado", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("licenciatura completa", "Licenciatura", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("licenciatura incompleta", "Preparatoria", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("maestria", "Posgrado", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("posgrado", "Posgrado", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("posgrado incompleto", "licenciatura", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("Posgrado incompleto", "licenciatura", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("licenciatura", "Licenciatura", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("ninguno", "Ninguno", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("otra", "Otra", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("preescolar", "Preescolar", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("preparatoria completa", "Preparatoria", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("preparatoria incompleta", "Secundaria", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("primaria completa", "Primaria", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("primaria incompleta", "Preescolar", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("secundaria completa", "Secundaria", data$NIVEL_EDU)
data$NIVEL_EDU = gsub("secundaria incompleta", "Primaria", data$NIVEL_EDU)

data <- filter(data, NIVEL_EDU !="No especificado")

data <- group_by(data, NIVEL_EDU)
data$tot <- 1
data <- summarize(data, tot_personas = sum(tot, na.rm=T))
data <- na.omit(data)
data$id <- seq(1, nrow(data))

# Convierto a factor
data$id<- as.factor(data$id)

# Gráfca
ggplot(data, aes(x=reorder(id, -tot_personas), y=tot_personas)) +  
  geom_bar(stat="identity", fill=alpha("steelblue4", 0.7)) +
    ylim(-5000,40000) +
    theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-4,7), "cm")      
  ) +
  coord_polar(start = 0) +
  geom_text(data=data, aes(x=reorder(id, -tot_personas), y=tot_personas+50, label=NIVEL_EDU), color="black", fontface="bold",alpha=0.7, size=2.5, inherit.aes = FALSE )
ggsave(paste(out, "educacion2.png", sep="/"), width = 5, height = 5, pointsize = 12) 

# 4. Número de abortos realizados
# Para hacer esta gráfica me basé en el siguiente recurso:
# https://www.r-graph-gallery.com/spider-or-radar-chart.html

# Preparo los datos
abortos<- aborto
abortos <- group_by(aborto, NABORTO)
abortos$tot <- 1
abortos <- summarize(abortos, tot_personas = sum(tot, na.rm=T))
abortos <- na.omit(abortos)
abortos = spread(abortos, NABORTO, tot_personas) 

# Uso el paquete fmsb, necesitamos agregar dos lineas al dataframe, el máximo y el mínimo
abortos <- rbind(rep(49312,3) , rep(3,3) , abortos)

# Grafico 
library("viridis", lib.loc="~/R/win-library/3.4")
library("oce", lib.loc="~/R/win-library/3.4")

# Gráfica radial
radarchart(abortos  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.7,0.4,0.9) , pfcol=rgb(0.2,0.7,0.4,0.3), plwd=3, 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="white", cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

