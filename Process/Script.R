# Carga de paquetes -------------------------------------------------------

pacman::p_load(dplyr,
               sjmisc,
               car,
               sjlabelled,
               stargazer,#
               haven)

# Carga BB_DD -------------------------------------------------------------

load("C:/Users/diego/OneDrive/Escritorio/Trabajo_2/Input/data-orig/Latinobarometro_2020_Esp_Rdata_v1_0.rdata")
###
dim(Latinobarometro_2020_Esp)
View(Latinobarometro_2020_Esp)
find_var(data = Latinobarometro_2020_Esp,"")

proc_data <- select("Latinobarometro_2020_Esp,
                    p4stgbs, # Situación económica actual del país
                    p5stgbs, # Situación económica del país respecto al pasado año
                    p28st,
                    idenpa")  # Como es inversión extranjera para desarrollo económico del país


paste("Estructura de carpetas:
      para mantener el orden se sugiere seguir un protocolo de estructura de carpetas de proyecto, 
      para lo que recomendamos el protocolo IPO, y que se adapta al flujo de trabajo presentado anteriormente.
      Básicamente son tres carpetas: input, procesamiento, output. 
      En la carpeta input crear la subcarpeta data-orig para guardar datos originales,
      y data-proc para los procesados. En procesamiento se guardan los archivos de código y en output las tablas y los gráficos.")
load("Input/data-orig/Latinobarometro_2020_Esp_Rdata_v1_0.rdata", Encoding("utf-8"))


####
base_datos <- read.csv("C://Users/diego/OneDrive/Escritorio/trabajoder/input/Base_datos_trabajo.csv", header=TRUE)
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

#cargamos la base de datos desde internet
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))
find_var(data = ,"Protección de la propiedad privada")
# variables
#P11STGBS_B P11STGBS.BSatisfacción con el funcionamiento de la economía en (país)
#p39n_a,p62st_d P39N.A Los inmigrantes son buenos para la economía del país
# P62ST.D P62ST.D La economía de mercado es el único sistema con el que el {PAÍS} puede
#29      29     p4stgbs                                      P4STGBS Situación económica actual del país
#30     p5stgbs                      P5STGBS Situación económica del país respecto al pasado año
#129    129       p28st            P28ST Como es inversión extranjera para desarrollo económico del país

proc_data <- latinobarometro2020 %>% select("p62st_d,
                                            p47st_d,
                                            idenpa") # pais 

# Comprobar
names(proc_data)
