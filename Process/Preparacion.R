# Carga de paquetes -------------------------------------------------------
install.packages("pacman")
library("pacman")

pacman::p_load(dplyr,
               sjmisc,
               car,
               sjlabelled,
               stargazer,#
               haven,
               sjPlot,# esto es para hacer tablas
               ggplot2) 


# Ajustar espacio de trabajo ----------------------------------------------

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

# Carga BB_DD -------------------------------------------------------------

load("Input/data-orig/Latinobarometro_2020_Esp_Rdata_v1_0.RData") #carga de bbdd de manera local
load("C:/Users/diego/OneDrive/Escritorio/Trabajo_2/Input/data-orig/Latinobarometro_2023_Esp_Rdata_v1_0.RData")
# Examen visual de la bbdd ------------------------------------------------
View(Latinobarometro_2023_Esp_v1_0) # ver bbdd
dim(Latinobarometro_2023_Esp_v1_0)  # dimensiones de la bbdd
colnames(Latinobarometro_2023_Esp_v1_0) # nombres de las variables de la bbdd

### Buscar lo relacionado a:
# economía

# Busqueda de Variables ---------------------------------------------------

#En general, ¿Diría Ud. que está muy satisfecho, más bien satisfecho, no muy satisfecho o nada satisfecho con el funcionamiento de
#funcionamiento de la economía?
find_var(data = Latinobarometro_2023_Esp_v1_0, "P11STGBS.B")

#Los inmigrantes son buenos para la economía del país
find_var(data = Latinobarometro_2023_Esp_v1_0, "P33N.A")

# ¿Cómo calificaría en general la situación económica actual del país? Diría Ud. que es.
find_var(data = Latinobarometro_2023_Esp_v1_0, "P5STGBS")

# ¿Considera Ud. que la situación económica actual del país está mucho mejor, 
# un poco mejor, igual, un poco peor, o mucho peor que hace doce meses?
find_var(data = Latinobarometro_2023_Esp_v1_0, "P6STGBS")

# Grado de acuerdo con las siguientes frases La economía de mercado es el único sistema con el que el (PAÍS)puede llegar a ser desarrollado
find_var(data = Latinobarometro_2023_Esp_v1_0, "P54ST.B")

#¿ diria usted que esta satisfecho con el funcionamiento de la democracia en el pais?
find_var(data= Latinobarometro_2023_Esp_v1_0,"P11STGBS.A")

# No me importaria que un gobiernon no democratico llegara al poder MA,A,D,MD,NS/NR      
find_var(data= Latinobarometro_2023_Esp_v1_0,"P18STM.B")

#¿Con cuál de las siguientes frases está Ud. más de acuerdo? 
#La democracia es preferible a cualquier
# otra forma de gobierno .................. .. 1 # 
#En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democrático.2
#A la gente como uno, nos da lo mismo un régimen democrático que uno no democrático . 3
find_var(data= Latinobarometro_2023_Esp_v1_0,"P10STGBS") 

#¿Y en los próximos doce meses cree Ud. que, en general,
#la situación económica del país SERÁ mucho mejor, un poco mejor, igual, un poco peor, o mucho peor que ahora?
find_var(data = Latinobarometro_2023_Esp_v1_0,"P7ST")

# Pais
find_var(data = Latinobarometro_2023_Esp_v1_0, "idenpa") #chile es 152

# Finalmente se visita documentación de la bbdd para seleccionar variables



# Data procesada ----------------------------------------------------------

proc_data <- Latinobarometro_2023_Esp_v1_0 %>% select(
                    P11STGBS.B, # satisfecho con el funcionamiento de la economía
                    P33N.A, # Los inmigrantes son buenos para la economía del país   
                    P5STGBS, # Situación económica actual del país
                    P6STGBS, # Situación económica del país respecto al pasado año
                    P54ST.B, # satisfaccion economia de mercado
                    P11STGBS.A,# Satisfecho con el funcionamiento de la democracia
                    P18STM.B,# No me importaria que un gobiernon no democratico llegara al poder
                    P10STGBS,# preferencia tipo regimen democracia, autoritarismo o no le importa
                    P7ST,# Situación económica futura 
                    idenpa)# pais

names(proc_data) #comprobar variables seleccionadas
get_label(proc_data) # se comprueba que no tienen etiquetas la variables

proc_data <- proc_data %>% filter(idenpa==152) # se recorta a ver chile #no hacer este paso
#ahora trabajaremos con todos los paises
#IDENPA Country
#32.- Argentina
#68.- Bolivia
#76.- Brasil
#152.- Chile
#170.- Colombia
#188.- Costa Rica
#214.- Rep. Dominicana
#218.- Ecuador
#222.- El Salvador
#320.- Guatemala
#340.- Honduras
#484.- México
#558.- Nicaragua OJO ESTE PAIS NO PARTICIPO EN ESTA ENCUESTA
#591.- Panamá
#600.- Paraguay
#604.- Perú
#724.- España
#858.- Uruguay
#862.- Venezuela

View(proc_data) # se comprueba bbddd creada

# Procesamiento de Variables ----------------------------------------------

# -5 es no responde
# -2 es no sabe
# -1 no responde

frq(proc_data$P11STGBS.B) # -2 y -1 para NA
frq(proc_data$P33N.A) # -5 para NA
frq(proc_data$P5STGBS) # -2 y -1 para NA
frq(proc_data$P6STGBS) # -2 y -1 para NA
frq(proc_data$P54ST.B) # -5 para NA
frq(proc_data$P11STGBS.A)#-2 y -1 para NA
frq(proc_data$P18STM.B)# -5 para NA
frq(proc_data$P10STGBS)#-2 y -1 para NA
frq(proc_data$P7ST)#-2 y -1 para NA

#Transformación de -5, -2 y -1 a NA para todas las variables
proc_data <- proc_data %>% set_na(., na = c(-5, -2, -1))


# Renombrar Variables -----------------------------------------------------
proc_data <- proc_data %>% 
  rename("fun_econ" = P11STGBS.B, # satisfaccion func. economia
         "inmg_econ" = P33N.A, # inmigrantes buenos economia
         "econ_act" = P5STGBS, #economia actual pais
         "econ_anio_pas" = P6STGBS, # economia respecto año pasado
         "econ_mercado" = P54ST.B, # satisfacción economia de mercado
         "fun_demo"= P11STGBS.A, # satisfaccion func. democracia
         "gob_nodemo"= P18STM.B, # No me importaria que un gobiernon no democratico llegara al poder
         "pref_reg"= P10STGBS, # Preferencia tipo regimen democratico vs autorirario
         "econ_fut"= P7ST #funcionamiento economia futuro
         )

# Re-etiquetar Variables --------------------------------------------------

proc_data$fun_econ <- set_label(x = proc_data$fun_econ, label = "Satisfacción: Economía") # signo peso sirve para elegir una variable de la base de datos
get_label(proc_data$fun_econ)


proc_data$inmg_econ <- set_label(x = proc_data$inmg_econ, label = "Inmigracion: Economía")
get_label(proc_data$inmg_econ)

proc_data$econ_act <- set_label(x = proc_data$econ_act, label = "País: Economía Actual")
get_label(proc_data$econ_act)

proc_data$econ_anio_pas <- set_label(x = proc_data$econ_anio_pas, label = "Economía: Año Pasado")
get_label(proc_data$econ_anio_pas)

proc_data$econ_mercado <- set_label(x = proc_data$econ_mercado, label = "Satisfacción: Economía de Mercado")
get_label(proc_data$econ_mercado)

proc_data$fun_demo <- set_label(x = proc_data$fun_demo, label = "Satisfacción funcionamiento: Democracia")
get_label(proc_data$fun_demo)

proc_data$gob_nodemo <- set_label(x = proc_data$gob_nodemo, label = "Acuerdo/Desacuerdo: gobierno no democratico")
get_label(proc_data$gob_nodemo)

proc_data$pref_reg <- set_label(x= proc_data$pref_reg, label= "Preferencia tipo régimen: democrático o autoritario")
get_label(proc_data$pref_reg)

proc_data$econ_fut <- set_label(x=proc_data$econ_fut, label = "Perspectiva futura: Economía")
get_label(proc_data$econ_fut)
View(proc_data)


# Generación de BBDD ------------------------------------------------------

proc_data <- as.data.frame(proc_data)
stargazer(proc_data, type = "text")

save(proc_data, file = "Output")

# Tabla -------------------------------------------------------------------

sjt.xtab(proc_data$econ_act,# economía actual 
         proc_data$fun_econ, #satisfacción con el funcionamiento de la economía
         encoding = "UTF-8")
# 1. Muy satisfecho
#2. Más bien satisfecho
#3. No muy satisfecho
#4. Nada satisfecho
#5. No sabe, se eliminaron
#6. No responde, se eliminaron 
#
#Del grafico se puede deducir que la mayoria de las personas durante el 2020
#se encontraron nada satisfechas

sjt.xtab(proc_data$inmg_econ,#economia: inmigración 
         proc_data$inv_ext, #Beneficio inversión extrenjera
         encoding = "UTF-8")
# descriptivos------------------------------------------------------------
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>% # Selecciona estadísticos
  kable(.,"markdown") # Esto es para que se vea bien en quarto



# Grafico ----------------------------------------------------------------



# Grafico de puntos
ggplot(proc_data, aes(x = as.factor(idenpa), y = econ_act, fill = idenpa)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3, alpha = 0.7) +
  labs(x = "Chile", y = "Economia Actual") +
  theme_classic()

ggplot(proc_data, aes(x = inmg_econ, y = inv_ext, fill = idenpa)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3, alpha = 0.7) +
  labs(x = "Inversión Extranjera", y = "Frecuencia") +
  theme_classic()

ggplot(proc_data, aes(x = inv_ext, y = inmg_econ, fill = idenpa)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3, alpha = 0.7) +
  labs(x = "Inversión extranjera", y = "Inmigración economía") +
  theme_classic()

ggplot(proc_data, aes(x = as.factor(idenpa), y = inmg_econ, fill = idenpa)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3, alpha = 0.7) +
  labs(x = "Chile", y = "Inmigración economía") +
  theme_classic()

ggplot(proc_data, aes(x = as.factor(idenpa), y = econ_act, fill = idenpa)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3, alpha = 0.7) +
  labs(x = "Chile", y = "Economia actual") +
  theme_classic()



# Grafico con distribución normal
ggplot(proc_data, aes(x = econ_act, fill = idenpa)) +
  geom_density(alpha = 0.5) +  # Curva de densidad
  geom_rug(data = proc_data, aes(x = econ_act, fill = idenpa), sides = "b", alpha = 0.5) +  # Rug plot
  labs(x = "Economia Actual", y = "Densidad") +  # Etiquetas de los ejes
  theme_classic()






