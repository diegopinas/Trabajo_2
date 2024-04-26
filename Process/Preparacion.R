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

# Examen visual de la bbdd ------------------------------------------------
View(Latinobarometro_2020_Esp) # ver bbdd
dim(Latinobarometro_2020_Esp)  # dimensiones de la bbdd
colnames(Latinobarometro_2020_Esp) # nombres de las variables de la bbdd

### Buscar lo relacionado a:
# economía

# Busqueda de Variables ---------------------------------------------------

# En general, ¿Diría Ud. que está muy satisfecho, más bien satisfecho, 
# no muy satisfecho o nada satisfecho con el funcionamiento de la economía?
find_var(data = Latinobarometro_2020_Esp, "P11STGBS.B")

#Los inmigrantes son buenos para la economía del país
find_var(data = Latinobarometro_2020_Esp, "p39n.a")

# ¿Cómo calificaría en general la situación económica actual del país? Diría Ud. que es.
find_var(data = Latinobarometro_2020_Esp, "p4stgbs")

# ¿Considera Ud. que la situación económica actual del país está mucho mejor, 
# un poco mejor, igual, un poco peor, o mucho peor que hace doce meses?
find_var(data = Latinobarometro_2020_Esp, "p5stgbs")

# ¿Considera Ud. que la inversión extranjera es beneficiosa o es perjudicial 
# para el desarrollo económico del país o no sabe lo suficiente para opinar?
find_var(data = Latinobarometro_2020_Esp, "p28st")

# Pais
find_var(data = Latinobarometro_2020_Esp, "idenpa") #chile es 152

# Finalmente se visita documentación de la bbdd para seleccionar variables



# Data procesada ----------------------------------------------------------

proc_data <- Latinobarometro_2020_Esp %>% select(
                    P11STGBS.B, # satisfecho con el funcionamiento de la economía
                    p39n.a, # Los inmigrantes son buenos para la economía del país   
                    p4stgbs, # Situación económica actual del país
                    p5stgbs, # Situación económica del país respecto al pasado año
                    p28st,   #inversión extranjera beneficiosa
                    idenpa) # pais

names(proc_data) #comprobar variables seleccionadas
get_label(proc_data) # se comprueba que no tienen etiquetas la variables

proc_data <- proc_data %>% filter(idenpa==152) # se recorta a ver chile
View(proc_data) # se comprueba bbddd creada

# Procesamiento de Variables ----------------------------------------------

# -5 es no responde
# -2 es no sabe
# -1 no responde

frq(proc_data$P11STGBS.B) # -2 y -1 para NA
frq(proc_data$p39n.a) # -5 para NA
frq(proc_data$p4stgbs) # -2 y -1 para NA
frq(proc_data$p5stgbs) # -1 para NA
frq(proc_data$p28st) # -2 y -1 para NA

#Transformación de -5, -2 y -1 a NA para todas las variables
proc_data <- proc_data %>% set_na(., na = c(-5, -2, -1))


# Renombrar Variables -----------------------------------------------------
proc_data <- proc_data %>% 
  rename("fun_econ" = P11STGBS.B, # satisfaccion func. economia
         "inmg_econ" = p39n.a, # inmigrantes buenos economia
         "econ_act" = p4stgbs, #economia actual pais
         "econ_anio_pas" = p5stgbs, # economia respecto año pasado
         "inv_ext" = p28st, # inversion extranjera beneficiosa
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

proc_data$inv_ext <- set_label(x = proc_data$inv_ext, label = "Beneficio: Inversión Extranjera")
get_label(proc_data$inv_ext)

View(proc_data)

# Generación de BBDD ------------------------------------------------------

proc_data <- as.data.frame(proc_data)
stargazer(proc_data, type = "text")

save(proc_data, file = "Output/Latinobar_Chile_2020.RData")


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
sjmisc::descr(proc_data)
sjmisc::descr(data,
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
  labs(x = "Inmigración economía", y = "Inversión Extranjera") +
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




