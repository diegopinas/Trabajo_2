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
               ggplot2,
               readxl) 

# Ajustar espacio de trabajo ----------------------------------------------

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

# Carga BB_DD -------------------------------------------------------------

load("Input/data-orig/Latinobarometro_2023_Esp_Rdata_v1_0.RData")
inversion_extranjera<- read_excel("Input\\data-orig\\Inversion_extranjera_neta.xlsx")

# Examen visual de la bbdd ------------------------------------------------
View(Latinobarometro_2023_Esp_v1_0) # ver bbdd
dim(Latinobarometro_2023_Esp_v1_0)  # dimensiones de la bbdd
colnames(Latinobarometro_2023_Esp_v1_0) # nombres de las variables de la bbdd

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

# Finalmente se visita documentación de la bbdd para seleccionar variables

#BBDD inversion extranjera------------------------------------------------------

find_var(data = inversion_extranjera,"indicator")

colnames("inversion_extranjera")

dim(inversion_extranjera)


#Data procesada Innversion extranjera-------------------------------------------

inversion_extranjera_directa <- inversion_extranjera %>%select(
  País__ESTANDAR,# variable texto
  Años__ESTANDAR,# variable numerica 
  inv_ext_dir = value)#variable numerica 


# Seleccion año de interes ------------------------------------------------

inversion_extranjera_directa <- inversion_extranjera_directa %>% filter(Años__ESTANDAR==2022)

# Borrar base con exceso de variables -------------------------------------

rm(inversion_extranjera)

# Exploracion nueva bbdd --------------------------------------------------

View(inversion_extranjera_directa)
names(inversion_extranjera_directa)
colnames(inversion_extranjera_directa)

# Procesamiento de variables inversion extranjera directa----------------------

frq(inversion_extranjera_directa$País__ESTANDAR)
frq(inversion_extranjera_directa$Años__ESTANDAR)
frq(inversion_extranjera_directa$value)
# Renombrar las variables de inversion extranjera directa-----------------------
inversion_extranjera_directa <- inversion_extranjera_directa %>%
  rename("idenpa" = País__ESTANDAR)
         
inversion_extranjera_directa <- inversion_extranjera_directa %>%
  rename("anos_pais"= Años__ESTANDAR)



# Data procesada ----------------------------------------------------------

proc_data <- Latinobarometro_2023_Esp_v1_0 %>%
  filter(idenpa != 862) %>%
  select(
    P11STGBS.B, # satisfecho con el funcionamiento de la economía
    P5STGBS,    # Situación económica actual del país
    P6STGBS,    # Situación económica del país respecto al pasado año
    P54ST.B,    # satisfaccion economia de mercado
    P11STGBS.A, # Satisfecho con el funcionamiento de la democracia
    P18STM.B,   # No me importaria que un gobiernon no democratico llegara al poder
    P10STGBS,   # preferencia tipo regimen democracia, autoritarismo o no le importa
    P7ST,       # Situación económica futura 
    idenpa      # pais
  )

View(proc_data)


names(proc_data) #comprobar variables seleccionadas
get_label(proc_data) # se comprueba que no tienen etiquetas la variables


# Borrar bbdd con exceso de variables -------------------------------------

rm(Latinobarometro_2023_Esp_v1_0)
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
         "econ_act" = P5STGBS, #economia actual pais
         "econ_anio_pas" = P6STGBS, # economia respecto año pasado
         "econ_mercado" = P54ST.B, # satisfacción economia de mercado
         "fun_demo"= P11STGBS.A, # satisfaccion func. democracia
         "gob_nodemo"= P18STM.B, # No me importaria que un gobiernon no democratico llegara al poder
         "pref_reg"= P10STGBS, # Preferencia tipo regimen democratico vs autorirario
         "econ_fut"= P7ST #funcionamiento economia futuro
         )

proc_data <- proc_data %>%
  mutate(idenpa = as.character(idenpa)) %>%
  mutate(idenpa = case_when(
    idenpa == "32" ~ "Argentina",
    idenpa == "68" ~ "Bolivia (Estado Plurinacional de)",
    idenpa == "76" ~ "Brasil",
    idenpa == "152" ~ "Chile",
    idenpa == "170" ~ "Colombia",
    idenpa == "188" ~ "Costa Rica",
    idenpa == "218" ~ "Ecuador",
    idenpa == "222" ~ "El Salvador",
    idenpa == "320" ~ "Guatemala",
    idenpa == "340" ~ "Honduras",
    idenpa == "484" ~ "México",
    idenpa == "591" ~ "Panamá",
    idenpa == "600" ~ "Paraguay",
    idenpa == "604" ~ "Perú",
    idenpa == "858" ~ "Uruguay"))



# Merge de BBDD -----------------------------------------------------------

data_final <- merge(proc_data, inversion_extranjera_directa, by="idenpa")

data_final <- na.omit(data_final)



# Generación de BBDD ------------------------------------------------------

data_final <- as.data.frame(data_final)
stargazer(data_final, type = "text")

save(proc_data, file = "Output")


# Tabla -------------------------------------------------------------------

#Abrimos la libreria para realizar graficos
pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

 



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
# correlaciones ----------------------------------------------------------------
#forma 1 de representar correlación entre variables
M <-cor(proc_data,use = "complete.obs")
M

#forma dos de representar correlación entre variables
sjPlot::tab_corr(proc_data, 
                 triangle = "lower")
corrplot.mixed(M)

# tercera forma de representar correlación entre variables
proc_data <- proc_data %>%
  rowwise() %>%
  mutate( econ_mercado= sum(c(fun_econ, fun_demo), na.rm = TRUE))
proc_data <- proc_data %>%
  rowwise() %>%
  mutate( inversión= sum(c(fun_econ, fun_demo), na.rm = TRUE))

ggpairs(proc_data)

# Cuarta forma es con el grafico de puntos
sjPlot::plot_scatter(proc_data,fun_demo ,fun_econ )



