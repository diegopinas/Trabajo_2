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
               readxl,
               psych) 

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

# ¿Cómo calificaría en general la situación económica actual del país? Diría Ud. que es.
find_var(data = Latinobarometro_2023_Esp_v1_0, "P5STGBS")

# Grado de acuerdo con las siguientes frases La economía de mercado es el único sistema con el que el (PAÍS)puede llegar a ser desarrollado
find_var(data = Latinobarometro_2023_Esp_v1_0, "P54ST.B")

#¿ diria usted que esta satisfecho con el funcionamiento de la democracia en el pais?
find_var(data= Latinobarometro_2023_Esp_v1_0,"P11STGBS.A")


#P35NA. (MOSTRAR TARJETA 11) Pensando en la mejor forma de gobernar para Ud., ¿cuál de estos aspectos considera usted el más importante?
#Concentrar el poder político en un líder ... 1 1
#Distribuir el poder en varias instituciones 2 2
#Que la prensa no cuestione el poder ........ 3 3
#Garantizar las libertades políticas de
#los ciudadanos 4 4
find_var(data=Latinobarometro_2023_Esp_v1_0,"P35NA")

#Corrupción
find_var(data=Latinobarometro_2023_Esp_v1_0,"P60ST") 

# Sobre los impuestos
find_var(data =Latinobarometro_2023_Esp_v1_0,"P22ST")

#confianza en gobierno
find_var(data =Latinobarometro_2023_Esp_v1_0,"P13ST.E")

#Confianza Congreso
find_var(data =Latinobarometro_2023_Esp_v1_0,"P13ST.D")

#Confianza Bancos
find_var(data =Latinobarometro_2023_Esp_v1_0,"P14ST.F")

#Confianza poder judicial
find_var(data =Latinobarometro_2023_Esp_v1_0,"P13ST.F")

#Desempeño Gobierno aprueba/no aprueba ##DE AQUI A ABAJO SON VARIABLES DE DESEMPEÑO GOBIERNO
find_var(data =Latinobarometro_2023_Esp_v1_0,"P15STGBS")

#la mejor forma dw gobernar es hay 4 opciones
find_var(data =Latinobarometro_2023_Esp_v1_0,"P35NA")

#Acuero o desacuerdo en que poder judicial sea independiente
find_var(data =Latinobarometro_2023_Esp_v1_0,"P18N.E")

### DE AQUI ABAJO VARIABLES DERECHOS que llamaremos "estado de derecho"
#protecion propiedad privada
find_var(data =Latinobarometro_2023_Esp_v1_0,"P41ST.D")

#proteccion contra el crimen
find_var(data =Latinobarometro_2023_Esp_v1_0,"P41ST.J")

###VARIABLES ECONOMICAS NUEVAS
# Imagen progreso del pais
find_var(data =Latinobarometro_2023_Esp_v1_0,"P2ST")

###vARIABLE PROTESTAS SOCIALES = CONFLICTO
find_var(data =Latinobarometro_2023_Esp_v1_0,"P54N.A")

###inestabilidad regimen politico
#a favor o encontra de apoyar un regimen militar
find_var(data= Latinobarometro_2023_Esp_v1_0,"P20STM") 

# No me importaria que un gobiernon no democratico llegara al poder MA,A,D,MD,NS/NR      
find_var(data= Latinobarometro_2023_Esp_v1_0,"P18STM.B")

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
    P54ST.B,    # satisfaccion economia de mercado
    P11STGBS.A, # Satisfecho con el funcionamiento de la democracia
    P18STM.B,   # No me importaria que un gobiernon no democratico llegara al poder MA,A,D,MD,NS/NR
    P10STGBS,   # preferencia tipo regimen democracia, autoritarismo o no le importa
    P60ST,      # Corrupción
    P22ST,      # percepción impuestos ¿es justo o no?
    P13ST.E,    #Confianza en el gobierno
    P13ST.D,    #Confianza en el Congreso
    P14ST.F,    #Confianza en Bancos
    P13ST.F,    #Confianza poder judicial
    P15STGBS,   # apruebo/no aprueba desempeño del gobierno
    P35NA,      #mejor forma de gobernar 4 opciones
    P18N.E,     #Acuerdo o desacuerdo en que poder judicial se independiente
    P41ST.D,    #Protección propiedad privada
    P41ST.J,    #Protección contra el crimen
    P2ST,       #imagen progreso del pais
    P54N.A,     #grado acuerdo con protestas
    P20STM,     # a favor o encontra de apoyar un regimen militar
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
frq(proc_data$P22ST) # -2 y -1 para NA
frq(proc_data$P5STGBS) # -2 y -1 para NA
frq(proc_data$P60ST) # -2 y -1 para NA
frq(proc_data$P54ST.B) # -5 para NA
frq(proc_data$P11STGBS.A)#-2 y -1 para NA
frq(proc_data$P18STM.B)# -5 para NA
frq(proc_data$P10STGBS)#-2 y -1 para NA
frq(proc_data$P13ST.E) #-2 y -1 para NA
frq(proc_data$P13ST.D) # -2 y -1 para NA
frq(proc_data$P14ST.F) # -2 y -1 para NA
frq(proc_data$P13ST.F) # -2 y -1 para NA
frq(proc_data$P15STGBS) # - 5 para NA
frq(proc_data$P35NA) #-2 y -1 para NA
frq(proc_data$P18N.E) # -5 para NA
frq(proc_data$P41ST.D) # -5 para NA
frq(proc_data$P41ST.J) # -5 para NA
frq(proc_data$P2ST) #-2 y -1 para NA
frq(proc_data$P54N.A) # -5 para NA
frq(proc_data$P20STM) #-2 y .1 para NA

#Transformación de -5, -2 y -1 a NA para todas las variables
proc_data <- proc_data %>% set_na(., na = c(-5, -2, -1))


# Renombrar Variables -----------------------------------------------------
proc_data <- proc_data %>% 
  rename("fun_econ" = P11STGBS.B, # satisfaccion func. economia
         "econ_act" = P5STGBS, #economia actual pais
         "econ_mercado" = P54ST.B, # satisfacción economia de mercado
         "fun_demo"= P11STGBS.A, # satisfaccion func. democracia
         "gob_nodemo"= P18STM.B, # No me importaria que un gobiernon no democratico llegara al poder
         "pref_reg"= P10STGBS, # Preferencia tipo regimen democratico vs autorirario
         "percep_corrup" = P60ST, # percepción corrupción
         "per_impuestos" = P22ST, #percepcion sobre evacion impuestos
         "conf_gob"= P13ST.E, #Confianza en el gobierno
         "conf_cong"= P13ST.D, #Confianza en el Congreso
         "conf_banc"= P14ST.F, #Confianza en Bancos
         "con_judicial"= P13ST.F,#Confianza poder judicial
         "desem_gob" = P15STGBS, # apruebo/no aprueba desempeño del gobierno
         "forma_gob" = P35NA, #forma de goberna existen cuatro respuestas:Concentrar el poder político en un líder,Distribuir el poder en varias instituciones,Que la prensa no cuestione el poder,Garantizar las libertades políticas de los ciudadanos
         "indep_judicial" = P18N.E, #Acuerdo o desacuerdo en que poder judicial se independiente 
         "prot_privada"= P41ST.D, #Protección propiedad privada
         "prot_crimen"= P41ST.J, #Protección crimen
         "imagen_prog_pais"= P2ST, #imagen progreso del pais
         "grad_protesta" = P54N.A, #grado acuerdo con protestas
         "apoyo_gob_militar"= P20STM, # a favor o encontra de apoyar un regimen militar
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


save(data_final, file = "Output/data-proc/data_final")


# Tabla -------------------------------------------------------------------

#limpieza de base de datos 
rm(inversion_extranjera_directa)
rm(proc_data)
#Abrimos la libreria para realizar graficos
pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

 



sjt.xtab(data_final$econ_act,# economía actual 
         data_final$fun_econ, #satisfacción con el funcionamiento de la economía
         encoding = "UTF-8")



# descriptivos------------------------------------------------------------

sjmisc::descr(data_final,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>% # Selecciona estadísticos
  kable(.,"markdown") # Esto es para que se vea bien en quarto



# Grafico ----------------------------------------------------------------



# Grafico de puntos
ggplot(data_final, aes(x = as.factor(idenpa), y = inv_ext_dir, fill = idenpa)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0), size = 3, alpha = 0.7) +
  labs(x = "paises", y = "inversión extranjera directa") +
  theme_classic()





# Grafico con distribución normal
ggplot(data_final, aes(x = idenpa, fill = inv_ext_dir)) +
  geom_density(alpha = 0.5) +  # Curva de densidad
  geom_rug(data = data_final, aes(x = idenpa, fill = inv_ext_dir), sides = "b", alpha = 0.5) +  # Rug plot
  labs(x = "Países", y = "Inversión extranjera directa") +  # Etiquetas de los ejes
  theme_classic()







# correlaciones ----------------------------------------------------------------
pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica

rm(data_numeric)
cor(data_final, use = "complete.obs")
corrplot.mixed(correlation_matrix)

#forma 1 de representar correlación entre variables

data_numeric <- select(data_final, where(is.numeric)) # pasamos la base de dato a numerica para realizar analisis
correlation_matrix <- cor(data_numeric)
cor(data_numeric)

#forma dos de representar correlación entre variables
sjPlot::tab_corr(data_numeric, 
                 triangle = "lower")

corrplot.mixed(correlation_matrix)

# tercera forma de representar correlación entre variables
data_final <- data_final %>%
  rowwise() %>%
  mutate( inv_ext_dir= sum(c(fun_demo, fun_econ), na.rm = TRUE))



ggpairs(correlation_matrix)


# Cuarta forma es con el grafico de puntos
sjPlot::plot_scatter(data_final,idenpa,inv_ext_dir)

# prueba de consistencia interna------------------------------------


psych::alpha(data_numeric)

# como consejo para el siguiente trabajo 4 agrupar las variables que tienen correlación para transformar un indice

result <- psych::alpha(data_numeric, check.keys = TRUE)
print(result)

