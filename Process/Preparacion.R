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

#variables percepción regimen politico/economico: 

#En general, ¿Diría Ud. que está muy satisfecho, más bien satisfecho, no muy satisfecho o nada satisfecho con el funcionamiento de
#funcionamiento de la economía?
find_var(data = Latinobarometro_2023_Esp_v1_0, "P11STGBS.B")
#Muy satisfecho ........... 1  
#Más bien satisfecho ...... 2 
#No muy satisfecho ........ 3 
#Nada satisfecho .......... 4 

#¿ diria usted que esta satisfecho con el funcionamiento de la democracia en el pais?
find_var(data= Latinobarometro_2023_Esp_v1_0,"P11STGBS.A")

#Muy satisfecho ........... 1  
#Más bien satisfecho ...... 2 
#No muy satisfecho ........ 3 
#Nada satisfecho .......... 4

#perspectiva economicas actuales (promediar estas para analisis posteriores)
find_var(Latinobarometro_2023_Esp_v1_0,"P5STGBS") 
#¿Cómo calificaría en general la situación económica actual del país? Diría Ud. que es... (LEA ALTERNATIVAS Y MARQUE UNA)
#Muy buena ................ 1
#Buena .................... 2
#Regular .................. 3
#Mala ..................... 4
#Muy mala ................. 5_________
#NO LEER No sabe .......... 8
#NO LEER No responde ...... 0
find_var(data=Latinobarometro_2023_Esp_v1_0,"P6STGBS") 
#¿Considera Ud. que la situación económica actual del país está mucho mejor, un poco mejor, igual, un poco peor, o mucho peor que hace doce meses? (ESPERE RESPUESTA Y MARQUE UNA)
# Mucho mejor .............. 1
# Un poco mejor ............ 2
# Igual .................... 3
# Un poco peor ............. 4
# Mucho peor ............... 5________
# NO LEER No sabe .......... 8
# NO LEER No responde ...... 0


# variables del sistema politico 

#variables confianza institucional
#confianza en gobierno
find_var(data =Latinobarometro_2023_Esp_v1_0,"P13ST.E")
#Confianza Congreso
find_var(data =Latinobarometro_2023_Esp_v1_0,"P13ST.D")
#Confianza Bancos
find_var(data =Latinobarometro_2023_Esp_v1_0,"P14ST.F")
#Confianza poder judicial
find_var(data =Latinobarometro_2023_Esp_v1_0,"P13ST.F")
#identificACION: Estas variables van de 1 a 4 , siendo 1 mucha confianza, 2 algo, 
# 3 poca y 4 ninguna


#variables corrupción 

#acabar con la Corrupción
find_var(data=Latinobarometro_2023_Esp_v1_0,"P60ST") 
#Mucho ................. 1
#Algo .................. 2
#Poco .................. 3
#Nada .................. 4



#Desempeño Gobierno 
#aprueba/no aprueba ##DE AQUI A ABAJO SON VARIABLES DE DESEMPEÑO GOBIERNO
find_var(data =Latinobarometro_2023_Esp_v1_0,"P15STGBS")
# Aprueba .................................. 1
# No aprueba ............................... 2


### DE AQUI ABAJO VARIABLES DERECHOS que llamaremos "estado de derecho"
#protecion propiedad privada
find_var(data =Latinobarometro_2023_Esp_v1_0,"P41ST.D") 

# Completamente garantizadas ..... 1
#Algo garantizadas .............. 2
#Poco garantizadas .............. 3
#Para nada garantizadas ......... 4

#proteccion contra el crimen
find_var(data =Latinobarometro_2023_Esp_v1_0,"P41ST.J")

# Completamente garantizadas .... 1
#Algo garantizadas .............. 2
#Poco garantizadas .............. 3
#Para nada garantizadas ......... 4


###VARIABLE expectativa pais
# Imagen progreso del pais
find_var(data =Latinobarometro_2023_Esp_v1_0,"P2ST")
#Está progresando ......... 1
#Está estancado ........... 2
#Está en retroceso ........ 3 

###vARIABLE CONFLICTO
find_var(data =Latinobarometro_2023_Esp_v1_0,"P54N.A")
#grado de acuerdo con las protestas 
# muy de acuerdo 1
# acuerdo 2
# desacuerdo 3
# muy desacuerdo 4

# No me importaria que un gobiernon no democratico llegara al poder MA,A,D,MD,NS/NR      
find_var(data= Latinobarometro_2023_Esp_v1_0,"P18STM.B")


###inestabilidad regimen politico
# a favor o encontra de apoyar un regimen militar
find_var(data= Latinobarometro_2023_Esp_v1_0,"P20STM")

#Apoyaría a un gobierno militar en reemplazo
#del gobierno democrático, si las cosas se
#ponen muy difíciles ......................... 1

#En ninguna circunstancia apoyaría a un
#gobierno militar ............................ 2



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
    P11STGBS.A, # Satisfecho con el funcionamiento de la democracia
    P18STM.B,   # No me importaria que un gobiernon no democratico llegara al poder MA,A,D,MD,NS/NR
    P6STGBS,    #perspectiva economia actual con respecto al año anterio
    P60ST,      # Corrupción
    P13ST.E,    #Confianza en el gobierno
    P13ST.D,    #Confianza en el Congreso
    P14ST.F,    #Confianza en Bancos
    P13ST.F,    #Confianza poder judicial
    P15STGBS,   # apruebo/no aprueba desempeño del gobierno
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
frq(proc_data$P5STGBS) # -2 y -1 para NA
frq(proc_data$P60ST) # -2 y -1 para NA
frq(proc_data$P11STGBS.A)#-2 y -1 para NA
frq(proc_data$P18STM.B)# -5 para NA
frq(proc_data$P13ST.E) #-2 y -1 para NA
frq(proc_data$P13ST.D) # -2 y -1 para NA
frq(proc_data$P14ST.F) # -2 y -1 para NA
frq(proc_data$P13ST.F) # -2 y -1 para NA
frq(proc_data$P15STGBS) # - 5 para NA
frq(proc_data$P41ST.D) # -5 para NA
frq(proc_data$P41ST.J) # -5 para NA
frq(proc_data$P2ST) #-2 y -1 para NA
frq(proc_data$P54N.A) # -5 para NA
frq(proc_data$P20STM) #-2 y .1 para NA
frq(proc_data$P6STGBS) #-2 y -1 para NA

#Transformación de -5, -2 y -1 a NA para todas las variables
proc_data <- proc_data %>% set_na(., na = c(-5, -2, -1))


# Renombrar Variables -----------------------------------------------------
proc_data <- proc_data %>% 
  rename("fun_econ" = P11STGBS.B, # satisfaccion func. economia
         "econ_act" = P5STGBS, #economia actual pais
         "econ_act_anipas" = P6STGBS, # economia actual con respecto año pasado
         "fun_demo"= P11STGBS.A, # satisfaccion func. democracia
         "gob_nodemo"= P18STM.B, # No me importaria que un gobiernon no democratico llegara al poder
         "percep_corrup" = P60ST, # percepción corrupción
         "conf_gob"= P13ST.E, #Confianza en el gobierno
         "conf_cong"= P13ST.D, #Confianza en el Congreso
         "conf_banc"= P14ST.F, #Confianza en Bancos
         "con_judicial"= P13ST.F,#Confianza poder judicial
         "desem_gob" = P15STGBS, # apruebo/no aprueba desempeño del gobierno
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
rm(Latinobarometro_2023_Esp_v1_0)
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
cor(correlation_matrix, use = "complete.obs")
corrplot.mixed(correlation_matrix)

#forma 1 de representar correlación entre variables

data_numeric <- select(data_final, where(is.numeric)) # pasamos la base de dato a numerica para realizar analisis
correlation_matrix <- cor(data_numeric)
cor(data_numeric)

#forma dos de representar correlación entre variables
sjPlot::tab_corr(data_numeric, 
                 triangle = "lower")

corrplot.mixed(correlation_matrix)


# Cuarta forma es con el grafico de puntos
sjPlot::plot_scatter(bb_dd,percepcion_politico_econ,confianza_inst)

# prueba de consistencia interna------------------------------------


psych::alpha(data_numeric)

# como consejo para el siguiente trabajo 4 agrupar las variables que tienen correlación para transformar un indice

result <- psych::alpha(data_numeric, check.keys = TRUE)
print(result)
#creacion de indices -------

#abrimos la libreria para establecer correlaciones
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,
               tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych     # para Alfa de Chronbach
)


options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

load("Output/data-proc/data_final")
view(dfSummary(data_final, headings=FALSE, graph.col = FALSE))


#creacion de dimensiones en base a los 16 indicadores seleccionados-------------

indicadores_estabilidad <- data_final %>% select(fun_econ,
                                                 econ_act,
                                                 econ_act_anipas,
                                                 fun_demo,
                                                 gob_nodemo, 
                                                 percep_corrup,  # percepción corrupción
                                                 conf_gob, #Confianza en el gobierno
                                                 conf_cong, #Confianza en el Congreso
                                                 conf_banc, #Confianza en Bancos
                                                 con_judicial,#Confianza poder judicial
                                                 desem_gob,  # apruebo/no aprueba desempeño del gobierno
                                                 prot_privada, #Protección propiedad privada
                                                 prot_crimen, #Protección crimen
                                                 imagen_prog_pais, #imagen progreso del pais
                                                 grad_protesta, #grado acuerdo con protestas
                                                 apoyo_gob_militar, # a favor o encontra de apoyar un regimen militar
                                                 idenpa,
                                                 anos_pais,
                                                 inv_ext_dir)
na.omit() %>% # Eliminar Na's
  mutate_all(~(as.numeric(.))) # Convertimos todas las variables a numéricas
# creamos 6 dimensiones para explicar la inversion extranejera indicadores para explicar la inversión extranjera en 
indicadores_estabilidad = indicadores_estabilidad %>%  
rowwise() %>% 
  mutate(confianza_inst= mean(c(conf_gob,conf_cong,conf_banc,con_judicial)),
         percepcion_econ=mean(c(econ_act,econ_act_anipas)),
         conflic_social=mean(c(grad_protesta,gob_nodemo)),
         estado_derecho=mean(c(prot_privada,prot_crimen)),
         imagen_progreso_pais=mean(c(imagen_prog_pais)),
         corrupcion=mean(c(percep_corrup)),
         desempeño_gobierno=mean(c(desem_gob)),
         inestabilidad_reg=mean(c(apoyo_gob_militar)),
         percepcion_politico_econ=mean(c(fun_demo,fun_econ))) %>% ungroup() 









#base de datos definitiva con un promedio entra las variables seleccionadas
 bb_dd <- indicadores_estabilidad %>% select(confianza_inst,
                                             percepcion_econ,
                                             conflic_social,
                                             estado_derecho,
                                             imagen_progreso_pais,
                                             corrupcion,
                                             desempeño_gobierno,
                                             inestabilidad_reg,
                                             percepcion_politico_econ,
                                             inv_ext_dir,
                                             idenpa,
                                             anos_pais)
 
 data_numerica <- select(bb_dd, where(is.numeric))
 
 cor(data_numerica)

 save(bb_dd, file = "Output/data-proc/bb_dd")
#para acceder a la base de datos nueva usar:
 load("Output/data-proc/bb_dd")
 

# resumen 
summary(bb_dd$inv_ext_dir) # Resumensummary indicadores inversión extranjera
#preguntar al profesor que no me queda claro la construccion de indices
#el promedio de inversion extranjera es 10483.8 millones de dolares


#estimacion modelos de regresión:-----------------------------------------------

pacman::p_load(dplyr, car, sjmisc, sjPlot, sjlabelled, stargazer, kableExtra, corrplot, texreg, ggplot2, ggpubr)
#abrimos librerias y limpiamos entorno de trabajo 
rm(data_final)
load("Output/data-proc/bb_dd")
M <- cor(data_numerica , use = "complete.obs") # Usar solo casos con observaciones completas
diag(M)= NA
corrplot::corrplot(M,
                   method = "color", # Cambia los círculos por color completo de cada cuadrante
                   addCoef.col = "#000390", # Color de los coeficientes
                   type = "upper", # Deja solo las correlaciones de arriba
                   tl.col = "black", # COlor letras, rojo por defecto
                   na.label = "-")

#Grafico x1 = ACT
graph1 <- ggplot(bb_dd, aes(x = desempeño_gobierno, y = inv_ext_dir)) +
  geom_point(size = 1) +  # Puntos
  geom_smooth(method = "lm", se = FALSE) +  # Recta de regresión
  labs(x = "desempeño gobierno", y = "inversión extranjera directa")  # Etiquetas de ejes

# Gráfico 2
graph2 <- ggplot(bb_dd, aes(x = percepcion_politico_econ, y = confianza_inst)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "percepcion politico-economica", y = "confianza institucional")
ggarrange(graph1, graph2, nrow = 1) # Unir graficos

# creación regresion lineal
reg1 <- lm(inv_ext_dir ~ desempeño_gobierno, data=bb_dd)
knitreg(list(reg1),
        custom.model.names = c("Modelo 1"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "desempeño gobierno"),
        caption = "inversión extranjera directa",
        caption.above = TRUE)
#grafico para presentación final
# Cargar las librerías necesarias
library(ggplot2)
library(ggeffects)

# Suponiendo que reg1 es tu modelo de regresión


# Crear el gráfico
predictions <- ggeffects::ggpredict(reg1, terms = "desempeño_gobierno")

grafico4<-ggplot(predictions, aes(x = x, y = predicted)) +
  labs(title = "Desempeño Gobierno", x = "Desempeño Gobierno", y = "Predicted") +
  theme_bw() +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill = "black") +
  scale_x_continuous(breaks = seq(min(predictions$x), max(predictions$x), by = 1)) + # Ajusta según tu rango de datos
  scale_y_continuous(limits = c(-100.91, 53889.96), 
                     breaks = seq(-100, 54000, by = 5000)) # Ajusta según tu rango de datos


ggsave(grafico4,file="Output/graphics/grafico4.png")
##grafico 2

ggplot(predictions, aes(x = x, y = predicted)) +
  labs(title = "Desempeño Gobierno", x = "Desempeño Gobierno", y = "Predicted") +
  theme_bw() +
  geom_smooth() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill = "black") +
  scale_x_continuous(breaks = seq(min(predictions$x), max(predictions$x), by = 1)) + # Ajusta según tu rango de datos
  scale_y_continuous(limits = c(-, 53889.96), 
                     breaks = seq(-100, 54000, by = 5000))

        