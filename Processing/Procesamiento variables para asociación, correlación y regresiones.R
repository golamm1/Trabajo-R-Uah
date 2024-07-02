#Instalación de paquetes a utilizar
install.packages("pacman")
#Carga de paquetes
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
#Ajuste del espacio de trabajo
rm(list=ls())       
options(scipen=999) 
#Abrimos base de datos
setwd("C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Input")
casen2022 <- read_dta("Base de datos Casen 2022 STATA_18 marzo 2024.dta")

#Chequeo de base de datos
dim(casen2022)
View(casen2022)

#Paso 1: Selección de variables
#1.1: Buscamos variables con la palabra creditos
find_var(data = casen2022,"educación")
#1.2: Selección de variables a utilizar para el estudio
casen_proce <- casen2022 %>% select(e18, # Tipo de crédito recibido para pagar la carrera (escala)
                                    e8,  # Establecimiento en el cual realizó su ed. superior(escala)
                                    e10, # Tipo de establecimiento en el que estudia
                                    y1, # Ingreso (recodificar por tramo y escala)
                                    e6b_no_asiste, #Último año aprobado de la educación superior al que asistió
                                    o1) #Situación laboral
remove(casen2022)
###Guardamos la nueva base de datos casen_proce para hacerla más liviana para Github###
write_dta(casen_proce, "casen_proce.dta")

remove(casen2022)

#1.2.1: Observamos cómo quedaron las variables seleccionadas                                          
names(casen_proce)
sjlabelled::get_label(casen_proce)
View(casen_proce)

#Paso 2: Procesamiento de variables
#2.1: Descriptivos
frq(casen_proce$e6a_asiste)
frq(casen_proce$e16)
frq(casen_proce$e18)
#2.2: Recodificación
casen_proce$e6b_no_asiste <- recode(casen_proce$e6b_no_asiste, "c(-99, -88)=NA")
casen_proce$e10 <- recode(casen_proce$e10, "c(-99, -88)=NA")
casen_proce$e18 <- recode(casen_proce$e18, "c(-99, -88)=NA")
casen_proce$o2 <- recode(casen_proce$o1, "c(-99, -88)=NA")
casen_proce$y1 <- recode(casen_proce$y1, "c(-99, -88)=NA")

#Paso 3: Etiquetado
casen_proce <- casen_proce %>% rename("anos_aprob"=e6b_no_asiste, #Ultimo año aprobado en la institución de educación superior
                                      "credi_sup"=e18, #Tipo de crédito al que optó (escala)
                                      "estab_sup "=e10, #Tipo de establecimiento en el cual estudia
                                      "sit_laboral"=o1, #Situación laboral
                                      "ingresos"=y1) #Ingresos (escala)
                                     
                                      
                                      
                                      
                                                                  
                                     
#3.1: Asociamos etiquetas a las variables
casen_proce$anos_aprob <- set_label(x = casen_proce$anos_aprob,label = "Último año aprobado")
get_label(casen_proce$anos_aprob)
casen_proce$credi_sup <- set_label(x = casen_proce$credi_sup,label = "Tipo de crédito")
get_label(casen_proce$credi_sup)
casen_proce$estab_sup <- set_label(x = casen_proce$estab_sup,label = "Tipo de establecimiento")
get_label(casen_proce$estab_sup)



#Revisamos
frq(casen_proce$nivel_educ) #Eliminaré datos que no corresponden con estudiantes de educacion superior
frq(casen_proce$credi_sup) #Recodificar en grupos de creditos
frq(casen_proce$pago_carrera) #Recodificar en Sí y No
frq(casen_proce$estab_sup)
frq(casen_proce$ingresos)


#3.1: Recodificacion de variables
casen_proce <- casen_proce %>%
  mutate(NSE = case_when(
    ingresos <= 300000 ~ "Bajo",
    ingresos > 300000 & ingresos <= 700000 ~ "Medio Bajo",
    ingresos > 700000 & ingresos <= 3000000 ~ "Medio Alto",
    ingresos > 3000000 ~ "Alto"))


#Filtrar otras categorías con otros créditos

casen_proce <- casen_proce %>% 
  filter(estab_sup != 1,2,3,4,5,6,7,13,14)

casen_proce$credi_sup <- car::recode(casen_proce$credi_sup, "c(1,2,3)=1; c(4,5,6,7,8)=0")
casen_proce$credi_sup <- set_labels(casen_proce$credi_sup,
                                    labels=c( "Créditos Estatales"=1,
                                              "Otro"=0))

casen_proce$paga_carrera <- car::recode(casen_proce$paga_carrera, "c(1,2)=0; c(3)=1")
casen_proce$paga_carrera <- set_labels(casen_proce$paga_carrera,
                                       labels=c( "Sí, paga el coste de la carrera"=0,
                                                 "No paga el coste de la carrera"=2))

casen_proce$estab_sup <- car::recode(casen_proce$estab_sup, "c(1,2,3,4,5,6,7,13,14)=1; c(8,9)=2; c(10,11)=3; c(12)=4")
casen_proce$estab_sup <- set_labels(casen_proce$estab_sup,
                                    labels=c( "No superiores"=1,
                                              "Instituto profesional y CFT"=2,
                                              "Universidad Privada"=3,
                                              "Universidad Estatal"=4,
                                              ))

#Recodificación a variables númericas que apuntan a la medición del fenómeno en un mismo sentido
casen_proce$credi_sup <- recode(casen_proce$credi_sup, "")

#Paso 4: Filtro los casos que estan en la educacion superior
casen_superior <- casen_proce %>% 
  filter(estab_sup %in% c(2,3,4,5))


#Paso 4: Generación de base de datos procesada para posterior análisis
casen_superior <-as.data.frame(casen_superior)
stargazer(casen_superior, type="text")

#5: Guardamos base de datos
save(casen_proce,file = "C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Processing/Procesamiento Trabajo Creditos Universitarios")
save(casen_superior,file = "C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Processing/Procesamiento Trabajo Creditos Universitarios.Rdata")

