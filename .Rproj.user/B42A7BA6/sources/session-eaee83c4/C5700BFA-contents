#Instalación de paquetes a utilizar
install.packages("pacman")
#Carga de paquetes
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
#Ajuste del espacio de trabajo
rm(list=ls())       
options(scipen=999) 
#Abrimos base de datos
setwd("C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Input")
casen_proce <- read_dta("casen2022.dta")

#Chequeo de base de datos
dim(casen2022)
View(casen2022)

#Paso 1: Selección de variables
#1.1: Buscamos variables con la palabra creditos
find_var(data = casen2022,"educación")
#1.2: Selección de variables a utilizar para el estudio
casen_proce <- casen_proce %>% select(e18, # Tipo de crédito recibido para pagar la carrera
                                    e6a_asiste,  # Nivel educacional al que asiste
                                    e16, # Paga por la carrera que estudia
                                    e10) #Tipo de establecimiento superior al que asiste 
remove(casen2022)
###Guardamos la nueva base de datos casen_proce para hacerla más liviana para Github###
write_dta(casen_proce, "casen_proce.dta")


  
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
casen_proce$e6a_asiste <- recode(casen_proce$e6a_asiste, "c(-99, -88)=NA")
casen_proce$e16 <- recode(casen_proce$e16, "c(-99, -88)=NA")
casen_proce$e18 <- recode(casen_proce$e18, "c(-99, -88)=NA")
casen_proce$e10 <- recode(casen_proce$e10, "c(-99, -88)=NA")

#Paso 3: Etiquetado
casen_proce <- casen_proce %>% rename("nivel_educ"=e6a_asiste, #Nivel educacion al que asiste
                                      "credi_sup"=e18, #Tipo de crédito al que optó
                                      "pago_carrera"=e16, #Paga la carrera
                                      "estab_sup"=e10) #Tipo de establecimiento al que asiste
#3.1: Asociamos etiquetas a las variables
casen_proce$nivel_educ <- set_label(x = casen_proce$nivel_educ,label = "Nivel educacional")
get_label(casen_proce$nivel_educ)
casen_proce$credi_sup <- set_label(x = casen_proce$credi_sup,label = "Tipo de crédito")
get_label(casen_proce$credi_sup)
casen_proce$pago_carrera <- set_label(x = casen_proce$pago_carrera,label = "Paga por la carrera que estudia")
get_label(casen_proce$pago_carrera)
casen_proce$estab_sup <- set_label(x = casen_proce$estab_sup,label = "Tipo de establecimiento")
get_label(casen_proce$estab_sup)



#Revisamos
frq(casen_proce$nivel_educ) #Eliminaré datos que no corresponden con estudiantes de educacion superior
frq(casen_proce$credi_sup) #Recodificar en grupos de creditos
frq(casen_proce$pago_carrera) #Recodificar en Sí y No
frq(casen_proce$estab_sup)


#3.1: Recodificacion de variables
casen_proce$nivel_educ <- car::recode(casen_proce$nivel_educ, "c(2,3,4,5,7,9,11)=1; c(12)=2; c(13)=3; c(14,15)=4")
casen_proce$nivel_educ <- set_labels(casen_proce$nivel_educ,
                                 labels=c( "No superior"=1,
                                           "Tecnico Nivel Superior"=2,
                                           "Profesional"=3,
                                           "Posgrados"=4))
casen_proce$credi_sup <- car::recode(casen_proce$credi_sup, "c(1,2,3,4,5,6,7)=1; c(8)=0")
casen_proce$credi_sup <- set_labels(casen_proce$credi_sup,
                                     labels=c( "Algun tipo de crédito"=1,
                                               "Sin crédito"=0))

casen_proce$pago_carrera <- car::recode(casen_proce$pago_carrera, "c(1)=1; c(2,3)=0")
casen_proce$pago_carrera <- set_labels(casen_proce$pago_carrera,
                                    labels=c( "Sí, paga el coste de la carrera"=1,
                                              "No paga el coste de la carrera"=0))
casen_proce$estab_sup <- car::recode(casen_proce$estab_sup, "c(1,2,3,4,5,6,7)=1; c(8,9)=2; c(10,11)=3; c(12)=4; c(13,14)=5")
casen_proce$estab_sup <- set_labels(casen_proce$estab_sup,
                                       labels=c( "Establecimientos no superiores"=1,
                                                 "Instituto profesional y CFT"=2,
                                                 "Universidad Privada"=3,
                                                 "Universidad Estatal"=4,
                                                 "Otro"=5))

#Paso 4: Filtro los casos que estan en la educacion superior
casen_superior <- casen_proce %>% 
  filter(estab_sup %in% c(2,3,4,5))


#Paso 4: Generación de base de datos procesada para posterior análisis
casen_superior <-as.data.frame(casen_superior)
stargazer(casen_superior, type="text")

#5: Guardamos base de datos
save(casen_proce,file = "C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Processing/Procesamiento Trabajo Creditos Universitarios")
save(casen_superior,file = "C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Processing/Procesamiento Trabajo Creditos Universitarios.Rdata")

