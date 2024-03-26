install.packages("pacman")
  pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
  load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))
  
dim(latinobarometro2020)  
View(latinobarometro2020)
find_var(data = latinobarometro2020,"Confianza") #Este codigo de sjmisc permite buscar una variable con una determinada palabra en especifico
proc_data <- latinobarometro2020 %>% select(p13st_e, # Confianza en el Gobierno
                                            p13st_d, # Confianza en el congreso
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g, # Confianza en los partidos políticos
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais 
names(proc_data)
sjlabelled::get_label(proc_data)
proc_data <- proc_data %>% dplyr::filter(idenpa==152) #este dplyr es el filter que se le aplica para dejar un variable en específico

#Una vez lista la base de datos con todos los filtros, procedemos a sacar los descriptivos con la funcion frq
frq(proc_data$p13st_e)
frq(proc_data$p13st_d)
frq(proc_data$p13st_f)
frq(proc_data$p13st_g)

#Recodificamos los valores, puesto que los -1 y los -2 son no sabe no responde, utilizando la función car del paquete car
proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")

proc_data <- proc_data %>% set_na(., na = c(-2, -1)) ####SI ESTAMOS MUY SEGUROS DE QUE EL -2 Y -1 SON NA UTILIZAR ESTE CODIGO QUE LOS BORRA DE TODA LA BASE

#recodifcar para que quede en un orden más lógico que el original con la función recode del paquete car
proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")

#Ahora procederemos a cambiarles el nombre a las categroias cambiandole las etiquetas
proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g) # Confianza en los partidos políticos 

###Cambiamos la etiqueta, que es lo que servirá para hacer tablas o gráficos
proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)
proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)
proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)
proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

#Creamos una nueva variable que suma las 4 variables que hicimos
proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol)
summary(proc_data$conf_inst)
get_label(proc_data$conf_inst)
proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "Confianza en instituciones")
frq(proc_data$conf_gob)
frq(proc_data$conf_inst)
#En esta parte los resultados son todos hechos a partir de los datos asociados al orden anterior de la reciodificacion
proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_cong <- set_labels(proc_data$conf_cong,
                                  labels=c( "Ninguna"=0,
                                            "Poca"=1,
                                            "Algo"=2,
                                            "Mucha"=3))

proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                     labels=c( "Ninguna"=0,
                                               "Poca"=1,
                                               "Algo"=2,
                                               "Mucha"=3))
frq(proc_data$conf_gob)
frq(proc_data$conf_cong)

#EDUCACION
frq(proc_data$reeduc_1)
#Empezamos a recodificar
# recodificacion usando funcion 'recode' de la libreria car
proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1,2,3)=1; c(4,5)=2; c(6,7)=3")
frq(proc_data$reeduc_1)
#etiquetado por el set_labels
proc_data$reeduc_1 <- set_labels(proc_data$reeduc_1,
                                 labels=c( "Educacion basica"=1,
                                           "Educacion media"=2,
                                           "Educacion superior"=3))
#cambio de nombre a la variable
proc_data <- rename(proc_data,"educacion"=reeduc_1)

save(proc_data, file = "C:/Users/Alumno/Documents/Trabajo-R-Uah/Input/latinobarometro_proc.Rdata")

