pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos
load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData")) #Cargar base de datos
names(proc_data) # Muestra los nombres de las variables de la base de datos
dim(proc_data) # Dimensiones
##Muestra estadisticos descriptivos 
stargazer(proc_data,type = "text")
#describir las data frame
sjmisc::descr(proc_data)
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")
summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE))
proc_data_original <-proc_data
dim(proc_data)
sum(is.na(proc_data))
#Eliminamos casos NA?
proc_data <-na.omit(proc_data)
dim(proc_data)
proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)


#Visualización de variables
ggplot()
ggplot(proc_data, aes(x = conf_inst))
proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar()
proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "blue")
proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "blue")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia")
# Crear el gráfico usando ggplot2
graph1 <- proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "blue")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia") +
  theme_bw()

graph1
##Guardamos en carpeta output
# y lo podemos guardar:

ggsave(graph1, file="C:/Users/UAH/Documents/maslatinobarirometro/Trabajo-R-Uah/Output/graph1.png")


sjt.xtab(proc_data$educacion, proc_data$sexo)
sjt.xtab(proc_data$educacion, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)
#Tablas de promedio de variables CONTINUA por una CATEGORIA
proc_data %>% # se especifica la base de datos
  select(conf_inst,educacion) %>% # se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(educacion)) %>% # se agrupan por la variable categórica y se usan sus etiquetas con as_label
  dplyr::summarise(Obs.=n(),Promedio=mean(conf_inst),SD=sd(conf_inst)) %>% # se agregan las operaciones a presentar en la tabla
  kable(, format = "markdown") # se genera la tabla
#
tapply(proc_data$conf_inst, proc_data$educacion, mean)

graph <- ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()

graph
# y lo podemos guardar:

ggsave(graph, file="/Uah/Documentos/maslatinobarirometro/Trabajo-R-Uah/Output/graph.png")
