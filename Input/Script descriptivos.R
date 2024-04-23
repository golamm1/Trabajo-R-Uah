#Libreria de paquetes
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
install.packages("kableExtra")
install.packages("colorspace")
install.packages("stringi")
install.packages("sjPlot")
install.packages("ggplot2")
install.packages("magick")
library(stringi)
library(kableExtra)
library(magick)
#Abrimos la base de datos procesada
load("C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Processing/Procesamiento Trabajo Creditos Universitarios.Rdata")
names(casen_superior)
dim(casen_superior)
#Paso 1: Realizamos tabla descriptiva con los datos
#1.1: Tabla descriptiva con stargazer
stargazer(casen_superior, type = "text")
#1.2 Pasamos a factor para poder trabajar mis variables en sjmisc
casen_superior <- as_factor(casen_superior)
#1.3: Tabla con sjmisc:
sjmisc::descr(casen_superior)
sjmisc::descr(casen_superior,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

#Tabla con summarytools
summarytools::dfSummary(casen_superior, plain.ascii = FALSE)
view(dfSummary(casen_superior, headings=FALSE))
#Paso 2: Eliminamos casos perdidos de las variables
casen_superior <- na.omit(casen_superior)
sum(is.na(casen_superior))
#Paso 3: Generacion de gráficos
# Crear el gráfico usando ggplot2: 
#3.1: Gráfico de cantidad de estudiantes por establecimiento de educacion superior
grafico1 <- casen_superior %>% ggplot(aes(x = estab_sup)) + 
  geom_bar(fill = "purple")+
  labs(title = "Establecimientos de educacion superior",
       x = "Cantidad de estudiantes por establecimiento",
       y = "Frecuencia") +
  theme_bw()

grafico1
ggsave(grafico1, file="C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Output/grafico1.png")
#3.2: Gráfico de estudiantes que tienen algun tipo de credito para estudiar
grafico2 <- casen_superior %>% ggplot(aes(x = nivel_educ)) + 
  geom_bar(fill = "skyblue")+
  labs(title = "Nivel de educacion",
       x = "Cantidad de estudiantes por nivel educacional",
       y = "Frecuencia") +
  theme_bw()

grafico2
ggsave(grafico2, file="C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Output/grafico2.png")

#Paso 4: Generación de tablas, esto lo haremos para observar cuántos estudiantes por nivel educacional estudianten con créditos
sjt.xtab(casen_superior$nivel_educ, casen_superior$credi_sup)
sjt.xtab(casen_superior$estab_sup, casen_superior$pago_carrera)

sjt.xtab(casen_superior$nivel_educ, casen_superior$credi_sup,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

sjt.xtab(casen_superior$estab_sup, casen_superior$pago_carrera,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)
#Paso 5: Asociación de dos variables por medio de un gráfico
#5.1: Pasamos variables a factor
casen_superior <- as_factor(casen_superior)
grafico5 <- casen_superior %>% ggplot(aes(x = estab_sup, fill = credi_sup)) + 
  geom_bar() +
  xlab("Establecimiento Educacional Superior") +
  ylab("Cantidad") + 
  labs(fill="Estudiantes con créditos universitarios")+
  scale_fill_discrete(labels = c('Algun tipo de crédito','Sin crédito')) + 
  theme(
    plot.background = element_rect(size = 1.5, color = "black"),
    axis.text.x = element_text(size = 6, angle = 35, hjust = 1),
    axis.text.y = element_text(size = 10)
  )
  
   
grafico5
ggsave(grafico5, file="C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Output/grafico5.png")

grafico4 <- casen_superior %>% ggplot(aes(x = estab_sup)) + 
  geom_bar() +
  xlab("Estudiantes con créditos universitarios por establecimientos de educacion superior") +
  ylab("Cantidad")+
  facet_wrap(~credi_sup)
ggsave(grafico4, file="C:/Users/franr/OneDrive/Documentos/GitHub/Trabajo-R-Uah/Output/grafico3.png")
