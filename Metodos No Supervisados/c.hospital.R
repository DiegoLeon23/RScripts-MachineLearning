# Diego Leon Ortiz - Métodos no supervisados

# Antes de nada, limpiamos el workspace, por si hubiera algún dataset o información cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Vamos a cargar las librerías necesarias
# (1) Si no tenemos un paquete, lo primero que hacemos es instalarlo
# install.packages("funModeling")
# (2) Una vez instalado, lo que hacemos es cargarlo en memoria
library(funModeling) #  Para hacer auditorías de datos
library(ggplot2)
library(clusterSim)
library(dplyr)
library(caret)
install.packages("doMC", repos="http://R-Forge.R-project.org")
install.packages("missmap")
library(doMC)
library(doParallel)
library(igraph)

# Fijamos los márgenes para la visualización de gráficas
par(mar=c(4,4,4,4))

# Leemos el dataset de clientes de una empresa de telecomunicaciones
estancias <- read.csv("data/LengthOfStay.csv")
hospitales <- read.csv("data/MetaData_Facilities.csv")

# Vamos a hacer un poco de exploración de datos
str(estancias)

#############################################################################################
# ESTANCIAS
#############################################################################################
# Aquí explicación de todas las variables
# https://revolution-computing.typepad.com/.a/6a010534b1db25970b01b8d280d87b970c-pi
# Aquí  más información: https://blog.revolutionanalytics.com/2017/05/hospital-length-of-stay.html
#############################################################################################
# 1. Identificador único de la admisión en el hospital
colnames(estancias)[1]<-"idAdmision"
estancias$idAdmision<-as.factor(estancias$idAdmision)

# 2. Fecha de visita
colnames(estancias)[2]<-"fechavisita"
estancias$fechavisita<-as.factor(estancias$fechavisita)

# 3. Número de readmisiones últimos 180 días
colnames(estancias)[3]<-"numReadmisiones"
estancias$numReadmisiones<-as.numeric(estancias$numReadmisiones)

# 4. Género
colnames(estancias)[4]<-"genero"
estancias$genero<-ifelse(estancias$genero=="M",0,1)
estancias$genero<-as.factor(estancias$genero)

# 5. Flag de enfermedad renal
colnames(estancias)[5]<-"renal"
estancias$renal<-as.factor(estancias$renal)

# 6. Flag de asma
colnames(estancias)[6]<-"asma"
estancias$asma<-as.factor(estancias$asma)

# 7. Flag de falta de hierro
colnames(estancias)[7]<-"faltaHierro"
estancias$faltaHierro<-as.factor(estancias$faltaHierro)

# 8. Flag de pneumonia
colnames(estancias)[8]<-"pneumonia"
estancias$pneumonia<-as.factor(estancias$pneumonia)

# 9. Flag de dependencia de sustancias
colnames(estancias)[9]<-"dependenciaSustancias"
estancias$dependenciaSustancias<-as.factor(estancias$dependenciaSustancias)

# 10. Flag de desorden psicológico
colnames(estancias)[10]<-"desordenPsicologico"
estancias$desordenPsicologico<-as.factor(estancias$desordenPsicologico)

# 11. Flag de depresión
colnames(estancias)[11]<-"depresion"
estancias$depresion<-as.factor(estancias$depresion)

# 12. Flag de otros desórdenes psiquicos
colnames(estancias)[12]<-"otrosDesordenesPsiquicos"
estancias$otrosDesordenesPsiquicos<-as.factor(estancias$otrosDesordenesPsiquicos)

# 13. Flag de fibrosis
colnames(estancias)[13]<-"fibrosis"
estancias$fibrosis<-as.factor(estancias$fibrosis)

# 14. Flag de malnutrición
colnames(estancias)[14]<-"malnutricion"
estancias$malnutricion<-as.factor(estancias$malnutricion)

# 15. Flag de desórdenes en la sangre
colnames(estancias)[15]<-"desordenSangre"
estancias$desordenSangre<-as.factor(estancias$desordenSangre)

# 16. Valor hematocrito (g/dL)
colnames(estancias)[16]<-"hematocrito"
estancias$hematocrito<-as.numeric(estancias$hematocrito)

# 17. Valor neutrófilos (células/microL)
colnames(estancias)[17]<-"neutrofilos"
estancias$neutrofilos<-as.numeric(estancias$neutrofilos)

# 18. Valor sodio (mmol/L)
colnames(estancias)[18]<-"sodio"
estancias$sodio<-as.numeric(estancias$sodio)

# 19. Valor glucosa (mmol/L)
colnames(estancias)[19]<-"glucosa"
estancias$glucosa<-as.numeric(estancias$glucosa)

# 20. Valor nitrógeno urea sangre (mg/dL)
colnames(estancias)[20]<-"ureaSangre"
estancias$ureaSangre<-as.numeric(estancias$ureaSangre)

# 21. Valor creatinina (mg/dL)
colnames(estancias)[21]<-"creatinina"
estancias$creatinina<-as.numeric(estancias$creatinina)

# 22. Valor BMI (kg/m2)
colnames(estancias)[22]<-"bmi"
estancias$bmi<-as.numeric(estancias$bmi)

# 23. Valor pulso (pulsaciones/minuto)
colnames(estancias)[23]<-"pulso"
estancias$pulso<-as.numeric(estancias$pulso)

# 24. Valor respiración (respiraciones/minuto)
colnames(estancias)[24]<-"respiracion"
estancias$respiracion<-as.numeric(estancias$respiracion)

# 25. Flag de diagnóstico secundario
colnames(estancias)[25]<-"diagnosticoSecundario"
estancias$diagnosticoSecundario<-as.factor(estancias$diagnosticoSecundario)

# 26. Fecha de alta
colnames(estancias)[26]<-"fechaAlta"
estancias$fechaAlta<-as.character(estancias$fechaAlta)

# 27. Número de readmisiones últimos 180 días
colnames(estancias)[27]<-"idHospital"
estancias$idHospital<-as.factor(estancias$idHospital)

# 28. Duración de la estancia
colnames(estancias)[28]<-"duracionEstancia"
estancias$duracionEstancia<-as.numeric(estancias$duracionEstancia)

# Vamos a cuidar todos los aspectos de calidad de datos
# 1. Perfilamos la estructura del dataset
df_status(estancias)

# Algunas de las métricas que obtenemos: 
# q_zeros: cantidad de ceros (p_zeros: en porcentaje)
# q_inf: cantidad de valores infinitos (p_inf: en porcentaje)
# q_na: cantidad de NA (p_na: en porcentaje)
# type: factor o numérico
# unique: cantidad de valores únicos

# ¿Por qué estas métricas son importantes?
#   - Ceros: Las variables con muchos ceros no serán muy útiles para los modelos. 
#         Pueden incluso llegar a sesgar mucho el modelo.
#   - NAs: Hay modelos que incluso excluyen filas que tengan NA (RF por ejemplo). 
#         Por ello, los modelos finales pueden tener sesgos derivados de que falten filas. 
#   - Inf.: Si dejamos los valores infinitos, va a haber funciones de R que no sepamos siquiera cómo trabajan. No genera coherencia.
#   - Tipo: Hay que estudiarlos bien, porque no siempre vienen con el formato adecuado, pese a que visualmente lo veamos.
#   - Único: Cuando tenemos mucha variedad en los diferentes valores de datos, podemos sufrir overfitting.

# Perfilamos los datos de entrada y obtenemos la tabla de estado
datos_status=df_status(estancias, print_results = F)

# Quitamos variables que tengan más de un 60% de valores a cero
# Gestión de valores únicos
#   Currency es constante, no nos aportará nada. ¿Por qué?
vars_to_remove=filter(datos_status, p_na > 60 | unique==1 | p_inf > 60)  %>% .$variable
vars_to_remove

# Dejamos todas las columnas salvo aquellas que estén en el vector que crea df_status 'vars_to_remove'
estancias=dplyr::select(estancias, -one_of(vars_to_remove))

# Ahora vamos a hacer el profiling pero visualizando.
plot_num(estancias)

# Hacemos un boxplot para ver la distribución de las variables
boxplot(estancias[,c(16:24)],las=2)
# ¿Qué vemos en los datos?
# ¿Y si normalizamos primero? Tipos de normalización
# n0 - without normalization
# n1 - standardization ((x-mean)/sd)
# n2 - positional standardization ((x-median)/mad)
# n3 - unitization ((x-mean)/range)
# n3a - positional unitization ((x-median)/range)
# n4 - unitization with zero minimum ((x-min)/range)
# n5 - normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))
# n5a - positional normalization in range <-1,1> ((x-median)/max(abs(x-median)))
# n6 - quotient transformation (x/sd)
# n6a - positional quotient transformation (x/mad)
# n7 - quotient transformation (x/range)
# n8 - quotient transformation (x/max)
# n9 - quotient transformation (x/mean)
# n9a - positional quotient transformation (x/median)
# n10 - quotient transformation (x/sum)
# n11 - quotient transformation (x/sqrt(SSQ))
# n12 - normalization ((x-mean)/sqrt(sum((x-mean)^2)))
# n12a - positional normalization ((x-median)/sqrt(sum((x-median)^2)))
# n13 - normalization with zero being the central point ((x-midrange)/(range/2))
datos_norm = data.Normalization(estancias[,c(16:24)],type="n4",normalization = "column")

# ¿Cómo vamos a ver ahora la escala?
boxplot(datos_norm,las=2)
# ¿Hay diferencia? :-)

# Vamos a hacer una gestión de outliers para limpiar los datos: ¿qué decisión tomamos en este caso?
estancias$outlier=FALSE
for (i in 1:ncol(estancias)-1){
  columna = estancias[,i]
  if (is.numeric(columna)){
    media = mean(columna)
    desviacion = sd(columna)
    estancias$outlier = (estancias$outlier | columna>(media+3*desviacion) | columna<(media-3*desviacion))
  }
}
# Marcamos los TRUE y FALSE
table(estancias$outlier)

# Separamos el dataframe donde tenemos los outliers... creo que merecen un buen estudio
datosOutliers = estancias[estancias$outlier,]

# Marcamos los outliers en el gr?fico, los eliminamos y dibujamos
estancias=estancias[!estancias$outlier,]

# Y ya no necesitamos que haya una columna "outlier"
estancias$outlier=NULL

#####################################################################
# Análisis de correlación
#####################################################################
# Sabemos que correlación no es causalidad, pero la correlación puede ser un buen indicador;
# En especial cuando la relación es lineal. Let's take a look.
correlaciones <- cor(estancias[c(16:24)])
corrplot::corrplot(correlaciones, method="square")

# Podemos verlo a través de los grafos también
g_correlaciones <- graph.adjacency(correlaciones,
                                   weighted = TRUE,
                                   diag = FALSE,
                                   mode = "upper")
# No vamos a pintar todas las correlaciones; simplemente aquellas que estén por encima de la media
cut.off <- mean(E(g_correlaciones)$weight)
g_correlaciones_2 <- delete_edges(g_correlaciones, E(g_correlaciones)[weight < cut.off])
# Para agrupar las variables, utilizamos una representación de comunidades
c_g_correlaciones_2 <- cluster_fast_greedy(g_correlaciones_2) 
# Lo visualizamos
plot(c_g_correlaciones_2, g_correlaciones_2,
     vertex.size = colSums(correlaciones) * 10,
     vertex.frame.color = NA, 
     vertex.label.color = "black", 
     vertex.label.cex = 0.8,
     edge.width = E(g_correlaciones_2)$weight * 15,
     layout = layout_with_fr(g_correlaciones_2),
     main = "Relación variables ingreso en hospital")

#############################################################################################
#############################################################################################
# MODELADO
#############################################################################################
#############################################################################################
#############################################################################################
# 1. MODELADO - DESCRIPTIVO
#############################################################################################

#############################################################################################
# 1.1 MODELADO - DESCRIPTIVO - CLUSTERING
#############################################################################################
estanciasNum<-estancias[,c(16:24)]
boxplot(estanciasNum, las=2)
datos_norm = data.Normalization(estanciasNum,type="n4",normalization = "column")
boxplot(datos_norm)

# Vamos a calcular el número óptimo de clústers
#   Método del codo
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(datos_norm, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# ¿Cuándo decae lentamente la curva?

# Podemos en adelante: 
# Agrupar por "datos": pero utilizando los Componentes Principales
kc <- kmeans(na.omit(datos_norm), 8)
kc$centers
# ¿Qué vemos?

# Variamos algunos parámetros para visualizar mejor el gráfico
clusplot(estanciasNum, kc$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Le asignamos a cada jugador su cluster correspondiente, como una característica más
estancias$Cluster <- kc$cluster 

# Obtener el significado de los clústers
aggregate(datos_norm, by=list(kc$cluster), FUN=mean)
# Vamos a analizar con detenimiento lo que sucede

#############################################################################################
# 1.2 MODELADO - DESCRIPTIVO - REGLAS DE ASOCIACIÓN
#############################################################################################
# Algoritmos principales para hacer reglas de asociación
library(arules)
# Librería que contiene representaciones visuales de mis datos de reglas de asociación
library(arulesViz)
# Volvemos al punto original de datos - Quitando la columna del cluster
# Vamos a quitar alguna variable para el entrenamiento: ¿por qué? 
variables_a_quitar <- c("idAdmision", "idHospital", "fechaAlta","fechavisita")
datosRules <- dplyr::select(estancias, -one_of(variables_a_quitar))
str(datosRules)

# # Tenemos que discretizar las variables que no sean categóricas
datosRules$numReadmisiones = as.factor(datosRules$numReadmisiones)
datosRules$hematocrito = discretize(datosRules$hematocrito)
datosRules$neutrofilos = discretize(datosRules$neutrofilos)
datosRules$sodio = discretize(datosRules$sodio)
datosRules$glucosa = discretize(datosRules$glucosa)
datosRules$ureaSangre = discretize(datosRules$ureaSangre,categories=2,method = "frequency")
datosRules$creatinina = discretize(datosRules$creatinina)
datosRules$bmi = discretize(datosRules$bmi)
datosRules$pulso = discretize(datosRules$pulso)
datosRules$respiracion = discretize(datosRules$respiracion,categories=2,method = "frequency")
datosRules$duracionEstancia = discretize(datosRules$duracionEstancia,categories=2,method = "frequency")

# Construimos las reglas "apriori"
rules = apriori(datosRules, parameter = list(supp=0.9, conf=0.8,minlen = 3, maxlen=6))
summary(rules)
inspect(rules[1:10])

inspect(sort(rules, by="support"   , decreasing=TRUE)[1:10],itemSep = " & ",ruleSep="-->",linebreak=FALSE)
inspect(sort(rules, by="confidence", decreasing=TRUE)[1:10],itemSep = " & ",ruleSep="-->",linebreak=FALSE)
inspect(sort(rules, by="lift"      , decreasing=TRUE)[1:10],itemSep = " & ",ruleSep="-->",linebreak=FALSE)

# Visualizamos las reglas de asociación
#   lhs - Left Hand Side --> Antecedente, o elemento que precede en una regla
#   rhs - Right Hand Side --> Consecuente, o elemento que es consecuencia en una regla
rules2 = subset(rules, subset = (lhs %in% "asma=0"))
inspect(rules2)
plot(rules2, method="graph")

# Podemos sacar las reglas que "apuntan a"....
rules_toRespiracion <- subset(rules, subset = rhs %in% "respiracion=[6.5,8.15]")
rules_toRespiracion<-sort(rules_toRespiracion, by="lift", decreasing=TRUE)
inspect(rules_toRespiracion)

# Podemos sacar las reglas que "salen" de....
rules_fromRespiracion <- subset(rules, subset = lhs %in% "respiracion=[6.5,8.15]")
rules_fromRespiracion<-sort(rules_fromRespiracion, by="lift", decreasing=TRUE)
inspect(rules_fromRespiracion)

# ¿Qué información hemos obtenido?

#############################################################################################
# 1.3 MODELADO - DESCRIPTIVO - ANÁLISIS DE COMPONENTES PRINCIPALES
#############################################################################################
# Definimos algún grupo para representarlos
estancias$sobrepeso<-ifelse(estancias$bmi>29.8,"Sobrepeso","No sobrepeso")

# perform pca and extract scores
pcaOutput <- prcomp(as.matrix(estancias[, c(16:24)]), scale = TRUE, center = TRUE)
pcaOutput
summary(pcaOutput) #¿Qué vemos?
pcaOutput2 <- as.data.frame(pcaOutput$x)

#############################################################################################
# 2. MODELADO - PREDICTIVO
#############################################################################################

# Vamos a quitar alguna variable para el entrenamiento: ¿por qué? 
variables_a_quitar <- c("idAdmision", "idHospital", "fechaAlta","fechavisita")
estancias <- dplyr::select(estancias, -one_of(variables_a_quitar))

# Para poder hacer frente a este problema, lo que hacemos es dividir unos datos para entrenamiento y otros 
#   para evaluar el modelo
# El paquete de R "caret" nos ayuda en eso.
set.seed(998)
indice <- createDataPartition(estancias$duracionEstancia, 
                              p = .75, 
                              list = FALSE)
training <- estancias[indice,]
testing <- estancias[ - indice,]
# Me voy a construir un dataframe para evaluar qué tal ha aprendido a predecir mi modelo
verificacionPredicciones <- subset(testing, select = c(duracionEstancia))

# 1. Entrenamos el primer modelo
parametrosEntrenamiento <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10)

# Acá lo que hago es paralelizar el entrenamiento de mi modelo
cl <- makeCluster(detectCores())
registerDoParallel(cl)
# doParalell, doMC: que me permiten paralelizar el entrenamiento; es decir hacer procesos de ML sobre BD

# (1) Regresión lineal: LM
modelo_lm1 <- train(duracionEstancia ~ .,
                    data = training,
                    method = "lm",
                    trControl = parametrosEntrenamiento)

# Detener el proceso de paralelización de mis datos
stopCluster(cl)
# Vamos a ver un resumen del modelo
summary(modelo_lm1)
# Vamos a ver su RMSE
modelo_lm1
# Lo representamos
verificacionPredicciones$modelo_lm1 <- predict(modelo_lm1,testing)

# A partir de aquí podemos tener en algunos casos problemas de tipos de da

#### CART (Classification And Regression Trees) ####
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
modelo_arbol <- train(duracionEstancia~ ., 
                 data = training, 
                 method = "rpart", 
                 trControl = ctrl, 
                 tuneLength = 20, 
                 metric = "RMSE")
stopCluster(cl)

modelo_arbol
plot(modelo_arbol)

verificacionPredicciones$modelo_arbol <- predict(modelo_arbol,testing)

#### Ridge Regression ####
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
lambdas <- seq(1,0,-0.001)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
modelo_ridge <- train(duracionEstancia~ ., 
                  data = training, 
                  method = "glmnet", 
                  trControl = ctrl, 
                  tuneLength = 20, 
                  metric = "RMSE", 
                  tuneGrid=expand.grid(alpha=0,
                                       lambda=lambdas))
stopCluster(cl)
modelo_ridge
plot(modelo_ridge)

verificacionPredicciones$modelo_ridge <- predict(modelo_ridge,testing)