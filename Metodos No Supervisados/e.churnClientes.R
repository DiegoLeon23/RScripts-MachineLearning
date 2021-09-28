# Antes de nada, limpiamos el workspace, por si hubiera algún dataset o información cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname("E:/DMC VIDEOS/MACHINE LEARNING WITH R/Sesion2"))
getwd()
# Cargamos las líbrerías que necesitamos
library(stats)
library(caTools)
# install.packages("Amelia")
library(Amelia) 
library(dplyr)
install.packages("Amelia")
# Leemos el dataset de clientes de una empresa de telecomunicaciones
datos <- read.csv("Sesion2/Data_Tema2/telcoChurn.csv")

# Vemos la estructura de los datos: ¿por qué es importante?
print(str(datos))

# Comprobamos que no haya NA
any(is.na(datos))

# Visualizamos los "missing values" usando el "missing map" del paquete Amelia
missmap(datos,col=c("yellow","red"))

# Creamos una nueva columna "tenure_interval" a partir de la columna "tenure"
group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}

# Aplicamos la función a cada fila del dataframe
datos$tenure_interval <- sapply(datos$tenure,group_tenure)
datos$tenure_interval <- as.factor(datos$tenure_interval)

# Quitamos las columnas "customerID" y "tenure" que no aportan a explicar comportamientos: ¿por qué?
datos <- dplyr::select(datos,-customerID,-tenure)

# Las siguientes variables pueden afectar al comportamiento del cliente
datos$MultipleLines <- as.character(datos$MultipleLines)
datos$OnlineSecurity <- as.character(datos$OnlineSecurity)
datos$OnlineBackup <- as.character(datos$OnlineBackup)
datos$DeviceProtection <- as.character(datos$DeviceProtection)
datos$TechSupport <- as.character(datos$TechSupport)
datos$StreamingTV <- as.character(datos$StreamingTV)
datos$StreamingMovies <- as.character(datos$StreamingMovies)

# Adaptamos un poco los datos
datos$MultipleLines[datos$MultipleLines=="No phone service"] <- "No"
datos$OnlineSecurity[datos$OnlineSecurity=="No internet service"] <- "No"
datos$OnlineBackup[datos$OnlineBackup=="No internet service"] <- "No"
datos$DeviceProtection[datos$DeviceProtection=="No internet service"] <- "No"
datos$TechSupport[datos$TechSupport=="No internet service"] <- "No"
datos$StreamingTV[datos$StreamingTV=="No internet service"] <- "No"
datos$StreamingMovies[datos$StreamingMovies=="No internet service"] <- "No"

# Lo convertimos en factor para poder trabajar con ello
datos$MultipleLines <- as.factor(datos$MultipleLines)
datos$OnlineSecurity <- as.factor(datos$OnlineSecurity)
datos$OnlineBackup <- as.factor(datos$OnlineBackup)
datos$DeviceProtection <- as.factor(datos$DeviceProtection)
datos$TechSupport <- as.factor(datos$TechSupport)
datos$StreamingTV <- as.factor(datos$StreamingTV)
datos$StreamingMovies <- as.factor(datos$StreamingMovies)

# Vamos a renombrar las columnas para manejarlas con más facilidad
names(datos)<-c("genero","senior","partner","dependientes","servicioTelefono","multiplesLineas",
                "tipoInternet","servicioSeguridad","servicioBackups","servicioProteccionDispositivo",
                "servicioSoporteTecnico","servicioStreaming","servicioPeliculas","tipoContrato",
                "tipoFactura","metodoPago","cargosMes","totalCargos","churn","permanencia")

# ¿Qué significa cada variable?
# ¿De qué nos damos cuenta a la hora de hacer modelos? ¿Queremos más?

# Quitamos aquellas filas que tengan muchos NA: nos pueden generar problemas
datos <- na.omit(datos)

# Comprobamos a ver qué tal han quedado los datos
str(datos)

############################################################################################
# 0. Análisis exploratorio de datos
############################################################################################
# (1.a) Estado de calidad de datos
#   Lo primero, cargamos las librerías necesarias
library(funModeling) #  Auditorías de calidad de datos

# Perfilamos la estructura del dataset
df_status(datos)

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
datos_status=df_status(datos, print_results = F)

# Quitamos variables que tengan más de un 60% de valores a cero
# Gestión de valores únicos
#   Currency es constante, no nos aportará nada. ¿Por qué?
vars_to_remove=filter(datos_status, p_na > 60 | unique==1 | p_inf > 60)  %>% .$variable
vars_to_remove

# Dejamos todas las columnas salvo aquellas que estén en el vector que crea df_status 'vars_to_remove'
datos=dplyr::select(datos, -one_of(vars_to_remove))

# Ahora vamos a hacer el profiling pero visualizando.
plot_num(datos)

# Boxplots
library(ggplot2)
library(clusterSim)
boxplot(datos[,c(2,17,18)],las=2)
# ¿Qué observamos? 
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
datos_norm = data.Normalization(datos[,c(2,17,18)],type="n4",normalization = "column")

# ¿Cómo vamos a ver ahora la escala?
boxplot(datos_norm,las=2)
# ¿Hay diferencia? :-)

# Fijamos la pantalla de visualización para ver una sola gráfica
par(mfrow=c(1,1))

# Vamos a hacer una gestión de outliers para limpiar los datos: ¿qué decisión tomamos en este caso?
datos$outlier=FALSE
for (i in 1:ncol(datos)-1){
  columna = datos[,i]
  if (is.numeric(columna)){
    media = mean(columna)
    desviacion = sd(columna)
    datos$outlier = (datos$outlier | columna>(media+3*desviacion) | columna<(media-3*desviacion))
  }
}
# Marcamos los TRUE y FALSE
table(datos$outlier)

# Separamos el dataframe donde tenemos los outliers... creo que merecen un buen estudio
datosOutliers = datos[datos$outlier,]

# Marcamos los outliers en el gr?fico, los eliminamos y dibujamos
datos=datos[!datos$outlier,]

# Y ya no necesitamos que haya una columna "outlier"
datos$outlier=NULL

############################################################################################
# 1. Modelado
############################################################################################
############################################################################################
# 1.1. Clustering
############################################################################################
# Cargamos las librerías necesarias
library(Rtsne)
library(cluster)
library(ggplot2)
library(reshape)
library(dplyr)
library(arules)
library(arulesViz)

# Vamos a calcular la distancia de Gower para los datos que tenemos
#   ¿Qué es la distancia de Gower?
gower_dist <- daisy(datos,
                    metric = "gower",
                    type = list(logratio = 3))

# Una buena manera de entender lo que hace la distancia de Gower es sacar las parejas más "parecidas"
#   y las menos "parecidas".
gower_mat <- as.matrix(gower_dist)
gower_mat

# La pareja más "similar"
datos[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# La pareja menos "similar"
datos[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Ahora vamos a calcular el "número de segmentos" de clientes óptimo
#   ¿Cómo hemos estado en las empresas agrupando clientes históricamente?
#   Clúster vs. Segmento
sil_width <- c(NA)

# Calculamos para un número de clústeres dado
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(1:10, sil_width,
     xlab = "Número de clústeres",
     ylab = "Anchura de la silueta")
lines(1:10, sil_width)

# ¿Con cuántos clústeres nos quedamos?
# ¿Qué significado tiene?

#   Aquella que minimice el tamaño de la silueta
pam_fit <- pam(gower_dist, diss = TRUE, k = 7)
pam_results <- datos %>%
  dplyr::select() %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

# Sacamos los medoides: ¿cómo interpretamos esto?
datos[pam_fit$medoids, ]
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

# Y lo visualizamos
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         numAsignaturas = datos$cargosMes)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

############################################################################################
# 1.2. Predictivo
############################################################################################
# Para fijar la reproducibilidad: que no se deje afectar por el azar cuando el modelo
#   sea ejecutado en cada iteración
set.seed(123)

# Nos aseguramos que los tipos de datos estén bien
str(datos)

# Vamos a enriquecerlo con algunos datos adicionales: ¿por qué hago esto?
for(i in 1:nrow(datos)){
  datos$numServiciosContratados[i]<-sum(ifelse(datos$servicioSeguridad[i]=="Yes",1,0),ifelse(datos$servicioBackups[i]=="Yes",1,0),
                                        ifelse(datos$servicioProteccionDispositivo[i]=="Yes",1,0),ifelse(datos$servicioSoporteTecnico[i]=="Yes",1,0),
                                        ifelse(datos$servicioStreaming[i]=="Yes",1,0),ifelse(datos$servicioPeliculas[i]=="Yes",1,0))
}
# Describir una "realidad", implica expresar nuevos conceptos que en bases de datos transaccionales tradicionales
#   quizás no estén. No hay un "estándar de datos a crear", sino que debemos conocer muy bien el dominio
#   y que como científicos de datos, usemos la imaginación para crear esas variables.


# Creamos (y usamos) una función para dibujar la distribución de la clase
dibuja_distribucion_clase = function (columna){
  datosplot = data.frame(table(columna))
  datosplot$Perc = datosplot$Freq/sum(datosplot$Freq)
  ggplot(datosplot,aes(x=columna,y=Perc,fill=columna))+geom_bar(position="stack", stat="identity")+
    geom_text(aes(label = Freq, x = columna, y = 0), position = position_dodge(width = 0.8), vjust = -0.6)
  
}
dibuja_distribucion_clase(datos$churn)
# ¿Tenemos problemas de balanceo de clases? Parece que no...

# Librería que incorpora todas las funcionalidades que necesitamos para hacer modelos predictivos
library(caret)
# Muestreamos los datos: 70 training - 30 test
indice = createDataPartition(datos$churn, p = 0.8, times = 1, list=FALSE)
datosTrain = datos[ indice,]
datosTest = datos[-indice,]

# Hemos particionado los datos. ¿Qué es esto? ¿Para qué sirve? Veamos los datos...
datosTrain
datosTest

str(datosTrain)

# Logistic regression model
#   (1) TRAIN: "estudiando" para el examen
modelo <- glm(churn ~ .,
              family=binomial(link="logit"),
              data=datosTrain)
print(summary(modelo))

#   (2) TEST: es decir, voy a ir a ver qué tal he estudiado/aprendido en el examen
# Testamos el modelo con los datos de test que dejamos fuera del entrenamiento
predicciones <- predict(modelo,
                        newdata=datosTest,
                        type="response")
predicciones

# Si os fijáis nos dice que numServiciosContratados, es NA: ¿por qué pasa esto? 
# Colinealidad de las variables: https://stats.stackexchange.com/questions/212903/na-in-glm-model
# Las variables, para tener capacidad predictiva, tienen que ser linealmente independientes.

# ¿Qué nos está devolviendo?
#   Son probabilidades --> ¿para qué podemos utilizar todo esto? 

# Si la probabilidad es mayor a 0.5, esos clientes los vamos a etiquetar como "Churners"
#   > 80% probabilidad de fuga como umbral
#   Etiquetar la probabilidad de fuga de un cliente
resultados <- ifelse(predicciones > 0.5,1,0)
datosTest$churn <- as.character(datosTest$churn)
datosTest$churn[datosTest$churn=="No"] <- "0"
datosTest$churn[datosTest$churn=="Yes"] <- "1"

# Calculamos la media de la tasa de fallo en clasificación
errorClasificacion <- mean(resultados!=datosTest$churn)
print(errorClasificacion)

# Tasa de precisión
tasaPrecision <- 1-errorClasificacion
print(tasaPrecision)

# Matriz de confusión
table(datosTest$churn,predicciones > 0.5)

# "Cbinding" los resultados reales con los predichos
final <- cbind(resultados,datosTest$churn)
colnames(final) <- c("predicción","real")
final_df <- as.data.frame(final)
print(final_df)

# ¿Cuánto contribuye cada variable a clasificar o predecir la variable target?
library(caret)
imp <- varImp(modelo)
imp

# Ordenamos las variables por importancia al modelo
imp_list <- rownames(imp)[order(imp$Overall, decreasing=TRUE)]
imp_list
pesoVariables <- as.data.frame(imp_list)

