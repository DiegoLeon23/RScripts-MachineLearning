# "04cCPelecciones.R"
# Diego Leon Ortiz - Métodos no supervisados


# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Leemos el dataset de municipios
datos<-read.csv2("datosMunicipios.csv")
# ¿Qué hacemos aquí?
datos1<-datos[-which.max(datos$PobDic15),]


# ¿Qué información puedo sacar con todos estos datos? 
# Posibilidades: (1) Análisis de proporciones; (2) Análisis de variaciones

datos.e<-datos1[-which.max(datos1$PobDic15),] ## Quitar Madrid y Barcelona
datos.e2<-datos1[-which.max(datos1$PobDic15),]  ## Quitar Madrid y Barcelona

#########################################################
# (1) PROPORCIÓN
datos.e$VotosH15=datos.e$ResGrales15/datos.e$PobDic15 ## Votos por habitantes
datos.e$JovH15=datos.e$PobJovDic15/datos.e$PobDic15 ## Proporción de jóvenes
datos.e$MayH15=datos.e$PoblMayDic15/datos.e$PobDic15 ## Proporción de mayores
datos.e$ExtrH15=datos.e$PobExtrDic15/datos.e$PobDic15 ## Proporción de extranjeros
datos.e$DesH15=datos.e$DesDic15/datos.e$PobDic15 ## Proporción de desempleados
datos.e$ContH15=datos.e$ContDic15/datos.e$PobDic15 ## Contrataciones / habitantes
datos.e$AbstH15=datos.e$Abst15/datos.e$PobDic15 ## Abstención por habitantes

# Me creo una matriz de datos con todos los datos de proporción que me he creado
de=data.frame(datos.e$VotosH15,datos.e$JovH15,datos.e$MayH15,datos.e$ExtrH15,datos.e$ContH15,datos.e$AbstH15)
# PCA de dicho dataframe
pca=princomp(de,center=TRUE,scale=TRUE) 
# Información de los Componentes Principales
summary(pca)
# Componentes de cada vector de componentes principales
pca$loadings
# En este vector cada fila es un municipio y cada columna una coordenada con respecto al correspondiente componente principal
pca$scores 

# Vamos a agrupar a los diferentes municipios en torno a estos dos ejes
library(cluster)
library(fpc)
comps12 <- pca$scores[,1:3]
# Clusterización con respecto a los dos primeros componentes
clus<-kmeans(comps12,centers=7) 
plot(comps12[,1],comps12[,2],col=1:3, pch=8, cex=2)
plotcluster(pca$scores[,1:3], clus$cluster)
aggregate(pca$scores[,1:3],by=list(clus$cluster),FUN=mean)

# agregamos los clusters al dataframe general
datos.e$Cluster <- clus$cluster 
write.csv(datos.e, file = "municipiosConClustersProporcion.csv")


############################################################################
# (2) VARIACIÓN
datos.e2$ChVotos=datos.e2$ResGrales15/datos.e2$ResGrales11-1 ## Variación Votos
datos.e2$ChAbst=datos.e2$Abst15/datos.e2$Abst11-1 ## Variación Abstención
datos.e2$ChJov=datos.e2$PobJovDic15/datos.e2$PobJovDic11-1 ## Variación Jóvenes
datos.e2$ChMay=datos.e2$PoblMayDic15/datos.e2$PoblMayDic11-1 ## Variación Mayores
datos.e2$ChExtr=datos.e2$PobExtrDic15/datos.e2$PobExtrDic11-1 ## Variación Extranjeros
datos.e2$ChDes=datos.e2$DesDic15/datos.e2$DesDic11-1 ## Variación Desempleo
datos.e2$ChCont=datos.e2$ContDic15/datos.e2$ContDic11-1 ## Variación Contrataciones
datos.e2$ChPob=datos.e2$PobDic15/datos.e2$PobDic11-1 ## Variación Población

de2=data.frame(datos.e2$ChVotos,datos.e2$ChAbst,datos.e2$ChJov,datos.e2$ChMay,datos.e2$ChExtr,datos.e2$ChDes, datos.e2$ChCont, datos.e2$ChPob) ## dataframe para el análisis del informe

pca2=princomp(de2,center=TRUE,scale=TRUE)
summary(pca2)
# Componentes de cada vector de componentes principales
pca2$loadings 
# En este vector cada fila es un municipio y cada columna una coordenada con respecto al correspondiente componente principal
pca2$scores 

# Vamos a agrupar a los diferentes municipios en torno a estos dos ejes
comps34 <- pca2$scores[,1:3]
# Clusterización con respecto a los dos primeros componentes
clus<-kmeans(comps34,centers=9) 
plot(comps34[,1],comps34[,2],col=1:3, pch=8, cex=2)
plotcluster(pca2$scores[,1:3], clus$cluster)
aggregate(pca2$scores[,1:3],by=list(clus$cluster),FUN=mean)
# agregamos los clusters al dataframe general
datos.e2$Cluster <- clus$cluster 
write.csv(datos.e2, file = "municipiosConClustersVariacion.csv")

#########################################################