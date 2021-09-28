# Diego Leon Ortiz - Métodos no supervisados

#Limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list=ls())

#Cambiar el directorio de trabajo
setwd(dirname("E:/MLearning"))
getwd()

#Cambiar ruta de lectura
datos<-read.csv("basketball.csv",encoding = "UTF-8", header = T)
dim(datos)
summary(datos)

hist(datos$height)


#Nos creamos una funcion para normalizar los datos
normalize = function(x){
    return ((x-min(x))/(max(x) - min(x)))
}

#Aplicamos la normalizacion a los datos
datos = as.data.frame(lapply(datos[,1:5], normalize))

#Tenemos que ejecutar tres pasos
#   (1)calcular el numero de agrupaciones optimo
#   (2) Hacer el numero de grupos optimo (es decir, el clustering)
#   (3) Interpretar las caracteristicas medias de esos clusters


#PASO 1: calcular el numero de agrupaciones optimo
#Creamos dos vectores
inter=c();
intra=c();

for(i in 1:10) {
  out=kmeans(datos,i)
  intra=append(intra,out$tot.withinss);
  inter=append(inter,out$betweenss);
}

intra 
inter

plot(intra,type='b',col="red")
lines(inter,type='b',col="green")
#Podemos concluir que tenemos 4 perfiles de jugadores

#PASO 2: Hacer el Clustering
#     Basicamente dos familias de clustering
#     - Numericos : KMEANs(clustering para datos numericos) 
            #[Centroide]: Valor medio que tendras todos los valores del mismo grupo
            
#     - Categoricos : PAM(Clustering para datos numericos + categoricos)
            #[Medoide]: Valor medio que tendras todos los valores del mismo grupo
out=kmeans(datos,4)
out
pairs(datos,col=out$cluster)

par(mfrow=c(1,1))
library(cluster)
kc<- kmeans(datos,4)
#PASO 3: Entender el significado de los clusteres
#     Obtener el significado de los clusteres
kc$centers

#Vamos a visualizar los grupos
#pairs(datos,col=kc$cluster,main="clustering, sin normalizar con outliers")
#Cargamos  la libreria "fpc", fpc: Flexible Procedures for Clustering
library(fpc)
clusterConOutliers =dist(datos, method="euclidean")
cluster.stats(clusterConOutliers,kc$cluster)

# Variamos algunos parametros para visualizar mejor el grafico
clusplot(datos, kc$cluster, color=TRUE, shade=TRUE, label=2,lines=0)

aggregate(datos, by=list(kc$cluster), FUN=mean)

