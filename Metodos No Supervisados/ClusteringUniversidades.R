# Diego Leon Ortiz - Métodos no supervisados

# El clustering nos permite entender mejor como agrupar una muestra en subgrupos
#     a partir de un conjunto de variables. Esto se puede hacer con todo tipo de variables.
#     PAM = Partitioning Around Medoids, empleando la distancia de Gower y la anchura de silueta


#   Antes de nada, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
#Limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list=ls())


#En el clustering hay que tomar siempre tres decisiones: 
#     (1) Calcular la distancia
#     (2) Elegir el algoritmo clustering
#     (3) Elegir el numero de clusters

# Vamos a utilizar para ello el dataset "College". que esta en el paquete ISLR
#Variables continuas
#     Acceptance Rate
#     Out of school tuition
#     Number of new students enrolled
#Variables categoricas
#     Whether a college is public/private
#     Whether a college is elite, defined as having more than 50% of new students who graduated
#       10% of their high school class
set.seed(1680)

library(dplyr) # Para limpieza de datos
library(ISLR) #Data college
#install.packages("ISLR")
library(cluster) #Similaridad de Gower y metodo PAM
library(Rtsne) # Grafica t-SNE
library(ggplot2)#Para visualizacion

# Limpieza de datos
#
#Acceptance rate is created by diving the number of acceptances by the number of applications
#isElite is created by labeling colleges with more than 50% of their new students who were in the 
# their high school class as elite

# La tasa de aceptacion se calcula dividiendo el numero de admitidos entre el total de solicitudes
# La variable "isELite" se crea para destacar aquelas universidades que tienen mas de un 50% de estudiantes
#   en origen en el top 10% de su instituto

college_clean<-College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps,
         isElite=cut(Top10perc,
                     breaks = c(0,50,100),
                     labels = c("Not Elite","Elite"),
                     include.lowest = TRUE)) %>%
  mutate(isElite = factor(isElite)) %>%
  dplyr::select(name,accept_rate,Outstate,Enroll,Grad.Rate,Private,isElite)

str(college_clean)

# (1) Calcular la distancia
#     - Necesitamos definir alguna nocion de (dis)similaridad entre las observaciones.
#     - Una eleccion muy popular es la distancia euclidea
#     - Sin embargo, se trata de una metrica solo valida para variables continuas
#     - En este caso, necesitamos alguna medida que pueda trabajar con tipos de datos mixtos
#     - Esa metrica se denomia la distancia de Gower. Utiliza medidas de ditancia para cada tipo
#         escalando cada una de ellas entre 0 y 1
#         *quantitative(interval): la distancia de Manhattan normalizada en rango
#         *ordinal : distancia de Manhattan con ajustes
#         *nominal : coeficiente de Dice
#     . Es una metrica muy sensible a no normalidad y a los outliers en los datos continuos
# Agrega la distancia entre variables continuas(numericas) y la similitud entre variables categoricas(texto)
gower_dist <- daisy(college_clean[,-1],
                    metric="gower",
                    type=list(logratio=3))
summary(gower_dist)

#Podemos comprobar aquellas universidades que son mas similares y las que menos 
#Primero, cogemos la matriz a partir de la distancia de Gower
gower_mat <-as.matrix(gower_dist)

#La pareja mas similar(similitud matemática: distancia para variables numericas y semejanza para variables categoricas)
college_clean[which(gower_mat==min(gower_mat[gower_mat != min(gower_mat)]),
                    arr.ind=TRUE)[1,],]

#La pared menos parecida
college_clean[which(gower_mat==max(gower_mat[gower_mat != max(gower_mat)]),
                    arr.ind=TRUE)[1,],]

# (2) Elegir el algoritmo de clustering
#   -Hay muchos algoritmos que permiten trabajar con matrices de distancia personalizadas
#   - Nosotros usaremos PAM: Partitioning Around Medoids. Pasos:
#   - Es muy parecido al algoritmo K-Means. Salvo que utiliza la distancia Euclidea como "centroide"


# (3)Elegir el numero de clusteres
#   -Hay muchas metricas para ello
#   -Usaremos la anchura de la silueta, una metrica de validacion interna que es una medida agregada de como de similar es 
#       cada observacion a su cluster vecino mas cercano

sil_width <-c(NA)
for(i in 2:10)
{
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k=i)
  sil_width[i]<-pam_fit$silinfo$avg.width
}

#Representamos la anchura de la silueta (cuanto mas grande mejor)
plot(1:10,sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10,sil_width)

#(4) Interpretacion del cluster
#   (4.1) A traves de la estadistica descriptiva
pam_fit <-pam(gower_dist,diss = TRUE,k=7)
pam_results <-college_clean %>%
  dplyr::select(-name)%>%
  mutate(cluster =pam_fit$clustering)%>%
  group_by(cluster) %>%
  do(the_summary=summary(.))
pam_results$the_summary

#Otra ventaja del algoritmo PAM es que el medoide sirve como ejemplo de cada cluster
college_clean[pam_fit$medoids,]

#(4.2) Via visualizacion
# -t-SNE=t-distributed Stochastic Neighborhood Embedding
# -Tecnica de reduccion de la dimensionalidad que pretende guardar la estructura local permitida
#   visibles los clusteres en 2 o 3 dimensiones

tsne_obj <-Rtsne(gower_dist, is_distance = TRUE)
tsne_data <-tsne_obj$Y %>%
  data.frame()%>%
  setNames(c("X","Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name= college_clean$name)

ggplot(aes(x=X,y=Y),data=tsne_data) + 
  geom_point(aes(color=cluster))

#Consultamos sobre algun cluster especifico
tsne_data%>%
  filter(X>15 & X<25,
         Y>-15 & Y< -10) %>%
  left_join(college_clean,by="name")%>%
  collect%>%
  .[["name"]]

