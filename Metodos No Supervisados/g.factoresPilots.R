# Diego Leon Ortiz

# Antes de nada, limpiamos el workspace, por si hubiera algun dataset o informacion cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

pilots <- read.table('data/PILOTS.DAT', col.names =  c('Group', 'Intelligence', 'Form Relations',
                                                  'Dynamometer', 'Dotting', 'Sensory Motor Coordination',
                                                  'Perservation'))
pilots$Group <- ifelse(pilots$Group == 1, 'Apprentice', 'Pilot')


head(pilots)


# Computing the principal components in R is straightforward with the functions prcomp() and princomp(). 
pilots.pca <- prcomp(pilots[,2:7])
pilots.pca
summary(pilots.pca)

# Las variables 1, 4 y 6 explicando 87% de la varianza de las características de mis pilotos
# He reducido el 50% de las variables
# he perdido un 13% de información
# pero he ganado un 50% de eficiencia a nivel de costo de computación



# Plotting of Principal Components
#   The first two principal components are often plotted as a scatterplot which may reveal 
#   interesting features of the data, such as departures from normality, outliers or non-linearity. 
#   The first two principal components are evaluated for each observation vector and plotted.

# The ggfortify package provides a handy method for plotting the first two principal components 
# with autoplot().
library(ggfortify)

# The autoplot() function also generates a useful data table of the calculated principal 
#   components we which we will use later.
pca.plot <- autoplot(pilots.pca, data = pilots, colour = 'Group')
pca.plot

# ¿Qué tipo de pilotos tenemos y a qué grupo/equipo los meteríamos?
pilots.pca <- princomp(pilots[,2:7])
# Vamos a agrupar a los diferentes pilotos en torno a estos tres ejes
comps34 <- pilots.pca$scores[,1:3]
# Clusterización con respecto a los dos primeros componentes
clus<-kmeans(comps34,centers=9) 
plot(comps34[,1],comps34[,2],col=1:3, pch=8, cex=2)
library(fpc)
plotcluster(pilots.pca$scores[,1:3], clus$cluster)
aggregate(pilots.pca$scores[,1:3],by=list(clus$cluster),FUN=mean)
# agregamos los clusters al dataframe general
pilots$Cluster <- clus$cluster 

