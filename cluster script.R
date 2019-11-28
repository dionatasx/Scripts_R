library(cluster)
library(dplyr)

setwd("~/Desktop/Bases/Cluster/")

#Carregar base de dados: 
mcdonalds <- read.table("MCDONALDS.csv", sep = ";", dec = ",", header = T)

rownames(mcdonalds) <- mcdonalds[,1]
mcdonalds <- mcdonalds[,-1]

#Padronizar variáveis
mcdonalds.padronizado <- scale(mcdonalds)

#calcular as distâncias da matriz utilizando a distância euclidiana
distancia <- dist(mcdonalds.padronizado, method = "euclidean")

#Calcular o Cluster: métodos disponíveis "average", "single", "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#Criar o gráfico e destacar os grupos
rect.hclust(cluster.hierarquico, k = 4, border = 2:5)




# Cluster K means ---------------------------------------------------------

set.seed(5)

library(cluster)    # Algoritmos de cluster
library(factoextra) #Visualização dos dados
library(gridExtra)

#Rodar o modelo
mcdonalds.k2 <- kmeans(mcdonalds.padronizado, centers = 2, nstart = 25 , iter.max = 100)


#Visualizar os clusters
fviz_cluster(mcdonalds.k2, data = mcdonalds.padronizado, main = "Cluster K2")




#Criar clusters
mcdonalds.k3 <- kmeans(mcdonalds.padronizado, centers = 3, nstart = 25)
mcdonalds.k4 <- kmeans(mcdonalds.padronizado, centers = 4, nstart = 25)
mcdonalds.k5 <- kmeans(mcdonalds.padronizado, centers = 5, nstart = 25)



#Criar gráficos
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = mcdonalds.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 5")

#Imprimir gráficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)




# Validar modelo ----------------------------------------------------------

library(purrr)

set.seed(5)
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "wss")
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "silhouette")

fviz_nbclust(mcdonalds.padronizado, FUN = hcut, method = "wss")
fviz_nbclust(mcdonalds.padronizado, FUN = hcut, method = "silhouette")

#obrigado
