## Clusteriza�ao Utilizando K-Means
library(cluster)# add o K-means


## importando os dados
 dataset <- read.csv("mallData.csv") #dados do shopping
 
## filtro com Salario anual e a pontua�ao de gastos
 dataset = dataset[4:5]
 
 
 # Usando o m�todo Elbow para definir o numero ideal de clusters ( joelho do grafico)
 set.seed(6)
 wcss = vector()
 for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
 plot(1:10,
      wcss,
      type = 'b',
      main = paste('M�todo Elbow'),
      xlab = 'Numero de grupos',
      ylab = 'WCSS')

# configure o K-means com o numero correto de grupos (5 nesse caso)
 set.seed(29)
 kmeans = kmeans(x = dataset, centers = 5)
 y_kmeans = kmeans$cluster
 
 # Visualizando os grupos
 clusplot(dataset,
          y_kmeans,
          lines = 0,
          shade = FALSE,#Sombreado
          color = TRUE,#colorir grupos
          labels = 2,
          plotchar = TRUE,# diferencia os pontos no grafico por grupo true = diferente
          span = TRUE,# tamanho da elipse que repesenta os grupos, true = minimo
          main = paste('Grupos de Clientes'),
          xlab = 'Salario Anual',
          ylab = 'Pontua�ao de Gastos')
 
 