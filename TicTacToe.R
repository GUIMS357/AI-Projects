library(neuralnet)# para confecção da rede neural
library (lattice)## ambos para uso da caret
library(ggplot2)##
library (caret)# para validaçao cruzada

####Importar dados

dados <- read.csv("JogoDaVelha.csv")  ## dataset onde o=0 x=1 e b(branco) = 3

####Particionar os dados

#partition <- createDataPartition(y=dados, p = 0.75, list = TRUE)
#treino <- dados[partition,]
#teste <- dados[-partition,]

part <- sample(2, nrow(dados), replace = TRUE, prob = c(0.75, 0.25))
treino <- dados[part==1,]
teste <- dados[part==2,]

#### CV via caret
folds <- createFolds(y=treino$Class, k=10, list=T)
sapply(folds, length)

train_control <- trainControl(method="cv", number = 10)

grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))


####Treinar a rede neural

nHidden <- 1
for (i in 1:12)## melhor numero de neuronios na hidden layer
{
  minha_rede_neural_teste <- neuralnet(Class ~ topleftsquare+topmiddlesquare+toprightsquare+
                                   middleleftsquare+middlemiddlesquare+middlerightsquare+
                                   bottomleftsquare+bottommiddlesquare+bottomrightsquare,
                                 hidden = nHidden,
                                 data = treino,
                                 err.fct = "ce",
                                 linear.output = FALSE,
                                 lifesign = "minimal",
                                 stepmax = 1e6,
                                 rep = 10,
                                 learningrate = 0.1) 
  nHidden <- (nHidden+1)
}


minha_rede_neural <- neuralnet(Class ~ topleftsquare+topmiddlesquare+toprightsquare+
                                       middleleftsquare+middlemiddlesquare+middlerightsquare+
                                       bottomleftsquare+bottommiddlesquare+bottomrightsquare,
                                     hidden = 10,
                                     data = treino,
                                     err.fct = "ce",
                                     linear.output = FALSE,
                                     lifesign = "minimal",
                                     stepmax = 1e6,
                                     rep = 10,
                                     learningrate = 0.1) 


plot(minha_rede_neural, rep=4)

####Testar a rede neural
####Matriz de confusao
####Misclassification error

avaliar_treino <- compute(minha_rede_neural, treino, rep=4)
pred_treino <- ifelse(avaliar_treino$net.result>0.5, 1, 0)
matriz_confusao_treino <- table(pred_treino, treino$Class)
matriz_confusao_treino

####Medias de desempenho

vp_treino <- matriz_confusao_treino[1, "0"]
fp_treino <- matriz_confusao_treino[2, "0"]
fn_treino <- matriz_confusao_treino[1, "1"]
vn_treino <- matriz_confusao_treino[2, "1"]


####exatidao/acuracia/accuracy
acuracia_treino <- (vp_treino+vn_treino)/(vp_treino+vn_treino+fp_treino+fn_treino)
acuracia_treino*100

####precisao/precision
precisao_treino <- vp_treino/(vp_treino+fp_treino)
precisao_treino*100

####sensibilidade/recall/sensibility
sensibilidade_treino <- vp_treino/(vp_treino+fn_treino)
sensibilidade_treino*100

####especificidade/specificity
especificidade_treino <- vn_treino/(vn_treino+fp_treino)
especificidade_treino*100


####avaliar teste
avaliar_teste <- compute(minha_rede_neural, teste, rep=4)
pred_teste <- ifelse(avaliar_teste$net.result>0.5, 1, 0)
matriz_confusao_teste <- table(pred_teste, teste$Class)
matriz_confusao_teste

vp_teste <- matriz_confusao_teste[1, "0"]
fp_teste <- matriz_confusao_teste[2, "0"]
fn_teste <- matriz_confusao_teste[1, "1"]
vn_teste <- matriz_confusao_teste[2, "1"]

####exatidao/acuracia/accuracy
acuracia_teste <- (vp_teste+vn_teste)/(vp_teste+vn_teste+fp_teste+fn_teste)
acuracia_teste*100

####precisao/precision
precisao_teste <- vp_teste/(vp_teste+fp_teste)
precisao_teste*100

####sensibilidade/recall/sensibility
sensibilidade_teste <- vp_teste/(vp_teste+fn_teste)
sensibilidade_teste*100

####especificidade/specificity
especificidade_teste <- vn_teste/(vn_teste+fp_teste)
especificidade_teste*100



####Misclassification error
1-sum(diag(matriz_confusao_treino)/sum(matriz_confusao_treino))
1-sum(diag(matriz_confusao_teste)/sum(matriz_confusao_teste))

