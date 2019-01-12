#####    Naive Bayes     #####

#install.packages("bnlearn")
#library(bnlearn)

#install.packages("caret")
#library(caret)


# PROBLEMA PROPOSTO:
  # Criar, Testar e Avaliar modelo de previsao de grau de severidade de acidentes para uma seguradora de veiculos


naiveBayes = function(){
  # Divide o conjunto de dados para Treino e para Teste
  particao = createDataPartition(1:20000, p = 0.7)
    # particiona 20k linhas (tamanho do conjunto de dados 'insurance')
    # p: porcentagem dos dados utilizados para treino
  seguroTreino = insurance[particao$Resample1,]
  seguroTeste = insurance[- particao$Resample1,]
  dim(seguroTreino); dim(seguroTeste)
    # visualiza divisao das linhas do registro dos dados de Treino e de Teste
  
  # Cria o modelo
  modelo = naive.bayes(x= seguroTreino, training = "Accident")
    # x: conjunto de dados utilizado
    # training: variavel de treinamento (classe)
  print(modelo)
  plot(modelo)
  
  # Testa o modelo obtido
  previsao = predict(modelo, seguroTeste)
  #print(previsao)   # descomente para visualizar a previsao
  
  # Avalia a previsao realizada
  confusionMatrix(previsao, seguroTeste$Accident) 
}
naiveBayes()
