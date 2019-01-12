#####    TabuSearch Algorithm     #####

# PROBLEMA PROPOSTO:
  # Buscar em um vetor um conjunto de valores que quando somados e multiplicados resultem em 7.11

compras = function(solucao) {
  
  #solucao = c(0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1)
  valores = c(1.1, 1.2, 1.25, 1.41, 1.5, 1.63, 2.05, 2.22, 2.65, 2.9, 3.04, 3.16)
  
  soma = 0;
  produto = 1;
  
  for (i in 1:12) {
    if (solucao[i] == 1) {
      soma = soma + valores[i];
      produto = produto * valores[i];
    }
  }
  
  if (soma == 7.11 & produto == 7.11) {
    return (7.11)
  } else {
    return (0)
  }
}

exec_TabuSearch = function() {
  #install.packages("tabuSearch")
  #library(tabuSearch)
  
  z = tabuSearch(size = 12, iters = 100, objFunc = compras, listSize = 9, nRestarts = 10, repeatAll = 1, verbose = T)
    # size:      numero de bits a enviar pra funcao objeto
    # iters:     numero de tentativas com diferentes combinacoes de bits
    # objFunc:   funcao objeto
    # listSize:  memoria tabu para itens proibidos
    # nRestarts: numero de vezes que ira reiniciar
    # repeatAll: quantas vezes ira repetir todo o processo
    # verbose:   se vai gerar alguma saida em forma de texto durante a execucao
  
  plot(z)
  summary(z)
  summary(z,verbose = T)
}
exec_TabuSearch()
