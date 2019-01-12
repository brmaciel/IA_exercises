#####    Genectic Algorithm     #####

#install.packages("GA")
#library(GA)


# =====   Caso: Valor Real   ===== #
funcAdaptacao_Real = function(x) {
  # PROBLEMA PROPOSTO:
    # Encontrar valor que resolve a equacao matematica
  
  resultado = 2*x + 5 #equacao = 2*x + 5 = 20
  
  if (resultado > 20) {
    return (20 - resultado)
  } else {
    return (resultado - 20)
  }
}

ga_ValorReal = function() {
  result_Real = ga("real-value", fitness = funcAdaptacao_Real, lower = c(-20), upper = c(20),
                   popSize = 10, maxiter = 20, monitor = T, names = c("a"))
    # type:       binary, permutation, real-value
    # fitness:    funcao adaptacao
    # lower, upper: valores minimos e maximos na busca (usados para 'Permutacao' e 'Valor-Real')
    # popSize:    quantidade de cromossomos
    # maxiter:    numero maximo de iteracoes
    # names:      nomes das variaveis
    # Pcrossover: probabilidade de permutacao (padrao 0.8)
    # Pmutation:  probabilidade de mutacao (0.01 ou -)
    # elitism:    porcentagem dos individuos que sao mais adaptados e devem sobreviver (geralmente 0.5)
  
  print(summary(result_Real))          # resultado
  plot(result_Real)             # grafico da evolucao
}
ga_ValorReal()



# =====   Caso: Binario   ===== #
funcAdaptacao_Binary = function(x) {
  # PROBLEMA PROPOSTO:
    # Em uma viagem a um acampamento dispoem-se de objetos, cada objeto tem seu valor em utilidade (pontos) e seu peso.
  # OBJETIVO:
    # Escolher a melhor combinacao de objetos (maior utilidade), nao ultrapassando o peso limite
  
  pesoLimite = 15
  mochila = data.frame(itens = c('canivete', 'feijao', 'batata', 'lanterna', 'saco de dormir', 'corda', 'bussola'),
                       pontos = c(10, 20, 15, 2, 30, 10, 30),
                       peso = c(1, 5, 10, 1, 7, 5, 1))
  
  pesoTotal = sum(x * mochila$peso)
  pontosTotal = sum(x * mochila$pontos)
  
  if (pesoTotal > pesoLimite) {
    return (0)
  } else {
    return (pontosTotal)
  }
}

ga_Binario = function() {
  result_Bin = ga("binary", fitness = funcAdaptacao_Binary, nBits = 7, popSize = 10, maxiter = 15, monitor = T, 
                  names = c('canivete', 'feijao', 'batata', 'lanterna', 'saco de dormir', 'corda', 'bussola'))
    # nBits = numero de bits (usado apenas para 'Binario')
  
  print(summary(result_Bin))
  plot(result_Bin)
}
ga_Binario()



# =====   Caso: Permutacao   ===== #
funcAdaptacao_Permutacao = function(x) {
  # PROBLEMA PROPOSTO: Caixeiro Viajante
  # OBJETIVO:
    # Encontrar a menor distancia necessaria para percorrer todas as cidades
  
  distancia = 0
  
  # Codigo para comecar em uma cidade e terminar em outra
  #cidadeInicial = x[1]                 # Descomente para considerar que deve retornar a cidade inicial
  for (i in 1:4) {
    cidade1 = x[i]
    cidade2 = x[i+1]
    distancia = distancia + mapa[cidade1, cidade2]
  }
  #distancia = distancia + mapa[cidade2, cidadeInicial]   # Descomente p/ considerar que deve retornar a 1 cidade
  return (-distancia)
}

ga_Permutacao = function() {
  result_Permutacao = ga("permutation", fitness = funcAdaptacao_Permutacao,
                         lower = c(1,1,1,1,1), upper = c(5,5,5,5,5), popSize = 10, maxiter = 100, monitor = T,
                         names = c('Linden', 'Parika', 'Lethem', 'Rosignol', 'New Amsterdam'))
  
  print(summary(result_Permutacao))
  plot(result_Permutacao)
}

ga_Permutacao()

# importa distancia entre as cidades
mapa = read.csv(file.choose(), header = TRUE, sep = ';',
                row.names = c('Linden', 'Parika', 'Lethem', 'Rosignol', 'New Amsterdam'))
