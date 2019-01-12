#####    Sistemas Especialistas     #####

#install.packages("expert")
#library(expert)

# =====   Funcoes Auxiliares - Modularizacao do codigo   ===== #
coletaDados = function() {
  expert1 = list(semente1 = c(75, 80, 85), semente2 = c(10, 15, 20), interesse = c(650, 800, 850))
  expert2 = list(semente1 = c(80, 90, 95), semente2 = c(25, 30, 35), interesse = c(500, 600, 700))
  expert3 = list(semente1 = c(65, 70, 80), semente2 = c(20, 25, 30), interesse = c(450, 650, 800))
  x = list(expert1, expert2, expert3)   # organiza os valores respondidos pelos especialistas
  probabilidade = c(0.1, 0.5, 0.9)  # Quantis 10, 50, 90
  sementesCorretas = c(80,25)
  
  resposta = list(x = x, probabilidade = probabilidade, sementesCorretas = sementesCorretas)
  return(resposta)
}

visualizaGraficos = function(inferencia) {
  par(bg = "white")
  split.screen(c(2,2))
  
  screen(1)
  hist(inferencia, col = "gray", main = "Distribuicao Agregada")
  
  screen(2)
  s = density(c(650, 800, 850))
  plot(s, main = "Especialista 1")
  polygon(s, col = "blue")
  
  screen(3)
  s = density(c(500, 600, 700))
  plot(s, main = "Especialista 2")
  polygon(s, col = "blue")
  
  screen(4)
  s = density(c(450, 650, 800))
  plot(s, main = "Especialista 3")
  polygon(s, col = "blue")
  
  close.screen(all = TRUE)
}



# =====   Modelo: Classico Cooke   ===== #
se_ModeloCooke = function() {
  dados = coletaDados();
  
  inferencia_Coo = expert(dados$x, method = "cooke", probs = dados$probabilidade, true.seed = dados$sementesCorretas)
    # x:         dados obtidos da entrevista com os especialistas
    # method:    modelo de inferencia a ser utilizado ('cooke', 'ms', 'wheights')
    # probs:     os quantis a serem usados
    # true.seed: valores corretos para as sementes
  
  hist(inferencia_Coo, col = "blue")
  
  visualizaGraficos(inferencia_Coo)
  
  print(summary(inferencia_Coo))
  print(quantile(inferencia_Coo))
  mean(inferencia_Coo)
  
  distribCumulativa = cdf(inferencia_Coo); plot(distribCumulativa)
  freqAcumulada = ogive(inferencia_Coo); plot(freqAcumulada)
  
}
se_ModeloCooke()



# =====   Modelo: Mendel-Sheridan   ===== #
se_MendelSheridan = function() {
  dados = coletaDados();
  
  inferencia_MS = expert(dados$x, method = "ms", probs = dados$probabilidade, true.seed = dados$sementesCorretas)
  
  hist(inferencia_MS, col = "blue")
  visualizaGraficos(inferencia_MS)
  print(summary(inferencia_MS))
  print(quantile(inferencia_MS))
  print(mean(inferencia_MS))
  
  distribCumulativa = cdf(inferencia_MS); plot(distribCumulativa)  # distribuicao cumulativa
  freqAcumulada = ogive(inferencia_MS); plot(freqAcumulada)        # frequencia acumulada
}
se_MendelSheridan()



# =====   Modelo: Manual   ===== #
se_Manual = function() {
  # No Modelo de Atribuicao Manual, eu defino os pesos de importancia da opiniao de cada especialista
  expert1 = list(interesse = c(650, 800, 850))
  expert2 = list(interesse = c(500, 600, 700))
  expert3 = list(interesse = c(450, 650, 800))
  x = list(expert1, expert2, expert3)   # organiza os valores respondidos pelos especialistas
  probabilidade = c(0.1, 0.5, 0.9)  # Quantis 10, 50, 90
  pesos = c(0.2,0.6,0.2)            # peso (%) de importancia para as opinioes de cada especialista
  
  inferencia_Man = expert(x, method = "weights", probs = probabilidade, w = pesos)
  
  hist(inferencia_Man, col = "blue")
  visualizaGraficos(inferencia_Man)
  print(summary(inferencia_Man))
  print(quantile(inferencia_Man))
  print(mean(inferencia_Man))
}
se_Manual()
