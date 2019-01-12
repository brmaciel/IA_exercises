#####    Simulated Annealing Algorithm     #####

# PROBLEMA PROPOSTO:
  # Buscar o minimo global da equacao (1-x)^2 + 100(y-x^2)^2
  # Resultado: minimo global = 0 quando (x,y) = (1,1)

Rosenbrocks = function(z) {
  x = z[1]
  y = z[2]
  
  return ( (1-x)^2 + 100*(y-x^2)^2 )
}


exec_SimulatedAnnealing = function() {
  #install.packages("GenSA")
  #library(GenSA)
  
  resultado = GenSA(lower = c(0,0), upper = c(9,9), fn = Rosenbrocks)
    # lower:   valor minimo que pode tentar no processo de otimizacao
    # upper:   valor maximo que pode tentar no processo de otimizacao
    # fn:      funcao objeto
  print(resultado)
  resultado$par   # valores de x e y obtidos
}

exec_SimulatedAnnealing()
