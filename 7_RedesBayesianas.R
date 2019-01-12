#####    Redes Bayesianas     #####

#install.packages("bnlearn")
#library(bnlearn)


# OBJETIVO:
  # Induzir as chances de um determinado nivel de acidente ocorrer
  # dadas as caracteristicas de um cliente

redesBayesianas = function() {
  # Induzir a estrutura da rede
  estruturaRede = hc(insurance) # utiliza algoritmo de hill-climbing para induzir a estrutura
  plot(estruturaRede)
  
  # Criar as tabelas de Distribuicao de Probabilidade
  modelo = bn.fit(estruturaRede, data=insurance)
  print(modelo$GoodStudent) # Visualizar as tabelas dos atributos
  
  # Inferencia
  cpquery(modelo, event = (Accident =='Moderate' | Accident == "Severe"),
          evidence = (Age == "Senior" & RiskAversion == "Adventurous" & MakeModel == "SportsCar"))
    # evento: evento que se quer prever, o risco de se ter um acidente moderado/severo
    # evidence: caracteristicas do cliente em estudo
}
redesBayesianas()
