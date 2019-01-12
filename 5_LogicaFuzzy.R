#####    Fuzzy Logic     #####

#install.packages("sets", dependencies = T)
#library(sets)


# PROBLEMA PROPOSTO:
  # Sistema Especialista para Asma (exemplo didatico)
# OBJETIVO:
  # determinar a  classificacao da gravidade da asma
# VARIAVEIS LINGUISTICAS:
  # frequencia de crises
      # ('<= 2 semanas', '> 2 semanas', 'diario', 'continuo')
      # centros = (30, 60, 70, 90)
  # uso do SABA (medicamento)
      # ('<= 2 semanas', '> 2 semanas', 'diario', '2x ao dia')
      # centros = (20, 30, 70, 90)
  # debito expiratorio (medido por exame)
      # ('50-80%', '33-55%', '<33%')
      # centros = (20, 30, 70)
  # gravidade:
      # ('Moderada', 'Aguda Grave', 'Risco de Vida')
      # centros = (30, 70, 90)

exec_LogicaFuzzy = function() {
  # criacao do universo
  sets_options("universe", seq(1, 100, 1))   # universo eh uma sequencia de 1 a 100 indo de 1 em 1
  
  # criacao das variaveis linguisticas
  variaveis = set(
    frequencia = fuzzy_partition(varnames = c(menosDe2Semanas = 30, MaisDe2Semanas = 60, Diario = 70, Continuo = 90), radius = 20, FUN = fuzzy_cone),
    usoSABA   = fuzzy_partition(varnames = c(MenosDe2Semanas = 20, MaisDe2Semanas = 30, Diario = 70, DuasXPorDia = 90), sd=10),
    debitoExp = fuzzy_partition(varnames = c(Cinq_Oitenta = 20, TrintaTres_CinqCinco = 30, maisDeTrintaTres = 70), sd=10),
    gravidade = fuzzy_partition(varnames = c(Moderada = 30, AgudaGrave = 40, RiscoDeVida = 60), sd=10)
  )
  
  # definicao das regras
  regras = set(
    fuzzy_rule(frequencia %is% MenosDe2Semanas && usoSABA %is% MenosDe2Semanas && debitoExp %is% Cinq_Oitenta, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MenosDe2Semanas && usoSABA %is% MenosDe2Semanas && debitoExp %is% TrintaTres_CinqCinco, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MenosDe2Semanas && usoSABA %is% MenosDe2Semanas && debitoExp %is% maisDeTrintaTres, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MenosDe2Semanas && usoSABA %is% MaisDe2Semanas && debitoExp %is% maisDeTrintaTres, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MaisDe2Semanas && usoSABA %is% MenosDe2Semanas && debitoExp %is% Cinq_Oitenta, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MaisDe2Semanas && usoSABA %is% MenosDe2Semanas && debitoExp %is% maisDeTrintaTres, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MaisDe2Semanas && usoSABA %is% MaisDe2Semanas && debitoExp %is% Cinq_Oitenta, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% MaisDe2Semanas && usoSABA %is% MaisDe2Semanas && debitoExp %is% TrintaTres_CinqCinco, gravidade %is% Moderada),
    fuzzy_rule(frequencia %is% Diario && usoSABA %is% Diario && debitoExp %is% TrintaTres_CinqCinco, gravidade %is% AgudaGrave),
    fuzzy_rule(frequencia %is% Diario && usoSABA %is% Diario && debitoExp %is% Cinq_Oitenta, gravidade %is% AgudaGrave),
    fuzzy_rule(frequencia %is% Diario && usoSABA %is% DuasXPorDia && debitoExp %is% TrintaTres_CinqCinco, gravidade %is% AgudaGrave),
    fuzzy_rule(frequencia %is% Diario && usoSABA %is% DuasXPorDia && debitoExp %is% maisDeTrintaTres, gravidade %is% AgudaGrave),
    fuzzy_rule(frequencia %is% Continuo && usoSABA %is% Diario && debitoExp %is% TrintaTres_CinqCinco, gravidade %is% RiscoDeVida),
    fuzzy_rule(frequencia %is% Continuo && usoSABA %is% Diario && debitoExp %is% maisDeTrintaTres, gravidade %is% RiscoDeVida),
    fuzzy_rule(frequencia %is% Continuo && usoSABA %is% DuasXPorDia && debitoExp %is% TrintaTres_CinqCinco, gravidade %is% RiscoDeVida),
    fuzzy_rule(frequencia %is% Continuo && usoSABA %is% DuasXPorDia && debitoExp %is% maisDeTrintaTres, gravidade %is% RiscoDeVida)
  )
  
  # construcao do sistema (modelo de inferencia)
  sistema = fuzzy_system(variaveis, regras)
  sistema
  plot(sistema)
  
  # fazendo inferencia
  valoresEntrada = list(frequencia = 80, usoSABA = 70, debitoExp = 80)   # valores a serem inferidos
  inferencia = fuzzy_inference(sistema, valoresEntrada)
  plot(inferencia)
  
  # processo de defuzzyficacao
  defuzzificado = gset_defuzzify(inferencia, method = "centroid")
    # method = ('meanofmax', 'smallestofmax', 'largestofmax', 'centroid')
  print(defuzzificado)
  
  # analisando o resultado
  plot(sistema$variables$gravidade)
  lines(inferencia, col = "red", lwd=4)
  
  # desfaz o universo
  sets_options("universe", NULL)
}
exec_LogicaFuzzy()
