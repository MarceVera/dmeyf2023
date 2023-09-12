import numpy as np

np.random.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob)


# defino las jugadoras
tiros = 10
cant_jugadoras = 100
mejor = 0.7
segunda = 500
peloton = np.array(range(segunda, (segunda+cant_jugadoras))/1000)
jugadoras = np.append(mejor, peloton)

# veo que tiene el vector
jugadoras

for i in range(tiros):
  vaciertos = []
  for jugadora in jugadoras:
    vaciertos.append(ftirar(jugadora, tiros))
  mejor_ronda = np.argmax(vaciertos)
  aciertos_torneo = vaciertos[mejor_ronda]
  aciertos_segunda = ftirar(jugadoras[mejor_ronda], tiros)
  print(aciertos_torneo, "\t", aciertos_segunda)
