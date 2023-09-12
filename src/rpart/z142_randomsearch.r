# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\Users\veramr\OneDrive - inspt.utn.edu.ar\MsDS\dmeyn")

# Poner sus semillas
semillas <- c(555521, 555523, 555557, 555589, 555593)

# Cargamos el dataset
dataset <- fread("./datasets/competencia_01.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202103]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]

# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# Número de iteraciones para la búsqueda aleatoria
num_iterations <- 50

# Lista para almacenar los resultados de la búsqueda aleatoria
resultados_random_search <- list()

# Realizar la búsqueda aleatoria
for (i in 1:num_iterations) {
  # Generar valores aleatorios para los hiperparámetros
  cp <- runif(1, -1, 0.01)
  md <- sample(c(5, 10, 15, 30), 1)
  ms <- sample(c(1, 50, 500, 1000), 1)
  mb <- sample(c(1, as.integer(ms / 2)), 1)
  
  # Entrenar y evaluar el modelo con los hiperparámetros aleatorios
  t0 <- Sys.time()
  gan_semillas <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset[, get("clase_binaria")],
                                              p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]
    
    modelo <- rpart(clase_binaria ~ .,
                    data = dtrain,
                    xval = 0,
                    cp = cp,
                    minsplit = ms,
                    minbucket = mb,
                    maxdepth = md)
    
    pred_testing <- predict(modelo, dtest, type = "prob")
    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
    
    gan_semillas <- c(gan_semillas, gan)
  }
  tiempo <-  as.numeric(Sys.time() - t0, units = "secs")
  
  # Almacenar los resultados en la lista
  resultados_random_search[[i]] <- list(
    tiempo = tiempo,
    cp = cp,
    mb = mb,
    ms = ms,
    md = md,
    gan = mean(gan_semillas)
  )
}

# Encontrar el índice del resultado con la mayor ganancia
indice_mejor_resultado <- which.max(sapply(resultados_random_search, function(resultado) resultado$gan))

# Imprimir los mejores parámetros y resultados
mejor_resultado <- resultados_random_search[[indice_mejor_resultado]]
cat("Mejores parámetros:\n")
cat("cp:", mejor_resultado$cp, "\n")
cat("mb:", mejor_resultado$mb, "\n")
cat("ms:", mejor_resultado$ms, "\n")
cat("md:", mejor_resultado$md, "\n")
cat("Ganancia media:", mejor_resultado$gan, "\n")
