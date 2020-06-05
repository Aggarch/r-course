
# K - nearest Neighbors in Regression ::: 

# La funcion knn.reg del paquete FNN para aplicar la tecnica KNN en regresion. 
# para construir el modelo y asi procesar las predicciones y ver como se comporta. 
# 

library(dummies)
library(FNN)
library(scales)
library(caret)

edu <- read.csv("../data/tema4/education.csv")
dms <- dummy(edu$region, sep = "_")

# region traducido a variables dummies variable (region)
edu <- cbind(edu, dms)


# K-nearest Neighbors se basa en calcular distancias de cada uno de los objets a los vecinos 
# se estandarizan las variables predictores la variable region ya fue estandarizada, 
# A continuación se escalan las variables numericas. entre un minimo (0), max(1).

edu$urban.s <- rescale(edu$urban)
edu$income.s <- rescale(edu$income)
edu$under18.s <- rescale(edu$under18)

# Particion de datos: 
set.seed(2018)

t.id <- createDataPartition(edu$expense, p=0.6, list = F)
tr   <- edu[t.id, ]
temp <- edu[-t.id, ]
v.id <- createDataPartition(temp$expense, p=0.5, list = F)
val  <- temp[v.id,]
test <- temp[-v.id,]


# Buscar el valor de K y evaluar error cuadratico medio (rmse)
# Funcion para hallar el error cuadratico medio: 

rmse <- function(actual, predicted){
  return(sqrt(mean((actual-predicted)^2)))
}

# la funcion knn.reg() consta de parametros importantes, por un lado las variables predictoras,
# los datos validadores a continuacion, la salida que tiene que dar en el conjunto de entrenamiento,
# el valor de (K), y el algoritmo que se debe especificar para calcular las distancias {brute}
# existen otros tipos de algoritmos ademas del ''brute'', consultar documentacion de knn.reg()


# K = (1)
reg1  <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=1,
                algorithm = "brute")
rmse1 <- sqrt(mean((reg1$pred-val$expense)^2))
rmse1

# K = (2)
reg2  <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=2,
                algorithm = "brute")
rmse2 <- rmse(val$expense, reg2$pred)
rmse2

# K = (3)
reg3  <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=3,
                algorithm = "brute")
rmse3 <- rmse(val$expense, reg3$pred)
rmse3

# extraer valores actuales y predichos 
df = data.frame(actual = val$expense, pred = reg3$pred)
plot(df)
abline(0,1)


# K = (4)
reg4 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=4,
                algorithm = "brute")
rmse4 <- rmse(val$expense, reg4$pred)
rmse4

# Errores
errors = c(rmse1, rmse2, rmse3, rmse4)


# Graficar los errores (RMSE) despues de K iteraciones
plot(errors, type = 'o', xlab = "k", ylab = "RMSE")


# Despues de identificar K, en este caso k=(4) == min(errors)
# Aplicamos la prueba, es decir el modelo KNN aplicado, pero en
# este caso, no al conj de datos de validacion, sino sobre el de testing
# el objetivo es observar si el error se mantiene igual o fluctua. 

reg.test <- knn.reg(tr[,7:12], test[,7:12], tr$expense, k=4,
                algorithm = "brute")
rmse.test <- rmse(test$expense, reg.test$pred)
rmse.test

# Generalemnte el resultado del RMSE sobre el conj de Testing no debe ser muy 
# distinto al resultado del conj de validacion. 
# es por esto que se suguere reservar dos partes distintas para evitar tomar 
# decisiones sesgadas que se ajusten demasiado a los datos que se han incorporado
# al conjunto de entrenamiento. 
# La tecnica es muy parecida a la utilizada en clasificacion, 
# Requiere que todas las variables predictoras o independientes sean numericas,
# porque buscamos obtener un valor numerico, asi que inicialmente se modifican las
# variables categoricas, a variables numericas, en este caso solo (region) que 
# paso de ser una variable categorica a una numerica utilizando dummies.
# y a continuación es recomendable reescalar todas las variables numericas, 
# para que esten en el rango de (0,1)

# Luego se realizaron 3 particiones del conjunto de datos original para que un poco mas de 
# la mitad de los datos se utilicen para crear el modelo de prediccion, y las otras dos 
# se utilicen para testear y para validacion cruzada. (Cross-Validation)

# Se han creado 4 modelos, para K = {1,2,3,4} utilizando las variables.

# Dataframe para ver que dice el modelo; donde la variable original corresponde a
# actual, test$expense, y la prediccion = pred = reg.test$pred
df = data.frame(actual = test$expense, pred = reg.test$pred)
plot(df)
abline(0,1)

# Automatizacion del proceso K-Nearest-Neighbors ----------------------------------------------

# Tener en cuenta::: 
# En el ejemplo anterior, utlizamos 3 particiones. Se pueden usar solo 2
# en cuyo caso, la funcion knn.reg, solo usaria la version original, (Cross-Validation)
# para predecir, para cada uno de los trozos de particion, 
# en este caso no pasariamos datos de testeo, parametro = NULL. 
# una vez calculado el RMSE ns quedariamos con el menor de ellos para aplicar la tecnica.

# Para usar la validacion cruzada en lugar de utilizar un conj de validacion para el modelo.
# Creamos un identificador original Particion o Split con la variable expense. 
# Nos quedamos solo con los datos de training y validation. 

t.id <- createDataPartition(edu$expense, p = 0.7, list = F)
tr <- edu[t.id, ]
val <- edu[-t.id,]

# Modelo
reg <- knn.reg(tr[,7:12], test = NULL, y = tr$expense,
               k = 3, algorithm = "brute")

rmse.reg <- sqrt(mean(reg$residuals^2))
rmse.reg

# Al no proporcionar un conj de testing para valorar, se elebora el modelo
# sin mucho margen de mejora. 

## Función para automatizar KNN
rda.knn.reg <- function(tr_predictor, val_predictors,
                          tr_target, val_target, k){
  library(FNN)
  res <- knn.reg(tr_predictor, val_predictors,
                 tr_target, k, algorithm = "brute")
  rmserror <- sqrt(mean((val_target - res$pred)^2))
  cat(paste("RMSE para k = ", toString(k), ": ", rmserror,"\n", sep = ""))
  rmserror
}

# Revisar que el conjunto de datos utilizado para la articion, 
# corresponda al conjunto original mas no a un subconjunto del mismo:::
rda.knn.reg(tr[,7:12], val[,7:12], tr$expense, val$expense, k=2)


## Función para realizar múltiples KNN
rda.knn.reg.multi <- function(tr_predictors, val_predictors,
                                tr_target, val_target, start_k, end_k){
  rms_errors <- vector()
  for(k in start_k:end_k){
    rms_error <- rda.knn.reg(tr_predictors, val_predictors,
                               tr_target, val_target, k)
    rms_errors <- c(rms_errors, rms_error)
  }
  plot(rms_errors, type = 'o', xlab = "k", ylab = "RMSE", col = "blue")
}


rda.knn.reg.multi(tr[,7:12], val[,7:12], 
                    tr$expense, val$expense, 1,30)

# Para que la funcion de knn.reg() multiple funcione, se debe primero compilar 
# la knn.reg() basica, y que esta de como output, el rmse 

# Nota: 
# La suma de los cuadrados de los errores, lo que hace es trazar
# lineas perpendiculares desde cada uno de los puntos hasta la diagonal,(error)
# El error puede ser positivo, negativo, 
# primero realizamos la suma de todos los errores, previamente elevados al cuadrado 
# con el fin de que todos sean positivos, entonces se promedian.
# una vez promediados, se saca la raiz cuadrada. 
# rmse -> root mean square error.
    
# Generalized::: 

reg.test <- knn.reg(edu[,7:12], edu[,7:12], edu$expense, k=4,
                    algorithm = "brute")


edu$pred <- reg.test$pred

edu %>% select(expense, pred) %>% plot 
abline(0,1)


