

# Regression Trees. ---------------------------------------------------------------------------

library(rpart)
library(rpart.plot)
library(caret)

# Data
bh <- read.csv("../data/tema4/BostonHousing.csv")
t.id <- createDataPartition(bh$MEDV, p = .7, list = F)

# Se predice la variable MEDV , (Mediana de Habitabilidad)
# Todas las variables son numericas 
bfit <- rpart(MEDV ~., data = bh[t.id,])
bfit

# Outputs de modelo (bfit)

# node), split, n, deviance, yval
# * denotes terminal node

# node: el nodo en cuestion, split: de los nodos o clasificacion, n: numero de casos en nodo.
# deviance: suma de los cuadrados de los errores de dichos nodos, basados en el promedio de volumen 
# de variables  que caen dentro del nodo, es decir, (la suma de los cuadrados de los errores).
# yval: el valor promedio de las variables de salida de todos los casos que caen en dicho nodo
# es decir, que cuando algun elemento caiga en ese nodo, yval sera su valor estimado. 
# * denotes terminal node, denota un nodo terminal, los nodos hoja estan marcados como (*)


par(mfrow=c(1,1))

# Representacion Grafica: 
prp(bfit, type = 2, nn=T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col = "gray")


# Resultados de los arboles con distintos numeros de nodos, asi como el promedio y la desv std
# en la variacion cruzada para cada uno de los arboles con el tamaño especificado. 
# CP = Factor de complejidad del arbol 
# nsplit = Numero de divisiones en el mejor arbol que se corresponde a CP, es decir, cuantas 
# divisiones se han hecho en el arbol para llegar a dicho factor de complejidad.
# rel error = Para el mejor arbol con el numero especifico de divisiones anterior se calcula el 
# error de clasificacion total con raiz cuadrada, sobre los datos que sean utilizados para construir
# el arbol, luego es la prop de la raiz cuadrada del error, computada a partir del nodo raiz 
# El error en el nodo raiz se basa en predecir cada caso como el promdedio de los valores de salida
# de todas las variables a traves de todos los casos. 
# xerror = Error de validacion cruzada promedio, se utiliza para de todos los arboles calculados, 
# con un numero de splits especifico, se calcula el promedio del error en la validacion cruzada.
# xstd : Desviacion estandar en la validacion cruzada, utilizada en el mejor arbol que ha salido, 
# con el numero especifico de los splits, de las divisiones.
# hablamos simpre de validacion cruzada porque para llegar al resultado se han llevado a cabo un conj
# determinado de arboles y nos hemos quedado siempre con el mejor, con el que menos error traia de serie.

# Podemos elegir e arbol con el menor error de validacion cruzada (xerror), o utilizar la regla de una 
# desviacion tipica y encontrar el arbol que encuentra dentro de una desviacion tipica del error minimo 
# que tenga menos nodos, es decir, arboles que tengan menos divisiones o sea un clasificador mas sencillo.
# min(xerror) + xstd() ; buscar el primer xerror que sea menor; logica de recorte (*)

bfit$cptable

# representacion grafica de los factores de complejidad: 
plotcp(bfit)

# arbol recortado, recorte de ramas (*)
bfitpruned <- prune(bfit, cp = 0.02343779)

# Visualización del arbol recortado: 
prp(bfitpruned, type = 2, nn=T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col = "gray")


# Prediccoines del conjunto de entrenamiento
preds <- predict(bfit, bh[t.id,])
sqrt(mean((preds - bh[t.id,]$MEDV)^2))

# Prediccion del conj de validacion. 
preds <- predict(bfit, bh[-t.id, ])
sqrt(mean((preds - bh[-t.id,]$MEDV)^2))

# Prediccion de conj de entrenamiento en modelo recortado :
preds <- predict(bfitpruned, bh[t.id,])
sqrt(mean((preds - bh[t.id,]$MEDV)^2))

# Prediccion de conj de validacion en modelo recortado :
preds <- predict(bfitpruned, bh[-t.id, ])
sqrt(mean((preds - bh[-t.id,]$MEDV)^2))

# Hay que tener en cuenta o buscar un balance entre el error en la casificacion,
# la longitud del proceso y el costo computacional de la clasificacion. 
# asi como tener en cuenta que si el arbol esta muy modelado, podemos sufrir overfitting.
# Es decir que la predicciones empeoren por estar demasiado ajustadas al conj original. 

# Luego siempre es buena practica observar la cptable y podar el arbol de forma correcta,
# eligiendo de todos los arboles que nos ha generado el modelo, aquel que esta una desviacion 
# tipica por encima del error mas pequeño, es decir tomamos el error mas pequeño y le sumamos 
# su desviacion estandar : min(xerror) + xstd = cp (cutt/off)



# Bagging & Boosting --------------------------------------------------------------------------


#predictores categóricos

ed <- read.csv("../data/tema4/education.csv")
ed$region <- factor(ed$region)

# data split 
t.id <- createDataPartition(ed$expense, p = 0.7, list = F)

# Model
fit <- rpart(expense ~ region+urban+income+under18, data = ed[t.id,])

# Plot
prp(fit, type = 2, nn=T, fallen.leaves = T, 
    faclen=4, varlen=8, shadow.col = "gray")


# Metodos de ensamblaje , para arboles de regression. 

# Bagging :
# Lo que hace es combinar de forma conjunta las predicciones de diferentes modelos, de
# diferentes arboles por separado, para dar un resultado mejor y superior
# suele ser una tecnica muy efectiva para metodos que tienen alta varianza como los trees.
library(ipred)


# Modelo utilizando mismo conj de datos y particiones 
bagging.fit <- bagging(MEDV~., data=bh[t.id, ])

# Prediccion del modelo conj datos de training
prediction.t <- predict(bagging.fit, bh[t.id,])
# RMSE
sqrt(mean((prediction.t-bh[t.id,]$MEDV)^2))

# Prediccion del modelo conj datos de validacion
prediction.v <- predict(bagging.fit, bh[-t.id,])
#RMSE
sqrt(mean((prediction.v-bh[-t.id,]$MEDV)^2))

#Boosting: 
# otro metodo de ensamblamiento desarrollado para reducir el desvio cuando los nuevos modelos 
# se construyen para aprender de los errores de mala clasificacion de modelos previos,
# es decir, cada vez que creamos un nuvo modelo, aprendemos de los errores que ese modelo genera 
# en la clasificacion, el nuevo modelo se construye preciamente a partir de ellos. 
# *El gradient boosting Machine, es una tecnica que esta generalizada y adaptada para utilizar
# los arboles de decision a la hora de clasificar o llevar a cabo una regression:::


library(gbm) # gradient boosting machine

gbmfit <- gbm(MEDV~., data = bh, distribution = "gaussian", n.trees = 10)
prediction.t <- predict(gbmfit, bh, n.trees = 10)





