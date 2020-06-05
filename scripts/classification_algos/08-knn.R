#K- Nearest Neighbors

# Algoritmo no parametrico que se utiliza para clasificacion y regresion 
# Se basa en elegir dentro de un conjunto de K(elemntos) los que se encuentran mas cerca
# para catalogar o clasificar el objeto en si mismo o hacer una regresion.
# el output dependera del uso, clasificacion o regresion.
# en caso de clasificacion, un vector.
# el objeto se clasificara por "voto de la mayria de vecinos"
# cuanto mas vecinos tenga cerca el objeto, la votacion indicara que pertenece a dicha categoria.
# K -> numero de vecinos existentes en la clasificacion. 
# En Regression, se elige una propiedad valor del objeto, entonces el valor sera el promedio 
# de los valores que dictamine cada uno de sus vecinos, a que segmento pertenece el nuevo elemento?
# la eleccion de la 'K' value es la clave :::
# si del conjunto de datos hay valores que son mucho mas grandes que otros, conviene normalizarlos.
# en este ejemplo se añaden dos columnas(variables) a la matriz, corresp a Income y Family_size
# las variables previamente estandarizadas y normalizadas por la normal(Z), seran las variables indep.
# La variable factor o categorica es la variable (target) o  variable a predecir. 
# 


library(class)
library(caret)

# Importacion y normalizacion de datos. 

vac <- read.csv("../data/tema3/vacation-trip-classification.csv")
vac$Income.z <- scale(vac$Income)
vac$Family_size.z <- scale(vac$Family_size)

set.seed(2018)

# Particion de datos doble
t.ids <- createDataPartition(vac$Result, p=0.5, list = F)
train <- vac[t.ids, ]
temp  <- vac[-t.ids, ]

# De array temporal dos nuevos datasets, conjunto de validacion y testing. 
v.ids <- createDataPartition(temp$Result, p=0.5, list = F)
val   <- temp[v.ids,]
test  <- temp[-v.ids,]

# Democracia:
pred1 <- knn(train[,4:5], val[,4:5], train[,3], k = 5)
errmat1 <- table(val$Result, pred1, dnn = c("Actual", "Predichos"))
errmat1

# Dictadura:
pred2 <- knn(train[,4:5], test[,4:5], train[,3], k = 1)
errmat2 <- table(test$Result, pred2, dnn = c("Actual", "Predichos"))
errmat2


# Elegir el mejor numero de vecinos para una decision 
knn.automate <- function(tr_predictors, val_predictors, tr_target,
                         val_target, start_k, end_k){
  for (k in start_k:end_k) {
    pred <- knn(tr_predictors, val_predictors, tr_target, k)
    tab <- table(val_target, pred, dnn = c("Actual", "Predichos") )
    cat(paste("Matriz de confusión para k = ",k,"\n"))
    cat("==============================\n")
    print(tab)
    cat("------------------------------\n")
  }
}

knn.automate(train[,4:5], val[,4:5], train[,3], val[,3], 1,8)


# Datos de Control 
# version generalizada del KNN, escalar, central y entrenamiento de las variables:
# number = No. de iteraciones, repeats = No. de validaciones
trcntrl <- trainControl(method="repeatedcv", number = 10, repeats = 3)

# funcion train(), generaliza el proceso  
caret_knn_fit <- train(Result ~ Family_size + Income, data = train,
                       method = "knn", trControl = trcntrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
# realiza muestreos , entreno y vaidacion. 

caret_knn_fit
# accuracy, eficacio de la asignacion,(k = 11 best-value)

# Uso del algoritmo KNN para obtener probabilidades, 
# KNN utiliza la mayoria de vecinos para determinar la clase del nuevo individuo, 
# luego se consideran todos los errores igual de importantes , como el exceso o defecto de 
# catalogar nuevos vecinos, sin embargo existiran situaciones con costes asimetricos, donde:
# se puede cometer mas de un tipo de error por encima de otro. 
# por ejemplo pueden existir situaciones en las que por circunstancias puntuales, decidamos asignar
# mas peso a los votos a favor que a los votos en contra.
# En el caso de que sea p.ej 10 veces mas costoso, clasificar un comprador como no comprador,
# que clasificar un no comprador como comprador, (10x más dificil), querremos entonces que la probabilidad,
# sea ligeramente inferior a 0.5, o 50% a la hora de clasificar dicho caso como comprador. 
# ya que al ser más "dificil", es menos probable, luego :
# para jugar con las probabilidades, en lugar de tomar la decision final en funcion
# de que sea menor o mayor a 0.5, no esta de más calcular las probabilidades con KNN y decidir 
# nosotros mismos la clasificación.

pred5 <- knn(train[,4:5], train[,4:5], train[,3], k=5, prob = T)
pred5

# generalization of the model, using probabilities and extracting the 
# probability attribute out of prediction knn.object. 

vac %>% mutate(Prob = attributes(knn(vac[,4:5], vac[,4:5], vac[,3], k=5, prob = T))$prob)

