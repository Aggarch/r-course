
#Neural Networks.

# Las redes neuronales, llamadas tambien sistemas conexionistas 
# Modelo algoritmico se base en un gran conunto de unidades neuronales simples,
# "neuronas sinteticas" simula el comportamiento de los axones de las neuronas 
# de los organismos biologicos.

# Cada unidad se conecta con muchas otras, y los enlaces entre ellas pueden incrementar, 
# o inhibir el estado de las neuronas que estan adyacentes a la misma.
# cada unidad emplea operaciones de suma, puede existir una funcion limitadora, 
# una funcion umbral en cada una de las conexiones de la propia unidad, de modo que la señal
# puede sobrepasar un limite antes de propagarse a otra neurona, estos sistemas van aprendiendo,
# y se forman a si mismos en lugar de ser programados de forma exxplicita, de hecho, son muy 'divertidos'
# porque nadie sabe como funcionan, y se denominan las 'black boxes' de la clasificacion, 
# normalmente se tienen unas cuantas variables de entrada, y otras menos de salida.
# hay 'al menos' una capa interna, y nunca pasa de una capa a la sgte al menos que se cumplan una
# serie de condiciones que internamente deben cumplirse.

# Parametros de intrada o Inputs -> Observables. 
# Capas Internas  -> No observables. 
# Parametros de salida o Outputs -> clasificacion observable.

# Las redes neuronales pueden ser tan complejas, que incluso el output puede ser:
# Binomial : 1, 2 ; si, no ; Alto, Bajo.
# Rango: un rango de valores. 


library(nnet)
library(caret)
library(ROCR)


#Data

# las variables predictoras (independientes) se requieren numericas, y la
# variable a predecir (target) o (dependiente), se requiere categorica
# nnet(), crea variables dummy, que se encargan de corregir errors del df
bn <- read.csv("../data/tema3/banknote-authentication.csv")
bn$class <- factor(bn$class)

#Partition 
t.id <- createDataPartition(bn$class, p= 0.7, list = F)

# Modelo con variable target, class, en funcion del resto V como predictores.
mod <- nnet(class ~ ., data = bn[t.id,], 
            size = 3, maxit = 10000, decay = .001, rang = 0.05,
            na.action = na.omit, skip = T)

# size  = No. de capas ocultas a utilizar; Promedio de No. de V observables. 
# maxit = Maximo de iteraciones hasta encontrar convergencia. 
# decay = Controlar el overfitting. 
# rang  = Especifica el rango de pesos iniciales a asignar a la nnet()



#rang * max(|variables|) ~ 1
apply(bn, 2, max)

# Prediccion sobre los datos del conjunto de opuesto al entrenamiento, 
# con el modelo entrenado:
pred <- predict(mod, newdata = bn[-t.id,], type = "class")

# Matriz de confusion:
table(bn[-t.id,]$class, pred,dnn = c("Actual", "Predichos"))
# Perfecta matriz con pocas iteraciones.
# NNets son muy usadas en segmentación, reconocimiento de imagenes,
# Clasificacion, no requiere mucho esfuerzo.
# evitar los valores NA. (parametro --- na.action = 'action')
# skip = TRUE; añade una capa adicional, para separar los nodos de entrada de
# los nodos de salida. 
# rango = especificar el peso aleatorio que se debe dar inicialmente a la nnet.
# si por defecto, los parametros de entrada tienen valores muy grandes, 
# intentar que el rango se ajuste de modo que el rango por el max de las variables 
# sea cercano a 1

# Rango por el maximo del valor absoluto de las variables, sea lo mas cercano a (1)
#rang * max(|variables|) ~ 1
apply(bn, 2, max)  # max() * 0.05

# para calcular valores probabilisticos y evaluar desempeño: 
pred2 <- predict(mod, newdata = bn[-t.id,], type = "raw")
perf <- performance(prediction(pred2, bn[-t.id,"class"]), 
                    "tpr", "fpr")
plot(perf) # Perfect ROCR
