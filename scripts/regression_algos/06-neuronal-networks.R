
# Neural Networks for Regression. -------------------------------------------------------------


library(nnet)
library(caret)
library(devtools)

bh <- read.csv("../data/tema4/BostonHousing.csv")

set.seed(2018)

# Partition
# 70% para entrenamiento, el resto para validacion, array.
t.id <- createDataPartition(bh$MEDV, p= 0.7, list = F)

# Buscar el rango de variabilidad de la respuesta par apoder escalarlo a (0,1)
summary(bh$MEDV)
# programaremos al modelo para que prediga MEDV pero dividido por 50 para escalar entre (0,1)
# entre las alternativas esta construir una columna adicional, pero al conocer el max podemos:::

#Model 
# aplicamos la funcion nnet(), a la variable dependiente o target MED/50 respecto a todos los datos,
# tomando el conjunto de datos de entrenamiento, el numero de de nodos en la capa interna == 6, 
# maximo de iteraciones = 1000 , el decaimiento a sufrir de cada nodo para entrar y salir en otro 
# el parametro linout = T, especifica si queremos una salida lineal en funcion de los 
# parametros de entrada y no logistica. ; linout = FALSE. 

fit <- nnet(MEDV/50 ~., data=bh[t.id, ],
            size = 6, decay = 0.1,
            maxit = 1000, linout=T)

# Fawda 123 Neuronal Network Plot : 
# Github de un tercero cuya libreria permite representar graficamente una Neural Network:
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")


# NN Plot ::: 
plot(fit, max.sp = T)

# RMSE en los fitted.values sobre el conj de entrenamiento :

# Teniendo en cuenta que ya tenemos los datos ajustados, 
# Raiz cuadrada del promedio de los valores ajustados del modelo,
# multiplicados por 50 menos del dataset original el conjunto de training,
# nos quedamos con la columna MEDV y elevamos todo al cuadrado. 

sqrt(mean((fit$fitted.values*50-bh[t.id,"MEDV"])^2))


# Prediccion de la red neuronal sobre el conjunto de validacion:
pred <- predict(fit, bh[-t.id,])
# RMSE
sqrt(mean((pred*50 -  bh[-t.id,"MEDV"])^2))
