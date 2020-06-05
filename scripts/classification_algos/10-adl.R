# Analisis del discriminante lineal ADL

# Relacionado con el analisis de la varianza y de la regresion, en el sentido de que,
# ambos intentas expresar las variables independientes como combinacion lineal del 
# resto de caracteristicas o medidas, sin embargo la ANOVA, utiliza variables independientes
# que son categoricas y la variable dependiente continua, en el caso del 'ADL', 
# las variables independientes son continuas y la dependiente es categorica.
# La "etiqueta" de catalogo para el objeto 
# Se trata de observar similitudes y diferencias entre los diferentes objetos a clasificar.
# Son las medias parecidas?, y las varianzas? se parecen .
# el algoritmo lda() del paquete MASS() se encargara de hacer los calculos.


library(MASS)
library(caret)

#Datos
bn<- read.csv("../data/tema3/banknote-authentication.csv")
bn$class <- factor(bn$class)

set.seed(2018)

#Partition:
t.id <- createDataPartition(bn$class, p=0.7, list = F)


#Modelo:
mod <- lda(bn[t.id,1:4], bn[t.id,5])
#mod <- lda(class ~., data = bn[t.id,]) # same sh**t

# Insertar columna en el dataset original con el valor de la prediction.
# los NA's surgen por usar en la implementacion solo los datos de la particion no entrenada.

#Datos de entrenamiento:
bn[t.id, "Pred"] <- predict(mod, bn[t.id, 1:4])$class

# Matrix de Confusion:
table(bn[t.id, "class"], bn[t.id, "Pred"], dnn = c("Actual", "Predichos"))

---- 

#Datos de Validación:   
bn[-t.id, "Pred"] <- predict(mod, bn[-t.id, 1:4])$class 
table(bn[-t.id, "class"], bn[-t.id, "Pred"], dnn = c("Actual", "Predichos"))

## al insertar primero los datos de entrenamiento y luego los de validacion,
## no es necesario este paso, pero conviene hacer una comparacion para verificar 
## la equivalencia de la generalizacion del modelo con la insercion modular.

# Generalizar el modelo a todos los datos:
# bn_gnral <- bn["Pred"] <- predict(mod, bn[, 1:4])$class

bn_gnral <- bn %>% mutate(Pred = predict(mod, bn[,1:4])$class)
test <- bn_gnral == bn
test <- test %>% as_tibble() %>% filter(Pred == FALSE); test

# Se logra apreciar, que la insersion sistematica de la prediccion del modelo, 
# a los datos de entrenamiento y validacion, arrojan el mismo resultado que la 
# generalizacion del modelo predictivo, en la totalidad del conjunto de datos.
# para comprobar esta conclusion se compararon ambos objetos sin diffs evidentes.
