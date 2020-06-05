

# Random Forest for Regression:::  ------------------------------------------------------------

# Predecir el valor promedio del precio de las casas de Boston , puede suponer una mejora 
# sobre el modelo de arboles de decision. 

library(randomForest)
library(caret)
bh <- read.csv("../data/tema4/BostonHousing.csv")

set.seed(2018)
# Partition 
t.id <- createDataPartition(bh$MEDV, p = 0.7, list = F)

# El modelo construira varios arboles de regresion y procesarlos
# donde: x = variables independientes o predictoras; y = variable dependiente (target) 
# importance indica si tiene que computar o no las puntuaciones de cada variable predictora.
# keep.forest indica si debe quedarse con los arboles que resultan del modelo. 
# necesarios para realizar predicciones basadas en dicho modelo. 

mod <- randomForest(x = bh[t.id, 1:13], y = bh[t.id, 14],
                    ntree = 1000, 
                    xtest = bh[-t.id, 1:13], ytest = bh[-t.id, 14],
                    importance = T, keep.forest = T)

mod

# observar el nivel de importancia para cada una de las variables predictoras,
# va haciendo permutacion de los valores a traves de los casos y genera distintas preds
mod$importance


# Comparacion de los valores actuales de las predicciones, 
# sobre le conj de entrenamiento con un grafico:
# Usamos datos de entrenamiento, para predecir, con el modelo generado, 
# la comparacion de los datos 'actual' es decir, los originales, vs los datos,
# "Predichos" es decir aquellos predecidos por el modelo.

plot(bh[t.id,]$MEDV, predict(mod, newdata = bh[t.id,]),
     xlab = "Actual", ylab = "Predichos")
abline(0,1)
# Añadimos una linea que empiece en cero y termine en uno (0,1)
# el resultado indica un buen comportamiento sobre el conj de training


# aplicacion de la misma tecnica para el conjunto de validacion: 
plot(bh[-t.id,]$MEDV, predict(mod, newdata = bh[-t.id,]),
     xlab = "Actual", ylab = "Predichos")
abline(0,1)
# Parece ser que la aproximacin es bastate buena para ambos conjuntos de datos,
# es una mejora del algo de arboles de decision , en lugar de un arbol, es el 
# resultado a la iteracion de (n) arboles, haciendo validaciones pertinentes,
# Observamos resultado suficientemente bueno y significativo. 

# Se pueden considerar otros factores adicionales para controlar la generacion del rForest
# parametros :: 

# mtry : numero de predicciones que deben hacerse aleatoriamente, sobre la muestra en cada division
# es decir, cuantos predictores vamosa tomar en cada una de las predicciones del bosque 
# nodesize: minimo tamaño que deben tener los nodos terminales para ser considerados. 
# como minimo necesitamos 5 elementos dentro de un nodo para que este sea considerado como tal.
# en caso de que el numero de elementos no alcance el minimo, desaparece el nodo terminal. 
# elegir un nodesize mayor causara que el arbol sea mas pequeño, casi sin nodos de ser muy alto 
# maxnodes : gestiona el taaño de arbol a traves del numero maximo de nodos terminales,
# que cualquiera de los arboles que se genere  a nivel bosque puede tener, si no se especifica,
# cualquier arbol podria crecer hata el valor maximo posible, dado por el nodesize.  
# por ende son un conj de parametros que sirven para podar el arbol. 

# puesto que es un Random forest en el que participan un maximo de arboles de 1000,
# la decision final no la toma un arbol sino el conjunto de arboles que conforman el bosque,
# por lo que cada uno de ellos aparece en unas variables u otras ponderadas segun la importancia 
# de cada una de ellas sobre el conjunto del bosque en su totalidad. 

#mtry = m/3, donde m = # de predictores
#nodesize = 5
#maxnodes 