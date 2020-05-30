
# Decision Trees

install.packages(c("rpart", "rpart.plot", "caret"))
library(caret)      # Particion de datos.
library(rpart)      # Contruccion de Arboles de clasificacion.
library(rpart.plot) # Dibujar datos.

banknote <- read.csv("../data/tema3/banknote-authentication.csv")

set.seed(2018)     # semilla aleatoria.

#train ids (partition) ----
training.ids <- createDataPartition(banknote$class, p = 0.7, list = F)


# Modelo del arbol ----
# Variable dependiente e independientes...Clasifica la clase respecto a las 4 vars
# class ~ . <-> class ~ variance + skew + curtosis + entropy
mod <- rpart(class ~ . , 
             data = banknote[training.ids,],                    #conjunto de datos de entrenamiento
             method = "class",                                  #arbol de clasificación v regression.
             control = rpart.control(minsplit = 20, cp = 0.01)) # No. de nodos es decir,
             # mas o menos elementos en cada clase ( minsplit ) + param de complejidad (cp)

mod # lectura de nodos del arbol. 


# plot model o diagrama de arbol. ----
prp(mod, type = 2, extra = 104, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col = "gray", roundint = F)


# Componentes principales:  ----
#{error relativo, error total & standard dev} 
mod$cptable

# Se puede elegir la componente principal con mayor valor, asegurar que el error total,
# no es mayor al error minimo que quiero cometer + su std dev. 
# para podar las rams del arbol, hay que reducir la probab de que el modelo sufra overfitting.
# Observar la complejidad generada debido a la validacion cruzada. 
# buscar el valor que no sea superior a la desv estandar + el error minimo (0.1452381)
# es decir min(xerror) + xstd > xerror  ::: 

mod$cptable %>% as_tibble() %>%
  mutate(test = min(xerror) + xstd,
         test_select = test>xerror) 

# Luego la componente principal a escoger es la sexta. 
# Al escoger la unica componente valida, no se ha logrado reducir los niveles de complejidad.

# Mod Reducido ----
# Modelo reducido, teniendo en cuenta el original, pero tomando la componente principal #6 
# El numero de Comp principales que se seleccionan es proporcional al tamaño del arbol. 
mod.pruned <- prune(mod, mod$cptable[5, 'CP'])

# Dibujar modelo reducido. ----
prp(mod.pruned, type = 2, extra = 104, nn = TRUE,
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col = "gray", roundint = F)

# Prediccion de factores para nuevos valores con modelo original  
pred.pruned <- predict(mod, banknote[-training.ids,], type="class")

# Matriz de confusion Modelo original -> completo
table(banknote[-training.ids,]$class, pred.pruned, 
      dnn = c("Actual", "Predicho"))
# En 228 casos, cuando el modelo predice que el valor es 0 el valor real es 0
# En 7   casos, cuando el modelo predice que el valor es 0 el valor real es 1
# En 7   casos, cuando el modelo predice que el valor es 1 el valor real es 0 
# En 169 casos, cuando el modelo predice que el valor es 1 el valor real es 1 

#Prediccion de factores Modelo 'Podado'. 
pred.pruned2 <- predict(mod.pruned, banknote[-training.ids,], type = "class")

# Matriz de confusion Modelo 'Podado' -> completo
table(banknote[-training.ids,]$class, pred.pruned2, 
      dnn = c("Actual", "Predicho"))

#modelo ajustado a probabilidades vs binaria de type = "class"
pred.pruned2 <- predict(mod.pruned, banknote[-training.ids,], type = "prob")

head(pred.pruned)
head(pred.pruned2)

library(ROCR)

#predict function used for models, prediction use is for ROC. 

pred <- prediction(pred.pruned2[,2], banknote[-training.ids, "class"])
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Se puede asumir que el modelo funciona bien segun la evaluacion ROC. 

banknote %>% mutate(pred = predict(mod, banknote, type = "class"),
                    test = pred == class) %>%
  group_by(test) %>%
  summarise(n=n())
