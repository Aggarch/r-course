# Logistic Regression 

# Analisis de regression que se utiliza para predecir el resultado de una variable categorica,
# en funcion de variables independientes  predictoras, es muy util para calcular la probabilidad
# de un evento que ocurre en funcion de otros factores, forma parte del conjunto de modelo :
# GLM (Generalized Lineal Model):
# La tecnica parte de la posibilidad de trabajar con probabilidades conocidas de diferentes eventos,
# y modelarlas con una transformacion logaritmica. 
# se lleva a cabo el logaritm de la probabilidad dividido (1), menos la probabilidad, 
# como transformacion logistica de cada una de las observaciones, 
# esto conduce a una funcion de probabiidad logistica, que una vez definida , se determina el 
# cutt/off; division entre la zona de arriba y la de abajo.
# momento en el cual la variable se dicotomiza (0,1)
# lo cual sera util para predecir el resultado.
# internamente se necesitan resolver ecuasiones diferenciales resueltas por la paqueteria.



library(caret)

bh <- read.csv("../data/tema3/boston-housing-logistic.csv")
bh$CLASS <- factor(bh$CLASS, levels = c(0,1))

# Restricciones, variables de salida normalmente son (0 y 1), exito o fracaso
# para ejemplo (0) = precio bajo, (1) = precio alto.
# Requiere que todas las variables predictoras (indep) sean numericas 
# variables dependientes binarias en el target. 

set.seed(2018)
t.id <- createDataPartition(bh$CLASS, p=0.7, list = F)

mod <- glm(CLASS ~ ., data = bh[t.id, ], family = binomial)

summary(mod)
# Genera los residuos de las desviaciones y los coeficientes. 
# Busca generar una ecuacion lineal pero logaritmica.
# Al aplicar el logaritmo a la funcion original, se transforma en una funcion lineal de (betas).
# summary arroja valores estimados, errores estandar, valor tipificado y p-value.
# p-value : Significatividad que tiene la variable para el modelo logistico (-***+)
# grados de libertad utilizado para los residuos.
# numero de iteraciones realizadas. 
# con el modelo realizado podemos calcular probabilidades :

# Probabilidades de que la combinacion de las variables de entrada den un exito:
# probabilidades en funcion de los parametros de clasificacion. 
# el cutt/off es decision del experto, es decir a partir de que probabilidad, consideramos un 
# exito, y a partir de que probabilidad un fracaso, default(50%)
bh[-t.id, "PROB_SUCCESS"] <- predict(mod, newdata = bh[-t.id,], type="response")
bh[-t.id, "PRED_50"] <- ifelse(bh[-t.id, "PROB_SUCCESS"]>=0.5, 1, 0)

# Matriz de Confusion:::  (conj de valid)
table(bh[-t.id,"CLASS"], bh[-t.id,"PRED_50"], dnn=c("Actual","Predicho"))

# Genelarized ::: 
bh_gnral <- bh %>% mutate(Pred = predict(mod, newdata = bh, type = "response"),
                          Succes = ifelse(Pred>.5, 1, 0),
                          test = Succes == CLASS)
