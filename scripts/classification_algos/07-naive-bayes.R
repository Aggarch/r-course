# Naive Bayes


library(e1071)
library(caret)

#dataframe of factors:::
ep <- read.csv("../data/tema3/electronics-purchase.csv")

set.seed(2018)

#partition
t.ids <- createDataPartition(ep$Purchase, p = 0.67, list = F)

#Clasificador probabilistico basado en teorema de Bayes e hipotesis 
#simplificadoras adicionales:
#hipotesis de independencia entre las variables predictoras.('naive') 

# Asume que la presencia o ausencia de una caracteristica particular, 
# nunca esta relacionada con la presencia o ausencia de cualquier otra
# fruta, roja, 7cm-diam -> apple 
# los clasificadores de naive bayes, se pueden entrenar de forma muy eficiente 
# en un entorno de aprendizaje supervisado.
# se puede utilizar para la estimacion de parametros, utilizando el metodo de
# maxima verosimilitud, y tambien para aceptar que algo pertenece o no a un grupo.
# con pocos datos funciona, no se necesitan conjuntos de datos muy grandes para 
# crear un modelo que estime los parametros (media y varianza) de las variables 
# que son necesarias, como suponemos independencia en las variables, solo es necesario
# determinar la varianza de cada una de las variables de la clase, y no la matriz de covar.

# requiere que todas las variables sean categoricas 

mod <- naiveBayes(Purchase ~ ., data = ep[t.ids,])
mod

# El modelo realiza una clasificacion con modelos discretos.
# probabilidades de clasificacion en Yes/No (.5,.5)
# probabilidades condicionadas, como los factores influyen en la clasificacion.
# de forma sencilla y asumiendo independencia e igualdad;
# cada una de las clases predictoras, es tomada independiente con respecto al resto.
# para tomar la decision de a que clase final pertenece. 

#prediccion sobre datos de particion no usados en el entrenamiento: 
pred <- predict(mod, ep[-t.ids,])

#matriz de confusion: 
tab <- table(ep[-t.ids,]$Purchase, pred, dnn = c("Actual", "Predicha"))
confusionMatrix(tab)
# se ha equivocado mucho el modelo.


ep %>% mutate(pred = predict(mod,ep),
              test = Purchase == pred) %>% 
  group_by(test) %>% summarise(n=n())

