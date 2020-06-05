# Random Forest 

install.packages("randomForest")
library(caret)
library(randomForest)

banknote <- read.csv("../data/tema3/banknote-authentication.csv")
banknote$class <- factor(banknote$class) # factorizacion de variable class para clasificar

set.seed(2018) 

# En Random Forest, no se necesitamos partir los datos para el modelo puesto que la construccion
# del arbol, tiene pasos que involucran particiones inherentes.
# Sin embargo es conveniente tener un conjunto de datos separados para ilustrar 
# El proceso de creacion del modelo por un lado, y la validacion por otro. 

# Subconjunto de datos. {caret}
training.ids <- createDataPartition(banknote$class, p = 0.6, list = F)

# dataframe original en funcion de la 'clase' como variable categorica.
# seleccionando el 70% del df original probab = 0.7 del df original. 
# fomato {array} ~ list == FALSE. 
# reserva de datos para cross validation. 

# Construir varios arboles de clasificación:::
mod <- randomForest(x = banknote[training.ids, 1:4],
                    y = banknote[training.ids, 5],
                    ntree = 500, 
                    keep.forest = TRUE)

# mod %>% write_rds("mod.rds") # save model in rds file. 


# Funcion randomForest donde x equivale al 70% del df original 
# seleccionando solo las columnas desde la 1 a la 4.
# columna 5 'clase' escogido como variable 'y' 
# x & y deben tener la misma dimension.
# ntree -> seleccionar el numero de arboles
# keep.forest -> retener outputs
# X = [1:4], variables predictoras, Y =[5], variable target.
# Parametro {keep.forest == TRUE} para utilizar el modelo en predicciones futuras.


pred <- predict(mod, banknote[-training.ids,], type = "class")
#seleccionamos los contrarios para intentar predecir la bondad de los no escogidos. 

# Matriz de Confusion: 
table(banknote[-training.ids,"class"], pred, dnn= c("Actual", "Predicho"))
# matrix interpretation detailed in classification-trees script. 


library(ROCR) # Performance 

#probabilidades de clasificacion:
probs <- predict(mod, banknote[-training.ids,], type = "prob")
head(probs) # exitos = 1 ; fracasos = 0 ; 
# Predecir con base al modelo, con que probabilidad banknote de la particion 
# no utlizada (df - 30%) y todas las columnas queda clasificado:
# inferencia necesaria para poder envaluar ROC: 

pred <- prediction(probs[,2], banknote[-training.ids,"class"])
# seleccionamos con base en probs, de la particion no utilizada 
# para la elaboracion del modelo, la variable "class".
# predecir con que probabilidad los objetos quedan catalogados o no. 


perf <- performance(pred, "tpr", "fpr")
plot(perf)


#Functions 

# predict     <- Calcular las probabilidades o la clasificacion (catalogar)
# prediction  <- Prediction porcentual, como de seguro estoy de que un elemento 
#                pertenece a una clase o no, basado en sus probabiidades.
# performance <- Verificar su bondad de eficiencia, input = valores predichos (pred)
#                output = "tpr" & "fpr" -> (verdaderos positivos vs falsos positivos)

# ROC debe superar a la ROC del arbol individual,se estan elaborando 500 arboles 
# y seleccionando los mejores para el ensamble del modelo. 

# A la hora de elegir un cut/off para la clasificacion, en lugar de utilizar la regla por defecto
# de la mayoria para clasificar, se puede especificar un vector de probabilidades de longitud = N(casos)
# de modo que la proporcion del ratio del cut/off determina la clase ganadora. 
# Aplicable tanto para la construccion de un arbol como para la elabboracion del modelo RandomForest.
# Y tener asi una buena herramienta para empezar a catalogar o clasificar los datos. 


# Valid implementation ?¡¿ 

# clasification predictions can be introduced into a mutate cause as an array
# probabilistics may be combine to original data, cbind cause is a column already 
# Or select only one column of the prob object to mutate it. 
# In case it's needed, calculate complement. 

imp <- banknote %>% 
  cbind(prob = predict(mod, banknote, "prob")) %>% 
  mutate( prob_si = scales::percent(prob.1)) %>% 
  select(-prob.0, -prob.1) %>% 
  as_tibble()

banknote %>%
  mutate(prob.0 = predict(mod, banknote, type = "prob")[1]) %>% 
  as_tibble()
  


# thestt

banknote %>% mutate(pred = predict(mod, banknote, type = "class"),
                    test = pred == class) %>%
  group_by(test) %>% 
  summarise(n=n())



