# Support Vector machines 

# install.packages("e1071")


# Classification & Regression 
# Etiquetar clases y Entrenar una SVM, para contruir un modelo que nos prediga 
# la clase de una nueva muestra no incluida en la inicial. 
# Elaborar un modelo que represente los puntos de muestra en el espacio e intentar 
# separar las clases o espacios de forma mas amplia posible, a traves de un
# hiperplano de separación, definido como el vector entre dos puntos de las clases cercanas. 
# -vector de soporte- cuando nuevas muestras se ponen en correspondencia con le modelo 
# la funcion determina a cual de los dos lados del hiperplano pertenece y entonces es clasificado
# en una rama u otra dependiendo de sus caracteristicas. 
# un hiperplano en una representacion bidimensional es una recta, entonces se buscan de todas las 
# rectas posibles aquella que mejor separe as muestras, la mejor sera aquella que permita obtener 
# un margen maximo entre los elementos de las categorias sin sacrificar elementos. 


library(caret)
library(e1071)

banknote <- read.csv("../data/tema3/banknote-authentication.csv")
banknote$class = factor(banknote$class)
set.seed(2018)

# Data Partition as array: 
t.ids <- createDataPartition(banknote$class, p = 0.7, list = F)


mod <- svm(class ~ ., data = banknote[t.ids, ], 
           class.weights = c("0"=0.3, "1"=0.7),
           cost=1000)
# 1st param variable independiente a partir del resto 


# Conf Matrix: respecto a datos de training  
# Perfecta::: separa al 100% los datos: 
table(banknote[t.ids,"class"], fitted(mod), dnn = c("Actual", "Predicho"))

# prediction. ("Pefect")
pred <- predict(mod, banknote[-t.ids,])
table(banknote[-t.ids, "class"], pred, dnn = c("Actual", "Predicho"))


# plot del modelo, aplicado a particion de entrenamiento,
# al tener cuatro predictores, escogemos solo dos de ellos:
# para evaluar la segmentacion del hiperplano...
# no separa con rectas, separa con un hiperplano en un espacio de dimension superior
# que al proyectarse sobre las variables de (skew & varianza) ofrece un resultado bueno 

plot(mod, data = banknote[t.ids,], skew ~ variance)
plot(mod, data = banknote[-t.ids,], skew ~ variance)

plot(mod, data = banknote[t.ids,], skew ~ curtosis)

# Se observa que hay dos categorias en las cuales caer segun la prediccion. (1 o 0)
# Para ambos conjuntos de datos, training y testing, la separacion es la misma.
# La tecnica utiliza un nucleo de convoluciones (kernel) para transformar info no lineal 
# en espacios de dimension superior donde la informacion se puede separar mas facilmente con
# una frontera o con hiperplanos que maximicen la anchura ó margen ontre los diferentes casos.
# al trabajar en espacios de dimensiones superiores se minimiza el problema de overfitting. 
# la funcion (svm) determina el tipo de modelo (clasificacion o regresion) basado en la naturaleza
# de la variable a predecir, en caso de que dicha variable es un factor, sera clasificador. 
# solo se le debe suministrar a la funcion, una formula y el dataset- (variables depte e indept)
# El objeto modelo, contiene, un modelo con el que podemos crear una matriz de confusion, 
# para evaluar el rendimiento, el modelo retiene los valores ajustados dentro de la particion, 

# la matriz de confusion se utiliza para evaluar el modelo
# la funcion **predict**, solo necesita el modelo original, y los datos a los que le queremos aplicar las predicciones. 
# La funcion plot diagrama el modelo, se elegigen variable sa representar, el grafico da la impresion de que
# hay puntos en el espacio mal clasificados, sin embargo el 100% de los casos ha sido clasificado exitosamente,
# pero el problema es que con dos dimensiones es imposible separar todo el conjunto. 
# se puede testear probango con las otras variables. 

# (svm) funcion permite uso de diferentes parametros; 
# (svm) escala todas las variables, tanto la predictora, como la salida, para que tengan media (0) y
# desviacion tipica unitaria, antes de contruir el modelo para que estos tengan mejores resultados.
# el argumento 'scale' , un vector logico que controle este procedimiento. La recomendacion es escalarlo.
# type, algorithm utilizado, existen dos tipos de regresiones y dos tipos de clasificaciones,
# todo dependiendo si el output es un factor o no. 
# estan disponibles otros metodos de separacion por convolución.
# el tamaño desproporcionado de las clases podria provocar el dominio de una sobre la otra, 
# para balancearlo existe la opcion de añadir pesos a las clases no equitativos, example::: 
#                   svm( class.weights = c("0" = 0.3, "1" = 0.7))
# el uso seharia en el caso tal de que la clase 0 sea demasiado frecuente en comparacion a la 1
# parametro cost : la constante "C", de regularización, en terminos de la formula de Lagrange.
# se traduce en que un coste bajo, permitira menos clasificaciones por error que un coste alto, p.ej
# cost = 1, daria mucho mas margen a la clasificacion, de manera que podriamos clasificar objetos donde no toca,
# por el svm, por lo contrario una clasificacion lo mas exacta posible y sin margen para error se sube el valor
# cost = 100 o 1000; y llegar asi a conseguir el valor perfecto. 


# Observacion. 

banknote %>%
  mutate(t = predict(mod, banknote),
         prueba = class == t) %>%
  group_by(prueba) %>% summarise(n=n())


# Funcion tune.svm, permite definir o ajustar performance , de manera que ajusta los parametros:
# la gamma y el cost para obtener los mejore parametros posibles a la hora de llevar a cabo el svm.
# se le indica a la funcion el rango de valores donde queremos que pruebe. 
# summary(tuned) -> ofrece todas las combinaciones posibles, ha probado con coste 10 y 100 
# para las differentes gammas, el resultado indica de todos los parametros que se han probado,
# el error que se optiene y su dispersion, se aboga por la maxima eficiencia y precision en la prediccion,
# luego; se intentara siempre obtener una gamma y coste que minimize el error para obtener lo mejor posible.


tuned <- tune.svm(class ~ ., data = banknote[t.ids,], 
                  gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
