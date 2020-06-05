
# Regression ::: Raiz del error cuadratico medio::: 


# de las tecnicas analiticas mas facsinantes dentro del marco de modelos predictivos. 
# hallar relacion entre variables dependientes e independientes, targets y predictores. 
# El objetivo no es clasificar sino encontrar valores más concretos, mas especificos.
# p.ej ::: 
# numero de vetas de un determinado producto en el futuro.
# cantidad de depositos que un banco va a recibir al sgte mes. 
# numero de copias que se venderan de un elemento.
# a que precio se espera vender un articulo.

# varios de los algoritmos de clasificacion, funcionan o se pueden ajustar
# para realizar regresiones, y se utiliza la validacion cruzada para ajustar
# y definir los limites del overfitting. 
# tambien utiliza datos de entrenamiento y validación. 
# ya no se utilizan probabilidades de acierto o matrices de confusion. 
# Cuanto nos hemos equivocado en la prediccion, respecto el dato original? 
# Se utiliza esta tecnica:::

# Se eleva al cuadrado la diferencia de los errores, al elevar al cuadrado, siempre
# nos queda el error siempre sera positivo, calculamos el promedio, es decir 
# sumamos todos esos errores y dividimos por el numero de muestras.
# como resultado obtemos el promedio del error cometido, para compensar el 
# cuadrado que habiamos introducido para que fuera positivo, le hacemos una raiz cuadrada. 


dat <- read.csv("../data/tema4/rmse.csv")

# error cuadratico medio: 
rmse<-sqrt(mean((dat$price - dat$pred)^2))
rmse
# se entiende el resultado como el error promedio entre la variable predicha y la orginal
# hacia arriba o abajo, que el error sea alto o bajo depende de su tamaño respecto al dato.

# Dibujar
plot(dat$price, dat$pred, xlab = "Actual", ylab="Predicho")
abline(0,1) 
# Se observa una tendencia, en la medida que el dato actual crece, el predicho tmabien. 
# se añadio la recta(0,1), aquella que pasa por el cero y tiene de pendiente (1) , recta 45°
# cuanto me alejo respecto a la recta? 
# conseguir que los valores de la prediccion caigan lo mas cerca posible de la recta(0,1)


# Funcion para calcular el error cuadratico medio. 

rmse <- function(actual, predicted){
  return(sqrt(mean((actual-predicted)^2)))
}

rmse(dat$price, dat$pred) 
# raiz cuadrada del error cuadratico medio. 

