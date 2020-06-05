

# K-fold cross validation ---------------------------------------------------------------------
# La validacion cruzada para evitar el overfitting, en modelos que no realicen este paso 
# de manera automatica, en los casos en los que este paso no se realiza sino que lo hacemos
# manualmente, con particion de datos, pueden surgir errores o sesgos de modelaje.
# el cross validation, nos da un enfoque mucho en la calificacion de la eficacia del modelo, 
# que solo llevar a cabo una particion, la validacion cruzada, puede darnos una vision mucho
# mas global mucho mejor que solo la vision relativa que nos da hacer una sola particion.

# programacion del metodo de k-fold-validation. 
# tecnica que funcione para cualquier dataset, 
# basicamnte para elegir y cruzar validaciones. 


bh <- read.csv("../data/tema4/BostonHousing.csv")


# K-Fold Crossvalidation para regression :::   
kfold.crossval.reg <- function(df, nfolds){
  fold <- sample(1:nfolds, nrow(df), replace = T)
  cat(fold)
  
  mean.sqr.errs <- sapply(1:nfolds, 
                          kfold.cval.reg.iter,
                          df, fold)
  
  list("MSE "= mean.sqr.errs,
       "Overall_Mean_Sqr_Error" = mean(mean.sqr.errs),
       "Std_Mean_Sqr_Error" = sd(mean.sqr.errs))
}

# k-fold Cross Valdiation iteration ::: 
kfold.cval.reg.iter <- function(k, df, fold){
  
  tr.ids <- !fold %in% c(k)
  cat("----------------------------------------")
  test.ids <- fold %in% c(k)
  mod <- lm(MEDV ~., data = df[tr.ids,])
  pred <- predict(mod, df[test.ids,])
  sqr.errs <- (pred - df[test.ids,"MEDV"])^2
  mean(sqr.errs)
}
 

res <- kfold.crossval.reg(bh, 5)

res

# Obtenemos los errores cuadraticos medios para cada una de las divisiones.

# Recap ::: 
# Se han contruido dos funciones para llevar a cabo la Validacion Cruzada, donde 
# en la primera de ellas creamos de manera aleatoria un muestreo, desde 1 hasta (n)
# siendo (n) el numero de iteraciones que queramos llevar a cabo, en el caso de {res}
# n = 5, por lo tanto lo que hacemos son 5 Subconjuntos del conjunto de partida original 
# por o tanto si tuvieramos en el dataframe original 1000 elementos y tuvieramos que 
# realizar n=5 divisiones, cada una tendria aproxmente 200 elementos.
# res objeto indica en cual de las 5 validaciones que vamos a llevar a cabo, el 1er 
# elemento sera considerado como parte del conj de validacion, es decir, repartimos el total
# de elementos, entre los distintos folds.

# invocamos el metodo iterativo, pasandole a cada caso, la iteracion y el conjunto 
# entonces bien si dentro del conj, se contiene dicha iteracion, formara parte del conj de testing
# y si no lo contiene formara parte del con de entrenamiento, 

# Entonces en la 2da funcion podemos observar que todas las iteraciones marcadas como el valor(k)
# pertenecen o no al conj fold, luego, se crea un modelo 
# calculamos el rmse, y el promedio del promedio de errores y desv stad e imprimos cada uno de ellos 

# para saber si la tecnica iimplementada es buena simplemente comparamos, el overall MSE con el MSE. 

# LOOCV ---------------------------------------------------------------------------------------

# Lead One Out Cross Validation; para la regression lineal, es adaptable para cualquiera 
# de las tecnicas de regresion que hemos visto, la idea es tener una buena herramienta, 
# para la validacion cruzada, en este caso dejando uno fuera para cada caso. 

loocv.reg <- function(df){
  mean.sqr.errors <- sapply(1:nrow(df), 
                            loocv.reg.iter, df)
  list("MSE"=mean.sqr.errors,
       "overall_mean_sqr_errors" = mean(mean.sqr.errors),
       "sd_mean_sqr_errors" = sd(mean.sqr.errors))
}

loocv.reg.iter <- function(k, df){
  mod <- lm(MEDV~., data = df[-k,])
  pred <- predict(mod, df[k,])
  sqr.error <- (pred - df[k,"MEDV"])^2
  sqr.error
}


res <- loocv.reg(bh)

res

# La funcion hace tantas ejecuciones como elementos se tengan, por lo que solo necesita el df
# se realizara una iteracion de todo el dataframe.
# A continuacion una funcion que recibe el dataframe, pq realiza tantos modelos lineales como 
# el tamaño del dataframe, luego el resultado de la funcion, es el MSE para cada modelo, 
# donde el numero de modelos equivale al numero de filas del dataset. 
# OMSE (Overall Mean Square Error)
# SDMSE (Standard Deviation Mean Square Error)

# Note::: 
# Tener en cuenta que la mayoria de algoritmos de machine learning incorporan esta tecnica. 
# Pero en caso de querer Cross Validation Home made o querer tener control de cada aspecto.
# cambiando los algoritmos podemos ajustar las funciones para que lleven a cabo, los mismos
# metodos que ya conocemos pero con mas control sobre el conjunto de validacion 
# Cross Validation o LOOCV 
# ambos super utiles para que los modelos no sufran de overfitting 
# y no tenga un desempeño obtimo en los conjuntos de validacion pertinentes.


