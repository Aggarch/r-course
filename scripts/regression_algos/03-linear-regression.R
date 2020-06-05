

# Linear Regression ::: ---------------------------------------------------------------------------

# Una de las tecnicas más utilizadas para predecir valores, ampliamente conocida y estudiada
# el paquete {stats} de R incorpora todo lo necesario para aplicar la tecnica. 

library(caret)

auto <- read.csv("../data/tema4/auto-mpg.csv")

# factor de la variable 'cylinders'
auto$cylinders <- factor(auto$cylinders,
                         levels = c(3,4,5,6,8),
                         labels = c("3c", "4c", "5c", "6c", "8c"))

set.seed(2018)
# Particion: 
# Buscaremos predecir el consumo del coche (mpg), en funcion del resto de caegorias.
t.id <- createDataPartition(auto$mpg, p = 0.7, list = F)
names(auto)

# De las variables disponibles, el 'No.' del carro no dice nada ya que se refiere a indice
# o conteo de las filas, la variable 'car_name', es una variable categrica que debe ser 
# excluida del analisis, muy probablemente el 'model_year' influya pero mejor omitirlo. 

# Modelo lineal, de todas las columnas del conj de datos de entrenamiento menos las antes 
# mencionadas, (No, model_year, car_name)
mod <- lm(mpg ~ ., data = auto[t.id,-c(1,8,9)])

# Relacion lineal de (mpg) y el resto de variables, ha tomado las variables categoricas 
# como (0,1), 
mod

# Para obtener un valor de 'mpg', conocidadas el resto de variables, habria que aplicar
# primero el termino independiente, el 'intercept' y luego 
# la sgte formula: 

# mpg = 37.284202 +
#     + 6.231475 *4c + 8.248195 *5c + 2.131026 *6c + 4.568171 *8c +
#     + 0.002245 * displacement - 0.057543 * horsepower +
#     - 0.004665 * weight +  0.050745 * acceleration

# donde, cada vehiculo tiene un numero especifico de cilindros, es decir 
# que no puede permanecer a mas de una categoria, (0,1), 
# el vehiculo que pertenezca a una categoria, sera = (1), entonces, 
# la adicion se da al intercepto de lo contrario seria cero hasta encontrar su categoria. 
# hay variables que tienen todos los coches, por ejemplo el desplazamiento, 
# por lo que habria que multiplica (0.002245 * displacement).
# habria que restar los caballos, restan al mpg, (-0.057543 * horsepower)
# y asi sucesivamente como indica el output del objeto modelo {mod}

# Ejemplo para un coche de 4 cilindros : 
# mpg = 37.284202+6.231475+(0.002245*140)-(0.057543*90)-(0.004665*2264)+(0.050745*15.5);mpg
# mpg = variable dependiente o target. 
# Resto = variables independientes o predictoras. 

# Se debe tener en cuenta, que no todas las variables tienen el mismo nivel de importancia 
# es decir no todas cntibuyen en igual magnitud al caluculo de la variable target o dependiente.
# puede ser que una tenga mas o menos presencia que el resto :

summary(mod)

# El summary del objeto modelo {mod} ofrece informacion adicional, indica que es un modelo lineal.
# Residuos: Las diferencias que se obtienen con el valor real y la prediccion (estadisticos)
# distribucion de los errores o residuos :

boxplot(mod$residuals)

# Luego tenemos los coeficientes, que son los mismos que constituyen la formula de regresion.
# ademas ofrecen informacion del error estandar, t-value y p-value respectivamente.
# en general los t,p-values nos dan una idea de que tan representativo es la
# estimacion de dicho parametro.
# los p-value, entre mas bajos posibles mejor, puesto que un p-value alto significaria 
# que dicho factor, no es representativo y podria llegar a cero.
# el p-value proviene del contraste de hipotesis, que es una tecnica estadistica usada 
# para saber si un valor es o no es (0), o si toma o no un valor. 
# por los que mas apartan son el intercepto o (ordenada en el origen) y los marcados (***)
# (*) = codigo de significacion. (1 menos el nivel de significatividad)
# el t-value muestra la probabiidad de que un coeficiente tenga valor. 
# residual estandar error - error cuadratico medio. 
# utiliza un test de chi-cuadrado para medir los grados de libertad, pero es muy parecido.
# R-squared: indican que porcentaje de la variacion, esta explicada por parte del modelo.
# Es decir, la variabilidad que presentan los datos 


# RSME ::: 
sqrt(mean((mod$fitted.values - auto[t.id,]$mpg)^2))

# Prediccion 
pred <- predict(mod, auto[-t.id, -c(1,8,9)])

# RSME de la prediccion: 
sqrt(mean((pred - auto[-t.id,]$mpg)^2))


# Interpretacion ------------------------------------------------------------------------------

# Diagrama del diablo: 
par(mfrow=c(2,2))
plot(mod)

# 1)
# Residuals vs Fitted:  
# Muestra si los residuos tienen un patron no lineal, podria existir un patron 
# no lineal entre las variables predictoras y la salida: 
# si existe esta relacion no lineal, se vera en este grafico, basicamente porque
# una regresion lineal, no podra nunca capturar una relacion no lineal. 
# que los puntos o residuos del grafico se distribuyan alrededor del eje horizontal
# es un buen indicador de que los errores no singuen una relacion no lineal.
# si los residuos dibujaran una forma que parezca no aleatoria, sino especifica,
# entonces probablemente sea un mal modelo, puesto que los residuos se estan quedando 
# parte de la informacion, en partcular significaria que la relacion que existe entre 
# las variables independentes y dependientes no es lineal, a lo mejor pueda ser :
# cuadratica, cúbica, logaritmica o exponencial, 
# en caso de una parabola, tocaria buscar un modelo cuadratico. 
# por lo tanto, los que observamos es que la varianza no constante en la distribucion 
# de los errores y por lo tanto saber si la opcion de una regresion lineal se ajusta o no.

# 2) 
# Normal Q-Q 
# El grafico quantil-quantil, se intenta representar si los errores se distribuyen segun 
# una normal estandar o no, la linea de puntos simboliza la diagonal, recta igual a (x)
# si los puntos se acumulan cerca de la recta, hay unas buenas expectativas de que los 
# errores, sigan una distribucion normal, que es una de las hipotesis necesarias para 
# poder aplicar, la regresion lineal. 
# Se distribuyen los errores como una linea recta y sigen una distribucion parecida a N(Z) ? 
# el Q-Q puede servir para buscar valores extremos(outliers) y eliminarlos o reescalarlos 
# es recomendable hacerlo para evitar problemas con el modelaje u optimizar el mismo.

# 3) 
# Scale - Location o Spread - Loc
# Escala y localizacion de los residuos, dipersion , intenta explicar los rangos que toman
# los errores de los predictores, es muy parecido al primer grafico, Residual - Fitted, 
# la diferencia es que esta vez, los errores estan estandarizados y con raiz cuadrada. 
# Se utiliza para verificar la asuncion de que todos los datos son o tienen la misma varianza
# es decir se cumple la Homocedasticidad ("Homocedasticos"), o no .. entonces 
# si volvemos a ver un grafico horizontal, donde todas la varianzas en este caso,
# de los errores cometidos, no siguen un patron definido sino que mas bien todos estan distribuidos,
# es un buen indicador de que la asuncion de que  todos los datos, tengan la misma varianza es buena.
# por lo tanto la regresion lineal es una buena tecnica para modelar este tipo de datos. 
# los residuas deben parecer aleatoriamente distribuidos por la pantalla sin seguir una determinada 
# estructura, esta ''regados'' horizontalmente sobre el eje (x), no hay tendencia.
# En caso de que el grafico exhiba una tendencia, ya sea hacia arriba o abajo, sera un indicativo, 
# de que las varianzas tienden a subir o bajar, es decir que la asuncion de que los datos utilizados,
# tienen la misma varianza, es falso, luego no se cumple el criterio de Homocedasticidad.
# que no haya un patron inherente. 

# 4) 
# Residuals vs Leverage 
# Los residuos vs el apalancamiento, nos ayuda a encontrar posibles sujetos influyentes, dentro de 
# nuestro modelo, no necesariamente cualquier outlier sera influenciador del modelo de regresion lineal
# pero podrian existir datos con valores tan extremos que tenga la capacidad de influir a la hora de 
# crear la recta de regresion lineal, esto significa que el resultado seria muy distinto si dichos 
# valores, fuera excluidos o incluidos en el analisis, en la mayoria de casos, los outliers, suelen 
# seguir la tendencia y no hace falta tenerlos en cuenta, pero habran casos en que alguno de ellos 
# pueda ser demasiado influenciador, a pesar de que parezca que esta en un rango de valor aceptable
# podria ser que e caso extremo dentro de la regresion lineal, podria alterar el resultado 
# por lo que lo mejor seria excluirlos del analisis puesto que arrastraria a la tendencia. 
# en este grafico el factor temporal no es muy relevante, en la zonas superior o inferior derecha
# sera donde iremos a localizar casos extremos de influencia normalmente estos datos se localizan 
# por fuera de una linea de puntos denominada distancia de ''Cook''
# datos que se encuentran por fuera de la linea discontinua, de Cook, los objetos identificados 
# son los que tienen mayor distancia, mayor puntuacion, estan alejados de la puntuacion normal, 
# luego son casos que influyen, en la regresion lineal, y el resultado final.
# Los resultados de la regresion seran alterados si se excluyen estos casos del modelo. 
# se puede reproducir el modelo sin tener en cuenta estos outliers (influencers)
# Al excluir a los outliers, tenemos que revisar el R Cuadrado. 
# es decir que el porcentaje de la variacion explicable por el modelo, puede subir, lo cual 
# seria una mejora sustancial. 

# El cuarteto de plots pueden mostrar casos problematicos, en el numeor de la fila del dataset
# esto e smuy util, ya que si hay algun caso identificado en los cuatro plots, sera un caso a 
# estudiar con lupa, y a tener en cuenta como un dato especial, ya sea un error o valor ilogico 


#a)
auto <- within(auto, 
                cylinders <- relevel(cylinders, ref="4c"))


# b) 
mod <- lm(mpg ~. , data = auto[t.id, -c(1,8,9)])
summary(mod)

# c) 
pred <- predict(mod, auto[-t.id, -c(1,8,9)])
sqrt(mean((pred-auto[-t.id,]$mpg)^2))
plot(mod)

library(MASS)

mod
summary(mod)

# Simplificar el modelo lineal 
#  Akaike information criterion (AIC), indice para la eliminacion de predictores.
step.model <- stepAIC(mod, direction="backward")
summary(step.model)

# El analisis hace iteraciones sucesivas del modelo y sugiere eliminar del modelo las 
# variables que menos contribuyen ara simplificar el modelo. 

# Si queremos que el modelo lineal utilice como referencia otra categoria de cilindros
# por ejemplo pq en los datos se encuentraan mas coches de 4 cilindros que de tres cyls
# obtariamos por hacer que el dataset auto, incorporara un reordenamiento de la variable Cyl,
# de modo que se indica within() para reordenar el dataframe,
# que la variable Cylinders sufra un redimensionamiento, donde el nivel de referencia dera 4cyls.
# esta modificacion hara que el modelo asuma que un coche por defecto es de 4 cilindros 
# La recomendacion es utilizar como categoria de referencia aquella que tenga mas elementos (a)* 

# Modelamos nuevamente   (b)* 
# predecimos y dibujamos (c)*

# El modelo toma en cuenta otros feactores pero es equivalente* 

# Puede pasar que el modelo no sea lineal, pero que observemos cierta tendencia en los residuos 
# que no fuera explicada por el modelo lineal original y que podamos incurrir en problemas de 
# funciones cuadraticas, ausencia de una constante etc. 
# 

