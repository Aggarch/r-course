

# Hierarchical Clustering  --------------------------------------------------------------------

# En ocasiones nos encontramos con datasets muy largos e informacion que no se encuentra etiquetada
# haciendo que muchas de las dimensiones o descripciones que nos aportan sean inutiles, y exista 
# la necesidad de reducir la complejidad de los datos, ya sea utilizando el analisis de componentes
# principales (PCA) o realizando un clusterizado de datos

# Un cluster en data science es una tecnica que se utiliza para encontrar grupos que compartan 
# ciertas relaciones que sean similares en termino de la distancia que se establece entre un 
# elemento y otro, o encontrar una serie de patrones dentro del dataset.
# Al reves de las tecnicas de aprendizaje supervisado como la clasificacion o regresion, 
# el analisis de los clusterings no utiliza ninguna informacion etiquetada, sino que utiliza
# las similitudes que se puedan encontrar entre diferentes rasgos, dentro de los datos y aglutinarlos
# o agruparlos dentro de un cluster, existen dos tecnicas basicas de clustering;

# Partitivo:    Toma el dataset original y lo dividira en trozos.
# Jerarquicos:  Proceso inverso, realizando unarbol o dendograma:
# intenta construir jerarquias a traves de las divisiones de los datos, aglomerativa o dividida
# ambos metodos utilizan una funcion de distancia para determinar que elementos o clusters deben
# agruparse o dividirse, se trata de un proceso recursivo, que continua hasta que solo queda un 
# cluster en el caso de aglomerativo o bien en el caso de divisitivo hasta que la unica opcion sea
# dividir o repartir en un nuevo cluster

# Se utliza el dendograma para representar visualmente las jerarquias


  
# Cosumo de proteinas en Europa (25 paises)
protein <- read.csv("../data/tema5/protein.csv")

# escalado de los datos excluyendo variable categorica (country)
data <- as.data.frame(scale(protein[,-1]))
data$Country = protein$Country
rownames(data) = data$Country

# Clustering aglomertivo, par agrupar paises cuyo consumo de proteinas se parece.
hc <- hclust(dist(data, method = "euclidean"),
             method = "ward.D2") # minima varianza
hc
# Los distintos tipos de distancia disponibles en; documentacion de dist()

# visualizar dendograma: 
plot(hc, hang = -0.01, cex = 0.7)
# Entre mas cercana a (0) es la junta de los paises, 
# mas parecidos son en el consumo de proteinas.  
# param, hang, hace que cada elemento cuelgue a una altura base
# la exclusion del parametro hace que el dendograma dibuje cada elemento
# en la altura que le corresponde. 
# param, cex, modifica el tamaño de la fuente. 


# Dividir un dendograma en clsuters 
# cataloga los paises dentro de 4 categorias,en funcion de las similitudes 
fit <- cutree(hc, k=4)

# Podemos observar cuantos elementos tiene cada clase, 
table(fit)

# Representar rectangulos por cada division de cutree; 
rect.hclust(hc, k=4, border="red")



# Uso del metodo de distancia 'euclidea' y metodo de clustering 'single', 
hc2 <- hclust(dist(data, method = "euclidean"),
              method = "single")
plot(hc2, hang=-0.01, cex = 0.7)
# los dendogramas respectivos para los objetos hc y hc2, son muy distintos,
# lo que indica la importancia de escoger bien el metodo de clusterizado. 

# En clustering jerarquico aglomerativo, se toma un enfoque de abajo hacia arriba,
# es decir, cada observacion inicial del dataset, es un cluster en si mismo, 
# luego se van calculando las similaridades y las distancias, de cada elemento 
# al resto y despues se agrupan o juntan aquellos que tengan mas similaridades,
# en comun es decir la agrupacion es entre aquellos que tienen menor distancia 
# entre sí, el calculo se repite iterativamente o sucesivamente, hasta que solo
# queda un clustering restante (la totalidad)

# El divisitivo tienen un enfoque de arriba hasta abajo, todas las observaciones,
# inicialmente empiezan desde un unico cluster, y posteriormente se van ejecutando
# divisiones sucesivas en dos o mas clusters recursivamente de forma que se separan 
# aquellos que no son similares, hasta que solo queda un cluster por observacion.


hc3 <- hclust(dist(data, method = "euclidean"),
              method = "complete")
plot(hc3, hang=-0.01, cex = 0.7)
hc3$merge

hc4 <- hclust(dist(data, method = "euclidean"),
              method = "average")
plot(hc4, hang=-0.01, cex = 0.7)
hc4$merge


# observar la matriz de distancias que se ha calculado:
# calcula las distancias de cada pais respecto al resto,
# especificamente en este caso, respecto al consumo de proteina
d <- dist(data, method = "euclidean")

d 

# vector de "Albania"
alb<-data["Albania",-10]
# Austria 
aus<-data["Austria",-10]

# estos dos vectores de distancia se podrian restar, y tendriamos asi una buena
# aproximacion de la distancia que hay en el consumo de proteinas entre ambos.
alb-aus

# Elevar los coeficientes al cuadrado para que todos sean positivos. y calcular
# la raiz cuadrada de la suma de los cuadrdos, basicamente una generalizacion del
# teorema de pitagoras, formula estandar para calcular distancias de un plano 
# bidimensional, por eso se llama distancia euclidea. 

sqrt(sum((alb-aus)^2))

# En la distancia de Manhattan, podemos observar que oes coeficientes son muy 
# diferentes, y esto sucede porque el calculo de dichos valores es distinto, 
# en lugar de utilizarse la raiz cudarada de la suma de los cuadrados.
# se toma la suma de los valores absolutos de las diferencias. 

d <- dist(data, method = "manhattan")
sum(abs(alb-aus))

# La matriz de distancia nos indica que elementos estan cerca y cuales estan lejos
# una vez sabemos que esta cerca y que esta lejos, hay que decidir como juntar los
# datos, esta aconglomeracion, tiene distintas formas o metodos de ejecucion.
# el metodo de agrupacion que escogemos para realizar el agrupamiento, influye
# en la estructura y forma del mismo. 

# Es muy importante observar la variable merge, ejemplo : 
hc4$merge
# Esta variable, indica para cada uno de los elementos basicos, con quien ha sido
# juntado y en que orden, los simbolos con un menor, indican los objetos basicos
# Ejemplo # 25, el ultimo (Yugoslavia), se junta con el # 18 (Romania), 
# asi sucesivamente, hasta que se empiezan a juntar con positivos, los coeficientes
# positivos, indican el clustering en formacion, entonces indica la union de un pais
# con el clustering en consideracion, se han creado 24 uniones hasta lograr el 
# cluster grupal o general del conj total


# Single: el metodo, recalcula las distancias respecto al resto de elementos,
# elige el minimo de las distancias para cada clusters, que tenia originalmnte

# Completo : funciona a la inversa de single, en lugar de escoger el minimo entre
# dos distancias, selecciona el maximo como distancia entre tres elementos en la 
# Matriz (d), por ejemplo al juntar de (d) Romania, Yugoslavia y Grecia.

# Average: genera un promedio y los escoge como distancia. 

# ward.D2 : combina los elementos de acuerdo al peso asignado, es decir 
# el numero de elementos que el clsuter ya tenga asignado. 
# ponderacion sobre el numero de elemntos del cluster.

# Objetivo: Para las distintas formas de la tecnica, el objetivo es común,
# categorizar, clasificar y simplificar la estructura de datos original.



# Cluster Divisitivo ::: --------------------------------------------------


install.packages("cluster")
library(cluster)
dv <- diana(data, metric = "euclidean")
par(mfrow=c(1,2))
plot(dv)



