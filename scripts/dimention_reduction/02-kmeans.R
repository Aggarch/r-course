

# Clsutering Generativo o Divisitivo --------------------------------------

# library(devtools)
# devtools::install_github("kassambara/factoextra")

# Con este metodo, se escoge a priori el numero de divisiones que queremos llevar a cabo 
# esta tecnica es mucho mas rapida, toma menos tiempo que la construccion de un arbol 
# gerarquico que es muy costoso computacionalmente.
# es una tecnica parecida a K Nearest Neighbors, cada uno de los objetos del cluster, 
# computara, con sus coordenadas numericas, el centro geometrico del mismo y los nuevos 
# objetos seran colocados en funcion de los anteriores, 

protein <- read.csv("../data/tema5/protein.csv")

# scalamiento de las variables para que tengan pesos equivalentes.
rownames(protein) = protein$Country
protein$Country = NULL
protein.scaled = as.data.frame(scale(protein))


# K-means donde  K = 4 (numero de divisiones)
km <- kmeans(protein.scaled, 4); km
# devuelve cada objeto y al grupo o cluster al que pertenece, el total_ss, indica el 
# resultado de sumar cada uno de los cuatro clusters y dividirlos entre el total.
# es decir que es una forma de saber el porcentaje de los datos que explicamos 
# Se puede realizar el calculo la media de consumo de cada cluster,para cada prodcto::
# Tener en cuenta que las variables estan reescaladas en funcion de la presencia o 
# ausencia de cada producto en cada pais. 
# Por lo tanto como resultado obtenemos los centros geometricos::: 

aggregate(protein.scaled, by = list(cluster = km$cluster), mean)


library(factoextra)
fviz_cluster(km, data = protein.scaled)

# Esta en juego elegir el numero de clusters a elegir, claramente entre mayor sea el numero
# de clusters, mas datos estaremos explicando, sin embargo habran menos divisiones y mas grupos
# individuales, se trata de escoger un balance entre el numero de clusters y la informacion que 
# explica el modelo, al K-Means utilizar la media, es muy suceptible a los outliers. 
# ya que basicamente, calcula el centroide a partir de esta, o centro geometrico, y aglutinar los 
# elementos mas cercanos, los valores muy fuera la normal, desvirtua los resultados. 
# 
