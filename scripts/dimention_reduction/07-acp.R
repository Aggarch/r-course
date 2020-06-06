
# Principa Components Analysis  ---------------------------------------------------------------



# En general el analisis de componentes principales, es una tecnica de reduccion de la dimension
# que transforma una entrada (m) - dimensional, un espacio de un numero (m) de dimensiones,
# por un espacio (n) dimensional, donde (n) es un numero estrictamente menor que (m)
# el objetivo es disminuir la cantidad de informacion o variabilidad, descartando (m -n)

# El objetivo es quitar todo aquello que no aporta nada a nuestra informacion,
# para tener un conjunto de datos mas reducido que el original.


bh <- read.csv("../data/tema5/BostonHousing.csv")

install.packages("corrplot")
library(corrplot)
corr <- cor(bh[,-14])
corr

corrplot(corr, method = "circle")
#scale = T, matriz de correlaciones
#scale = F, matriz de covarianzas
bh.acp <- prcomp(bh[,-14], scale = T)

summary(bh.acp)

plot(bh.acp)
plot(bh.acp, type = "lines")

biplot(bh.acp, col = c("gray", "red"))


head(bh.acp$x,5)

bh.acp$rotation

bh.acp$sdev
