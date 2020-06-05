install.packages("ROCR")
library(ROCR)

# la curva ROC busca fiabilidad en la clasificacion bimodal. 
# La recta representa la peor clasificacion posible.
# la curva representa la clasificacion teniendo en cuenta :
# Sensibilidad    -> Tasa de verdaderos positivos.
# 1-Especificidad -> Tasa de falsos positivos.
# dichos conceptos no son contrarios ni complementarios, 
# aportan la probabilidad de acierto o fracaso. 
# ambas medidas suelen subir en igual proporcion, observar el 
# grafico para identifica la zona con mayor area. 

data1 <- read.csv("../data/tema3/roc-example-1.csv") # clases numericas.
data2 <- read.csv("../data/tema3/roc-example-2.csv") # clases categoricas. 
#0 -> fallo
#1 -> éxito

# Clases Numericas:  ----
pred1 <- prediction(data1$prob, data1$class)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)
lines(par()$usr[1:2], par()$usr[3:4])

# Calcular la probabilidad de los cortes
prob.cuts.1 <- data.frame(cut = perf1@alpha.values[[1]],
                          fpr = perf1@x.values[[1]],
                          tpr = perf1@y.values[[1]])
head(prob.cuts.1)
# Probabilidades de obtener un tpr o fpr

tail(prob.cuts.1)
# Si el valor es muy bajo ambos tienden a uno 
# El valor asceptable depende de la probabilidad de corte 

# seleccionar todas las filas que tengan un prob.cuts.1 de 
# verdaderos positivos superior a 0.8 
# Luego la probabilidad de exito, debe ser superior a .50
# buscar el balance:::
prob.cuts.1[prob.cuts.1$tpr>=0.8,]


# Clases Categoricas : ----
pred2 <- prediction(data2$prob, data2$class, label.ordering = c("non-buyer", "buyer"))
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2)
lines(par()$usr[1:2], par()$usr[3:4])
