#Tesis
#Minerva María Romero Pérez 

library(bibliometrix)
library(pubmedR)
library(ggplot2)
#Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.

# Si tenemos una clave API
#api_key <- "your API key"

# Si no tenemos una clave API
api_key = NULL

#CONSULTA
#### revisar la consulta ver como se hacen 
query <- "(bibliometr*[Title] OR scientometr*[Title])  AND (COVID-19)AND 2019:2023[pdat]"
#####"
#Ahora queremos saber cuántos documentos podría recuperar nuestra consulta.
#Para hacer eso, usamos la función pmQueryTotalCount:

res <- pmQueryTotalCount(query = query, api_key = api_key)

res$total_count
D$query_translation

#Podríamos decidir cambiar la consulta o continuar descargando toda la colección o una parte de ella (estableciendo el argumento de límite inferior a res$total_count).
#Decidimos descargar toda la colección compuesta por 1122 documentos:
D <- pmApiRequest(query = query, limit = res$total_count, api_key = NULL)

#Finalmente, transformamos el objeto D estructurado xml en un marco de datos, con casos correspondientes a documentos y variables a etiquetas de campo 
M <- pmApi2df(D)

str(M)
#Luego, agregamos algunos metadatos a la colección pubmed y usamos las funciones "biblioAnalysis" y "summary" para realizar un análisis descriptivo del marco de datos:
M <- convert2df(D, dbsource = "pubmed", format = "api")

results <- biblioAnalysis(M)
summary(results)


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ##########################################################
#ANÁLISIS DE LA LITERATURA

#Ley de lotka
L <- lotka(results)
L

lotkaTable=cbind(L$AuthorProd[,1],L$AuthorProd[,2],L$AuthorProd[,3],L$fitted)
knitr::kable(lotkaTable, caption = "Frequency Of Authors Based on Lotka's Law", digits = 3, align = "cccc", format = "html",col.names = c("Number of article", "Number of authors", "Frequency based on data", "Frequency based on Lotka's law")) %>%
  kable_classic(full_width = F, position = "center")
lotkaTable
# Productividad del autor. Distribución empírica
L$AuthorProd
# Estimación del coeficiente beta
L$Beta
# Constante
L$C
# Goodness of fit
L$R2
# Valor p de la prueba de dos muestras K-S
L$p.value
# Distribución observada
Observed=L$AuthorProd[,3]

# Distribución teórica con Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Artículos",ylab="Freq. de autores",main="Productividad científica")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("teórica (B=2)","observada"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")


#Coocurrencias de palabras clave
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Gráfica la red de coocurrencias de palabras clave con 30 y 50 nodos, la de 30 nodos se gráfica de tipo círculo y de tipo fruchterman
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocurrencias de palabras", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocurrencias de palabras", type = "circle", size=T,edgesize = 5,labelsize=0.7)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 50, Title = "Co-ocurrencias de palabras", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
