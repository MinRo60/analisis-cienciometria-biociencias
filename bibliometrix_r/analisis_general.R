#Tesis
#Minerva María Romero Pérez 

library(bibliometrix)
library(pubmedR)
library(ggplot2)
#Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#Journal of Informetrics, 11(4), pp 959-975, Elsevier.

# Si tenemos una clave API
#api_key <- "your API key"

# Si no tenemos una clave API
api_key = NULL

#CONSULTA
#### revisar la consulta ver como se hacen 
query <- "Bibliometrics[mesh] OR entitymetr*[Title] OR scientometr*[Title] OR altmetr*[Title] OR infometr*[Title]"
#####"Bibliometrics"[Mesh] OR ENTITYMETR*[Title] OR SCIENTOMETR*[Title] OR ALTMETR*[Title] OR INFOMETR*[Title]
#Ahora queremos saber cuántos documentos podría recuperar nuestra consulta.
#Para hacer eso, usamos la función pmQueryTotalCount:

res <- pmQueryTotalCount(query = query, api_key = api_key)

res$total_count

#Podríamos decidir cambiar la consulta o continuar descargando toda la colección 
#o una parte de ella (estableciendo el argumento de límite inferior a res$total_count).
#Decidimos descargar toda la colección compuesta por 1122 documentos:
D <- pmApiRequest(query = query, limit = res$total_count, api_key = NULL)
#Podemos ver la consulta extendida d
D$query_translation
#Finalmente, transformamos el objeto D estructurado xml en un marco de datos, con casos correspondientes
#a documentos y variables a etiquetas de campo 
M <- pmApi2df(D)

str(M)
#Luego, agregamos algunos metadatos a la colección pubmed y usamos las funciones "biblioAnalysis" y 
#"summary" para realizar un análisis descriptivo del marco de datos:
M <- convert2df(D, dbsource = "pubmed", format = "api")

results <- biblioAnalysis(M)
summary(results)


##########################################################
#ANÁLISIS DE LA LITERATURA

#Coocurrencias de palabras clave
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Gráfica la red de coocurrencias de palabras clave con 30 y 50 nodos, la de 30 nodos se gráfica de tipo círculo y de tipo fruchterman
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocurrencias de palabras", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocurrencias de palabras", type = "circle", size=T,edgesize = 5,labelsize=0.7)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 50, Title = "Co-ocurrencias de palabras", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
