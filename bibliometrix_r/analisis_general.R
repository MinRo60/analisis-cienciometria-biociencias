#Tesis
#Minerva María Romero Pérez 

library(bibliometrix)
library(pubmedR)
library(kableExtra)
library(ggplot2)
#Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#Journal of Informetrics, 11(4), pp 959-975, Elsevier.

# Si tenemos una clave API
api_key <- "502dad7087344ed54bd61f5e9caa08f2cd09"

# Si no tenemos una clave API
#api_key = NULL

#CONSULTA
#### revisar la consulta ver como se hacen 
query <- "bibliometr*[Title]OR scientometr*[Title]"
#####Bibliometrics[Mesh] OR ENTITYMETR*[Title] OR SCIENTOMETR*[Title] OR ALTMETR*[Title] OR INFOMETR*[Title]

#Ahora queremos saber cuántos documentos podría recuperar nuestra consulta.
#Para hacer eso, usamos la función pmQueryTotalCount:

res <- pmQueryTotalCount(query = query, api_key = api_key)

res$total_count

#Podríamos decidir cambiar la consulta o continuar descargando toda la colección 
#o una parte de ella (estableciendo el argumento de límite inferior a res$total_count).
#Decidimos descargar toda la colección compuesta por 9536 documentos:
D <- pmApiRequest(query = query, limit = res$total_count, api_key = api_key)
#Podemos ver la consulta extendida 
D$query_translation
#Finalmente, transformamos el objeto D estructurado xml en un marco de datos, con casos correspondientes
#a documentos y variables a etiquetas de campo 
M <- pmApi2df(D)

str(M)
#Luego, agregamos algunos metadatos a la colección pubmed y usamos las funciones "biblioAnalysis" y 
#"summary" para realizar un análisis descriptivo del marco de datos:
M <- convert2df(D, dbsource = "pubmed", format = "api")

results <- biblioAnalysis(M)
a<-summary(results)

#Generamos una tabla de los resultados con Kable, 
#esta también nos permite manajer esta tabla con distinto sformatos como html, latex, Markdown, etc.

#Información principal sobre los artículos
knitr::kable(a$MainInformationDF, caption = "Información principal sobre los artículos",align = "llccl", format = "html") %>% 
  kable_classic(full_width = F, position = "center")
knitr::kable(a$MainInformationDF, caption = "Información principal sobre los artículos",align = "llccl", format = "latex") %>% 
  kable_classic(full_width = F, position = "center")

#Producción anual de artículos científicos
knitr::kable(a$AnnualProduction, caption = "Producción Anual de Documentos Científicos",align = "cc", format = "html") %>%
  kable_classic(full_width = F, position = "center")

#Autores que más publican
knitr::kable(a$MostProdAuthors, caption = "10 principales autores", align = "lclc", format = "html") %>% kable_classic(full_width = F, position = "center")

#Palabras clave y terminos MeSH principales
knitr::kable(a$MostRelKeywords, caption = "10 principales palabras clave y terminos MeSH", align = "lclc", format = "html") %>%
  add_footnote(c("DE: Keywords Extracted from Articles","ID: Keywords Extracted from References of Articles"), notation="alphabet") %>%  
  kable_classic(position = "center")

#Principales países según la frecuencia de publicaciones
knitr::kable(head(sort(table(M$SO_CO),decreasing=TRUE),20), caption = "Principales países según la frecuencia de publicaciones en sus revistas", col.names =c("Country", "Frequency"), align = "lc", format = "html") %>%
  kable_classic(full_width = F, position = "center" )
knitr::kable(head(sort(table(M$SO_CO),decreasing=TRUE),20), caption = "Principales países según la frecuencia de publicaciones en sus revistas", col.names =c("Country", "Frequency"), align = "lc", format = "latex") %>%
  kable_classic(full_width = F, position = "center" )

##########################################################
#ANÁLISIS DE LA LITERATURA

#Los 10 autores principales y su línea de tiempo de producción
res <- authorProdOverTime(M, k=10)


####################No me deja correrlo 
#Coocurrencias de palabras clave
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Gráfica la red de coocurrencias de palabras clave con 30 y 50 nodos, la de 30 nodos se gráfica de tipo círculo y de tipo fruchterman
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocurrencias de palabras", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Co-ocurrencias de palabras", type = "circle", size=T,edgesize = 5,labelsize=0.7)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 50, Title = "Co-ocurrencias de palabras", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


#Análisis descriptivo de las características del gráfico de red.
#La función networkStat calcula estadísticas resumidas de la red.
netstat <- networkStat(NetMatrix)
names(netstat$network)
names(netstat$vertex)
summary(netstat, k=30)

#Coocurrencias de palabras clave del autor
NetMatrix1 <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
net1=networkPlot(NetMatrix1, normalize="association", weighted=T, n = 30, Title = "Coocurrencias de palabras del autor", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
net1=networkPlot(NetMatrix1, normalize="association", weighted=T, n = 50, Title = "Coocurrencias de palabras del autor", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
net1=networkPlot(NetMatrix1, normalize="association", weighted=T, n = 60, Title = "Coocurrencias de palabras del autor", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

#Análisis descriptivo de las características del gráfico de red.
#La función networkStat calcula varias estadísticas resumidas.
netstat <- networkStat(NetMatrix1)
names(netstat$network)
names(netstat$vertex)
summary(netstat, k=30)
####################################################################################################

###########################Leyes 
#Ley de lotka
L <- lotka(results)

lotkaTable=cbind(L$AuthorProd[,1],L$AuthorProd[,2],L$AuthorProd[,3],L$fitted)
knitr::kable(lotkaTable, caption = "Frequency Of Authors Based on Lotka's Law", digits = 3, align = "cccc", format = "html",col.names = c("Number of article", "Number of authors", "Frequency based on data", "Frequency based on Lotka's law")) %>%
  kable_classic(full_width = F, position = "center")

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

#Ley de Bradford
bradford(M)


########################### 


#Red de palabras clave del autor
A1 <- cocMatrix(M, Field = "DE", sep = ";") #"field" es de donde se va a extraer la iformación en esta nos interesan dos
sort(Matrix::colSums(A1), decreasing = TRUE)[1:50]

#Red de palabras clave Plus (seran los terminos MeSH)
A2 <- cocMatrix(M, Field = "ID", sep = ";")
A2mat <- sort(Matrix::colSums(A2), decreasing = TRUE)[1:50]
sort(Matrix::colSums(A1), decreasing = TRUE)[1:100]
networkPlot(A2mat, normalize="association", weighted=T, n = 30, Title = "Coocurrencias de palabras del autor", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

#Análisis de co-palabras sirve para mapear la estructura conceptual de un marco 
#utilizando las palabras clave co-ocurrencias en una colección bibliográfica.
CSDE1 <- conceptualStructure(M,field="DE", method="MDS", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
CSDE2 <- conceptualStructure(M,field="DE",method="CA",stemming=FALSE, minDegree=3, k.max = 5)#minDegree es un número entero. Indica las ocurrencias mínimas de términos a analizar y trazar
CSDE3 <- conceptualStructure(M,field="DE",method="MCA",stemming=FALSE, minDegree=3, k.max = 5) #k.max es un número entero. Indica el número máximo de clusters a mantener.
CSDE4 <- conceptualStructure(M,field="DE",method="MDS",stemming=FALSE, minDegree=3, k.max = 5)

#Análisis de co-palabras sirve para mapear la estructura conceptual de un marco 
#utilizando las palabras de los terminos MeSH co-ocurrencias en una colección bibliográfica.
CSm1 <- conceptualStructure(M,field="ID", method="MDS", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=5)
CSm2 <- conceptualStructure(M,field="ID",method="CA",stemming=FALSE, minDegree=3, k.max = 5)
CSm3 <- conceptualStructure(M,field="ID",method="MCA",stemming=FALSE, minDegree=3, k.max = 5)
CSm4 <- conceptualStructure(M,field="ID",method="MDS",stemming=FALSE, minDegree=3, k.max = 5)

#El mapa temático es un gráfico muy intuitivo y podemos analizar los temas según 
#el cuadrante en el que se encuentran: 
#(1) cuadrante superior derecho: temas-motores; 
#(2) cuadrante inferior derecho: temas básicos; 
#(3) cuadrante inferior izquierdo: temas emergentes o desaparecidos; 
#(4) cuadrante superior izquierdo: temas muy especializados/de nicho.
#Terminos MeSH
Map=thematicMap(M, field = "ID", n = 250, minfreq = 4,stemming = FALSE, size = 0.7, n.labels=5, repel = TRUE)
plot(Map$map)
#palabras clave 
Map1=thematicMap(M, field = "DE", n = 250, minfreq = 4,stemming = FALSE, size = 0.7, n.labels=5, repel = TRUE)
plot(Map1$map)

#Asociación entre autores, DE y revistas.
threeFieldsPlot(M)