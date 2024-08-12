#Tesis Análisis de la Cienciometría en biociencias
#Minerva María Romero Pérez 

library(bibliometrix)
#Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#Journal of Informetrics, 11(4), pp 959-975, Elsevier.

library(kableExtra)
library(ggplot2)
library(ggwordcloud)
library(readxl)


paletamine13<-c("#ee0080", "#e51088", "#db2090", "#d23098", "#c840a0", "#bf50a8", "#b660b1", "#ac70b9", "#a380c1", "#9990c9", "#90a0d1", "#86b0d9", "#7dc0e1")
paletamine5<-c("#ee0080", "#d23098", "#b660b1", "#9990c9", "#7dc0e1")
paletamine4<-c("#ee0080", "#b660b1", "#9990c9", "#7dc0e1")

M <- read_excel("MINERVA/git/tesis_minerva/codigo/biociencias.xlsx")
results <- biblioAnalysis(M)
S<-summary(results)

##############################################################################
###################Ley de lotka###############################################
L <- lotka(results)

lotkaTable=cbind(L$AuthorProd[,1],L$AuthorProd[,2],L$AuthorProd[,3],L$fitted)

knitr::kable(lotkaTable, caption = "Frequency Of Authors Based on Lotka's Law", digits = 3, align = "cccc", format = "html",col.names = c("Number of article", "Number of authors", "Frequency based on data", "Frequency based on Lotka's law")) %>%
  kable_classic(full_width = F, position = "center")

#knitr::kable(lotkaTable, caption = "Frequency Of Authors Based on Lotka's Law", digits = 3, align = "cccc", format = "simple",col.names = c("Number of article", "Number of authors", "Frequency based on data", "Frequency based on Lotka's law")) %>%
#  kable_classic(full_width = F, position = "center")

# Productividad del autor. Distribución empírica
L$AuthorProd
# Estimación del coeficiente beta
L$Beta
#[1] 1.756255

# Constante
L$C
#[1] 0.1522589

# Goodness of fit
L$R2
#[1] 0.8829081

# Valor p de la prueba de dos muestras K-S
L$p.value
#[1] 0.8829081

# Distribución observada
Observed=L$AuthorProd[,3]

# Distribución teórica con Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

lotkaTable2=cbind(L$AuthorProd[,1],L$AuthorProd[,2],L$AuthorProd[,3],L$fitted,Theoretical)

knitr::kable(lotkaTable2, caption = "Frecuencia de los autures basada en la ley de Lotka", digits = 4, align = "cccc", format = "html",col.names = c("Numero de artículos", "Numero de autores", "Freciencia basada en los datos", "Freciencia basada en la ley de Lotka con Beta calculada", "Frecuencia basdada en la ley Lotka con Beta = 2")) %>%
  kable_classic(full_width = F, position = "center")

plot(L$AuthorProd[,1],Theoretical,type="l",col="#ee0080",ylim=c(0, 0.75), xlab="Artículos",ylab="Freq. de autores",main="Productividad científica")
lines(L$AuthorProd[,1],Observed,col="#7dc0e1")
lines(L$AuthorProd[,1],L$fitted,col="#b660b1")
legend(x="topright",c("Teórica (Beta=2)","Observada", "Teórica (Beta=1.756255)"),col=c("#ee0080","#7dc0e1","#b660b1" ),lty = c(1,1,1),cex=0.6,bty="n")


##############################################################################
######Ley de Zipf para palabras clave, términos mesh, titulos y resúmenes##### 

pal <- read_excel("R/palabrasfrecuencia.xlsx", 
                  sheet = "Hoja1")

freq_by_rank <- pal %>% 
  group_by(Palabras) %>% 
  mutate(rank = row_number(), 
         term_frequency = Occurrences/total) %>%
  ungroup()

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = Palabras)) + 
  geom_abline(intercept = -1.1641, slope = -0.9149, 
              color = "gray", linetype = 2, show.legend = FALSE) +
  geom_line(linewidth = .9, alpha = .9) + 
  scale_x_log10() +
  scale_y_log10()+
  xlab("rank")+ # eje x
  ylab("Frecuencia de la n-esima palabra")+ # eje y
  ggtitle("Ley de Zipf")+
  scale_color_manual(values = paletamine4)

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
# 
# Call:
# lm(formula = log10(term_frequency) ~ log10(rank), data = rank_subset)
# 
# Coefficients:
# (Intercept)  log10(rank)  
#  -1.1641      -0.9149  
 

##############################################################################
##################### Palabras clave enfermedades ############################
df <- read_excel("R/palabrasfrecuencia.xlsx", 
                 sheet = "enfermedades DE")

##Categoría Bibliometría
set.seed(1)
ggplot(df, aes(label = DE, size = Recuento, color = Recuento)) +
  geom_text_wordcloud(shape = "square") +
  scale_size_area(max_size = 19) +
  theme_minimal()+
  scale_color_gradientn(colours = paletamine13)

################ Terminos MeSH enfermedades ##################
#
df <- read_excel("R/palabrasfrecuencia.xlsx", 
                 sheet = "enfermedades ID")

##Categoría Bibliometría
set.seed(1)
ggplot(df, aes(label = ID, size = Recuento, color = Recuento)) +
  geom_text_wordcloud(shape = "square") +
  scale_size_area(max_size = 19) +
  theme_minimal()+
  scale_color_gradientn(colours = paletamine13)

##############################################################################
############Publicaciones por año, y ajuste a una exponencial#################

# Numero de publicaciones por año
#Años de 1975 al 2023
year <- c(1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
          1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
          2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022,
          2023)
# Artículos por año
articles <- c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 2, 1, 0, 2, 5, 3, 2, 7, 6, 8, 5, 9, 13,
              19, 15, 25, 29, 38, 58, 59, 65, 95, 85, 127, 177, 172, 255, 343, 552, 968, 1823, 2116)

# Crear un data frame con los datos
data <- data.frame(year, articles)

# Ajustar el modelo exponencial
fit <- nls(articles ~ a * exp(b * (year - 1975)), data = data, start = list(a = 1, b = 0.1))
#Formula: articles ~ a * exp(b * (year - 1975))

# Mostrar los resultados del ajuste
summary(fit)

#Parametros:
#  Estimate Std. Error t value Pr(>|t|)    
#a 9.037e-06  6.900e-06    1.31    0.197    
#b 4.028e-01  1.617e-02   24.91   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error estándar residual: 59.18 en 47 grados de libertad

#Número de iteraciones para la convergencia: 46 
#Tolerancia de convergencia lograda: 6.734e-06

# Obtener los coeficientes del modelo
coef_fit <- coef(fit)
a <- coef_fit[1]
b <- coef_fit[2]

# Crear una cadena de texto con la ecuación del modelo
equation <- paste("y = ", round(a, 6), " * exp(", round(b, 4), " * (x - 1975))", sep = "")

# Predicciones basadas en el ajuste
pred <- predict(fit)

# Crear un nuevo data frame para las líneas de ajuste
predicted_data <- data.frame(year = data$year, predicted = pred)

# Graficar los datos y la curva ajustada con etiquetas, leyendas y ecuación
ggplot() +
  geom_point(data = data, aes(x = year, y = articles, color = "Datos reales")) +
  geom_line(data = predicted_data, aes(x = year, y = predicted, color = "Ajuste exponencial")) +
  #geom_text(data = data, aes(x = year, y = articles, label = articles), vjust = -1, hjust = 1, size = 3) + # Etiquetas para los puntos
  annotate("text", x = 2000, y = 1400, label = equation, color = "#ee0080", size = 4) + # Añadir la ecuación
  scale_color_manual(name = " ", values = c("Datos reales" = "black", "Ajuste exponencial" = "#ee0080")) +
  labs(title = "Ajuste Exponencial de Artículos por Año", x = "Año", y = "Número de Artículos") +
  theme_minimal()


# Calcular la tasa de crecimiento anual (R)
R <- exp(b) - 1
R
#0.4959895

# Calcular el tiempo de duplicación (D)
D <- log(2) / b
D
#1.720874

# Valores observados y predichos
obs <- data$articles


# Calcular SST (Suma Total de los Cuadrados)
SST <- sum((obs - mean(obs))^2)

# Calcular SSR (Suma de los Cuadrados de los Residuos)
SSR <- sum((obs - pred)^2)

# Calcular R²
R2 <- 1 - (SSR / SST)
R2
#[1] 0.9801874

# Número de parámetros
p <- 2 

# Número de observaciones
n <- length(obs) 

# Calcular SSM (Suma de los Cuadrados del Modelo)
SSM <- SST - SSR

# Calcular el estadístico F
F_statistic <- (SSM / p) / (SSR / (n - p - 1))

# Calcular el p-valor del estadístico F
p_value <- pf(F_statistic, p, n - p - 1, lower.tail = FALSE)

# Mostrar resultados
F_statistic
p_value

#> F_statistic
#[1] 1137.877
#> p_value
#[1] 6.75553e-40
> 
