#Tesis Análisis de la Cienciometría en biociencias
#Minerva María Romero Pérez 

library(bibliometrix)
#Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#Journal of Informetrics, 11(4), pp 959-975, Elsevier.

library(kableExtra)
library(ggplot2)
library(ggwordcloud)
library(ggpmisc)
library(readxl)
library(tidyverse)
library(extrafont)
library(igraph)

loadfonts(device = "win")
par(family = "Times New Roman")
paletamine13<-c("#ee0080", "#e51088", "#db2090", "#d23098", "#c840a0", "#bf50a8", "#b660b1", "#ac70b9", "#a380c1", "#9990c9", "#90a0d1", "#86b0d9", "#7dc0e1")
paletamine5<-c("#ee0080", "#d23098", "#b660b1", "#9990c9", "#7dc0e1")
paletamine4<-c("#ee0080", "#b660b1", "#9990c9", "#7dc0e1")

M <- read_excel("R/biociencias.xlsx")
results <- biblioAnalysis(M)
S<-summary(results)

M <- as.data.frame(M)
##############################################################################
###################publicaciones por año#####################################
# Numero de publicaciones por año
#Años de 1975 al 2023
años <- c(1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
          1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
          2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022,
          2023)
# Artículos por año
articulos <- c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 2, 1, 0, 2, 5, 3, 2, 7, 6, 8, 5, 9, 13,
              19, 15, 25, 29, 38, 58, 59, 65, 95, 85, 127, 177, 172, 255, 343, 552, 968, 1823, 2116)
# Crear un data frame con los datos
Dñ <- data.frame(años, articulos)

#Gráfica de publicaciones por año
ggplot(Dñ, aes(x = años, y = articulos)) +
  geom_area(fill = "#ee0080", alpha = 0.9) +
  geom_line() +
  geom_text(aes(label = articulos, vjust = ifelse(años %% 2 == 0, -2, -.9)), 
            size = 1.9, color = "black", family = "Times New Roman") +
  scale_x_continuous(breaks = seq(min(años), max(años), by = 2)) +
  labs(title = "Publicaciones por año",
       x = "Año", y = "Artículos") +
theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"))
ggsave("publicacionesaño.png", width = 1608, height = 1185, units = "px", dpi = 300)

##############################################################################
###################Ley de lotka ###############################################
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

# Crear el data frame para la visualización
df <- data.frame(
  Articulos = L$AuthorProd[,1],
  Observada = L$AuthorProd[,3],
  Teorica_Beta2 = 10^(log10(L$C) - 2 * log10(L$AuthorProd[,1])),
  Teorica_BetaCalculada = L$fitted,
  Teorica_Beta1 = 10^(log10(L$C) - 1 * log10(L$AuthorProd[,1]))
)

# Crear la gráfica con ggplot
ggplot(df, aes(x = Articulos)) +
  geom_line(aes(y = Teorica_Beta2, color = "Teórica (Beta=2)"), linewidth = 1) +
  geom_line(aes(y = Teorica_Beta1, color = "Teórica (Beta=1)"), linewidth = 1) +
  geom_line(aes(y = Observada, color = "Observada"), linewidth = 1) +
  geom_line(aes(y = Teorica_BetaCalculada, color = "Teórica (Beta=1.756255)"), linewidth = 1) +
  scale_x_continuous(limits = c(0, 80), oob = scales::squish) +
  scale_y_continuous(limits = c(0, 0.5), oob = scales::squish) +
  scale_color_manual(values = c("Teórica (Beta=2)" = "#ee0080",
                                "Teórica (Beta=1)" = "grey",
                                "Observada" = "#7dc0e1",
                                "Teórica (Beta=1.756255)" = "#b660b1")) +
  labs(x = "Artículos",
       y = "Frecuencia de autores",
       title = "Ley de Lotka",
       color = "Distribución") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),  # Aplica la fuente a la leyenda
    legend.title = element_text(family = "Times New Roman")  # Aplica la fuente al título de la leyenda
  )
ggsave("ley_lotka.png", width = 1608, height = 1185, units = "px", dpi = 300)

##############################################################################
######Ley de Bradford ##### 
#Importar los datos
leyBradford <- read_excel("R/Bradford.xlsx", 
                       sheet = "RevistasBradford")


# Filtrar solo las revistas que pertenecen a la Zona 1
datosNucleo <- subset(leyBradford, Zone == "Zone 1")
# Acortar las etiquetas de las revistas (columna So)
datosNucleo$So <- ifelse(nchar(datosNucleo$So) > 20, 
                             paste0(substr(datosNucleo$So, 1, 20), "..."), 
                             datosNucleo$So)
head(datosNucleo)
# Crear gráfico de área con las revistas ordenadas de mayor a menor frecuencia
ggplot(datosNucleo, aes(x = reorder(So, -Freq), y = Freq, group = 1)) +  # group = 1 es necesario para geom_area
  geom_area(fill = "#ee0080", alpha = 0.6) +  # Usar geom_area para el área rellena
  geom_line(color = "#b660b1", size = 1) +  # Añadir la línea para destacar la forma
  labs(title = "Revistas en el Núcleo de la Ley de Bradford (Zona 1)",
       x = "Revista", y = "Cantidad de artículos") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"))  # Rotar las etiquetas del eje X
ggsave("ley_bradford.png", width = 1608, height = 1185, units = "px", dpi = 300)

##############################################################################
######Ley de Zipf para palabras clave, términos mesh, titulos y resúmenes ##### 

leyZipf <- read_excel("R/palabrasfrecuencia.xlsx", 
                  sheet = "Hoja1")

freq_by_rank <- leyZipf %>% 
  group_by(Palabras) %>% 
  mutate(rank = row_number(), 
         term_frequency = Occurrences/total) %>%
  ungroup()

freq_by_rank
freq_by_rank <- freq_by_rank %>% 
  filter(!is.na(rank) & !is.na(term_frequency) & term_frequency > 0)

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = Palabras)) + 
  geom_abline(intercept = -1.1668, slope = -0.9138, 
              color = "black", linetype = 2, show.legend = FALSE) +
  geom_line(linewidth = .9, alpha = .9) + 
  scale_x_log10() +
  scale_y_log10()+
  xlab("rank")+ # eje x
  ylab("Frecuencia de la n-esima palabra")+ # eje y
  ggtitle("Ley de Zipf")+
  scale_color_manual(values = paletamine4)+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),  
    legend.title = element_text(family = "Times New Roman") )  
ggsave("ley_zipf.png", width = 1608, height = 1185, units = "px", dpi = 300)

rank_subset <- freq_by_rank %>% 
  filter(rank < 1000,
         rank > 0)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
# 
# Call:
# lm(formula = log10(term_frequency) ~ log10(rank), data = rank_subset)
# 
# Coefficients:
# (Intercept)  log10(rank)  
#    -1.1668      -0.9138 
################################# Ley de Zipf para cada categoría #################################

# Leer el archivo CSV (ajusta la ruta si es necesario)
datos <- read_excel("R/palabrasfrecuencia.xlsx", 
                    sheet = "Hoja1")

# Calcular el ranking y la estimación de Zipf por tipo
datos_zipf <- datos %>%
  group_by(Palabras) %>%
  arrange(desc(Occurrences)) %>%
  mutate(Rank = row_number(),
         Zipf_Estimate = max(Occurrences) / Rank) %>%
  ungroup()

# Exportar los resultados a un nuevo archivo CSV
write_csv(datos_zipf, "zipf_resultados.csv")

# Mostrar algunos resultados
print(head(datos_zipf))

# Graficar comparación real vs Zipf estimado
ggplot(datos_zipf, aes(x = Rank)) +
  geom_line(aes(y = Occurrences, color = Palabras)) +
  geom_line(aes(y = Zipf_Estimate, color = "Zipf"), linetype = 5) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~ Palabras, scales = "free") +
  labs(title = "Ley de Zipf por categoria de palabra",
       x = "Rango (log)",
       y = "Frecuencia (log)") +
  scale_color_manual(values = paletamine5) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        plot.title = element_text(family = "Times New Roman"),
        axis.title.x = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),  
        legend.title = element_text(family = "Times New Roman") )
ggsave("ley_zipf_categorias.png", width = 2000, height = 1500, units = "px", dpi = 300)


parametros_zipf <- datos_zipf %>%
  filter(Occurrences > 0) %>%
  mutate(log_Rank = log10(Rank),
         log_Occurrences = log10(Occurrences)) %>%
  group_by(Palabras) %>%
  summarise(
    modelo = list(lm(log_Occurrences ~ log_Rank)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    intercept = coef(modelo)[1],
    slope = coef(modelo)[2],
    r_squared = summary(modelo)$r.squared,
    C = 10^intercept,
    s = -slope  # signo negativo porque Zipf decrece
  ) %>%
  select(Palabras, C, s, r_squared)

# Exportar parámetros
write_csv(parametros_zipf, "zipf_parametros_por_tipo.csv")

# Mostrar por consola
print(parametros_zipf)


##############################################################################
##################### Palabras clave enfermedades ############################
df <- read_excel("R/palabrasfrecuencia.xlsx", 
                 sheet = "enfermedades DE")

##Categoría Bibliometría
set.seed(1)
ggplot(df, aes(label = DE, size = Recuento, color = Recuento)) +
  geom_text_wordcloud(shape = "square", family = "Times New Roman") +
  scale_size_area(max_size = 18) +
  theme_minimal()+
  theme_minimal() +
  scale_color_gradientn(colours = paletamine13) +
  labs(title = "Nube de Palabras Clave")+
  theme(plot.title = element_text(family = "Times New Roman", size = 14, face = "bold", hjust = 0.5))
ggsave("nube_palabras_clave.png", width = 2000, height = 1500, units = "px", dpi = 300)


################ Terminos MeSH enfermedades ##################
#
dfM <- read_excel("R/palabrasfrecuencia.xlsx", 
                 sheet = "enfermedades ID")

##Categoría Bibliometría
set.seed(1)
ggplot(dfM, aes(label = ID, size = Recuento, color = Recuento)) +
  geom_text_wordcloud(shape = "square", family = "Times New Roman") +
  scale_size_area(max_size = 18) +
  theme_minimal()+
  theme_minimal() +
  scale_color_gradientn(colours = paletamine13) +
  labs(title = "Nube de Términos MeSH")+
  theme(plot.title = element_text(family = "Times New Roman", size = 14, face = "bold", hjust = 0.5))
ggsave("nube_terminos_mesh.png", width = 2000, height = 1500, units = "px", dpi = 300)


##############################################################################
############Ley de Price #################

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
equation <- paste("D_ñ = ", round(a, 6), " * exp(", round(b, 4), " * (ñ))", sep = "")

# Predicciones basadas en el ajuste
pred <- predict(fit)

# Crear un nuevo data frame para las líneas de ajuste
predicted_data <- data.frame(year = data$year, predicted = pred)

# Graficar los datos y la curva ajustada con etiquetas, leyendas y ecuación
ggplot() +
  geom_point(data = data, aes(x = year, y = articles, color = "Datos reales")) +
  geom_line(data = predicted_data, aes(x = year, y = predicted, color = "Ajuste exponencial")) +
  #geom_text(data = data, aes(x = year, y = articles, label = articles), vjust = -1, hjust = 1, size = 3) + # Etiquetas para los puntos
  annotate("text", x = 2000, y = 1400, label = equation, color = "#ee0080", size = 4, family = "Times New Roman") + # Añadir la ecuación
  scale_color_manual(name = " ", values = c("Datos reales" = "black", "Ajuste exponencial" = "#ee0080")) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 2))  +
  labs(title = "Ley de Price", x = "Año", y = "Número de artículos") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),  
    legend.title = element_text(family = "Times New Roman") )  
ggsave("ley_price.png", width = 1608, height = 1185, units = "px", dpi = 300)



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

# Calcular r²
r2 <- 1 - (SSR / SST)
r2
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


##############################################################################
############Palabras clave más frecuentes por año #################

PalAñoClave <- read_excel("R/tesis_mine_resultados_tablas_biociencias.xlsx", 
           sheet = "temas en auge palabras clave")

# Ordenar los términos por año
PalAñoClave <- PalAñoClave %>%
  mutate(Term = reorder(Term, `Year (Median)`))

# Filtrar datos desde 2016
PalAñoClave_2016 <- PalAñoClave %>%
  filter(`Year (Median)` >= 2016)

# Identificar los límites de cada año solo desde 2017
lineas_y <- PalAñoClave_2016 %>%
  mutate(Term = as.character(Term)) %>%  # Convertir Term a carácter
  group_by(`Year (Median)`) %>%
  summarize(min_y = min(Term), max_y = max(Term)) %>%
  ungroup()


ggplot(PalAñoClave, aes(x = `Year (Median)`, y = Term, size = Frequency)) +
  geom_point(alpha = 0.7, color = "#ee0080") +
  geom_hline(data = lineas_y, aes(yintercept = min_y), linetype = "dashed", color = "gray70", alpha = 0.5) +
  geom_hline(data = lineas_y, aes(yintercept = max_y), linetype = "dashed", color = "gray70", alpha = 0.5) +
  scale_size_continuous(range = c(2, 15)) +
  scale_x_continuous(breaks = 2006:2023) +
  theme_minimal() +
  labs(
    title = "Palabras clave más frecuentes por año",
    x = "Año Mediano",
    y = "Término",
    size = "Frecuencia"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
ggsave("temas en augePC.png", width = 3000, height = 2200, units = "px", dpi = 300)


##############################################################################
############Términos MeSH más frecuentes por año #################

PalAñoMeSH <- read_excel("R/tesis_mine_resultados_tablas_biociencias.xlsx", 
                          sheet = "temas en auge mesh")

# Ordenar los términos por año
PalAñoMeSH <- PalAñoMeSH %>%
  mutate(Term = reorder(Term, `Year (Median)`))

# Filtrar datos desde 2010
PalAñoMeSH_2010 <- PalAñoMeSH %>%
  filter(`Year (Median)` >= 2010)

# Identificar los límites de cada año solo desde 2017
lineas_ym <- PalAñoMeSH_2010 %>%
  mutate(Term = as.character(Term)) %>%  # Convertir Term a carácter
  group_by(`Year (Median)`) %>%
  summarize(min_y = min(Term), max_y = max(Term)) %>%
  ungroup()

# Crear la gráfica
ggplot(PalAñoMeSH, aes(x = `Year (Median)`, y = Term, size = Frequency)) +
  geom_point(alpha = 0.7, color = "#ee0080") +
  geom_hline(data = lineas_ym, aes(yintercept = min_y), linetype = "dashed", color = "gray70", alpha = 0.5) +
  geom_hline(data = lineas_ym, aes(yintercept = max_y), linetype = "dashed", color = "gray70", alpha = 0.5) +
  scale_size_continuous(range = c(2, 15)) +
  scale_x_continuous(breaks = 2006:2023) +  # << Forzar todos los años
  theme_minimal() +
  labs(
    title = "Términos MeSH en auge por año",
    x = "Año",
    y = "Término",
    size = "Frecuencia"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman")
  )
ggsave("temas en augeMeSH.png", width = 3000, height = 2200, units = "px", dpi = 300)


timespan <- c(1975, 2023)
res <- fieldByYear(M,
                   field = "ID", timespan = timespan,
                   min.freq = 5, n.items = 5, graph = TRUE
)
##############################################################################
############Países #################


M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
