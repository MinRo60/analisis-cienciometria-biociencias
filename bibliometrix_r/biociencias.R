library(tidyverse)
library(ggplot2)
library(ggwordcloud)


################ Lotka ##################
df <- read.xlsx("MINERVA/git/tesis_minerva/codigo/resultados.xlsx")
col <- sapply(seq(0.1, 1, 0.05), function(x) adjustcolor("#EE0080", x, alfa.f=0.5))
##Categoría Bibliometría
set.seed(1)
ggplot(df, aes(label = ID, size = conteo, color = conteo)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()+
  scale_color_gradientn(colours = col)

