R.home() #chequeo que esté trabajando en conda

# Importo la tabla a través de import dataset

# Al final instalé los paquetes via conda
library(tidyverse)# Manipulación y visualización de datos (incluye ggplot2, dplyr, readr, etc.)
tidyverse_update()  # (opcional, te dice si hay paquetes que actualizar)
library(lubridate)# Manejo de fechas y horas
library(permute)
library(vegan)# Análisis multivariados (PCA, NMDS, RDA, etc.)
library(ggpubr)# Extensión de ggplot2 para gráficos listos para publicaciones
library("nloptr")
library("RcppEigen")
library("tidyr")
library("lme4")
library("pbkrtest")
library("car")
library("stats")
library("rstatix")
install.packages("factoextra")   # Visualización de resultados de análisis multivariados (PCA, clustering, etc.)
install.packages("reshape2")     # Reorganización de datos (aunque puede usarse `pivot_longer` de `tidyverse` en su lugar)
install.packages("ggplot2")
library(ggplot2)
library("factoextra")   # Visualización de resultados de análisis multivariados (PCA, clustering, etc.)
library("reshape2")     # Reorganización de datos (aunque puede usarse `pivot_longer` de `tidyverse` en su lugar)


# Leer los datos, importo dataset

# Procesamiento de fechas (ajustar nombre si fuera distinto)
df$fecha <- as.Date(df$fecha, format="%Y-%m-%d")

# Crear columna promedio de calidad de hábitat ribereño (app1 a app5)
df$habitat_calidad_prom <- rowMeans(df[, grep("^app[1-5]$", names(df))], na.rm = TRUE)

# ---------------------------
# 1. Variación temporal y espacial de parámetros
# ---------------------------

# Variables físico-químicas (ajustar a tus nombres exactos si son distintos)
variables_fisicoquimicas <- c("cond", "pH", "o2", "po4", "no3", "no2", "temp", "durcar", "durtot", "cl", "cau")

# Boxplots por sitio
for (var in variables_fisicoquimicas) {
  ggplot(df, aes(x = sitio, y = .data[[var]], fill = sitio)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Distribución de", var, "por sitio"),
         y = var, x = "Sitio") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> plot
  print(plot)
}

# Tendencias temporales
for (var in variables_fisicoquimicas) {
  ggplot(df, aes(x = fecha, y = .data[[var]], color = sitio)) +
    geom_line() + geom_point() +
    theme_minimal() +
    labs(title = paste("Evolución temporal de", var),
         y = var, x = "Fecha") -> plot
  print(plot)
}

# ---------------------------
# 2. Análisis multivariado
# ---------------------------

# Filtrar variables numéricas para PCA
vars_pca <- df %>%
  select(all_of(variables_fisicoquimicas), habitat_calidad_prom) %>%
  drop_na()

# Escalar variables
pca_res <- prcomp(vars_pca, scale. = TRUE)

# Visualizar PCA
fviz_pca_biplot(pca_res, label = "var", habillage = df$sitio[complete.cases(vars_pca)],
                addEllipses = TRUE, ellipse.level = 0.95,
                title = "PCA de variables ambientales")

# Análisis de agrupamientos (clustering jerárquico)
distancia <- dist(scale(vars_pca))
hc <- hclust(distancia, method = "ward.D2")

plot(hc, main = "Dendrograma de clustering jerárquico")

# ---------------------------
# 3. Relación entre calidad de hábitat y otras variables
# ---------------------------

# Correlación simple
cor_matrix <- cor(df[, c(variables_fisicoquimicas, "habitat_calidad_prom")], use = "complete.obs")
print(round(cor_matrix, 2))

# Visualización con ggplot2
library(reshape2)
melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() + scale_fill_gradient2() +
  theme_minimal() + labs(title = "Matriz de correlaciones")
