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
library(dplyr)


# Leer los datos, importo dataset


# 1) Filtrar para quedarnos sólo con los sitios que nos interesan
df <- df %>%
  filter(sitio != "otro")

# 2) Paso las variables a factor
#df$temp

# 3) (Opcional) Resetear niveles del factor para que no quede "otro" en las leyendas
df$sitio <- droplevels(df$sitio)

# A partir de aquí, todos tus plots y análisis ya no verán el sitio "otro"  


# Procesamiento de fechas (ajustar nombre si fuera distinto)
df$fecha <- as.Date(df$fecha, format="%Y-%m-%d")

# Crear columna promedio de calidad de hábitat ribereño (app1 a app5)
df$habitat_calidad_prom <- rowMeans(df[, grep("^app[1-5]$", names(df))], na.rm = TRUE)

# ---------------------------
# 1. Variación temporal y espacial de parámetros
# ---------------------------

# Primero ordeno los sitios de aguas arriba a abajo
unique(df$sitio) # chequeo los nombres de los sitios
df$sitio <- factor(df$sitio,
                   levels = c("cand", "vsf", "pca", "pao", "camp"),
                   exclude = "otro")  # opcionalmente sacás “otro” de los niveles
# los ordeno como quiero 


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

#<<<<<<< HEAD
# Por sitio y estación
library(dplyr) 
library(ggplot2)
library(lubridate)
# library(forcats) # si querés ordenar factores

# --- CONFIGURABLES ---
# Si no tenés definido el vector, descomentá y ajustá:
variables_fisicoquimicas <- c("conductividad", "pH", "od", "fosfato")  # ej.

# Orden opcional de sitios (si querés controlarlo):
df$sitio <- factor(df$sitio, levels = c("cand", "vsf", "pca", "pao", "camp"))

# Asegurar tipo fecha
if (!inherits(df$fecha, "Date")) {
  df <- df %>% mutate(fecha = as.Date(fecha))
}

# Función para mapear meses a estaciones (hemisferio sur)
estacion_sur <- function(fecha) {
  m <- month(fecha)
  case_when(
    m %in% c(1, 2, 3) ~ "Verano",
    m %in% c(4, 5, 6)  ~ "Otoño",
    m %in% c(7, 8, 9)  ~ "Invierno",
    TRUE               ~ "Primavera"
  )
}

# Precalcular estación como factor ordenado
df_est <- df %>%
  mutate(estacion = factor(estacion_sur(fecha),
                           levels = c("Otoño","Invierno","Primavera","Verano")))

# Resumen por sitio x estación (mediana y n)
# --- BUCLE DE GRÁFICOS ---
for (var in variables_fisicoquimicas) {  
    resumen <- df_est %>%
    group_by(sitio, estacion) %>%
    summarise(
      mediana = median(.data[[var]], na.rm = TRUE),
      n = sum(!is.na(.data[[var]])),
      .groups = "drop"
    )
  
  p <- ggplot(resumen, aes(x = sitio, y = mediana, fill = estacion)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8) +
    # Etiquetas opcionales con el n de puntos por barra (sacá esta capa si no las querés)
    #geom_text(aes(label = n),
    #          position = position_dodge(width = 0.85),
    #          vjust = -0.5, size = 3, show.legend = FALSE) +
    theme_minimal() +
    labs(
      title = paste(var, "por sitio y estación"),
      x = "Sitio", y = var, fill = "Estación"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
p 
  # Valores por sitio y estación
library(dplyr)
library(ggplot2)
library(lubridate)

# --- 1) Función auxiliar: estación (Hemisferio Sur) ---
estacion_sur <- function(fecha) {
  est <- case_when(
    month(fecha) %in% c(1, 2, 3) ~ "Verano",
    month(fecha) %in% c(4, 5, 6)  ~ "Otoño",
    month(fecha) %in% c(7, 8, 9)  ~ "Invierno",
    month(fecha) %in% c(10, 11, 12)  ~ "Primavera",
    TRUE ~ NA_character_
  )
  factor(est, levels = c("Otoño", "Invierno", "Primavera", "Verano"))
}

# --- 2) Etiquetas de eje Y por variable ---
y_labels <- c(
  cond = "Conductividad (uS/cm)",
  o2   = "Oxígeno disuelto (mg/L)",
  po4  = "Fosfatos (mg/L)",
  temp = "Temperatura del agua (°C)",
  cau  = "Caudal (m3/s)"
)

# --- 3) Loop de gráficos ---
for (var in variables_fisicoquimicas) {
  
  df_plot <- df %>%
    mutate(estacion = estacion_sur(fecha))
  
  # Filtrado especial para PO4
  if (var == "po4") {
    df_plot <- df_plot %>%
      filter(!estacion %in% c("Otoño", "Invierno"))
  }
  
  # Agrupar por sitio y estación
  df_sum <- df_plot %>%
    group_by(sitio, estacion) %>%
    summarise(valor = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  # Etiqueta del eje Y segura
  ylab_txt <- if (var %in% names(y_labels)) y_labels[[var]] else var
  
  # Gráfico
  p <- ggplot(df_sum, aes(x = sitio, y = valor, fill = estacion)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Valores de", ylab_txt, "por sitio y estación"),
      x = "Sitio",
      y = ylab_txt
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_blank()
    )
  print(p)
}
p

# ---------------------------
# 2. Análisis multivariado
# ---------------------------

# Filtrar variables numéricas para PCA
vars_pca <- df %>%
  select(all_of(variables_fisicoquimicas), habitat_calidad_prom) %>%
  drop_na()

# Calcula la varianza de cada variable en vars_pca
variancias <- sapply(vars_pca, function(x) var(x, na.rm = TRUE))
print(variancias) # no puedo tener variables con varianza 0

  # Identifica columnas de varianza cero
cols_constantes <- variancias == 0

# Ver nombres de esas columnas
names(vars_pca)[cols_constantes]
## p. ej.: "ph" "caudal"

# Crea un nuevo data.frame sin esas columnas
vars_pca_ok <- vars_pca[, !cols_constantes]


# Escalar variables
pca_res <- prcomp(vars_pca_ok, scale. = TRUE)
summary(pca_res)

# Visualizar PCA
fviz_pca_biplot(pca_res, label = "var", habillage = df$sitio[complete.cases(vars_pca_ok)],
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
