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

###################################
####### Índice de Ribera ##########
###################################

# Paquetes
library(tidyverse)
library(ggplot2)

# ========= 0) Partimos del df YA cargado =========
# Seguridad por si el df tiene menos de 109 filas:
if (exists("df") && is.data.frame(df)) {
  filas_a_quitar <- intersect(c(108, 109), seq_len(nrow(df)))
  if (length(filas_a_quitar) > 0) df <- df[-filas_a_quitar, ]
} else {
  stop("No encuentro el objeto 'df'. Asegurate de haber importado la tabla (xlsx).")
}

# ========= 1) Detectar columna de Sitio de forma robusta =========
# a) Candidatos frecuentes por nombre
site_name_candidates <- c("Sitio","sitio","SITIO","site","Site","NombreSitio","nombre_sitio","SITIO_NOMBRE")

site_col <- intersect(site_name_candidates, names(df)) %>% dplyr::first()

# b) Si no aparece por nombre, intentamos inferir por contenido
if (is.null(site_col)) {
  target_sites <- c("CAND","VSF","PCA","PAO","CAMP")
  char_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  if (length(char_cols)) {
    scores <- sapply(char_cols, function(nm) {
      vals <- toupper(trimws(as.character(df[[nm]])))
      sum(unique(vals) %in% target_sites)
    })
    if (max(scores) > 0) {
      site_col <- names(scores)[which.max(scores)]
    }
  }
}

if (is.null(site_col)) {
  stop("No pude identificar la columna de sitios. Revisá names(df) para ver cómo se llama.")
}

# Normalizar nombre a 'Sitio'
df <- df %>% rename(!!sym("Sitio") := all_of(site_col))

# ========= 2) Calcular Hidrología (promedio de Desbordado y Entubado) =========
# Detectar columnas de Desbordado/Entubado tolerando mayúsculas/minúsculas
find_col <- function(cands) {
  out <- intersect(cands, names(df))
  if (length(out)) out[[1]] else NA_character_
}

desbord_col <- find_col(c("Desbordado","desbordado","DESBORDADO"))
entub_col   <- find_col(c("Entubado","entubado","ENTUBADO"))

if (is.na(desbord_col) || is.na(entub_col)) {
  stop("No encontré columnas 'Desbordado' y/o 'Entubado'. Revisá names(df).")
}

df <- df %>%
  mutate(Hidrología = rowMeans(cbind(.data[[desbord_col]], .data[[entub_col]]), na.rm = TRUE))

# ========= 3) Seleccionar variables problemáticas (tolerante a guiones) =========
candidatas <- c(
  "Uso del suelo",
  "Vegetación de la ribera",
  "Vegetación acuática",
  "Agua – Transparencia","Agua - Transparencia",
  "Agua – Olor","Agua - Olor",
  "Basura",
  "Agua – Aceites","Agua - Aceites",
  "Vegetación en las márgenes","Vegetación en las margenes",
  "Exóticas – Acacia","Exóticas - Acacia",
  "Exóticas – Ligustro","Exóticas - Ligustro"
)

vars_existentes <- unique(intersect(candidatas, names(df)))
vars_finales <- c(vars_existentes, "Hidrología")

if (!length(vars_existentes)) {
  stop("No encontré columnas de problemáticas. Revisá names(df) y ajustá 'candidatas'.")
}

# ========= 4) Agregar por Sitio y pasar a formato largo =========
df_agregado <- df %>%
  select(Sitio, all_of(vars_finales)) %>%
  group_by(Sitio) %>%
  summarise(across(all_of(vars_finales), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = -Sitio, names_to = "Problematica", values_to = "Valor")

# ========= 5) Categorías de color por rangos =========
df_agregado <- df_agregado %>%
  mutate(
    color_cat = case_when(
      Valor > 8                    ~ "azul",
      Valor > 6  & Valor <= 8      ~ "verde",
      Valor > 4  & Valor <= 6      ~ "amarillo",
      Valor > 2  & Valor <= 4      ~ "naranja",
      Valor <= 2                   ~ "rojo",
      TRUE                         ~ NA_character_
    ),
    # (opcional) fijar orden del eje X
    Problematica = factor(
      Problematica,
      levels = c(
        "Agua – Transparencia","Agua - Transparencia",
        "Agua – Olor","Agua - Olor",
        "Agua – Aceites","Agua - Aceites",
        "Hidrología",
        "Basura",
        "Uso del suelo",
        "Vegetación de la ribera",
        "Vegetación acuática",
        "Vegetación en las márgenes","Vegetación en las margenes",
        "Exóticas – Acacia","Exóticas - Acacia",
        "Exóticas – Ligustro","Exóticas - Ligustro"
      ) %>% (\(x) x[x %in% df_agregado$Problematica])()
    ),
    ## <-- agregá ESTA línea para ordenar los paneles:
    Sitio = factor(Sitio, levels = c("Candonga","Vado San Francisco",
                                     "Puente Cerro Azul","Puenta Agua de Oro",
                                     "Camping El Algarrobo"))
  )

colores_custom <- c(
  "azul" = "blue",
  "verde" = "green",
  "amarillo" = "yellow",
  "naranja" = "orange",
  "rojo" = "red"
)

# ========= 6) Gráfico facetado =========
p_facet <- ggplot(df_agregado, aes(x = Problematica, y = Valor, fill = color_cat)) +
  geom_col(width = 0.8) +
  facet_wrap(~ Sitio, ncol = 1) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_fill_manual(
    values = colores_custom,
    breaks = c("azul","verde","amarillo","naranja","rojo"),
    labels = c("Muy bueno","Bueno","Regular","Malo","Muy malo"),
    name   = NULL
  ) +
  guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = "Promedio de calidad ambiental") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_facet)

unique(df_agregado$Sitio)

library(stringr)

# carpeta de salida
dir.create("graficos_sitio", showWarnings = FALSE)

# función de ploteo SIN leyenda
plot_site <- function(d, site_name) {
  ggplot(d, aes(x = Problematica, y = Valor, fill = color_cat)) +
    geom_col(width = 0.8) +
    coord_cartesian(ylim = c(0, 10)) +
    scale_fill_manual(values = colores_custom, guide = "none") +  # sin leyenda
    labs(x = NULL, y = "Promedio (0–10)",
         title = paste("Problemáticas –", site_name)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}

# guardar un PNG por sitio usando .y (clave del grupo)
df_agregado %>%
  dplyr::group_by(Sitio) %>%
  dplyr::group_walk(~{
    site_name <- as.character(.y$Sitio[[1]])
    g <- plot_site(.x, site_name)
    fname <- paste0(
      "problematicas_",
      str_replace_all(site_name, "[^A-Za-z0-9]+", "_"),
      ".png"
    )
    ggsave(file.path("graficos_sitio", fname),
           plot = g, width = 9, height = 5, dpi = 300)
  })

library(stringr)

# asegurar que Sitio sea texto
df_agregado <- df_agregado %>% mutate(Sitio = as.character(Sitio))

# carpeta de salida
dir.create("graficos_sitio", showWarnings = FALSE)

# (opcional) ver cuántos sitios hay 
print(table(df_agregado$Sitio))

# guardar un PNG por sitio con nombres únicos y sin leyenda
sitios <- unique(df_agregado$Sitio)

for (site in sitios) {
  d <- df_agregado %>% filter(Sitio == site)
  g <- plot_site(d, site)  # usa tu función plot_site(d, site_name) definida antes
  fname <- paste0("problematicas_", str_replace_all(site, "[^A-Za-z0-9]+", "_"), ".png")
  ggsave(filename = fname, path = "graficos_sitio",
         plot = g, width = 9, height = 5, dpi = 300)
  message("Guardado: ", file.path("graficos_sitio", fname), " (", nrow(d), " barras)")
}
