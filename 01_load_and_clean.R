# scripts/01_load_and_clean.R
# ================================
# Objetivo: cargar la base, renombrar columnas útiles, crear variables (edad), guardar clean csv

library(tidyverse)
library(janitor)

# Paths
raw_path <- "C:/Users/Usuario/OneDrive/Documentos/Facultad/Ciencia de Datos/TP Final/data/raw/base_araucano.csv"
clean_path <- "C:/Users/Usuario/OneDrive/Documentos/Facultad/Ciencia de Datos/TP Final/data/clean/base_araucano_clean.csv"

# Cargar
df <- read_csv(raw_path, show_col_types = FALSE) %>%
  clean_names()

# Inspección rápida
glimpse(df)
summary(df$salario)

# Crear variables útiles
# edad aproximada
if(!"anionac" %in% names(df)) stop("La columna 'anionac' no existe - verificar nombre")
df <- df %>%
  mutate(
    edad = anio - anionac,
    region = factor(region_id),
    genero = factor(genero_id),
    gestion = factor(gestion_id),
    tipo_titulo = factor(tipo_titulo_id),
    rama = factor(rama_id)
  )

# Guardar versión limpia básica (sin filtrar por salario NA)
if(!dir.exists("data/clean")) dir.create("data/clean", recursive = TRUE)
write_csv(df, clean_path)
message("Saved cleaned file to: ", clean_path)

