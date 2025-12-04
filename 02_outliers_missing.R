# scripts/02_outliers_missing.R
# ================================
# Objetivo: analizar missing/outliers en salario y decidir tratamiento

install.packages("naniar")

library(tidyverse)
library(naniar)

clean_path <- "C:/Users/Usuario/OneDrive/Documentos/Facultad/Ciencia de Datos/TP Final/data/clean/base_araucano_clean.csv"
processed_path <- "C:/Users/Usuario/OneDrive/Documentos/Facultad/Ciencia de Datos/TP Final/data/processed/base_araucano_processed.csv"

if(!file.exists(clean_path)) stop("Ejecutar 01_load_and_clean.R antes")
df <- read_csv(clean_path, show_col_types = FALSE)

# Missing per column
miss_summary <- df %>% summarise_all(~mean(is.na(.))) %>% pivot_longer(everything(), names_to = "var", values_to = "prop_na")
if(!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)
write_csv(miss_summary, "output/tables/missing_summary.csv")

# Diagnóstico salario
sal <- df$salario
summary(sal)
quantile(sal, probs = c(0.01,0.05,0.10,0.25,0.5,0.75,0.90,0.95,0.99), na.rm = TRUE)

# Decisiones propuestas (documentar en informe):
# 1) Mantener sólo observaciones con salario no NA
# 2) Acotar salario entre percentil 1 y 99 para reducir el efecto de outliers extremos

df_proc <- df %>%
  filter(!is.na(salario)) %>%
  filter(salario >= quantile(salario, 0.01, na.rm = TRUE) & salario <= quantile(salario, 0.99, na.rm = TRUE))

# Guardar
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
write_csv(df_proc, processed_path)
write_csv(df_proc %>% group_by(region) %>% summarise(n = n(), mean_sal = mean(salario), med_sal = median(salario), sd_sal = sd(salario)), "output/tables/salary_by_region_summary.csv")
message("Processed data saved to: ", processed_path)




