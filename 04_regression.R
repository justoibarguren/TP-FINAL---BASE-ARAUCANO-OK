# scripts/04_regression.R
# ================================
# Objetivo: estimar regresiones del salario sobre variables relevantes (region principal)

library(tidyverse)
library(broom)

processed_path <- "data/processed/base_araucano_processed.csv"
df <- read_csv(processed_path, show_col_types = FALSE)

# Variables candidate: region, edad, genero, gestion, tipo_titulo, rama
# Model 1: salario sobre region (base)
mod1 <- lm(salario ~ region, data = df)
mod1_sum <- summary(mod1)
write_lines(capture.output(mod1_sum), "output/tables/regression_mod1.txt")

# Model 2: add controls
mod2 <- lm(salario ~ region + edad + genero + gestion + tipo_titulo + rama, data = df)
mod2_sum <- summary(mod2)
write_lines(capture.output(mod2_sum), "output/tables/regression_mod2.txt")

# Log-salary model (robusto ante skewness)
df <- df %>% mutate(log_sal = log(salario))
mod3 <- lm(log_sal ~ region + edad + genero + gestion + tipo_titulo + rama, data = df)
write_lines(capture.output(summary(mod3)), "output/tables/regression_mod3.txt")

# VIF para multicolinealidad
library(car)
vif_mod2 <- car::vif(mod2)
write_lines(capture.output(vif_mod2), "output/tables/vif_mod2.txt")

# Guardar objetos
saveRDS(mod1, "output/tables/mod1.rds")
saveRDS(mod2, "output/tables/mod2.rds")
saveRDS(mod3, "output/tables/mod3.rds")
