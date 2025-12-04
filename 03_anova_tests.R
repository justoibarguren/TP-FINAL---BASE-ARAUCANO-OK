# scripts/03_anova_tests.R
# ================================
# Objetivo: realizar ANOVA para comparar medias de salario entre regiones y tests post-hoc

library(tidyverse)
library(broom)

processed_path <- "C:/Users/Usuario/OneDrive/Documentos/Facultad/Ciencia de Datos/TP Final/data/processed/base_araucano_processed.csv"
if(!file.exists(processed_path)) stop("Ejecutar 02_outliers_missing.R antes")
df <- read_csv(processed_path, show_col_types = FALSE)

# ANOVA simple: salario ~ region
df <- df %>% mutate(region = factor(region))

aov_mod <- aov(salario ~ region, data = df)
summary(aov_mod)
write_lines(capture.output(summary(aov_mod)), "output/tables/anova_region_summary.txt")

# Post-hoc Tukey
tuk <- TukeyHSD(aov_mod)
write_lines(capture.output(tuk), "output/tables/tukey_region.txt")

# Test de supuestos: homogeneidad de varianzas (Levene)
library(car)
levene_test <- car::leveneTest(salario ~ region, data = df)
write_lines(capture.output(levene_test), "output/tables/levene_region.txt")

# Si Levene rechaza, usar Kruskal-Wallis como alternativa no paramétrica
kw <- kruskal.test(salario ~ region, data = df)
write_lines(capture.output(kw), "output/tables/kruskal_region.txt")

# Guardar objeto
saveRDS(aov_mod, "output/tables/aov_mod_region.rds")






