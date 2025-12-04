# scripts/05_figures.R
# ================================
# Objetivo: crear los 2 gráficos editorializados + gráficos auxiliares

library(tidyverse)

processed_path <- "data/processed/base_araucano_processed.csv"
df <- read_csv(processed_path, show_col_types = FALSE)

if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# Figure 1: Boxplot salario por region (editorializado)
p1 <- ggplot(df, aes(x = region, y = salario)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  labs(title = "Distribución de salarios por región",
       subtitle = "Boxplot (sin outliers extremos: p1-p99)",
       x = "Región (id)", y = "Salario") +
  theme_minimal(base_size = 12)

ggsave("output/figures/boxplot_salary_by_region.png", p1, width = 10, height = 7)

# Figure 2: Medias por región con barras de error (editorializado)
means <- df %>% group_by(region) %>% summarise(mean = mean(salario), se = sd(salario)/sqrt(n()), n = n())

p2 <- ggplot(means, aes(x = reorder(region, mean), y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.2) +
  coord_flip() +
  labs(title = "Salario promedio por región",
       subtitle = "Con intervalos de confianza al 95%",
       x = "Región (ordenada por media)", y = "Salario promedio") +
  theme_minimal(base_size = 12)

ggsave("output/figures/mean_salary_by_region.png", p2, width = 10, height = 7)

# Gráficos auxiliares
p3 <- ggplot(df, aes(x = salario)) + geom_histogram(bins = 50) + labs(title = "Histograma de salarios")
ggsave("output/figures/hist_salary.png", p3, width = 8, height = 5)

# Residual plot del modelo con controles
library(broom)
mod2 <- readRDS("output/tables/mod2.rds")
res <- augment(mod2)
pres <- ggplot(res, aes(.fitted, .resid)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 0) + labs(title = "Residuals vs Fitted (mod2)", x = "Fitted", y = "Residuals")
ggsave("output/figures/residuals_mod2.png", pres, width = 8, height = 5)
