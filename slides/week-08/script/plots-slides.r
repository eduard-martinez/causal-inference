# =====================================================
# Simulación de soporte común en PSM
# Autor: Eduard F. Martínez González
# Fecha: Octubre 2025
# =====================================================

# Librerías
library(ggplot2)
library(dplyr)

set.seed(123)

# ------------------------------
# 1. Simulación: soporte insuficiente
# ------------------------------
n <- 1000
treated_insuf <- data.frame(
  ps = rbeta(n, 6, 2),   # distribución desplazada a la derecha
  group = "Tratados"
)

control_insuf <- data.frame(
  ps = rbeta(n, 2, 6),   # distribución desplazada a la izquierda
  group = "Control"
)

df_insuf <- bind_rows(treated_insuf, control_insuf)

p_insuf <- ggplot(df_insuf, aes(x = ps, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#1F78B4", "#E31A1C")) +
  labs(title = "",
       x = "Propensity Score",
       y = "Densidad") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("plot/support_common_insufficient.png", p_insuf, width = 6, height = 4, dpi = 300)


# ------------------------------
# 2. Simulación: soporte suficiente
# ------------------------------
treated_suf <- data.frame(
  ps = rbeta(n, 5, 3),
  group = "Tratados"
)

control_suf <- data.frame(
  ps = rbeta(n, 3, 5),
  group = "Control"
)

df_suf <- bind_rows(treated_suf, control_suf)

p_suf <- ggplot(df_suf, aes(x = ps, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#1F78B4", "#E31A1C")) +
  labs(title = "",
       x = "Propensity Score",
       y = "Densidad") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("plot/support_common_sufficient.png", p_suf, width = 6, height = 4, dpi = 300)

# ------------------------------
# Mostrar los gráficos en pantalla
# ------------------------------
p_insuf
p_suf


