# =========================================================
# Simulación y gráficos: Manipulación del running variable
# Ejemplo RD con corte de elegibilidad (SISBén - Familias en Acción)
# =========================================================

set.seed(1234)

# ---- Paquetes ----
packs <- c("ggplot2", "dplyr", "patchwork", "scales")
to_install <- packs[!packs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packs, library, character.only = TRUE))

# ---- Parámetros ----
n <- 6000
cutoff <- 11             # Corte de elegibilidad (ejemplo SISBén)
binw   <- 1.0            # ancho de bin en histograma
tau    <- 5              # efecto causal (escala outcome)
beta   <- 0.8            # pendiente base
sigma  <- 8              # ruido outcome

# ---- Función outcome ----
sim_outcome <- function(x, cutoff, tau, beta, sigma) {
  t <- as.numeric(x >= cutoff)
  y <- tau * t + beta * x + rnorm(length(x), 0, sigma)
  y
}

# =========================================================
# 1) SIN MANIPULACIÓN
# =========================================================
# Usamos distribución Beta(2,5) * 100 para simular sesgo a la izquierda
X_clean <- rbeta(n, 2, 5) * 100
Y_clean <- sim_outcome(X_clean, cutoff, tau, beta, sigma)

df_clean <- tibble(X = X_clean, Y = Y_clean)

g_clean_density <- ggplot(df_clean, aes(x = X)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = binw, fill = "#9ecae1", color = "white") +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = cutoff, linetype = "dashed", linewidth = 1) +
  annotate("text", x = cutoff + 2, y = 0.07,
           label = "Corte SISBén\n(Familias en Acción)",
           hjust = 0, vjust = -0.5, size = 3.2) +
  labs(title = "Sin manipulación: distribución del puntaje SISBén",
       x = "Puntaje SISBén",
       y = "Densidad") +
  theme_minimal(base_size = 12)

ggsave("rd_sin_manipulacion.png", g_clean_density, width = 8, height = 5, dpi = 300)

# =========================================================
# 2) CON MANIPULACIÓN (BUNCHING)
# =========================================================
X_base <- rbeta(n, 2, 5) * 100

# Cerca del corte (ventana de 2 puntos), algunos se "mueven" a la derecha
window_w <- 2
shift_amt <- 3
p_shift <- 0.6

near_cut   <- abs(X_base - cutoff) <= window_w
will_shift <- near_cut & (runif(n) < p_shift)
X_shifted  <- X_base
X_shifted[will_shift] <- X_shifted[will_shift] + shift_amt

# Agregar masa extra justo a la derecha del corte
extra_n <- round(0.1 * n)
X_extra <- rnorm(extra_n, mean = cutoff - 2, sd = 1)

X_manip <- c(X_shifted, X_extra)
X_manip <- pmax(X_manip, 0) # truncar en cero
Y_manip <- sim_outcome(X_manip, cutoff, tau, beta, sigma)

df_manip <- tibble(X = X_manip, Y = Y_manip)

g_manip_density <- ggplot(df_manip, aes(x = X)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = binw, fill = "#fdae6b", color = "white") +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = cutoff, linetype = "dashed", linewidth = 1) +
  annotate("text", x = cutoff + 2, y = 0.09,
           label = "Corte SISBén\n(Familias en Acción)",
           hjust = 0, vjust = -0.5, size = 3.2) +
  labs(title = "Con manipulación: bunching a la derecha del corte",
       x = "Puntaje SISBén",
       y = "Densidad") +
  theme_minimal(base_size = 12)

ggsave("rd_con_manipulacion.png", g_manip_density, width = 8, height = 5, dpi = 300)

cat("✅ Listo: se generaron\n- rd_sin_manipulacion.png\n- rd_con_manipulacion.png\n")
