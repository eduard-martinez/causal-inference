# =========================================================
# Simulación y gráficos: Manipulación del running variable
# Ejemplo RD con corte de elegibilidad (SISBén - Familias en Acción)
# =========================================================

## setup
rm(list=ls())

## packages
require(pacman)
p_load("ggplot2", "dplyr")

# ----- 1) Simulación base (mismo set de datos para ambos paneles)
set.seed(123)
n      <- 1800
xlim   <- c(-4, 4)
ylim   <- c(-12, 42)
z      <- runif(n, min = xlim[1], max = xlim[2])
D      <- as.integer(z >= 0)
beta0  <- 0
beta1  <- 3.0       # tendencia en z (¡genera sesgo en el contraste ingenuo!)
tau    <- 15
sigma  <- 2
y      <- beta0 + beta1*z + tau*D + rnorm(n, sd = sigma)
df     <- tibble(z, D, y)

# ----- 2) Función para construir un panel con bandwidth h (contraste ingenuo)
make_panel <- function(h, title = NULL) {
  
  # puntos dentro de la ventana
  df2 <- df %>% mutate(in_win = abs(z) <= h)
  
  # medias por lado dentro de la ventana
  mean_L <- df2 %>% filter(z < 0,  z >= -h) %>% summarise(m = mean(y)) %>% pull(m)
  mean_R <- df2 %>% filter(z >= 0, z <=  h) %>% summarise(m = mean(y)) %>% pull(m)
  TE_h   <- mean_R - mean_L
  
  # colores
  col_control <- "grey20"
  col_treated <- "#C2171C"
  
  # gráfico
  p <- ggplot() +
    # puntos fuera de la ventana (tenues)
    geom_point(data = df2 %>% filter(!in_win),
               aes(z, y), color = "grey75", size = 1.1, alpha = 0.35) +
    # puntos dentro de la ventana (coloreados por lado)
    geom_point(data = df2 %>% filter(in_win, D == 0),
               aes(z, y), color = col_control, size = 1.2, alpha = 0.5) +
    geom_point(data = df2 %>% filter(in_win, D == 1),
               aes(z, y), color = col_treated, size = 1.2, alpha = 0.5) +
    # líneas de regresión global por lado (para contexto visual)
    geom_smooth(data = df %>% filter(D == 0),
                aes(z, y), method = "lm", formula = y ~ x,
                se = FALSE, color = col_control, linewidth = 1.1) +
    geom_smooth(data = df %>% filter(D == 1),
                aes(z, y), method = "lm", formula = y ~ x,
                se = FALSE, color = col_treated, linewidth = 1.1) +
    # verticales: -h | 0 | +h
    geom_vline(xintercept = 0,  linewidth = 0.9) +
    geom_vline(xintercept = c(-h, h), linetype = "dashed", linewidth = 0.8) +
    # medias por lado dentro de la ventana (segmentos horizontales)
    geom_segment(aes(x = -h, xend = 0,  y = mean_L, yend = mean_L),
                 color ="blue", linewidth = 1.3) +
    geom_segment(aes(x = 0,  xend = h, y = mean_R, yend = mean_R),
                 color = "blue", linewidth = 1.3) +
    # salto ingenuo en 0 (diferencia de medias)
    geom_segment(aes(x = 0, xend = 0, y = mean_L, yend = mean_R),
                 linetype = "dotdash", linewidth = 0.9) +
    annotate("text", x = 0.15, y = (mean_L + mean_R)/2,
             label = sprintf("\u03C4 = %.1f", TE_h),
             hjust = 0, size = 4.2) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = expression(z[i]), y = "Ingresos (log)", title = title) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 6))
    )
  
  return(p)
}



# ----- 3) Paneles con h pequeño vs h grande
h_small <- 0.6   # vecindad estrecha
h_large <- 2.5   # vecindad amplia

p_left  <- make_panel(h_small, title = "Vecindad pequeña (h chico)")
p_right <- make_panel(h_large, title = "Vecindad grande (h grande)")

# Mostrar lado a lado
p_left + p_right
ggsave("slides/week-05/plots/bw_compare.png", p_left + p_right, width = 12, height=5)

