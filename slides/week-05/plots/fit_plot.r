# =========================================================
# Simulación y gráficos: Manipulación del running variable
# Ejemplo RD con corte de elegibilidad (SISBén - Familias en Acción)
# =========================================================

## setup
rm(list=ls())

## packages
require(pacman)
p_load("ggplot2", "dplyr")

# ================================================
# RD estilo B/N: U a la izquierda, ∩ a la derecha
rd_bw_target <- function(n = 400, z0 = 0.5, tau = 0.20, sigma = 0.055,
                         # Curvatura y vértices
                         aL = 6.0,  bL = 3.0,   # izquierda: f_L(s)=c0 + bL*s + aL*s^2   (aL>0 → forma U)
                         aR = 2.5,  bR = 1.5,   # derecha:   f_R(s)=c0 + bR*s - aR*s^2   (aR>0 → forma ∩), vértice en s=bR/(2*aR)
                         c0 = 0.50,             # nivel base continuo en z0 (antes del salto)
                         xlim=c(0,1), ylim=c(0,1.5),
                         seed = 123) {
  
                         ## fijar semilla
                         set.seed(seed)
                        
                        # 1) Running variable y tratamiento
                        z <- runif(n, min = xlim[1], max = xlim[2])
                        D <- as.integer(z >= z0)
                        s <- z - z0  # centrar en el umbral
                        
                        # 2) f(z) verdadera por lado (continua en z0); salto τ a la derecha
                        f_true <- ifelse(s < 0, c0 + bL*s + aL*s^2,
                                         c0 + bR*s - aR*s^2)
                        mu <- f_true + tau*D
                        y  <- mu + rnorm(n, sd = sigma)
                        df <- tibble(z, s, D, y)
                        
                        # 3) Ajustes locales por lado (cuadrático)
                        fitL <- lm(y ~ s + I(s^2), data = filter(df, z < z0))
                        fitR <- lm(y ~ s + I(s^2), data = filter(df, z >= z0))
                        
                        gridL <- tibble(z = seq(xlim[1], z0, length.out = 200), s = z - z0) |>
                                 mutate(yhat = predict(fitL, newdata = cur_data_all()))
                        gridR <- tibble(z = seq(z0, xlim[2], length.out = 200), s = z - z0) |>
                                 mutate(yhat = predict(fitR, newdata = cur_data_all()))
                        
                        y0_L <- as.numeric(predict(fitL, newdata = data.frame(s = 0)))
                        y0_R <- as.numeric(predict(fitR, newdata = data.frame(s = 0)))
                        TE   <- y0_R - y0_L
                        
                        # 4) Gráfico
                        p <- ggplot(df, aes(z, y)) +
                             geom_point(color = "grey40", size = 1.6, alpha = 0.55) +
                             geom_line(data = gridL, aes(z, yhat), linewidth = 1.2, color = "black") +
                             geom_line(data = gridR, aes(z, yhat), linewidth = 1.2, color = "black") +
                             geom_vline(xintercept = z0, linetype = "dotted", linewidth = 0.8) +
                             coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
                             labs(x = expression(z[i]), y = "Ingresos (log)") +
                             annotate("text", x = z0 + 0.02, y = (0 + 1.1)/2,
                                      label = sprintf("\u03C4 = 0.2"),
                                       hjust = 0, size = 4) +
                             theme_classic(base_size = 14) +
                             theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
                                   axis.ticks.length = unit(0.18, "cm"))
p
}

## export data
rd_bw_target(n=1000 , tau=0.2) 
ggsave(filename = "slides/week-05/plots/f_good_fit.png")







# ================================================
# RD B/N con AJUSTE LINEAL por lado (mala especificación)
rd_bw_misspec_linear <- function(n = 400, z0 = 0.5, tau = 0.20, sigma = 0.055,
                                 # Curvatura y vértices de la DGP (verdad)
                                 aL = 6.0,  bL = 3.0,    # izquierda: c0 + bL*s + aL*s^2  (U)
                                 aR = 2.5,  bR = 1.5,    # derecha:   c0 + bR*s - aR*s^2  (∩)
                                 c0 = 0.50,
                                 xlim = c(0, 1), ylim = c(0, 1.5),
                                 seed = 123,
                                 overlay_quadratic = FALSE,   # opcional: mostrar la curva “correcta”
                                 filename = NULL) {
                                 set.seed(seed)
                                
                                # 1) Running variable y tratamiento
                                z <- runif(n, min = xlim[1], max = xlim[2])
                                D <- as.integer(z >= z0)
                                s <- z - z0
                                
                                # 2) f(z) verdadera por lado (continua en z0); salto τ a la derecha
                                f_true <- ifelse(s < 0, c0 + bL*s + aL*s^2,
                                                 c0 + bR*s - aR*s^2)
                                mu <- f_true + tau*D
                                y  <- mu + rnorm(n, sd = sigma)
                                df <- tibble(z, s, D, y)
                                
                                # 3) Ajuste LINEAL por lado (MIS-ESPECIFICACIÓN)
                                fitL_lin <- lm(y ~ s, data = filter(df, z < z0))
                                fitR_lin <- lm(y ~ s, data = filter(df, z >= z0))
                                
                                gridL <- tibble(z = seq(xlim[1], z0, length.out = 200), s = z - z0)
                                gridR <- tibble(z = seq(z0, xlim[2], length.out = 200), s = z - z0)
                                gridL$yhat_lin <- predict(fitL_lin, newdata = gridL)
                                gridR$yhat_lin <- predict(fitR_lin, newdata = gridR)
                                
                                # Predicción en el umbral (s = 0) con el modelo lineal
                                y0_L_lin <- as.numeric(predict(fitL_lin, newdata = data.frame(s = 0)))
                                y0_R_lin <- as.numeric(predict(fitR_lin, newdata = data.frame(s = 0)))
                                TE_lin   <- y0_R_lin - y0_L_lin
                                
                                # (Opcional) Ajuste cuadrático por lado para comparar (línea tenue)
                                if (overlay_quadratic) {
                                  fitL_quad <- lm(y ~ s + I(s^2), data = filter(df, z < z0))
                                  fitR_quad <- lm(y ~ s + I(s^2), data = filter(df, z >= z0))
                                  gridL$yhat_quad <- predict(fitL_quad, newdata = gridL)
                                  gridR$yhat_quad <- predict(fitR_quad, newdata = gridR)
                                }
                                
                          # 4) Gráfico en B/N
                          p <- ggplot(df, aes(z, y)) +
                               geom_point(color = "grey40", size = 1.6, alpha = 0.55) +
                               geom_line(data = gridL, aes(z, yhat_lin), linewidth = 1.2, color = "black") +
                               geom_line(data = gridR, aes(z, yhat_lin), linewidth = 1.2, color = "black") +
                               {if (overlay_quadratic)
                                 geom_line(data = gridL, aes(z, yhat_quad), linewidth = 0.9,
                                           linetype = "longdash", color = "grey20")} +
                               {if (overlay_quadratic)
                                    geom_line(data = gridR, aes(z, yhat_quad), linewidth = 0.9,
                                              linetype = "longdash", color = "grey20")} +
                                geom_vline(xintercept = z0, linetype = "dotted", linewidth = 0.8) +
                                # Salto estimado con el modelo LINEAL
                                geom_segment(aes(x = z0, xend = z0, y = y0_L_lin, yend = y0_R_lin),
                                             linetype = "dashed", linewidth = 0.8) +
                                 annotate("text", x = z0 + 0.02, y = (y0_L_lin + y0_R_lin)/2,
                                          label = paste0("\u03C4 = ",sprintf(" %.2f",TE_lin)),
                                          hjust = 0, size = 4) +
                                coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
                                labs(x = expression(z[i]), y = "Ingresos (log)") +
                                theme_classic(base_size = 14) +
                                theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
                                      axis.ticks.length = unit(0.18, "cm"))
p
}

## export data
rd_bw_misspec_linear(n=1000 , tau=0.2) 
ggsave(filename = "slides/week-05/plots/f_bad_fit.png")




