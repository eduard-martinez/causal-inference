## =========================================================
## Sintética RD (ranking municipal con cutoff en el puesto 10)
## =========================================================
rm(list=ls())
set.seed(123)
require(dplyr)

##====================================================
## Parámetros

## Base
n_mun   <- 203
cupo    <- 10                      # top-10 elegibles por municipio
minN    <- 18; maxN <- 25          # personas por municipio
z0      <- 10                      # corte en el puesto 10 (running = orden - 10)

## Tamaños por municipio
n_i <- sample(minN:maxN, n_mun, replace = TRUE)
cod_mpio <- 1:n_mun

## Efectos aleatorios por municipio (heterogeneidad)
u_mun <- rnorm(n_mun, mean = 0, sd = 100)  # ~USD

##====================================================
## Generar registros por municipio

## Dataframe
df <- lapply(seq_len(n_mun), function(m){
      nm <- n_i[m]
      tibble(cod_mpio = m,
             id_persona    = sprintf("M%03d-%03d", m, seq_len(nm)),
             calificacion  = round(runif(nm, 65, 90), 2))}) %>% bind_rows()

## Ranking dentro de municipio (1 = mejor puntaje)
df <- df %>%
      group_by(cod_mpio) %>%
      arrange(desc(calificacion), .by_group = TRUE) %>%
      mutate(orden = row_number(),
             running = orden - z0,          
             elegible = as.integer(calificacion>=75)) %>%
      ungroup()

##====================================================
## Tratamiento (80% entre elegibles; 0% entre no elegibles)

## tratados
df <- mutate(df , tratado=ifelse(elegible==1 , rbinom(n(), 1, 0.80), 0L))

##====================================================
## Covariables predeterminadas
set.seed(456)
df <- df %>%
      mutate(edad = pmin(75, pmax(18, round(rnorm(n(), 44, 12)))),
             mujer = rbinom(n(), 1, 0.46),
             lee = rbinom(n(), 1, 0.35),
             ingreso_usd_t0 = runif(n(),1000,1500),
             esc_annos_t0 = pmin(16, pmax(0, round(rnorm(n(), 7, 3)))) ,
             tam_parcela_ha_t0 = round(rlnorm(n(), meanlog = log(3), sdlog = 0.7),2) , 
             acceso_riego_t0 = rbinom(n(), 1, 0.35),
             acceso_credito_t0 = rbinom(n(), 1, 0.30),
             monto_credito_12m_t0 = ifelse(acceso_credito_t0==1, round(rlnorm(n(), log(1200), 0.8)), 0))
          
##====================================================
## Diversificación de cultivos 

## parametros
tau_div <- 1.0

## Variable
f_div <- with(df, ifelse(
              running <= 0,    
              0.10*(-running) + 0.04*(running^2),   
              0.06*( running) + 0.03*(running^2)))
lambda_div <- 1.8 + f_div + tau_div*df$elegible   
df$diver_cultivos <- pmin(6, pmax(1, rpois(nrow(df), pmax(0.2, lambda_div)) + 1))

##====================================================
## Ingreso anual USD: salto en el corte 

## parametros
base_ing     <- 1400
tau_elegible <- 100     # ITT: efecto de elegibilidad (Z*)
tau_tratado  <- 300     # efecto total si RECEBE el subsidio
sigma        <- 80

# Ruido fuera de mutate
set.seed(123)
ruido <- rnorm(nrow(df), 0, sigma)

# Coefs de la forma funcional estilo rd_bw_target:
# f(s) = c0 + bL*s + aL*s^2  (s<0)   y   c0 + bR*s - aR*s^2  (s>=0)
c0 <- 0         # usamos base_ing como nivel; deja c0=0 aquí
aL <- 6.0; bL <- 3.0
aR <- 2.5; bR <- 1.5

# Escala para que s sea ~[-1,1] (ajústalo según tu rango de running)
s_scale <- 8
df <- df %>%
      mutate(s = running / s_scale,
            f_piece = if_else(s < 0, c0 + bL*s + aL*s^2 , c0 + bR*s - aR*s^2),
            efecto_mpio = u_mun[cod_mpio],
            ingreso_usd = base_ing + f_piece +
                          tau_elegible * elegible +
                          (tau_tratado - tau_elegible) * tratado +
                          efecto_mpio + ruido,
            ingreso_usd = pmin(3000, pmax(1000, round(ingreso_usd))))

# --- Reordenar y dejar nombres claros
df <- df %>%
  select(cod_mpio, id_persona, 
         calificacion, orden, running, elegible, tratado,
         edad, mujer, lee, 
         esc_annos_t0 , ingreso_usd_t0 , acceso_credito_t0 , tam_parcela_ha_t0 , 
         ingreso_usd , diver_cultivos)

## clean 
rm(list = setdiff(ls(envir = .GlobalEnv), "df"), envir = .GlobalEnv)
cat("\f")


