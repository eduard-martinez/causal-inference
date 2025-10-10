##============================
# Author: Eduard-Martinez     
# R version 4.1.1 (2021-08-10)

## initial configuration
rm(list=ls())
require(pacman)
p_load(tidyverse , rio , lfe)

## load data
df <- import("https://raw.githubusercontent.com/eduard-martinez/causal-inference/main/slides/week-07/data/panel_grids_regressions.rds")

## subset data
data <- df %>% 
        subset(sample_control_1==1) %>% 
        subset(year %in% 2011:2021)

## Models
modelo_1 <- as.formula("crops ~ treat | id_grid + year | 0 | id_grid") 
modelo_2 <- as.formula("crops ~ treat + as.factor(provincia):as.factor(year) | id_grid + year | 0 | id_grid")

##=== Extensive Margin ===## 

## Sin tendencias
m1_nb <- rename(data , treat=treat_staggered) %>% 
         felm(formula=modelo_1) 
summary(m1_nb)

## con tendencias
m2_nb <- rename(data, treat=treat_staggered) 
         felm(formula=modelo_2) 
m2_nb$coefficients[1]








##=== Intensive Margin ===## 
m1_fxs <- df %>% 
         mutate(treat_staggered=ifelse(treat_staggered==1,n_ha_in_grid,0)) %>%
         subset(sample_control_1==1) %>% 
         subset(year %in% 2011:2021) %>% 
         rename(treat=treat_staggered) %>% 
         felm(formula=modelo_1)

m2_fxs <- df %>% 
         mutate(treat_staggered=ifelse(treat_staggered==1,n_ha_in_grid,0)) %>%
         subset(sample_control_1==1) %>% 
         subset(year %in% 2011:2021) %>% 
         rename(treat=treat_staggered) %>% 
         felm(formula=modelo_2)

        





writeLines(table_eng,"../../Aplicaciones/Overleaf/FxS_Crops/tables/main-regressions/main-reg_crops_2011_2021.tex") 

