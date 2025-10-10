#==============================================================================#
# Author: Eduard-Martinez
# update: 15-11-2023
# R version 4.1.1 (2021-08-10)
#==============================================================================#

## initial configuration
rm(list=ls())
source("00_program/packages.R")

## function
f_reg <- function(name_file,label,year_ini,year_end){
  
        ##=== Extensive Margin ===## 
        m1_nb <- df %>% subset(sample_control_2==1) %>% 
        subset(year %in% year_ini:year_end) %>% 
        rename(treat=treat_staggered) %>% 
        felm(formula=modelo_1) 
        
        m2_nb <- df %>% subset(sample_control_2==1) %>% 
        subset(year %in% year_ini:year_end) %>% 
        rename(treat=treat_staggered) %>% 
        felm(formula=modelo_2) 
        
        ##=== Intensive Margin ===## 
        m1_fxs <- df %>% 
        mutate(treat_staggered=ifelse(treat_staggered==1,n_ha_in_grid,0)) %>%
        subset(sample_control_2==1) %>% 
        subset(year %in% year_ini:year_end) %>% 
        rename(treat=treat_staggered) %>% 
        felm(formula=modelo_1)
        
        m2_fxs <- df %>% 
        mutate(treat_staggered=ifelse(treat_staggered==1,n_ha_in_grid,0)) %>%
        subset(sample_control_2==1) %>% 
        subset(year %in% year_ini:year_end) %>% 
        rename(treat=treat_staggered) %>% 
        felm(formula=modelo_2)
        
        ## mean and N-treatment N-control
        n_control <- df %>% subset(sample_control_2==1 & d_control_2==1) %>%
        select(id_grid) %>% distinct_all() %>% nrow()
        
        n_treated <- df %>% subset(sample_control_2==1 & d_treated_2==1) %>%
        select(id_grid) %>% distinct_all() %>% nrow()
        
        mean_dep_var <- df %>% subset(year %in% year_ini:year_end) %>%
        subset(sample_control_2==1 & d_control_2==1 & treat_staggered==0) %>%
        select(crops) %>% unlist() %>% mean(na.rm=T) %>% round(4)
  
        ##=== export results English ===## 
        labels_dependientes = c('\\text{FxS}')
        stargazer(m1_nb , m2_nb , m1_fxs , m2_fxs ,
                  header=FALSE, type= 'text',
                  dep.var.labels.include = F,
                  label = label,
                  keep = c('treat'),
                  column.separate = c(2,2),
                  column.labels = c('Extensive Margin','Intensive Margin'),
                  covariate.labels = labels_dependientes, 
                  table.placement = 'H', 
                  df = FALSE,
                  digits = 3, 
                  omit.stat = c('f', 'ser', 'rsq', 'adj.rsq'),
                  add.lines = list(c('Mean Dep. Var.',rep(mean_dep_var,4)), 
                                   c('N. Treated', rep(n_treated,4)),
                                   c('N. Control', rep(n_control,4)),
                                   c('T.E. (Prov-Year)', 'No', 'Yes', 'No', 'Yes'),
                                   c('Period', rep(paste0(year_ini,"-",year_end),4))
                  ),
                  notes = '',
                  notes.align = "l", notes.append = F,
                  out = paste0("../../Aplicaciones/Overleaf/FxS_Crops/tables/main-regressions/staggered_",name_file,year_ini,"_",year_end,".tex"))
}

## Models
modelo_1 <- as.formula("crops ~ treat | id_grid + year | 0 | id_grid") 
modelo_2 <- as.formula("crops ~ treat + as.factor(provincia):as.factor(year) | id_grid + year | 0 | id_grid")

## load data
df <- import("03_make_panel_data/output/panel_grids_regressions.rds")

## Regression
f_reg(label="reg:staggered_crops" , name_file="crops_" , year_ini=2011 , year_end=2021)

## tabla en espanol
table_eng <- read_csv(file="../../Aplicaciones/Overleaf/FxS_Crops/tables/main-regressions/staggered_crops_2011_2021.tex" , col_names=F) %>% 
             unlist() %>% as.character()
table_eng <- c("\\begin{table}[H]","\\centering \\footnotesize \\captionsetup{width=0.8\\textwidth}",table_eng)
table_eng[3] <- "\\renewcommand{\\arraystretch}{0.7} \\setlength{\\tabcolsep}{6pt}"
table_eng[4] <- "\\caption{Effect of Land Formalization on Coca Crop Areas:}" 
table_eng[5] <- "\\label{reg:staggered_crops} \\vspace{-5mm}"
table_eng[9] <- " & \\multicolumn{4}{c}{\\textbf{Share of the grid covered with coca crops:}} \\\\" 
table_eng[21] <- "T.E. (Prov-Year) &  & \\checkmark &   & \\checkmark \\\\"
table_eng[24] <- "\\bottomrule \\bottomrule"
table_eng[25] <- "\\end{tabular}"  
table_eng[26] <- "\\begin{minipage}{0.65\\textwidth}"
table_eng[27] <- "\\begin{tablenotes}[scriptsize,flushleft]"   
table_eng[28] <- "\\item Notes: *** p<0.01; ** p<0.05 ; * p<0.1. The dependent variable is expressed as the share of the grid covered with coca crops. Columns 1 and 2 present the estimation of equation \\ref{eq:twfe}, where \\text{FxS} is a dichotomous variable equal to 1 if the grid contains at least one hectare of landholdings with a delivered land title. Columns 3 and 4 present the estimation of equation \\ref{eq:twfe}, where \\text{FxS} represents the share of titled area within the grid. All regressions include year and grid fixed effects. Standard errors are clustered at the grid level."
table_eng[29] <- "\\end{tablenotes}"
table_eng[30] <- "\\end{minipage}"
table_eng[31] <- "\\end{table}"
writeLines(table_eng,"../../Aplicaciones/Overleaf/FxS_Crops/tables/main-regressions/staggered_crops_2011_2021.tex") 



  
  
