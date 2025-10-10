
##==: initial setup
rm(list=ls())
source("00_setup/00_packages.R")
source("00_setup/01_function_reg.R")

##==: load data
panel <- import(file="05_join_db/output/panel_mnz.rds" , setclass="data.table") %>%
         subset(time %in% -6:6 & dist_rest>=-1 & dist_rest<=1) %>%
         mutate(treatment=case_when(dist_rest<=0~"Motorcycle Ban" , dist_rest>0~"Control"))
table(panel$type_rest , panel$treatment)
table(panel$type_rest , panel$treated)

##==: subset data

##==: compute means of dependent variables
mean_baq <- baq %>% 
            subset(treated==0 & post==0) %>% 
            summarise(total = round(mean(total_crime),3) ,
                      hurto_per = round(mean(hurto_per),3) ,
                      arma = round(mean(arma_crime),3))

mean_bog <- bog %>% 
            subset(treated==0 & post==0) %>% 
            summarise(total = round(mean(total_crime),3) ,
                      hurto_per = round(mean(hurto_per),3) ,
                      arma = round(mean(arma_crime),3))

mean_sol <- sol %>% 
            subset(treated==0 & post==0) %>% 
            summarise(total = round(mean(total_crime),3) ,
                      hurto_per = round(mean(hurto_per),3) ,
                      arma = round(mean(arma_crime),3))

mean_nev <- nev %>% 
            subset(treated==0 & post==0) %>% 
            summarise(total = round(mean(total_crime),3) ,
                      hurto_per = round(mean(hurto_per),3) ,
                      arma = round(mean(arma_crime),3))

mean_baq ; mean_bog ; mean_nev ; mean_sol

## n grids
n_baq <- length(unique(baq$unique_id)) %>% format(big.mark = ",")
n_bog <- length(unique(bog$unique_id)) %>% format(big.mark = ",")
n_nev <- length(unique(nev$unique_id)) %>% format(big.mark = ",")
n_sol <- length(unique(sol$unique_id)) %>% format(big.mark = ",")

##==: main regressions: 

## total crimes
total_baq <- feols(data=baq , total_crime ~ post*treated | unique_id + time) 
total_bog <- feols(data=bog , total_crime ~ post*treated | unique_id + time) 
total_nev <- feols(data=nev , total_crime ~ post*treated | unique_id + time) 
total_sol <- feols(data=sol , total_crime ~ post*treated | unique_id + time) 

## hurto personas
hp_baq <- feols(data=baq , hurto_per ~ post*treated | unique_id + time) 
hp_bog <- feols(data=bog , hurto_per ~ post*treated | unique_id + time) 
hp_nev <- feols(data=nev , hurto_per ~ post*treated | unique_id + time) 
hp_sol <- feols(data=sol , hurto_per ~ post*treated | unique_id + time) 

## armas in crime
ar_baq <- feols(data=baq , arma_crime ~ post*treated | unique_id + time) 
ar_bog <- feols(data=bog , arma_crime ~ post*treated | unique_id + time) 
ar_nev <- feols(data=nev , arma_crime ~ post*treated | unique_id + time) 
ar_sol <- feols(data=sol , arma_crime ~ post*treated | unique_id + time) 

##==: make tables

## modelo nulo
nulo <- feols(data=panel , total_crime ~ 1) 

## homicidios
total <- etable(nulo,total_baq,nulo,total_bog,nulo,total_nev,nulo,total_sol,
                digits=3,
                drop="Constant" , fitstat=~n , se.below=T ,
                tex=T , replace=T , view=F) %>% as.character()

## hurto personas
hurto_per <- etable(nulo,hp_baq,nulo,hp_bog,nulo,hp_nev,nulo,hp_sol,
                    digits=3,
                    drop="Constant" , fitstat=~n , se.below=T ,
                    tex=T , replace=T , view=F) %>% as.character()

## armas
arma_per <- etable(nulo,ar_baq,nulo,ar_bog,nulo,ar_nev,nulo,ar_sol,
                   digits=3,
                   drop="Constant" , fitstat=~n , se.below=T ,
                   tex=T , replace=T , view=F) %>% as.character()

total ; hurto_per ; arma_per

##==: Prepare latex

## clean 
cat("\f")

## setup
tbl <- "\\centering \\footnotesize \\setlength{\\tabcolsep}{4pt} \\renewcommand{\\arraystretch}{0.8}"
tbl[2] <- "\\begin{tabular}{lcccccccc}"
tbl[3] <- "\\toprule \\toprule"
tbl[4] <- "& & \\multicolumn{3}{c}{\\textit{\\textbf{Male Passenger}}} & & 
               \\multicolumn{1}{c}{\\textit{\\textbf{Passenger}}} & & 
               \\multicolumn{1}{c}{\\textit{\\textbf{Full Motorcycle}}} \\\\
               \\cmidrule(lr){3-5} \\cmidrule(lr){7-7} \\cmidrule(lr){9-9}
           & & (1) &  & (2) &  & (3) &  & (4) \\\\"
tbl[5] <- "\\midrule"

## panel A
tbl <- append(x=tbl , "\\multicolumn{9}{l}{\\textbf{Panel A: Number of total crimes}} \\\\")
tbl <- append(x=tbl , values=c(total[9:10],"& & & & & & & & \\\\"))
tbl <- append(x=tbl , values=paste0("\\textit{Control Group Mean (Dep. Var.)} & &",mean_baq$homi,"& &",mean_bog$homi,"& &",mean_nev$homi,"& &",mean_sol$homi,"\\\\ \\\\"))

## panel B
tbl <- append(x=tbl , "\\multicolumn{9}{l}{\\textbf{Panel B: Number of personal theft}} \\\\")
tbl <- append(x=tbl , values=c(hurto_per[9:10],"& & & & & & & & \\\\")) 
tbl <- append(x=tbl , values=paste0("\\textit{Control Group Mean (Dep. Var.)} & &",mean_baq$hurto_per,"& &",mean_bog$hurto_per,"& &",mean_nev$hurto_per,"& &",mean_sol$hurto_per,"\\\\ \\\\"))

## panel C
tbl <- append(x=tbl , "\\multicolumn{9}{l}{\\textbf{Panel C: Number of armed assaults and robberies}} \\\\")
tbl <- append(x=tbl , values=c(arma_per[9:10],"& & & & & & & & \\\\")) 
tbl <- append(x=tbl , values=paste0("\\textit{Control Group Mean (Dep. Var.)} & &",mean_baq$arma,"& &",mean_bog$arma,"& &",mean_nev$arma,"& &",mean_sol$arma,"\\\\"))

## replace names
tbl <- str_replace_all(tbl,"post \\$\\\\times\\$ treated","Motorcycle Ban x Post")

## add complement
tbl <- append(x=tbl , values=c("\\midrule"))
tbl <- append(x=tbl , values="\\textit{Block F.E.} & & \\checkmark & & \\checkmark &  & \\checkmark & & \\checkmark \\\\") 
tbl <- append(x=tbl , values="\\textit{Month F.E.} & & \\checkmark & & \\checkmark &  & \\checkmark & & \\checkmark \\\\") 
tbl <- append(x=tbl , values=paste0("\\textit{N. Blocks} & &",n_baq,"& &",n_bog,"& &",n_nev,"& &",n_sol,"\\\\"))
tbl <- str_replace_all(string=tbl,"Observations","\\\\textit{Observations}")
tbl <- append(x=tbl , values=total[17]) %>% str_remove_all("148,164")

## replace complement
tbl <- append(x=tbl , values="\\bottomrule \\bottomrule")
tbl <- append(x=tbl , values="\\end{tabular}")
tbl %>% str_remove_all("  ")

## export
writeLines(tbl , "10_overleaf/tables/reg/dd_type_crime.tex")
