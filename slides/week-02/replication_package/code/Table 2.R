## Eduard Martinez
## Sep 4, 2025
## Replication Paper: Duflo, Hanna & Ryan (AER 2012)

##==: 1. Setup 

## clean environment 
rm(list=ls())

## load packages
require(pacman)
p_load(tidyverse , rio , fixest) 

## load data
data <- import("input/randomcheck_CODED.dta")

## load data
teacher <- import("input/Teacher_test.rds") %>% 
           select(schid,score)

## combine data
data <- left_join(data , teacher , "schid") 

## gen var of median test scores
data <- data %>% 
        group_by() %>%
        mutate(med_score=median(score,na.rm=T),
               above_score=ifelse(score>=med_score,1,0)) %>%
        ungroup()

## filter post
endline <- filter(data,time>1)

##==: 1. Panel Left

## Panel A
left_A <- endline %>%
          group_by(treat) %>%
          summarise(mean = mean(open, na.rm = TRUE),
                    sd = sd(open, na.rm = TRUE),
                    n = n() , .groups = "drop")
left_A

## Panel B
left_B <- filter(endline,above_score==1) %>%
          group_by(treat) %>%
          summarise(mean = mean(open, na.rm = TRUE),
                    sd = sd(open, na.rm = TRUE),
                    n = n() , .groups = "drop")
left_B

## Panel C
left_C <- filter(endline,above_score==0) %>%
          group_by(treat) %>%
          summarise(mean = mean(open, na.rm = TRUE),
                    sd = sd(open, na.rm = TRUE),
                    n = n() , .groups = "drop")
left_C

##==: 2. Panel Right

## Panel A
right_A_4 <- lm(open ~ treat , data=filter(endline,time<9))
summary(right_A_4)

right_A_4 <- feols(open ~ treat, 
                   data = filter(endline, time < 9), 
                   cluster = ~ schid)
summary(right_A_4)

right_A_5 <- feols(open ~ treat, 
                   data = filter(endline,time>8 & time<16), 
                   cluster = ~ schid)
summary(right_A_5)

right_A_6 <- feols(open ~ treat, 
                   data = filter(endline,time>15), 
                   cluster = ~ schid)
summary(right_A_6)

