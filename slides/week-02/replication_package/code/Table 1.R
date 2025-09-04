## Eduard Martinez
## Sep 4, 2025
## Replication Paper: Duflo, Hanna & Ryan (AER 2012)

##==: 1. Setup 

## clean environment 
rm(list=ls())

## load packages
require(pacman)
p_load(tidyverse , rio , broom) 

## load data
data <- import("input/randomcheck.rds")

## skim 
glimpse(data)

## subset 
bl <- filter(data , time==1)

##==: 2. Panel A

## means
mean_A <- bl %>%
          group_by(treat) %>%
          summarise(mean = mean(open, na.rm = TRUE),
                    sd = sd(open, na.rm = TRUE),
                    n = sum(!is.na(open)),
                    .groups = "drop")
mean_A

## reg
reg_A <- lm(open ~ treat , data=bl) %>% tidy() 
reg_A

##==: 3. Panel B

## means
mean_B <- filter(bl , open==1) %>%
          group_by(treat) %>%
          summarise(mean = mean(students, na.rm = TRUE),
                    sd = sd(students, na.rm = TRUE),
                    n = sum(!is.na(students)),
                    .groups = "drop")
mean_B

## reg
reg_B <- lm(students ~ treat , data=filter(bl , open==1)) %>% tidy() 
reg_B

##==: 4. Panel C

## load data
teacher <- import("input/Teacher_test.rds") %>% select(schid,score)

## get treated
treated <- select(data,schid,RC,treat) %>% distinct()

## combine data
teacher <- left_join(teacher,treated,"schid") %>% filter(RC==1)

## means
mean_C <- filter(teacher) %>%
          group_by(treat) %>%
          summarise(mean = mean(score, na.rm = TRUE),
                    sd = sd(score, na.rm = TRUE),
                    n = sum(!is.na(score)),
                    .groups = "drop")
mean_C

## reg
reg_C <- lm(score ~ treat , data=teacher) %>% tidy() 
reg_C

##==: 5. Figure 1

## summary data
fig_1 <- data %>%
         group_by(treat, time) %>%
         summarise(p_open = mean(open, na.rm = TRUE),
                   n_obs  = n(),
                   .groups = "drop")

## plot
ggplot(fig_1, aes(x=time , y=p_open , color=factor(treat))) +
geom_line() + geom_point() +
geom_vline(xintercept=c(9,14)) +
labs(x="Mes (Aug-2003 = 1)" , y="% escuelas abiertas" , color="treat") +
scale_y_continuous(labels = scales::percent_format()) +
theme_minimal()

