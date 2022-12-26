# Petra Moser & Vasily Rusanov
# Data lab #7
# Moser, Petra, Alessandra Voena, and Fabian Waldinger. 2014. 
# "German Jewish Émigrés and US Invention." 
# American Economic Review, 104 (10): 3222-55.

library(tidyverse) 
library(huxtable) 
library(haven) # to read .dta files
library(gridExtra)
library(lfe) # to run fixed effect models
rm(list = ls())	

setwd("~/Dropbox/NYU/teach innovation ug/data_labs/8_german_jewish/code_data")
chemistry <- read.csv("patent_classes.csv", stringsAsFactors=FALSE)

#### the usual glancing at the data before working with it
print(unique(chemistry[["gyear"]])) #check what years we have
print(length(unique(chemistry[["main"]]))) #check how many classes there are

# doing more complicated things using dplyr:
chemistry %>% group_by(main) %>% summarise(nyears = n())
chemistry %>% group_by(main) %>% summarise(nyears = n()) %>% 
  select(nyears) %>% distinct() #check that the panel is "balanced" (51 obs/class)

# There is an issue. Treat_us is only 1 after 1933
chemistry %>% filter(treat_us==1) %>% select(gyear) %>% distinct()

# Let's generate treat_us_ay, which is equal to 1 for all years if the class is treated
# Doing this will require a combination of mutate() and group_by()
chemistry <- chemistry %>% group_by(main) %>%
  # "look up the value for treat_us in gyear == 1970 and assign it to a new var treat_us_ay"
  mutate(treat_us_ay = treat_us[gyear ==1970]) 

chemistry <- chemistry %>% mutate(post_1933 = as.integer(gyear >=1933))

#### IV regression
# to run an IV regression, use the felm() command
# and put your IV in the part after the second |
reg_iv <- felm(count_us ~ treat_us_ay + post_1933 | 
                 0 | # no fixed effects
                 # this says "`treat_us` is instrumented with `treat_dis_pre33`
                 (treat_us ~ treat_dis_pre33), 
               data = chemistry)

summary(reg_iv)

# same with fixed effects
reg_iv_fe <- felm(count_us ~ 1 | # 1 means "no covariates, just a constant
                 gyear + main |
                 (treat_us ~ treat_dis_pre33), 
               data = chemistry)

summary(reg_iv)

# to get the first stage, extract the stage1 object
names(reg_iv)  #print the list of object names contained in reg_iv
first_stage <- reg_iv$stage1
summary(first_stage)
  