#asymmetry codes
# Preambles----
library(tidyverse)
library(synthdid)
library(did)
# library(DIDmultiplegt)
# library(DIDmultiplegtDYN)
library(Synth)
# library(TwoWayFEWeights)
library(staggered)
library(fixest)
library(texreg)

data <- read_csv('data/subway_1fin.csv')

data <- data %>% mutate(diff = side1-side2,
                        normalized_diff = diff/(side1+side2))
temp <- data %>% mutate(sum = side1+side2)
write_csv(temp, 'data/subway_1fin_update.csv')

r2 <- feols(diff ~ level|location, vcov = 'iid', data =temp)
r3 <- feols(diff ~level, vcov = 'iid', data = temp)

screenreg(list(r2, r3),
       # custom.coef.names =  c('ATT'),
       # custom.header = list('Two-Way Fixed Effects' = 1:3),
       stars = c(0.01, 0.05, 0.1), digits = 3, 
       custom.model.names = c('With Fixed Effects', 'Without Fixed Effects'),
       include.proj = F,
       caption = 'Regression on Asymmetry Levels'
)

texreg(list(r2, r3), 'doc/t3.tex', 
       # custom.coef.names =  c('ATT'),
       # custom.header = list('Two-Way Fixed Effects' = 1:3),
       stars = c(0.01, 0.05, 0.1), digits = 3, 
       custom.model.names = c('With Fixed Effects', 'Without Fixed Effects'),
       include.proj = F,
       caption = 'Regression on Asymmetry Levels'
)

