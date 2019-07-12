library(tidyverse)
library(gt)

### READ IN ARD - Simple 2 study demog example
ARD <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTwUHjU4ZMYQwwCGnO3xlkd0oJbOVdt20YuzWGr2XkQaSyrDcYJyWv-zx5fVMAN38A7k_4js10BlN_t/pub?output=csv")

ARD %>% glimpse()

### FILTER ON DEMOG FOR BOTH STUDIES
DEMOG <- ARD %>%
  filter(PARAMCD == "AGE" & ANALTYP1 == "SUMMARY")


DEMOG %>% glimpse()

DEMOG %>% 
  filter(STAT == "MEAN") %>%
  ggplot(aes(x = STUDYID, y = STATVAL)) + 
  geom_point()
