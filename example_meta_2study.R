library(tidyverse)
library(gt)

### READ IN ARD - Simple 2 study demog example
ARD <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTwUHjU4ZMYQwwCGnO3xlkd0oJbOVdt20YuzWGr2XkQaSyrDcYJyWv-zx5fVMAN38A7k_4js10BlN_t/pub?output=csv")

ARD %>% glimpse()



### FILTER ON All cause mortality FOR BOTH STUDIES - summary statistics
ACMsum <- ARD %>%
  filter(PARAMCD == "ACM" & ANALTYP1 == "SUMMARY")

ACMsum %>% glimpse()

## Arrange in to table format
T1 <- 
  ACMsum %>% 
  select(STUDYID, TRTVAL, STAT, STATVAL) %>%
  group_by(STUDYID, TRTVAL, STAT) %>%
  mutate(id = row_number()) %>% 
  gather(groupname, value, -id, -STUDYID, -TRTVAL, -STAT) %>% 
  spread(STAT, value)

T1

### FILTER ON All cause mortality FOR BOTH STUDIES - inferntial statistics
ACM <- ARD %>%
  filter(PARAMCD == "ACM" & ANALTYP1 == "COMPARISON")

ACM %>% glimpse()

## Arrange in to table format
T2 <- 
  ACM %>% 
  select(STUDYID, TRTVAL, STAT, STATVAL) %>%
  group_by(STUDYID, TRTVAL, STAT) %>%
  mutate(id = row_number()) %>% 
  gather(groupname, value, -id, -STUDYID, -TRTVAL, -STAT) %>% 
  spread(STAT, value)


## Simple plot
T2 %>% ggplot(aes(x = TRTVAL, y = HR)) +
  geom_pointrange(aes(ymin = LOWER95, 
                      ymax = UPPER95), 
                  alpha = 0.7, color = "#c0392b", size = 0.25) +
  coord_flip() +
  facet_wrap(~STUDYID, ncol=1) + 
  theme_minimal()



