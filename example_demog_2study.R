library(tidyverse)
library(gt)

### READ IN ARD - Simple 2 study demog example
ARD <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTwUHjU4ZMYQwwCGnO3xlkd0oJbOVdt20YuzWGr2XkQaSyrDcYJyWv-zx5fVMAN38A7k_4js10BlN_t/pub?output=csv")

ARD %>% glimpse()

### FILTER ON DEMOG FOR BOTH STUDIES
DEMOG <- ARD %>%
  filter(PARAMCD == "AGE" & ANALTYP1 == "SUMMARY")

DEMOG %>% glimpse()

## Arrange in to table format
T1 <- 
  DEMOG %>% 
  select(STUDYID, TRTVAL, STAT, STATVAL) %>%
  group_by(STUDYID, TRTVAL, STAT) %>%
  mutate(id = row_number()) %>% 
  gather(groupname, value, -id, -STUDYID, -TRTVAL, -STAT) %>% 
  spread(STAT, value)


## Simple plot
T1 %>% ggplot(aes(x = TRTVAL, y = MEAN)) +
  geom_pointrange(aes(ymin = MEAN - 1.96 * SD, 
                      ymax = MEAN + 1.96 * SD), 
                  alpha = 0.7, color = "#c0392b", size = 0.25) +
  coord_flip() +
  facet_wrap(~STUDYID, ncol=1) + 
  theme_minimal()
