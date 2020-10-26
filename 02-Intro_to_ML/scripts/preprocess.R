# This script contains code for preprocessing of the raw data
# From Xu. et al, 2020 (https://doi.org/10.1038/s41597-020-0448-0):
# Raw data: https://github.com/beoutbreakprepared/nCoV2019/tree/master/latest_data
library(tidyverse)

rawdata = read_csv("/Users/ulas/Drive/Projects/HIBIT20_ML_Workshop/latestdata.csv")

rawdata %>%
  select(outcome, age, sex, latitude, longitude) %>%
  drop_na() %>%
  mutate(outcome = tolower(outcome)) %>%
  filter(outcome %in% c("dead", "death", "deceased", "died", "recovered"),
         nchar(age) == 2) %>%
  mutate(outcome = ifelse(outcome == "recovered", "Recovered", "Died"),
         sex = ifelse(sex == "female", "Female", "Male"),
         age = as.numeric(age)) %>% 
  write_csv("/Users/ulas/Drive/Projects/HIBIT20_ML_Workshop/processeddata.csv", col_names=TRUE)

