library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(ggplot2)

inspection <- read.csv(file = 'evaluation/method-invoke/inspection.csv') %>%
  mutate(
    ok = str_detect(comment, '^ok')
  )

invokes <- inspection %>%
  filter(invokes == 'true') %>%
  group_by(class, method) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(methods = n(), invokes = sum(n))

ok <- inspection %>%
  filter(!ok) %>%
  group_by(comment) %>%
  summarize(n = n())

invokes
ok
