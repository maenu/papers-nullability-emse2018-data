library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(ggplot2)

inspection <- read.csv(file = 'evaluation/dereference/inspection.csv') %>%
  mutate(
    ok = as.character(nullness) == as.character(nullness.actual)
  )

dereferenced <- inspection %>%
  group_by(nullness) %>%
  summarize(n = n())

ok <- inspection %>%
  filter(!ok) %>%
  group_by(nullness, comment) %>%
  summarize(n = n())

dereferenced
ok
