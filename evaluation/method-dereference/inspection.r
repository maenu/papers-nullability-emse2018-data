library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(ggplot2)

inspection <- read.csv(file = 'evaluation/method-dereference/inspection.csv') %>%
  mutate(
    ok = str_detect(comment, '^ok')
  )

dereferenced <- inspection %>%
  filter(dereferences == 'true') %>%
  group_by(class, method) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(methods = n(), dereferences = sum(n))

ok <- inspection %>%
  filter(!ok) %>%
  group_by(comment) %>%
  summarize(n = n())

dereferenced
ok
