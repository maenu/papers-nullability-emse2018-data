library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(ggplot2)

confusion <- read.csv(file = 'evaluation/getter/inspection.csv') %>%
  mutate(
    predicted = ifelse(getter == 'true', TRUE, FALSE),
    actual = ifelse(classification == 'TRUE', TRUE, FALSE)
  ) %>%
  group_by(predicted, actual) %>%
  summarize(n = n()) %>%
  mutate(classification = ifelse(
    actual,
    ifelse(predicted, 'TP', 'FN'),
    ifelse(predicted, 'FP', 'TN')
  )) %>%
  ungroup() %>%
  arrange(predicted, predicted)

classification.size <- function (data, classification_) {
  s <- data %>%
    filter(classification == classification_) %>%
    select(n) %>%
    slice(1) %>%
    unlist() %>%
    first()
  s <- ifelse(is.na(s), 0, s)
  return(s)
}

precision.recall <- function(confusion) {
  tp <- classification.size(confusion, 'TP')
  fp <- classification.size(confusion, 'FP')
  fn <- classification.size(confusion, 'FN')
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  return(c(precision, recall))
}

confusion
confusion %>% summarize(n = sum(n))
precision.recall(confusion)
