library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(ggplot2)

read.artifact.summary <- function() {
  return(read.csv(file = 'evaluation/usage/summary-data.csv'))
}

artifact.summary <- read.artifact.summary()

artifact.summary %>%
  mutate(
    guava = ifelse(guava == 'true', 1, 0),
    commons_io = ifelse(commons_io == 'true', 1, 0),
    jre = ifelse(jre == 'true', 1, 0)
  ) %>%
  group_by(
    groupId,
    artifactId
  ) %>%
  summarize(
    version = n(),
    guava = sum(guava),
    commons_io = sum(commons_io),
    jre = sum(jre)
  ) %>%
  ungroup() %>%
  group_by(
    groupId
  ) %>%
  summarize(
    artifactId = n(),
    version = sum(version),
    guava = sum(guava),
    commons_io = sum(commons_io),
    jre = sum(jre)
  ) %>%
  ungroup() %>%
  summarize(
    groupId = n(),
    artifactId = sum(artifactId),
    version = sum(version),
    guava = sum(guava),
    commons_io = sum(commons_io),
    jre = sum(jre)
  )
