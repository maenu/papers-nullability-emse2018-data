library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(ggplot2)

reduce.annotation <- function(nullness) {
  # verify that jaif is consistent
  nullness <- data.frame(nullness = nullness) %>%
    filter(nullness != 'Unknown',
           nullness != '',!is.na(nullness)) %>%
    unique()
  if (nullness %>% nrow() == 1) {
    return(nullness %>%
             slice(1) %>%
             unlist() %>%
             as.character() %>%
             first())
  }
  # nullable is stronger than non-null
  if (nullness %>% filter(nullness == 'Nullable') %>% unique() %>% nrow() == 1) {
    return('Nullable')
  }
  return('Unknown')
}

reduce.nullness <- function(nullness) {
  # reduce nullness on overriden method return types, only one should be not unknown
  nullness <- data.frame(nullness = nullness) %>%
    filter(nullness != 'UNKNOWN',
           nullness != '',!is.na(nullness)) %>%
    unique()
  if (nullness %>% nrow() == 1) {
    return(nullness %>%
             slice(1) %>%
             unlist() %>%
             as.character() %>%
             first())
  }
  return('UNKNOWN')
}

read.jar <- function(artifact) {
  # multiple defs for same methods with different return types removed, jar & jaif
  return(
    read.csv(file = paste0(
      'evaluation/ground-truth/', artifact, '-jar.csv'
    )) %>%
      mutate(
        name = str_replace(name, '\\).*$', ')'),
        overriddes = as.character(class) != as.character(rootClass)
      ) %>%
      select(
        class,
        attribute,
        name,
        index,
        type,
        primitive,
        rootClass,
        abstract,
        classVisibility,
        visibility
      ) %>%
      unique()
  )
}

read.jaif <- function(artifact) {
  # multiple defs for same methods with different return types removed, jar & jaif
  return(
    read.csv(file = paste0(
      'evaluation/ground-truth/', artifact, '-jaif.csv'
    )) %>%
      mutate(name = str_replace(name, '\\).*$', ')')) %>%
      select(class,
             attribute,
             name,
             index,
             nullness) %>%
      unique()
  )
}

read.jaif.integrity.disagree <- function(artifact) {
  return(read.csv(
    file = paste0(
      'evaluation/ground-truth/',
      artifact,
      '-jaif-integrity-disagree.csv'
    )
  ))
}

read.definition <- function(artifact) {
  # reduce nullness on overriden method return types, only one should be not unknown
  .definition <-
    read.csv(file = paste0('evaluation/definition/', artifact, '-data.csv')) %>%
    mutate(name = str_replace(name, '\\).*$', ')')) %>%
    select(class,
           attribute,
           name,
           index,
           nullness)
  return(
    .definition %>%
      group_by(class,
               name,
               index) %>%
      summarize(o = n()) %>%
      ungroup() %>%
      filter(o > 1) %>%
      inner_join(.definition, by = c('class', 'name', 'index')) %>%
      group_by(class, name, index) %>%
      summarize(nullness = reduce.nullness(nullness)) %>%
      right_join(
        .definition,
        by = c('class', 'name', 'index'),
        suffix = c('.reduced', '.original')
      ) %>%
      mutate(nullness = ifelse(
        is.na(nullness.reduced),
        as.character(nullness.original),
        as.character(nullness.reduced)
      )) %>%
      select(-nullness.reduced, -nullness.original) %>%
      unique() %>%
      filter(nullness != '') %>%
      ungroup() %>%
      mutate(
        class = as.character(class),
        name = as.character(name),
        index = as.numeric(index),
        attribute = as.character(attribute),
        nullness = as.character(nullness)
      )
  )
}

read.usage <- function(artifact) {
  # grouping removes duplicates, should not exist though
  return(
    read.csv(file = paste0('evaluation/usage/', artifact, '-data.csv')) %>%
      mutate(name = str_replace(name, '\\).*$', ')')) %>%
      group_by(class,
               attribute,
               visibility,
               name,
               index,
               internal,
               nullness) %>%
      summarize(n = sum(n)) %>%
      ungroup()
  )
}

combine.coverage <-
  function (jar,
            jaif,
            jaif.integrity.disagree,
            definition,
            usage) {
    return(
      jar %>%
        mutate(
          use = ifelse(index == -1, 'return', 'parameter'),
          jar = 'jar',
          jar.public = ifelse(
            classVisibility == 'public' &
              visibility != 'private',
            'jar.public',
            NA
          ),
          jar.original = ifelse(
            as.character(class) == as.character(rootClass),
            'jar.original',
            NA
          )
        ) %>%
        select(
          rootClass,
          abstract,
          class,
          attribute,
          name,
          use,
          index,
          primitive,
          jar,
          jar.public,
          jar.original
        ) %>%
        unique() %>%
        full_join(
          jaif %>%
            rename(nullness.jaif = nullness) %>%
            mutate(
              jaif = 'jaif',
              jaif.known = ifelse(nullness.jaif != 'Unknown', 'jaif.known', NA)
            ) %>%
            select(class, name, index, nullness.jaif, jaif, jaif.known) %>%
            unique(),
          by = c('class', 'name', 'index')
        ) %>%
        full_join(
          jaif.integrity.disagree %>%
            mutate(jaif.integrity.disagree = 'jaif.integrity.disagree') %>%
            select(class, name, index, jaif.integrity.disagree) %>%
            mutate(
              class = as.character(class),
              name = as.character(name),
              index = as.numeric(index)
            ) %>%
            unique(),
          by = c('class', 'name', 'index')
        ) %>%
        full_join(
          definition %>%
            mutate(
              definition = 'definition',
              definition.known = ifelse(nullness == 'UNKNOWN', NA, 'definition.known')
            ) %>%
            rename(nullness.definition = nullness) %>%
            select(
              class,
              name,
              index,
              definition,
              definition.known,
              nullness.definition
            ) %>%
            unique(),
          by = c('class', 'name', 'index')
        ) %>%
        full_join(
          usage %>%
            mutate(internal = 'true' == internal) %>%
            group_by(class, name, index) %>%
            summarize(
              usage.internal = any(internal),
              usage.external = !all(internal)
            ) %>%
            ungroup() %>%
            mutate(
              usage = 'usage',
              usage.internal = ifelse(usage.internal, 'usage.internal', NA),
              usage.external = ifelse(usage.external, 'usage.external', NA)
            ) %>%
            unique(),
          by = c('class', 'name', 'index')
        ) %>%
        filter(primitive == 'false')
    )
  }

combine.coverage.filter <-
  function(coverage, attribute_, use_, sets) {
    return(
      coverage %>%
        filter(attribute == attribute_,
               use == use_) %>%
        group_by_(.dots = sets) %>%
        summarize(n = n()) %>%
        mutate(group = paste(!!!rlang::syms(sets), sep = '&')) %>%
        ungroup() %>%
        select(group, n) %>%
        mutate(group = str_replace_all(group, 'NA&|&NA', '')) %>%
        filter(group != 'NA') %>%
        spread(group, n) %>%
        c() %>%
        unlist()
    )
  }

write.coverage <- function(artifact, coverage) {
  .sets <- c('jar.public',
             'jaif.known',
             'jaif.integrity.disagree',
             'definition.known',
             'usage')
  pdf(paste0(
    'evaluation/usage/',
    artifact,
    '-coverage-method-return.pdf'
  ))
  print(plot(
    euler(
      combine.coverage.filter(coverage, 'method', 'return', .sets),
      shape = 'ellipse'
    ),
    main = 'coverage-method-return',
    quantities = list()
  ))
  dev.off()
  pdf(paste0(
    'evaluation/usage/',
    artifact,
    '-coverage-method-parameter.pdf'
  ))
  print(plot(
    euler(
      combine.coverage.filter(coverage, 'method', 'parameter', .sets),
      shape = 'ellipse'
    ),
    main = 'coverage-method-parameter',
    quantities = list()
  ))
  dev.off()
  return()
}

process <- function(artifact) {
  jar <- read.jar(artifact)
  jaif <- read.jaif(artifact)
  jaif.integrity.disagree <- read.jaif.integrity.disagree(artifact)
  definition <- read.definition(artifact)
  usage <- read.usage(artifact)
  coverage <-
    combine.coverage(jar, jaif, jaif.integrity.disagree, definition, usage)
  write.coverage(artifact, coverage)
  return()
}

process('guava')
process('commons-io')
process('jre')
