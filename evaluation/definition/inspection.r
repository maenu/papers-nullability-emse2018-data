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
      filter(nullness != '')
  )
}

combine.coverage <-
  function (jar,
            jaif,
            jaif.integrity.disagree,
            definition) {
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
  .sets <- c('jar',
             'jar.public',
             'jaif.known',
             'jaif.integrity.disagree',
             'definition.known')
  pdf(paste0(
    'evaluation/definition/',
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
    'evaluation/definition/',
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

combine.jaif.definition <- function(coverage) {
  return(
    coverage %>%
      filter(
        is.na(jaif.integrity.disagree),!is.na(jaif.known),!is.na(definition.known)
      ) %>%
      mutate(
        nullness.definition = ifelse(nullness.definition == 'NON_NULL', 'NonNull', 'Nullable'),
        compatible = ifelse(
          use == 'parameter',
          nullness.jaif == 'NonNull' |
            nullness.jaif == nullness.definition,
          nullness.jaif == 'Nullable' |
            nullness.jaif == nullness.definition
        )
      )
  )
}

write.jaif.definition <- function(artifact, jaif.definition) {
  write.csv(
    jaif.definition,
    file = paste0('evaluation/definition/', artifact, '-jaif-definition.csv')
  )
  return()
}

summarize.jaif.definition <- function(jaif.definition) {
  return(
    jaif.definition %>%
      group_by(use, nullness.jaif, nullness.definition, compatible) %>%
      summarize(n = n()) %>%
      ungroup()
  )
}

process <- function(artifact) {
  jar <- read.jar(artifact)
  jaif <- read.jaif(artifact)
  jaif.integrity.disagree <- read.jaif.integrity.disagree(artifact)
  definition <- read.definition(artifact)
  coverage <-
    combine.coverage(jar, jaif, jaif.integrity.disagree, definition)
  write.coverage(artifact, coverage)
  jaif.definition <- combine.jaif.definition(coverage)
  write.jaif.definition(artifact, jaif.definition)
  return(summarize.jaif.definition(jaif.definition))
}

process('guava')
process('commons-io')
#process('jre')
