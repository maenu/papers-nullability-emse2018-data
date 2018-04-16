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

check.jaif.integrity <- function(jar, jaif) {
  return(
    jar %>%
      select(rootClass,
             abstract,
             class,
             attribute,
             name,
             index,
             primitive) %>%
      inner_join(
        jaif %>%
          select(class,
                 name,
                 index,
                 nullness),
        by = c('class', 'name', 'index')
      ) %>%
      select(rootClass,
             attribute,
             name,
             index,
             primitive,
             nullness) %>%
      unique() %>%
      group_by(rootClass,
               attribute,
               name,
               index,
               primitive) %>%
      filter(n() > 1) %>%
      summarize(nullness = reduce.annotation(nullness)) %>%
      ungroup() %>%
      filter(nullness != 'Unknown') %>%
      left_join(
        jar %>%
          select(rootClass,
                 class,
                 name) %>%
          unique(),
        by = c('rootClass', 'name')
      ) %>%
      inner_join(
        jaif %>%
          filter(nullness != 'Unknown') %>%
          select(class,
                 name,
                 index,
                 nullness),
        by = c('rootClass' = 'class', 'name', 'index'),
        suffix = c('.reduced', '.annotated')
      ) %>%
      select(
        rootClass,
        attribute,
        name,
        index,
        primitive,
        nullness.reduced,
        nullness.annotated
      ) %>%
      unique() %>%
      filter(nullness.reduced != nullness.annotated) %>%
      left_join(
        jar %>%
          select(rootClass,
                 class,
                 name) %>%
          unique(),
        by = c('rootClass', 'name')
      ) %>%
      left_join(
        jaif %>%
          select(class,
                 name,
                 index,
                 nullness),
        by = c('class', 'name', 'index')
      ) %>%
      arrange(rootClass,
              attribute,
              name,
              index,
              class)
  )
}

check.jaif.integrity.disagree <-
  function(jaif.integrity) {
    return(
      jaif.integrity %>%
        filter(
          !is.na(nullness),
          nullness != 'Unknown',
          primitive == 'false',
          (
            index == -1 &
              nullness.annotated == 'NonNull' &
              nullness == 'Nullable' |
              index > -1 &
              nullness.annotated == 'Nullable' &
              nullness == 'NonNull'
          )
        ) %>%
        select(
          rootClass,
          class,
          attribute,
          name,
          index,
          nullness.reduced,
          nullness.annotated,
          nullness
        ) %>%
        unique()
    )
  }

combine.coverage <-
  function (jar,
            jaif,
            jaif.integrity.disagree) {
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

write.jaif.integrity.disagree <-
  function(artifact, jaif.integrity.disagree) {
    return(write.csv(
      jaif.integrity.disagree,
      file = paste0(
        'evaluation/ground-truth/',
        artifact,
        '-jaif-integrity-disagree.csv'
      )
    ))
  }

write.coverage <- function(artifact, coverage) {
  .sets <- c('jar',
             'jar.public',
             'jaif.integrity.disagree',
             'jaif.known')
  pdf(paste0(
    'evaluation/ground-truth/',
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
    'evaluation/ground-truth/',
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
  jaif.integrity <- check.jaif.integrity(jar, jaif)
  jaif.integrity.disagree <-
    check.jaif.integrity.disagree(jaif.integrity)
  coverage <-
    combine.coverage(jar, jaif, jaif.integrity.disagree)
  write.jaif.integrity.disagree(artifact, jaif.integrity.disagree)
  write.coverage(artifact, coverage)
  return(
    jaif.integrity.disagree %>%
      group_by(attribute, nullness.annotated, nullness.reduced) %>%
      summarize(n())
  )
}

process('guava')
process('commons-io')
process('jre')
