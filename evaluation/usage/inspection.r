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
    read.csv(file = paste0(
      'evaluation/usage/', artifact, '-data.csv'
    )) %>%
      mutate(name = str_replace(name, '\\).*$', ')')) %>%
      group_by(
        class,
        attribute,
        visibility,
        name,
        getter,
        index,
        internal,
        nullness
      ) %>%
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
            mutate(internal = 'true' == internal,
                   getter = 'true' == getter) %>%
            group_by(class, name, index) %>%
            summarize(
              getter = any(internal),
              usage.internal = any(internal),
              usage.external = !all(internal)
            ) %>%
            ungroup() %>%
            mutate(
              getter = ifelse(getter, 'getter', NA),
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
  .sets <- c(
    'jar.public',
    'jaif.known',
    'jaif.integrity.disagree',
    'definition.known',
    'usage'
  )
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

combine.inference <-
  function(jar,
           jaif,
           jaif.integrity.disagree,
           definition,
           usage) {
    return(
      jar %>%
        filter(attribute == 'method',
               primitive == 'false') %>%
        select(rootClass,
               abstract,
               class,
               name,
               index) %>%
        inner_join(
          jaif %>%
            select(class,
                   name,
                   index,
                   nullness),
          by = c('class', 'name', 'index')
        ) %>%
        # remove disagreed methods
        anti_join(
          jaif.integrity.disagree %>%
            select(class,
                   name,
                   index) %>%
            mutate(
              class = as.character(class),
              name = as.character(name),
              index = as.numeric(index)
            ) %>%
            unique(),
          by = c('class', 'name', 'index')
        ) %>%
        left_join(
          # only method
          definition %>%
            filter(attribute == 'method') %>%
            select(class,
                   name,
                   index,
                   nullness) %>%
            rename(nullness.definition = nullness) %>%
            mutate(
              nullness.definition = ifelse(
                nullness.definition == 'NON_NULL',
                'NonNull',
                ifelse(nullness.definition == 'NULLABLE',
                       'Nullable',
                       'Unknown')
              )
            ),
          by = c('class', 'name', 'index')
        ) %>%
        inner_join(
          usage %>%
            filter(attribute == 'method') %>%
            select(class,
                   name,
                   getter,
                   index,
                   internal,
                   nullness,
                   n) %>%
            mutate(getter = getter == 'true') %>%
            rename(n.usage = n,
                   internal.usage = internal),
          by = c('class', 'name', 'index'),
          suffix = c('.jaif', '.usage')
        ) %>%
        group_by(
          rootClass,
          class,
          name,
          getter,
          index,
          nullness.jaif,
          nullness.definition,
          internal.usage
        ) %>%
        summarize(
          nullness.usage.null = sum(n.usage[nullness.usage == 'NULL']),
          nullness.usage.non.null = sum(n.usage[nullness.usage == 'NON_NULL']),
          nullness.usage.unknown = sum(n.usage[nullness.usage == 'UNKNOWN']),
          n.usage = sum(n.usage)
        ) %>%
        ungroup() %>%
        mutate(use = ifelse(index == -1, 'return', 'parameter')) %>%
        mutate(
          nullability.evidence = ifelse(
            use == 'parameter',
            nullness.usage.null,
            nullness.usage.non.null
          ),
          nullability = nullability.evidence / n.usage
        )
    )
  }

merge.inference <- function(inference) {
  return(
    inference %>%
      group_by(
        rootClass,
        class,
        name,
        getter,
        use,
        index,
        nullness.jaif,
        nullness.definition
      ) %>%
      summarize(
        nullness.usage.null = sum(nullness.usage.null),
        nullness.usage.non.null = sum(nullness.usage.non.null),
        nullness.usage.unknown = sum(nullness.usage.unknown),
        n.usage = sum(n.usage)
      ) %>%
      ungroup() %>%
      mutate(
        nullability.evidence = ifelse(
          use == 'parameter',
          nullness.usage.null,
          nullness.usage.non.null
        ),
        nullability = nullability.evidence / n.usage
      )
  )
}

filter.inference.jaif <- function(inference) {
  return(
    inference %>%
      filter(!is.na(nullness.jaif),
             nullness.jaif != 'Unknown') %>%
      rename(nullness.actual = nullness.jaif)
  )
}

filter.inference.definition <- function(inference) {
  return(
    inference %>%
      filter(
        !is.na(nullness.definition),
        nullness.definition != 'Unknown'
      ) %>%
      rename(nullness.actual = nullness.definition)
  )
}

infer.nullness <- function(inference, min.n.usage) {
  return(inference %>%
           mutate(nullness.usage = ifelse(
             n.usage >= min.n.usage,
             ifelse(nullability > 0,
                    'Nullable',
                    'NonNull'),
             'Unknown'
           )))
}

as.confusion <- function(nullness.inferred, nullness.actual_, nullness.other_) {
  return(
    nullness.inferred %>%
      mutate(
        nullness.usage = ifelse(
          nullness.usage == nullness.actual_,
          nullness.usage,
          nullness.other_
        ),
        classification = ifelse(
          nullness.actual == nullness.actual_,
          ifelse(nullness.usage == nullness.actual, 'tp', 'fn'),
          ifelse(nullness.usage == nullness.actual, 'tn', 'fp')
        )
      ) %>%
      group_by(classification) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      spread(classification, n) %>%
      mutate(
        tp = ifelse('tp' %in% names(.), tp, 0),
        fn = ifelse('fn' %in% names(.), fn, 0),
        tn = ifelse('tn' %in% names(.), tn, 0),
        fp = ifelse('fp' %in% names(.), fp, 0)
      ) %>%
      unlist()
  )
}

precision.recall <- function(confusion) {
  tp <- confusion['tp'] %>% first()
  fp <- confusion['fp'] %>% first()
  fn <- confusion['fn'] %>% first()
  tn <- confusion['tn'] %>% first()
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  n <- sum(confusion)
  return(data.frame(
    precision = ifelse(is.nan(precision), 0, precision),
    recall = ifelse(is.nan(recall), 0, recall),
    n = n,
    tp = tp,
    fn = fn,
    tn = tn,
    fp = fp
  ))
}

evaluate.perfomance <- function(inference) {
  .r <- data.frame()
  for (x in c(1, 2, 3, 4, 5, 10, 20, 50, 100)) {
    nullness.inferred <- infer.nullness(inference, x)
    row <-
      precision.recall(as.confusion(nullness.inferred, 'Nullable', 'NonNull'))
    row['nullness'] <- 'Nullable'
    row['x'] <- x
    .r <- bind_rows(.r, row)
    row <-
      precision.recall(as.confusion(nullness.inferred, 'NonNull', 'Nullable'))
    row['nullness'] <- 'NonNull'
    row['x'] <- x
    .r <- bind_rows(.r, row)
  }
  .r <- .r %>%
    mutate_at(vars(-nullness), as.numeric)
  return(.r)
}

evaluate.perfomance.stats <-
  function(inference) {
    return(
      inference %>%
        group_by(nullness.actual) %>%
        summarize(n = n()) %>%
        spread(nullness.actual, n) %>%
        mutate(
          Nullable = ifelse('Nullable' %in% names(.), Nullable, 0),
          NonNull = ifelse('NonNull' %in% names(.), NonNull, 0)
        ) %>%
        slice(1) %>%
        unlist()
    )
  }

plot.performance <- function (performance, performance.stats) {
  return(
    ggplot(performance, aes(x = x)) +
      geom_step(aes(y = precision, color = 'precision')) +
      geom_step(aes(y = recall, color = 'recall')) +
      theme_minimal() +
      labs(
        caption = paste0(
          'N = ',
          sum(performance.stats),
          ', Nullable = ',
          performance.stats['Nullable'],
          ', NonNull = ',
          performance.stats['NonNull']
        ),
        color = NULL,
        x = 'min support (log10)',
        y = 'precision / recall'
      ) +
      ylim(c(0, 1)) +
      scale_x_log10(
        breaks = c(1, 2, 3, 4, 5, 10, 20, 50, 100),
        minor_breaks = NULL
      )
  )
}

write.performance <-
  function(artifact,
           performance,
           performance.stats,
           ground.truth,
           use_,
           nullness_) {
    plot.performance(performance %>% filter(nullness == nullness_),
                     performance.stats) +
      labs(title = paste0(use_, ' ', nullness_, ' inferrence from ', ground.truth)) +
      ggsave(
        paste0(
          'evaluation/usage/',
          artifact,
          '-performance-',
          ground.truth,
          '-',
          use_,
          '-',
          nullness_,
          '.pdf'
        ),
        width = 14,
        height = 10,
        units = 'cm'
      )
  }

evaluate.performance.use <-
  function(artifact, inference, ground.truth, use_) {
    .d <- inference %>% filter(use == use_)
    performance <- evaluate.perfomance(.d)
    performance.stats <- evaluate.perfomance.stats(.d)
    write.performance(artifact,
                      performance,
                      performance.stats,
                      ground.truth,
                      use_,
                      'Nullable')
    write.performance(artifact,
                      performance,
                      performance.stats,
                      ground.truth,
                      use_,
                      'NonNull')
  }

evaluate.performance.ground.truth <-
  function(artifact, inference, ground.truth) {
    evaluate.performance.use(artifact, inference, ground.truth, 'parameter')
    evaluate.performance.use(artifact, inference, ground.truth, 'return')
  }

evaluate.performance.ground.truth.use <-
  function(artifact, inference) {
    if (artifact != 'jre') {
      evaluate.performance.ground.truth(artifact,
                                        filter.inference.definition(inference),
                                        'definition')
    }
    evaluate.performance.ground.truth(artifact, filter.inference.jaif(inference), 'jaif')
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
  inference <-
    combine.inference(jar, jaif, jaif.integrity.disagree, definition, usage)
  inference.merged <- merge.inference(inference)
  evaluate.performance.ground.truth.use(artifact, inference.merged)
  return()
}

process('guava')
process('commons-io')
process('jre')

artifact <- 'guava'
jar <- read.jar(artifact)
jaif <- read.jaif(artifact)
jaif.integrity.disagree <- read.jaif.integrity.disagree(artifact)
definition <- read.definition(artifact)
usage <- read.usage(artifact)
coverage <-
  combine.coverage(jar, jaif, jaif.integrity.disagree, definition, usage)
write.coverage(artifact, coverage)
inference <-
  combine.inference(jar, jaif, jaif.integrity.disagree, definition, usage)
inference.merged <- merge.inference(inference)
evaluate.performance.ground.truth.use(artifact, inference.merged)
