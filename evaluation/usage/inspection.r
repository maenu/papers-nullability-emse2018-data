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
           nullness != '', !is.na(nullness)) %>%
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
           nullness != '', !is.na(nullness)) %>%
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
      select(-nullness.reduced,-nullness.original) %>%
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
          nullness.usage.null = sum(n.usage[nullness.usage == 'NULL']) / sum(n.usage),
          nullness.usage.non.null = sum(n.usage[nullness.usage == 'NON_NULL']) / sum(n.usage),
          nullness.usage.unknown = sum(n.usage[nullness.usage == 'UNKNOWN']) / sum(n.usage),
          n.usage = sum(n.usage)
        ) %>%
        ungroup() %>%
        mutate(use = ifelse(index == -1, 'return', 'parameter')) %>%
        mutate(
          nullability = ifelse(
            use == 'parameter',
            nullness.usage.null,
            nullness.usage.non.null
          )
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
        nullness.usage.null = sum(n.usage * nullness.usage.null) / sum(n.usage),
        nullness.usage.non.null = sum(n.usage * nullness.usage.non.null) / sum(n.usage),
        nullness.usage.unknown = sum(n.usage * nullness.usage.unknown) / sum(n.usage),
        n.usage = sum(n.usage)
      ) %>%
      ungroup() %>%
      mutate(
        nullability = ifelse(
          use == 'parameter',
          nullness.usage.null,
          nullness.usage.non.null
        )
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
      filter(!is.na(nullness.definition),
             nullness.definition != 'Unknown') %>%
      rename(nullness.actual = nullness.definition)
  )
}

infer.nullness <- function(inference, use_, min.n.usage) {
  return(inference %>%
           filter(use == use_) %>%
           mutate(nullness.usage = ifelse(
             n.usage >= min.n.usage,
             ifelse(nullability > 0,
                    'Nullable',
                    'NonNull'),
             'Unknown'
           )))
}

infer.nullness.f <- function(inference, use) {
  return(function(min.n.usage) {
    return(infer.nullness(inference, use, min.n.usage))
  })
}

as.confusion <- function(nullness.inferred) {
  return(
    nullness.inferred %>%
      filter(nullness.usage != 'Unknown') %>%
      mutate(
        classification = ifelse(
          nullness.actual == 'Nullable',
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
  return(c(
    precision = precision,
    recall = recall,
    n = n,
    tp = tp,
    fn = fn,
    tn = tn,
    fp = fp
  ))
}

summarize.inference.classification <- function(inference.actual) {
  return(
    inference.actual %>%
      group_by(use,
               getter,
               nullness.actual,
               nullness.usage) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      mutate(
        classification = ifelse(
          nullness.usage == 'Unknown',
          'unclassified',
          ifelse(nullness.usage == nullness.actual,
                 'correct',
                 'incorrect')
        ),
        compatible = ifelse(
          use == 'parameter',
          nullness.actual == 'NonNull' |
            nullness.actual == nullness.usage,
          nullness.actual == 'Nullable' |
            nullness.actual == nullness.usage
        )
      )
  )
}

summarize.inference <-
  function(inference.classification.summary) {
    return(
      inference.classification.summary %>%
        group_by(use,
                 getter,
                 compatible,
                 classification) %>%
        summarize(n = sum(n))
    )
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


artifact <- 'guava'
jar <- read.jar(artifact)
jaif <- read.jaif(artifact)
jaif.integrity.disagree <- read.jaif.integrity.disagree(artifact)
definition <- read.definition(artifact)
usage <- read.usage(artifact)
coverage <-
  combine.coverage(jar, jaif, jaif.integrity.disagree, definition, usage)
inference <-
  combine.inference(jar, jaif, jaif.integrity.disagree, definition, usage)
inference.merged <- merge.inference(inference)




d <- filter.inference.definition(inference.merged)
r <- data.frame()
for (x in seq(0, max(d$n.usage))) {
  row <- precision.recall(as.confusion(infer.nullness(d, 'parameter', x)))
  row['x'] <- x
  r <- bind_rows(r, row)
}
  group_by(x) %>%
  rowwise() %>%
  bind_cols(precision.recall(as.confusion(infer.nullness(d, 'parameter', x))))

precision.recall(as.confusion(infer.nullness(d, 'parameter', 2)))

precision.recall(as.confusion(infer.nullness(d, 'parameter', 2)))
x <- infer.nullness(d, 'parameter', 2)
precision.recall(as.confusion(x))

# TODO internal vs. external
inference.jaif <- infer.jaif.usage(inference.merged, 0, 0, 0, 0)
inference.definition <- infer.definition.usage(inference.merged, 0, 0, 0, 0)
inference.jaif.summary.classification <- summarize.inference.classification(inference.jaif)
inference.definition.summary.classification <- summarize.inference.classification(inference.definition)
inference.jaif.summary <- summarize.inference(inference.jaif.summary.classification)
inference.definition.summary <- summarize.inference(inference.definition.summary.classification)

# TODO decision tree for jaif & definition
