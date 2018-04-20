library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(scales)
library(ggplot2)

read.documentation <- function(artifact) {
  return(
    read.csv(file = paste0(
      'evaluation/documentation/', artifact, '-data.csv'
    ))
  )
}

combine.documentation.public <-
  function(documentation) {
    return(
      documentation %>%
        filter(attribute == 'method', visibility == 'public') %>%
        select(class,
               name,
               getter,
               index,
               internal,
               nullness,
               n,
               constructor,
               documentation) %>%
        mutate(getter = getter == 'true') %>%
        group_by(class,
                 name,
                 getter,
                 index,
                 internal) %>%
        summarize(
          # assumes constant constructor and documentation
          constructor = first(constructor),
          documentation = first(documentation),
          nullness.null = sum(n[nullness == 'NULL']),
          nullness.non.null = sum(n[nullness == 'NON_NULL']),
          nullness.unknown = sum(n[nullness == 'UNKNOWN']),
          n = sum(n)
        ) %>%
        ungroup() %>%
        mutate(use = ifelse(index == -1, 'return', 'parameter')) %>%
        mutate(
          nullability.evidence = ifelse(use == 'parameter',
                                        nullness.null,
                                        nullness.non.null),
          nullability = nullability.evidence / n,
          nullability.0 = ifelse(nullability == 0, 1, 0),
          nullability.01 = ifelse(nullability > 0 &
                                    nullability < 1, 1, 0),
          nullability.1 = ifelse(nullability == 1, 1, 0),
          nullability.label = ifelse(
            nullability == 0,
            '[0]',
            ifelse(nullability == 1, '[1]', ']0,1[')
          )
        )
    )
  }

process <- function(artifact) {
  documentation <- read.documentation(artifact)
  documentation.public <- combine.documentation.public(documentation)
  return()
}

process('guava')
process('commons-io')
process('jre')

artifact <- 'guava'
documentation <- read.documentation(artifact)
documentation.public <- combine.documentation.public(documentation)

documentation.public %>%
  group_by(documentation)




distribution <- distribution %>%
  mutate(
    documentation = ifelse(
      documentation == 'CLASS_NOT_FOUND',
      0,
      ifelse(
        documentation == 'METHOD_NOT_FOUND',
        1,
        ifelse(documentation == 'NOT_MENTIONED', 2, 3)
      )
    ),
    lucene = (lucene == 'true'),
    getter = (getter == 'true')
  ) %>%
  group_by(groupId,
           artifactId,
           majorVersion,
           class,
           method,
           lucene) %>%
  summarize(
    documentation = max(documentation),
    getter = sum(getter) > 0,
    dereferences = sum(dereferences),
    nonNulls = sum(nonNulls),
    nulls = sum(nulls),
    unknowns = sum(unknowns),
    nullability = nonNulls / dereferences
  ) %>%
  ungroup() %>%
  mutate(
    documentation = factor(
      documentation,
      levels = c(0, 1, 2, 3),
      labels = c(
        'CLASS_NOT_FOUND',
        'METHOD_NOT_FOUND',
        'NOT_MENTIONED',
        'MENTIONED'
      )
    ),
    getter = ifelse(getter, 'getter', 'processor'),
    lucene = ifelse(lucene, 'internal', 'external'),
    nullable = nullability > nullability.cutoff,
    id = paste(nullable, getter, lucene, sep = '-')
  ) %>%
  filter(dereferences >= dereferences.cutoff) %>%
  arrange(desc(dereferences))
pdf(
  'evaluation/structure/lucene/4-nullability-distribution.pdf',
  width = 5,
  height = 4
)
ggplot(distribution.both, aes(nullability)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(breaks = pretty_breaks()) +
  facet_grid(lucene ~ getter) +
  theme_minimal() +
  labs(title = "Nullability distribution",
       subtitle = '568 methods used internally and externally',
       y = 'methods') +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )
