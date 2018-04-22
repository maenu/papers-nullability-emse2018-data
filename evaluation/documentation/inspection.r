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
    )) %>%
      mutate(
        documentation = documentation == 'True',
        documentation.description = documentation.description == 'True',
        documentation.throws = documentation.throws == 'True',
        documentation.method.found = documentation.method.found == 'True',
        documentation.class.found = documentation.class.found == 'True'
      )
  )
}

combine.documentation.public <-
  function(documentation) {
    return(
      documentation %>%
        filter(attribute == 'method', visibility == 'public') %>%
        mutate(
          getter = getter == 'true',
          use = ifelse(index == -1, 'return', 'parameter')
        ) %>%
        group_by(class,
                 name,
                 getter,
                 use,
                 index) %>%
        summarize(
          # assumes constant constructor and documentation
          constructor = first(constructor),
          documentation = first(documentation),
          documentation.description = first(documentation.description),
          documentation.throws = first(documentation.throws),
          documentation.method.found = first(documentation.method.found),
          documentation.class.found = first(documentation.class.found),
          nullness.null = sum(n[nullness == 'NULL']),
          nullness.non.null = sum(n[nullness == 'NON_NULL']),
          nullness.unknown = sum(n[nullness == 'UNKNOWN']),
          n = sum(n)
        ) %>%
        ungroup() %>%
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
        ) %>%
        mutate(
          doc = ifelse(
            !documentation.class.found,
            'CLASS_NOT_FOUND',
            ifelse(
              !documentation.method.found,
              'METHOD_NOT_FOUND',
              ifelse(
                documentation,
                'MENTIONED',
                ifelse(
                  documentation.throws,
                  'MENTIONED_THROWS',
                  ifelse(
                    documentation.description,
                    'MENTIONED_GENERAL',
                    'NOT_MENTIONED'
                  )
                )
              )
            )
          )
        )
    )
  }

plot.documentation.public <- function(documentation) {
  documentation %>%
    filter(doc != 'CLASS_NOT_FOUND',
           doc != 'METHOD_NOT_FOUND') %>%
    mutate(
      doc = doc != 'NOT_MENTIONED',
      nullability.label = factor(
        nullability.label,
        levels = c('[0]', ']0,1[', '[1]'),
        labels = c('[0]', ']0,1[', '[1]')
      ),
      getter.label = ifelse(getter, 'getter', 'processor')
    ) %>%
    group_by(use, getter.label, nullability.label, doc) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    ggplot(aes(x = nullability.label, y = n, fill = doc)) +
    geom_bar(stat = 'identity') +
    facet_grid(use ~ getter.label) +
    scale_fill_manual(
      name = 'documented nullness',
      breaks = c(TRUE, FALSE),
      labels = c('yes', 'no'),
      values = c('#bae4bc', '#7bccc4')
    ) +
    scale_y_sqrt(breaks = c(1, 100, 400, 1000, 4000, 10000),
                 minor_breaks = NULL) +
    theme_minimal() +
    labs(y = 'n (sqrt)', x = 'nullability') +
    theme(
      legend.position = 'bottom',
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

write.documentation.public <-
  function(artifact, plot.documentation) {
    plot.documentation +
      labs(title = paste0('nullness documentation, ', artifact)) +
      ggsave(
        paste0(
          'evaluation/documentation/',
          artifact,
          '-documentation-public.pdf'
        ),
        width = 14,
        height = 10,
        units = 'cm'
      )
  }

process <- function(artifact) {
  documentation <- read.documentation(artifact)
  documentation.public <-
    combine.documentation.public(documentation)
  write.documentation.public(artifact, plot.documentation.public(documentation.public))
}

process('guava')
process('commons-io')
process('jre')
