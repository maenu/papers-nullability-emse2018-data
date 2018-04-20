library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(eulerr)
library(scales)
library(ggplot2)

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

combine.inference <-
  function(usage) {
    return(
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
        group_by(class,
                 name,
                 getter,
                 index,
                 internal) %>%
        summarize(
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

merge.inference <- function(inference) {
  return(
    inference %>%
      group_by(class,
               name,
               getter,
               use,
               index) %>%
      summarize(
        nullness.null = sum(nullness.null),
        nullness.non.null = sum(nullness.non.null),
        nullness.unknown = sum(nullness.unknown),
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
      )
  )
}

filter.inference.both <- function(inference) {
  return (inference %>%
            group_by(class,
                     name,
                     index) %>%
            filter(n() == 2) %>%
            ungroup())
}

plot.distribution.internal.external <- function(inference.both) {
  return(
    inference.both %>%
      group_by(use) %>%
      mutate(use.label = paste0(
        use,
        ', N = ',
        n(),
        ', (',
        sum(nullability.0),
        ', ',
        sum(nullability.01),
        ', ',
        sum(nullability.1),
        ')'
      )) %>%
      ungroup() %>%
      mutate(internal.label = ifelse(
        internal == 'true', 'internal', 'external'
      )) %>%
      ggplot(aes(x = nullability)) +
      facet_grid(internal.label ~ use.label) +
      geom_histogram() +
      coord_cartesian(xlim = c(0, 1))  +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_sqrt(breaks = c(1, 4, 10, 40, 100, 400, 1000, 4000, 10000)) +
      theme_minimal() +
      labs(title = "Nullability, external vs. internal", y = 'methods') +
      theme(
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  )
}

plot.distribution.getter.processor <- function(inference.merged) {
  return(
    inference.merged %>%
      filter(use == 'return') %>%
      group_by(getter) %>%
      mutate(
        getter.label = paste0(
          ifelse(getter, 'getter', 'processor'),
          ' return, N = ',
          n(),
          ', (',
          sum(nullability.0),
          ', ',
          sum(nullability.01),
          ', ',
          sum(nullability.1),
          ')'
        )
      ) %>%
      ungroup() %>%
      ggplot(aes(x = nullability)) +
      facet_grid(. ~ getter.label) +
      geom_histogram() +
      coord_cartesian(xlim = c(0, 1))  +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_sqrt(breaks = c(1, 4, 10, 40, 100, 400, 1000, 4000, 10000)) +
      theme_minimal() +
      labs(title = "Nullability, getter vs. processor", y = 'methods (sqrt)') +
      theme(
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  )
}

plot.distribution <- function (inference.merged) {
  return(
    inference.merged %>%
      group_by(use) %>%
      mutate(use.label = paste0(
        use,
        ', N = ',
        n(),
        ', (',
        sum(nullability.0),
        ', ',
        sum(nullability.01),
        ', ',
        sum(nullability.1),
        ')'
      )) %>%
      ungroup() %>%
      ggplot(aes(x = nullability)) +
      facet_grid(. ~ use.label) +
      geom_histogram() +
      xlim(0, 1) +
      coord_cartesian(xlim = c(0, 1))  +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_sqrt(breaks = c(1, 4, 10, 40, 100, 400, 1000, 4000, 10000)) +
      theme_minimal() +
      labs(title = "Nullability", y = 'methods (sqrt)') +
      theme(
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  )
}

plot.dereferences.methods <- function(inference) {
  .x.1 <- inference %>%
    mutate(c = 1) %>%
    filter(use == 'return',
           getter == TRUE) %>%
    group_by(use,
             getter,
             nullable = nullability.0 == 0) %>%
    summarize(
      methods.internal = sum(c[internal == 'true']),
      methods.external = sum(c[internal == 'false']),
      dereferences.internal = sum(n[internal == 'true']),
      dereferences.external = sum(n[internal == 'false'])
    )
  .x.2 <- inference %>%
    mutate(c = 1) %>%
    filter(use == 'return',
           getter == FALSE) %>%
    group_by(use,
             getter,
             nullable = nullability.0 == 0) %>%
    summarize(
      methods.internal = sum(c[internal == 'true']),
      methods.external = sum(c[internal == 'false']),
      dereferences.internal = sum(n[internal == 'true']),
      dereferences.external = sum(n[internal == 'false'])
    )
  return(
    data.frame() %>%
      bind_rows(.x.1) %>%
      bind_rows(.x.2) %>%
      gather(
        key = 'type',
        value = 'count',
        methods.internal,
        dereferences.internal,
        methods.external,
        dereferences.external
      ) %>%
      mutate(
        internal = str_replace(type, "^(.*)\\.(.*)$", "\\2"),
        type = str_replace(type, "^(.*)\\.(.*)$", "\\1")
      ) %>%
      mutate(internal = internal == 'internal') %>%
      group_by(getter,
               internal,
               type) %>%
      mutate(ratio = count / sum(count)) %>%
      ungroup() %>%
      select(getter,
             internal,
             type,
             nullable,
             count,
             ratio) %>%
      mutate(
        getter.label = ifelse(getter, 'getter', 'processor'),
        internal.label = ifelse(internal, 'internal', 'external')
      ) %>%
      ggplot(aes(
        x = type,
        y = ratio,
        fill = nullable,
        label = count
      )) +
      geom_bar(stat = 'identity') +
      facet_grid(internal.label ~ getter.label) +
      geom_text(position = position_stack(vjust = 0.5), size = 3) +
      scale_fill_manual(
        name = 'nullability',
        breaks = c(FALSE, TRUE),
        labels = c('[0]', ']0, 1]'),
        values = c('#bae4bc', '#7bccc4', '#43a2ca')
      ) +
      theme_minimal() +
      labs(title = "dereferences vs. methods", y = 'ratio') +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()
      )
  )
}

write.distribution <-
  function(artifact, suffix, distribution.plot) {
    distribution.plot +
      ggsave(
        paste0(
          'evaluation/distribution/',
          artifact,
          '-distribution',
          suffix,
          '.pdf'
        ),
        width = 14,
        height = 10,
        units = 'cm'
      )
  }

write.dereferences.methods <-
  function(artifact, dereferences.methods.plot) {
    dereferences.methods.plot +
      ggsave(
        paste0(
          'evaluation/distribution/',
          artifact,
          '-dereferences-methods.pdf'
        ),
        width = 14,
        height = 10,
        units = 'cm'
      )
  }

process <- function(artifact) {
  usage <- read.usage(artifact)
  inference <- combine.inference(usage)
  inference.merged <- merge.inference(inference)
  inference.both <- filter.inference.both(inference)
  if (artifact != 'jre') {
    write.distribution(
      artifact,
      '-internal-external',
      plot.distribution.internal.external(inference.both)
    )
  }
  write.distribution(
    artifact,
    '-getter-processor',
    plot.distribution.getter.processor(inference.merged)
  )
  write.distribution(artifact,
                     '',
                     plot.distribution(inference.merged))
  if (artifact != 'jre') {
    write.dereferences.methods(artifact, plot.dereferences.methods(inference.both))
  } else {
    write.dereferences.methods(artifact, plot.dereferences.methods(inference))
  }
  return()
}

process('guava')
process('commons-io')
process('jre')
