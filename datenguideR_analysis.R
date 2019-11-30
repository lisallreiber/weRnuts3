
library(ggtextures)
library(ggstatsplot)
library(tidyverse)
library(datenguideR)

(df <- dg_call(
  nuts_nr = 1,
  stat_name = "AEW010",
  year = c(
    2006,
    2007,
    2008,
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017,
    2018
  )
))

readr::write_csv(df, "datenguideR.csv")

df_trash <-
  dplyr::left_join(
    x = dplyr::select(df, -name),
    y = datenguideR::dg_regions %>%
      dplyr::filter(level == "nuts1") %>%
      dplyr::select(id, name),
    by = "id"
  )

df_trash %<>%
  dplyr::group_by(name, year) %>%
  dplyr::summarise(mean_trash = mean(value, na.rm = TRUE)) %>%
  dplyr::ungroup()

ggplot(df_trash, aes(as.factor(year), value, color = name)) +
  geom_jitter() + geom_path(aes(group = name)) +
  hrbrthemes::theme_ipsum_tw()

ggsave(
  filename = "datenGuideR.png",
  plot = ggstatsplot::ggbetweenstats(
    df,
    year,
    value,
    outlier.tagging = TRUE,
    outlier.label = name,
    outlier.coef = 2.5,
    messages = FALSE,
    bf.message = FALSE,
    ggtheme = hrbrthemes::theme_ipsum_tw(),
    package = "ggsci",
    palette = "default_igv"
  ),
  width = 20,
  height = 15
)
