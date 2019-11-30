

# needed libraries
library(ggstatsplot)
library(tidyverse)
library(gganimate)
library(datenguideR)

# set working dir
ggstatsplot::set_cwd()

# data
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

# writing the data file
# (just because the API is not stable and sometimes returns NAs)
# readr::write_csv(df, "datenguideR.csv")

# extracting id names for each region
df_trash <-
  dplyr::left_join(
    x = dplyr::select(df, -name),
    y = datenguideR::dg_regions %>%
      dplyr::filter(level == "nuts1") %>%
      dplyr::select(id, name),
    by = "id"
  )

# creating means by name and year
df_trash %<>%
  dplyr::group_by(name, year) %>%
  dplyr::summarise(mean_trash = mean(value, na.rm = TRUE)) %>%
  dplyr::ungroup()

# a broad take on the data
ggsave(
  filename = "datenGuideR.png",
  plot = ggstatsplot::ggbetweenstats(
    data = df,
    x = year,
    y = value,
    title = "Trash produced by different regions in Germany",
    xlab = "year",
    ylab = "trash weight (tons)",
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

# adding cumulative sum
df_trash %<>%
  dplyr::group_by(name) %>%
  dplyr::mutate(cumsum = cumsum(mean_trash)) %>%
  dplyr::ungroup()

# creating a fancy visualization
gganimate::anim_save(
  filename = paste("trash.gif"),
  animation = ggplot(
    dplyr::mutate(df_trash, cumsum_mil = cumsum / 100000),
    aes(name, cumsum_mil)
  ) +
    geom_point(aes(color = name), size = 6) +
    scale_y_continuous(breaks = seq(0, 3500, 500), limits = c(0, 3500)) +
    coord_flip() +
    ggthemes::theme_tufte() +
    theme(legend.position = "none", text = element_text(size = 20)) +
    guides(legend = FALSE) +
    labs(
      title = "How much trash does Germany accumulate over time? \nYear: {frame_time}",
      y = "cumulative sum of trash weight (in million tons)",
      x = "region"
    ) +
    transition_time(year) +
    ease_aes("linear"),
  width = 800,
  height = 500
)
