

# needed libraries
library(ggstatsplot)
library(tidyverse)
library(gganimate)
library(datenguideR)

# set working dir
ggstatsplot::set_cwd()

# data
(df_population <- dg_call(
  nuts_nr = 1,
  stat_name = "BEV028",
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
  )) %>%  
  select(id, year, population = value)
)


(df_trash <- dg_call(
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
  )) %>% 
    select(
           trash = value,
           everything(),
           -name)
)


# writing the data file
# (just because the API is not stable and sometimes returns NAs)
# readr::write_csv(df, "datenguideR.csv")

# extracting id names for each region
df_combined <- df_trash %>% 
  dplyr::left_join(x = .,
                   y = df_population, by = c("id", "year")) %>% 
  dplyr::left_join(
    x = .,
    y = datenguideR::dg_regions %>%
      dplyr::filter(level == "nuts1") %>%
      dplyr::select(id, name),
    by = "id"
  )

# creating means by name and year
df_combined %<>%
  dplyr::group_by(name, year) %>%
  dplyr::summarise(mean_trash = mean(trash, na.rm = TRUE), population = population) %>%
  dplyr::ungroup() %>% 
  mutate(mean_trash_std_ton = mean_trash / population,
         mean_trash_std_kg = mean_trash_std_ton * 1000)


# a broad take on the data
# ggsave(
#   filename = "datenGuideR.png",
#   plot = ggstatsplot::ggbetweenstats(
#     data = df,
#     x = year,
#     y = value,
#     title = "Trash produced by different regions in Germany",
#     xlab = "year",
#     ylab = "trash weight (tons)",
#     outlier.tagging = TRUE,
#     outlier.label = name,
#     outlier.coef = 2.5,
#     messages = FALSE,
#     bf.message = FALSE,
#     ggtheme = hrbrthemes::theme_ipsum_tw(),
#     package = "ggsci",
#     palette = "default_igv"
#   ),
#   width = 20,
#   height = 15
# )

# adding cumulative sum
df_combined %<>%
  dplyr::group_by(name) %>%
  dplyr::mutate(cumsum = cumsum(mean_trash_std_kg)) %>%
  dplyr::ungroup()

# Label
dg_descriptions %>% 
  filter(stat_name == "AEW010") %>% 
  select(stat_description_full)

# creating a fancy visualization
gganimate::anim_save(
  filename = "trash.gif",
  animation = ggplot(df_combined,
    aes(name, cumsum)
    ) +
    geom_point(aes(color = name), size = 6) +
    coord_flip() +
    ggthemes::theme_tufte() +
    theme(legend.position = "none", text = element_text(size = 20)) +
    guides(legend = FALSE) +
    labs(
      title = "How much trash does Germany accumulate over time? 
      \n (Since 2006 to {frame_time})",
      y = "cumulative amount of waste discharged (in kilogram per capita)",
      x = "",
      caption = "Source: GENESIS-Statistik 'Erhebung der Abfallentsorgung' (32111)"
    ) +
    scale_y_continuous(label = scales::label_number_si(unt = "kg")) +
    transition_time(year) +
    ease_aes("linear"),
  width = 800,
  height = 500
)


# Sources:
#  - https://www.regionalstatistik.de/genesis/online/data;sid=1B9D622CFEA587BAE92DE292DC3AE1A8.reg2?operation=statistikLangtext&levelindex=0&levelid=1575195102089&index=1
#  - https://www.destatis.de/DE/Methoden/Qualitaet/Qualitaetsberichte/Umwelt/abfallentsorgung.pdf?__blob=publicationFile&v=4
