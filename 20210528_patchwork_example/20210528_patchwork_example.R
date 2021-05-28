library(tidyverse)
library(patchwork)

sanger_url <- "https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv"

sanger_data <- read_tsv(sanger_url)

sanger_data_max <- sanger_data %>%
  filter(WeekEndDate == max(WeekEndDate))

sanger_extra <- bind_rows(
  sanger_data,
  sanger_data_max %>% mutate(WeekEndDate = lubridate::ymd("2021-05-22")),
  sanger_data_max %>% mutate(WeekEndDate = lubridate::ymd("2021-05-29")),
)

phe_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv"

phe_data <- read_csv(phe_url)


nomis_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100"

nomis_data <- read_csv(nomis_url)

population_data <- nomis_data %>%
  transmute(areaCode = GEOGRAPHY_CODE,
         population = as.numeric(OBS_VALUE))

sanger_rates <- sanger_extra %>%
  arrange(LTLA, WeekEndDate) %>%
  group_by(LTLA, WeekEndDate) %>%
  mutate(total_cases = sum(Count),
         rate = Count/total_cases) %>%
  ungroup()

b16172_incidence <- sanger_rates %>%
  filter(Lineage == "B.1.617.2") %>%
  select(week_date = WeekEndDate,
         areaCode = LTLA,
         b16172_prop = rate)

locality_data <- phe_data %>%
  arrange(areaCode, date) %>%
  group_by(areaCode) %>%
  mutate(
    rolling_sum = slider::slide_index_sum(
        x = newCasesBySpecimenDate,
        i = date,
        before = 7,
        na_rm = TRUE))%>%
  ungroup() %>%
  select(areaCode, areaName, date, rolling_sum)

recent_data <- locality_data %>%
  filter(date > lubridate::ymd("2021-03-20")) %>%
  mutate(offset = 7 - lubridate::wday(date, week_start = 7),
         week_date = date + offset) %>%
  left_join(b16172_incidence, by = c("week_date", "areaCode")) %>%
  left_join(population_data, by = "areaCode") %>%
  rename(total_cases = rolling_sum) %>%
  mutate(
    pop100k = population/100000,
    b16172_prop = replace_na(b16172_prop, 0),
    b16172_cases = round(total_cases * b16172_prop,0),
    all_other = total_cases - b16172_cases,
    b16172_rate = b16172_cases / pop100k,
    other_rate = all_other / pop100k,
    total_rate = total_cases / pop100k)

ft_group <- c("Bedford", "Blackburn with Darwen", "Bolton", "Burnley", "Bury", 
              "Central Bedfordshire", "Croydon", "Ealing", "Hillingdon", 
              "Hounslow", "Kirklees", "Leicester", "Luton", "Manchester", 
              "North Tyneside", "Nottingham", "Reading",
              "Rossendale", "Sefton")

ft_group_data <- recent_data %>%
  filter(areaName %in% ft_group) %>%
  select(date, areaName, b16172 = b16172_rate, all_other = other_rate) %>%
  pivot_longer(cols = c(b16172, all_other), names_to = "variant", values_to = "rate") %>%
  mutate(variant = as_factor(variant))

ft_bedford <- ft_group_data %>%
  filter(areaName == "Bedford")

ft_blackburn_bolton <- ft_group_data %>%
  filter(areaName == "Blackburn with Darwen" | areaName == "Bolton") %>%
  mutate(areaName = str_remove_all(areaName, " with Darwen"))

ft_sefton <- ft_group_data %>%
  filter(areaName == "Sefton")

ft_15 <- ft_group_data %>%
  filter(
    str_detect(areaName,
               "^Bedford$|^Blackburn with Darwen$|^Bolton$|^Sefton$",
               negate = TRUE)) %>%
  mutate(areaName = str_replace_all(areaName, "Bedfordshire", "Beds."))

ft_15_labels <- ft_15 %>%
  distinct(areaName) %>%
  mutate(
    x = lubridate::ymd("2021-03-21"),
    y = 170
  )


custom_theme <- ggplot2::theme(
  text = element_text("Barlow"),
  plot.margin = margin(t = 3, r = 3, b = 3, l = 3),
  plot.title = element_text(face = "bold"),
  plot.background = element_rect(fill = "#FFF1E5", colour = NA),
  panel.grid.major.y = element_line(colour = "#F2E5DA", size = 1),
  axis.text.y = element_text(
    colour = "#66605C",
    margin = margin(r = 6, l = 6),
    size = 8),
  axis.line.x = element_line(colour = "#66605C", size = 1),
  axis.text.x = element_text(
    colour = "#66605C", 
    margin = margin(t = 6, b = 6),
    size = 8),
  axis.ticks = element_line(colour = "#66605C"),
  strip.placement = "outside",
  strip.text = element_text(
    hjust = 0, size = 8, family = "Barlow", face = "bold",
    margin = margin(t = 3))
)

custom_scale <- c("b16172" = "#CC0000", "all_other" = "#E6D9CE")

# Blackburn and Bolton plots
p4 <- ggplot(ft_blackburn_bolton, aes(x = date, y = rate, fill = variant)) +
  geom_area(position = "stack", show.legend = FALSE) +
  facet_wrap(~areaName, ncol = 2) +
  scale_fill_manual(values = custom_scale) +
  scale_y_continuous(
    breaks = seq(0, 550, 50),
    limits = c(0, 575),
    position = "right",
    expand = expansion(add = 0)) +
  scale_x_date(
    breaks = lubridate::ymd(c("2021-04-01", "2021-05-01")),
    labels = c("Apr", "May"),
    expand = expansion(add = 0)) +
  theme_void() +
  custom_theme

# Bedford plot
p2 <- ggplot(ft_bedford, aes(x = date, y = rate, fill = variant)) +
  geom_area(position = "stack", show.legend = FALSE) +
  facet_wrap(~areaName, ncol = 1) +
  scale_fill_manual(values = custom_scale) +
  scale_y_continuous(
    breaks = seq(0, 350, 50),
    limits = c(0, 350),
    position = "right",
    expand = expansion(add = 0)) +
  scale_x_date(
    breaks = lubridate::ymd(c("2021-04-01", "2021-05-01")),
    labels = c("Apr", "May"),
    expand = expansion(add = 0)) +
  theme_void() +
  custom_theme +
  theme(plot.margin = margin(b = 0))

# Sefton plot
p3 <- ggplot(ft_sefton, aes(x = date, y = rate, fill = variant)) +
  geom_area(position = "stack", show.legend = FALSE) +
  facet_wrap(~areaName, ncol = 1) +
  scale_fill_manual(values = custom_scale) +
  scale_y_continuous(
    breaks = seq(0, 175, 50),
    limits = c(0, 200),
    position = "right",
    expand = expansion(add = 0)) +
  scale_x_date(
    breaks = lubridate::ymd(c("2021-04-01", "2021-05-01")),
    labels = c("Apr", "May"),
    expand = expansion(add = 0)) +
  theme_void() +
  custom_theme

# 15 plot
p1 <- ggplot(ft_15, aes(x = date, y = rate, fill = variant)) +
  geom_area(position = "stack", show.legend = FALSE) +
  facet_wrap(~areaName, ncol = 5, scales = "free_x") +
  scale_fill_manual(values = custom_scale) +
  scale_y_continuous(
    breaks = seq(0, 175, 50),
    limits = c(0, 175),
    oob = scales::squish,
    position = "left",
    expand = expansion(add = 0)) +
  scale_x_date(
    breaks = lubridate::ymd(c("2021-04-01", "2021-05-01")),
    labels = c("Apr", "May"),
    expand = expansion(add = 0)) +
  theme_void() +
  custom_theme


patch_layout <- c(
  area(t = 1, l = 1, b = 3, r = 5),
  area(t = 1, l = 6, b = 2),
  area(t = 3, l = 6),
  area(t = 1, l = 7, b = 3, r = 8)
)

p1 + p2 + p3 + p4 + plot_layout(design = patch_layout) + 
  plot_annotation(
    title = "Many areas of England are now seeing resurgences driven by B.1.617.2",
    subtitle = "Weekly cases per 100k people, by variant",
    caption = paste(
      "@mattkerlogue after @jburnmurdoch (for The Financial Times)",
      "Local authority cases by variant estimated by applying proportions of sequenced samples to total cases.",
      "Sources: Sanger Institute and Public Health England",
      sep = "\n"),
    theme = custom_theme
    )

