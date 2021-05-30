library(tidyverse)
library(patchwork)

### Get data ----

# Sanger institute data
sanger_url <- "https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv"
sanger_data <- read_tsv(sanger_url)

# use latest estimates for most recent weeks
sanger_data_max <- sanger_data %>%
  filter(WeekEndDate == max(WeekEndDate))

sanger_extra <- bind_rows(
  sanger_data,
  sanger_data_max %>% mutate(WeekEndDate = lubridate::ymd("2021-05-22")),
  sanger_data_max %>% mutate(WeekEndDate = lubridate::ymd("2021-05-29")),
)

# PHE data
phe_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv"
phe_data <- read_csv(phe_url)

# Population data
nomis_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100"

nomis_data <- read_csv(nomis_url)

population_data <- nomis_data %>%
  transmute(areaCode = GEOGRAPHY_CODE,
         population = as.numeric(OBS_VALUE))



### Rates for variants ----

# Get weekly rate for each variant
sanger_rates <- sanger_extra %>%
  arrange(LTLA, WeekEndDate) %>%
  group_by(LTLA, WeekEndDate) %>%
  mutate(total_cases = sum(Count),
         rate = Count/total_cases) %>%
  ungroup()

# Select the Indian variant (B.1.617.2)
b16172_incidence <- sanger_rates %>%
  filter(Lineage == "B.1.617.2") %>%
  select(week_date = WeekEndDate,
         areaCode = LTLA,
         b16172_prop = rate)

### Local area data ----

# calculate rolling sum
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

# filter to recent data
# calculate reference week
# merge variant data
# calculate case estimates
# calculate per 100k estimates
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

### Plotting ----

# FT plot authorities
ft_group <- c("Bedford", "Blackburn with Darwen", "Bolton", "Burnley", "Bury", 
              "Central Bedfordshire", "Croydon", "Ealing", "Hillingdon", 
              "Hounslow", "Kirklees", "Leicester", "Luton", "Manchester", 
              "North Tyneside", "Nottingham", "Reading",
              "Rossendale", "Sefton")

# create plotting dataset
ft_group_data <- recent_data %>%
  filter(areaName %in% ft_group) %>%
  select(date, areaName, b16172 = b16172_rate, all_other = other_rate) %>%
  pivot_longer(cols = c(b16172, all_other), names_to = "variant", values_to = "rate") %>%
  mutate(variant = as_factor(variant))

# data for Bedford plot
ft_bedford <- ft_group_data %>%
  filter(areaName == "Bedford")

# data for Blackburn and Bolton plots
ft_blackburn_bolton <- ft_group_data %>%
  filter(areaName == "Blackburn with Darwen" | areaName == "Bolton") %>%
  mutate(areaName = str_remove_all(areaName, " with Darwen"))

# data for Sefton plot
ft_sefton <- ft_group_data %>%
  filter(areaName == "Sefton")

# data for all other authorities
ft_15 <- ft_group_data %>%
  filter(
    str_detect(areaName,
               "^Bedford$|^Blackburn with Darwen$|^Bolton$|^Sefton$",
               negate = TRUE)) %>%
  mutate(areaName = str_replace_all(areaName, "Bedfordshire", "Beds."))

# create a theme based on FT colour swatch
# https://registry.origami.ft.com/components/o-colors@5.3.1?brand=master
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
    margin = margin(t = 3, b = 3))
)

# create fill scale based on FT colour swatch
custom_scale <- c("b16172" = "#CC0000", "all_other" = "#E6D9CE")

# Blackburn and Bolton plots
p4 <- ggplot(ft_blackburn_bolton, aes(x = date, y = rate, fill = variant)) +
  geom_area(position = "stack", show.legend = FALSE) +
  facet_wrap(~areaName, ncol = 2) +
  scale_fill_manual(values = custom_scale) +
  scale_y_continuous(
    breaks = seq(0, 600, 50),
    limits = c(0, 660),
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
    breaks = seq(0, 400, 50),
    limits = c(0, 430),
    position = "right",
    expand = expansion(add = 0)) +
  scale_x_date(
    breaks = lubridate::ymd(c("2021-04-01", "2021-05-01")),
    labels = c("Apr", "May"),
    expand = expansion(add = 0)) +
  theme_void() +
  custom_theme

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

# create a patchwork layout
patch_layout <- c(
  area(t = 1, l = 1, b = 3, r = 5),
  area(t = 1, l = 6, b = 2),
  area(t = 3, l = 6),
  area(t = 1, l = 7, b = 3, r = 8)
)

# check patchwork
plot(patch_layout)

# combine plots, apply layout and annotations
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


# use separate facets for each row

patch_layout2 <- c(
  area(t = 1, l = 1, b = 1, r = 5),
  area(t = 2, l = 1, b = 2, r = 5),
  area(t = 3, l = 1, b = 3, r = 6),
  area(t = 1, l = 6, b = 2),
  area(t = 1, l = 7, b = 3, r = 8)
)

plot(patch_layout2)

ft_row1 <- ft_group_data %>%
  filter(str_detect(areaName, "Burnley|Bury|Bedfordshire|Croydon|Ealing")) %>%
  mutate(areaName = str_replace(areaName, "Bedfordshire", "Beds."))

ft_row2 <- ft_group_data %>%
  filter(str_detect(areaName, "Hillingdon|Hounslow|Kirklees|Leicester|Luton"))

ft_row3 <- ft_group_data %>%
  filter(
    str_detect(
      areaName, 
      "Manchester|North Tyneside|Nottingham|Reading|Rossendale|Sefton"))

row1_plot <- ggplot(ft_row1, aes(x = date, y = rate, fill = variant)) +
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

row2_plot <- ggplot(ft_row2, aes(x = date, y = rate, fill = variant)) +
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

row3_plot <- ggplot(ft_row3, aes(x = date, y = rate, fill = variant)) +
  geom_area(position = "stack", show.legend = FALSE) +
  facet_wrap(~areaName, ncol = 6, scales = "free_x") +
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

plot(patch_layout2)

row1_plot + row2_plot + row3_plot + p2 + p4 + 
  plot_layout(design = patch_layout2) + 
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

