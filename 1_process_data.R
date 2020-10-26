library(tidyverse)

msoa_cases <- read_csv("https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv")
write_csv(msoa_cases, "source_data/msoa_latest.csv")

msoa_pop_file <- "source_data/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx"

readxl::excel_sheets(msoa_pop_file)
msoa_estimates <- readxl::read_xlsx(msoa_pop_file, sheet = "Mid-2019 Persons")

msoa_pop <- msoa_estimates %>%
  janitor::row_to_names(4) %>%
  .[c(1,7)] %>%
  janitor::clean_names() %>%
  mutate(all_ages = as.numeric(all_ages)) %>%
  rename(msoa11_cd = msoa_code)

convert_na <- function(x, val = -99) {
  x <- as.double(x)
  x <- ifelse(x == val, NA_real_, x)
  return(x)
}

msoa_cases <- msoa_cases %>%
  mutate(across(where(is.numeric), convert_na)) %>%
  select(msoa11_cd, msoa11_hclnm, starts_with("wk"))

proc_oa_files <- function(folder) {

  oa_files <- dir(folder)
  
  z <- tibble(OA11CD = character(), `All Ages` = character())
  
  for (xlsx_file in oa_files) {
    
    path <- file.path(folder, xlsx_file)
    
    x <- readxl::read_xlsx(path, "Mid-2019 Persons")
    
    y <- x[c(1,3)] %>%
      janitor::row_to_names(4)
    
    z <- bind_rows(z, y)
    
  }
    
  return(z)
}

oa_pop <- proc_oa_files("source_data/oa_estimates/")
write_csv(oa_pop, "source_data/oa_pop.csv")

oa_clusters <- read_csv("source_data/2011 OAC Clusters and Names csv v2.csv")

oac_reduced <- oa_clusters[c(1, 10)] %>%
  set_names(c("oa11cd", "subgroup"))

oac_labels <- oa_clusters %>%
  janitor::clean_names() %>%
  select(subgroup_code, subgroup_name) %>%
  distinct() %>%
  arrange(subgroup_code)

oa_lookup <- read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv", 
                      col_types = cols(.default = col_character())) %>%
  janitor::clean_names()


msoa_oa_lookup <- oa_lookup %>%
  janitor::clean_names() %>%
  select(oa11cd, msoa11cd)

oa_data <- oa_pop %>%
  janitor::clean_names() %>%
  mutate(all_ages = as.numeric(all_ages)) %>%
  left_join(oac_reduced, by = "oa11cd") %>%
  left_join(msoa_oa_lookup, by = "oa11cd")

msoa_oac <- oa_data %>%
  group_by(msoa11cd) %>%
  add_count(subgroup, wt = all_ages, name = "subgroup_pop") %>%
  add_count(subgroup, name = "subgroup_oas") %>%
  add_count(msoa11cd, name = "total_oas") %>%
  mutate(total_pop = sum(all_ages)) %>%
  select(-oa11cd, -all_ages) %>%
  distinct() %>% 
  mutate(subgroup_pop_pc = subgroup_pop/total_pop,
         subgroup_oa_pc = subgroup_oas/total_oas,
         score = subgroup_pop_pc * subgroup_oa_pc)
  
msoa_oac_assign <- msoa_oac %>%
  group_by(msoa11cd) %>%
  filter(score == max(score)) %>%
  select(msoa11_cd = msoa11cd, subgroup_code = subgroup) %>%
  left_join(oac_labels, by = "subgroup_code")


msoa_data <- msoa_pop %>%
  inner_join(msoa_cases, by = "msoa11_cd") %>%
  left_join(msoa_oac_assign, by = "msoa11_cd") %>%
  select(msoa11_cd, msoa11_hclnm, subgroup_code, subgroup_name, all_ages, everything())

write_csv(msoa_data, "data/msoa_data.csv")