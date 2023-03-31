#df orgs using purr package
# obtain list of countries from cleaned infolink data ---------------------
countries <- complete_clean_data %>% group_by(country) %>% 
  mutate(country = recode(country, "DRC" = "Democratic Republic of the Congo")) %>%
  summarise(.groups = "drop")
countries <- c(countries$country)

df_orgs <- map_df(countries, orgs_func)

lao_orgs <- df_orgs %>% filter(regionorcountry_name == "Laos")
