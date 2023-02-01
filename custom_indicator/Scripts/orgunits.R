tanz_info <- complete_clean_data %>% clean_names() %>% filter(country=="Tanzania")

tanz_org <- read_csv("https://www.datim.org/api/sqlViews/DataExchOUs/data.csv?var=OU:TZA")
