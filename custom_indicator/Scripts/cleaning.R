snapshot_indicators <- c("TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_PVLS_ELIGIBLE")
site_type_indicators <- c("TX_NEW_VERIFY","TX_CURR_VERIFY","TX_PVLS_VERIFY")
last_month <- c("Dec","Mar","Jun","Sep")
indicators_less_than_20 <- c("TX_NEW_VERIFY","TX_RTT_VERIFY","TX_CURR_VERIFY","TX_PVLS_VERIFY", "PrEP_OFFER","PrEP_CT_VERIFY", "PrEP_NEW_VERIFY")
less_than_20 <- c("<01","01-04","05-09","10-14","15-19")
keypop <- c("FSW","MSM","PWID","TG","Prison")

#KP disaggs cleaning
kp_disaggs_counts <- read.csv("Data/kp_disaggs_counts_fy23_q1.csv")

kp_disaggs_counts_clean <- kp_disaggs_counts %>%
  rename(indicator = Data.Element.Short.Name, population = All.Target.Populations, otherdisaggregate = PEPFAR, value = Value) %>%
  unite(reportingperiod, c("Fiscal.Year.Short.Name","Fiscal.Quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom"), sep = "[ ]") %>%
  mutate(numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         population = recode(population,
                "Female Sex Workers" = "FSW",
                "Transgender People" = "TG"),
         population = case_when(!(population %in% keypop) ~ "Non-KP GP", TRUE ~ as.character(population)),
         otherdisaggregate = recode(otherdisaggregate,
                                 "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                 "PEPFAR Supported Site" = "PEPFAR"),
         filter = case_when(indicator %in% snapshot_indicators & !(Month.Name %in% last_month) ~ "DEL", TRUE ~ "KEEP"), # try changing this using subset function
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate))) %>%
  filter(filter == "KEEP") %>%
  select(reportingperiod, Country, contains("SNU"), indicator, otherdisaggregate, population, numdenom, value) %>%
  group_by(reportingperiod, Country,  SNU.1, SNU.2, SNU.3, SNU.4, SNU.1.ID, SNU.2.ID, SNU.3.ID, SNU.4.ID, indicator, otherdisaggregate, population, numdenom, value) %>%
  summarise(value = sum(value))

#Age and Sex cleaning

age_sex_counts <- read.csv("Data/age_sex_counts_fy23_q1.csv")

age_sex_counts_clean <- age_sex_counts %>%
  rename(indicator = Data.Element.Short.Name, otherdisaggregate = PEPFAR, value = Value, age = All.Age.Groups...For.DATIM.entry, sex = Sex) %>%
  unite(reportingperiod, c("Fiscal.Year.Short.Name","Fiscal.Quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom"), sep = "[ ]") %>%
  mutate(numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         otherdisaggregate = recode(otherdisaggregate,
                                    "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                    "PEPFAR Supported Site" = "PEPFAR"),
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate)),
         age = recode(age,
                      "<1" = "<01",
                      "1-4" = "01-04",
                      "5-9" = "05-09",
                      "Age Unknown" = "Unknown Age"),
         age = if_else(indicator %in% indicators_less_than_20 & age %in% less_than_20, "<20", as.character(age))) %>%
  select(reportingperiod, Country, contains("SNU"), indicator, sex, age, otherdisaggregate, numdenom, value)%>%
  group_by(reportingperiod, Country,  SNU.1, SNU.2, SNU.3, SNU.4, SNU.1.ID, SNU.2.ID, SNU.3.ID, SNU.4.ID, indicator, sex, age, otherdisaggregate, numdenom, value) %>%
  summarise(value = sum(value))

age_sex_snapshot <- read.csv("Data/age_sex_snapshots_fy23_q1.csv")
  
age_sex_snapshot_clean <- age_sex_snapshot %>%
  rename(indicator = Data.Element.Short.Name, otherdisaggregate = PEPFAR, value = Value, age = All.Age.Groups...For.DATIM.entry, sex = Sex) %>%
  unite(reportingperiod, c("Fiscal.Year.Short.Name","Fiscal.Quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom"), sep = "[ ]") %>%
  mutate(numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         otherdisaggregate = recode(otherdisaggregate,
                                    "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                    "PEPFAR Supported Site" = "PEPFAR"), 
         filter = case_when(indicator %in% snapshot_indicators & !(Month.Name %in% last_month) ~ "DEL",
                            TRUE ~ "KEEP"), # try changing this using subset function, keep this here
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate)),
         age = recode(age,
                      "<1" = "<01",
                      "1-4" = "01-04",
                      "5-9" = "05-09",
                      "Age Unknown" = "Unknown Age"),
         age = if_else(indicator %in% indicators_less_than_20 & age %in% less_than_20, "<20", as.character(age))) %>%
  filter(filter == "KEEP") %>%
  select(reportingperiod, Country, contains("SNU"), indicator, sex, age, otherdisaggregate, numdenom, value)%>%
  group_by(reportingperiod, Country,  SNU.1, SNU.2, SNU.3, SNU.4, SNU.1.ID, SNU.2.ID, SNU.3.ID, SNU.4.ID, indicator, sex, age, otherdisaggregate, numdenom, value) %>%
  summarise(value = sum(value))

#merge all three files
complete_clean_data <- full_join(age_sex_counts_clean, age_sex_snapshot_clean) %>% 
  bind_rows(kp_disaggs_counts_clean) %>%
  relocate(population, .after = "otherdisaggregate")

  