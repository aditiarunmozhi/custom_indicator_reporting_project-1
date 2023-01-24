snapshot_indicators <- c("TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_PVLS_ELIGIBLE")
site_type_indicators <- c("TX_NEW_VERIFY","TX_CURR_VERIFY","TX_PVLS_VERIFY")

#KP disaggs cleaning
kp_disaggs_counts <- read.csv("Data/kp_disaggs_counts_fy23_q1.csv")

kp_disaggs_counts_clean <- kp_disaggs_counts %>%
  rename(indicator = Data.Element.Short.Name, population = All.Target.Populations, otherdisaggregate = PEPFAR, value = Value) %>%
  unite(reportingperiod, c("Fiscal.Year.Short.Name","Fiscal.Quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom","keypop"), sep = "[ ]") %>%
  mutate(keypop = if_else(str_detect(numdenom, "P"), "(KP)", as.character(keypop)),
         numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         population = recode(population,
                "Female Sex Workers" = "FSW",
                "Transgender People" = "TG"),
         otherdisaggregate = recode(otherdisaggregate,
                                 "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                 "PEPFAR Supported Site" = "PEPFAR"),
         filter = case_when(indicator %in% snapshot_indicators & Fiscal.Month.Number != 3 ~ "DEL",
                            TRUE ~ "KEEP"),
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate))) %>%
  filter(filter == "KEEP")
  select(-Month.Name, -Fiscal.Month.Number, -filter)
  
  #group_by(Country, SNU.1, SNU.2, SNU.3, SNU.4, SNU.1.ID, SNU.2.ID, SNU.3.ID, SNU.4.ID, indicator, numdenom, population,
  #         otherdisaggregate, reportingperiod) %>%
  #summarise_at(value = sum(value))

#Age and Sex cleaning
age_sex_counts <- read.csv("Data/age_sex_counts_fy23_q1.csv")

age_sex_counts_clean <- age_sex_counts %>%
  rename(indicator = Data.Element.Short.Name, otherdisaggregate = PEPFAR, value = Value) %>%
  unite(reportingperiod, c("Fiscal.Year.Short.Name","Fiscal.Quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom","pop"), sep = "[ ]") #%>%
  mutate(pop = if_else(str_detect(numdenom, "P"), "(KP)", as.character(keypop)), #change these to account for GP and PP
         numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"), # no (N) pr (D) so crosscheck with CIRG long table for note down which is D and N
         otherdisaggregate = recode(otherdisaggregate,
                                    "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                    "PEPFAR Supported Site" = "PEPFAR"), 
         filter = case_when(indicator %in% snapshot_indicators & Fiscal.Month.Number != 3 ~ "DEL",
                            TRUE ~ "KEEP"),
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate))) %>%
  filter(filter == "KEEP") %>%
  select(-Month.Name, -Fiscal.Month.Number, -filter)

  
age_sex_snapshot <- read.csv("Data/age_sex_snapshots_fy23_q1.csv")
  
