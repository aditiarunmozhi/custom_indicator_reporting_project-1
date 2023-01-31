#merge with mech ref file
mech_complete_clean_data <- left_join(complete_clean_data, mech_id_ref_table_fy23, by = c("Country" = "country")) %>%
  relocate(mech_code:mech_name, .after = "reportingperiod")

#data validation with infolink
#kp data
country_kp <- mech_complete_clean_data %>% filter(population %in% keypop & indicator %in% c("TX_NEW_VERIFY","TX_RTT_VERIFY","PrEP_OFFER","PrEP_NEW_VERIFY","PrEP_CT_VERIFY")) %>% 
  group_by(reportingperiod, Country, indicator) %>% summarize(value = sum(value))

#non-kp data

country_age_sex <- mech_complete_clean_data %>% filter(Country == "Burkina Faso" & indicator == "TX_NEW_VERIFY") %>% 
  group_by(Country) %>% summarize(value = sum(value))
