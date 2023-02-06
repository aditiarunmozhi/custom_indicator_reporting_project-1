#merge with mech ref file
mech_complete_clean_data <- left_join(complete_clean_data, mech_id_ref_table_fy23, by = "country") %>%
  relocate(mech_code:mech_name, .after = "reportingperiod")

#data validation with infolink
#kp data
country_kp_count <- mech_complete_clean_data %>% filter(population %in% keypop & indicator %in% c("TX_NEW_VERIFY","TX_RTT_VERIFY","PrEP_OFFER","PrEP_NEW_VERIFY","PrEP_CT_VERIFY")) %>% 
  dplyr::group_by(reportingperiod, country, indicator, ) %>% dplyr::summarize(value = sum(value))

country_kp_snapshot <- mech_complete_clean_data %>% filter(population== "MSM" & indicator %in% c("TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_PVLS_ELIGIBLE")) %>% 
  dplyr::group_by(reportingperiod, country, indicator, population) %>% dplyr::summarize(value = sum(value))

#non-kp data
country_age_sex_count <- mech_complete_clean_data %>% filter(!(population %in% keypop) & indicator == "TX_NEW_VERIFY") %>% 
  dplyr::group_by(reportingperiod, country, indicator) %>% dplyr::summarize(value = sum(value))

country_age_sex_snapshot <- mech_complete_clean_data %>% filter(!(population %in% keypop) & indicator == "TX_CURR_VERIFY") %>% 
  dplyr::group_by(reportingperiod, country, indicator) %>% dplyr::summarize(value = sum(value))
