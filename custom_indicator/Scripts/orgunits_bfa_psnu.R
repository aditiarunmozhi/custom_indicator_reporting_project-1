#psnu level 5
table(bfa$orgunit_level)
bfa_orgs_clean <- bfa_5_6 %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

bfa_level_5 <- bfa_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)
bfa_level_6 <- bfa_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)

bfa_orgunit_table <- full_join(bfa_level_5, bfa_level_6, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all") %>% 
  select(sort(colnames(.))) %>%
  select(orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

bfa_merge_psnu <- bfa %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

bfa_data_check <- bfa_merge_psnu %>% group_by(reportingperiod, psnu, psnu_uid, indicator, age, sex) %>% summarize(value = sum(value))

