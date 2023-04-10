#has levels 7 and 6 in data and prioritization level is 5
#transform and create table
tza_orgs_clean <- tza_orgs %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

tza_level_5 <- tza_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)
tza_level_6 <- tza_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)
tza_level_7 <- tza_orgs_clean %>% filter(orgunit_level == 7) %>%
  rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid, orgunit_7_uid = orgunit_uid, orgunit_7 = orgunit_name) %>%
  select(-orgunit_level)

tza_orgunit_table <- full_join(tza_level_5, tza_level_6, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all") %>%
  full_join(tza_level_7, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% select(sort(colnames(.))) %>%
  select(orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

#merge with data
tza_7_clean <- tza %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
tza_6_clean <- tza %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

tza_merge_psnu <- left_join(tza_7_clean, tza_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(tza_6_clean)
