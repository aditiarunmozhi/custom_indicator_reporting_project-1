#transform and create table
kgz_orgs_clean <- kgz_orgs %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

kgz_level_5 <- kgz_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)
kgz_level_6 <- kgz_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)
kgz_level_7 <- kgz_orgs_clean %>% filter(orgunit_level == 7) %>%
  rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid, orgunit_7_uid = orgunit_uid, orgunit_7 = orgunit_name) %>%
  select(-orgunit_level)

kgz_orgunit_table <- full_join(kgz_level_5, kgz_level_6, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all") %>%
  full_join(kgz_level_7, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% select(sort(colnames(.))) %>%
  select(orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

#merge with data
kgz_clean <- kgz %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

kgz_merge_psnu <- left_join(kgz_clean, kgz_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)