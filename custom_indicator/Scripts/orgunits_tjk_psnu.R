#transform and create table
tjk_orgs_clean <- tjk_orgs %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

level_5 <- tjk_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)
level_6 <- tjk_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)
level_7 <- tjk_orgs_clean %>% filter(orgunit_level == 7) %>%
  rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid, orgunit_7_uid = orgunit_uid, orgunit_7 = orgunit_name) %>%
  select(-orgunit_level)

tjk_orgunit_table <- full_join(level_5, level_6, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all") %>%
  full_join(level_7, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% select(sort(colnames(.))) %>%
  select(orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

#merge with data
tjk_clean <- tjk %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

tjk_merge_psnu <- left_join(tjk_clean, tjk_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)