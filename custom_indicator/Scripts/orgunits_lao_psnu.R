#transform and create table
lao_orgs_clean <- lao_orgs %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

lao_level_5 <- lao_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)
lao_level_6 <- lao_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)
lao_level_7 <- lao_orgs_clean %>% filter(orgunit_level == 7) %>%
  rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid, orgunit_7_uid = orgunit_uid, orgunit_7 = orgunit_name) %>%
  select(-orgunit_level)

lao_orgunit_table <- full_join(lao_level_5, lao_level_6, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all") %>%
  full_join(lao_level_7, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% select(sort(colnames(.))) %>%
  select(orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

#merge with data
lao_clean <- lao %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

lao_merge_psnu <- left_join(lao_clean, lao_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)
