#psnu level 6
table(gha$orgunit_level)

#transform and create table
gha_orgs_clean <- gha_7_8 %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

gha_level_7 <- gha_orgs_clean %>% filter(orgunit_level == 7) %>%
  rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid, orgunit_7_uid = orgunit_uid, orgunit_7 = orgunit_name) %>%
  select(-orgunit_level)
gha_level_8 <- gha_orgs_clean %>% filter(orgunit_level == 8) %>%
  rename(orgunit_7 = orgunit_parent, orgunit_7_uid = orgunit_parent_uid, orgunit_8_uid = orgunit_uid, orgunit_8 = orgunit_name) %>%
  select(-orgunit_level)

gha_orgunit_table <- full_join(gha_level_7, gha_level_8, by = join_by(orgunit_7_uid, orgunit_7), multiple = "all") %>%
  select(sort(colnames(.)))

#merge with data
gha_clean <- gha %>% rename(orgunit_7 = orgunit_parent, orgunit_7_uid = orgunit_parent_uid)

gha_merge_psnu <- left_join(gha_clean, gha_orgunit_table, by = join_by(orgunit_7_uid, orgunit_7), multiple = "all") %>% 
  select(-c(contains("orgunit_7"))) %>% select(-c(contains("orgunit_8"))) %>%  distinct() %>%
  rename(psnu = orgunit_6, psnu_uid = orgunit_6_uid)
