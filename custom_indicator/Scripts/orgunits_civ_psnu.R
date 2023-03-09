#psnu level 5
table(civ$orgunit_level) #contains both 6 and 7

#transform and create table
civ_orgs_clean <- civ_6_7 %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

civ_level_6 <- civ_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)
civ_level_7 <- civ_orgs_clean %>% filter(orgunit_level == 7) %>%
  rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid, orgunit_7_uid = orgunit_uid, orgunit_7 = orgunit_name) %>%
  select(-orgunit_level)

civ_orgunit_table <- full_join(civ_level_6, civ_level_7, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>%
  select(sort(colnames(.)))

#merge with data
civ_7_clean <- civ %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
civ_6_clean <- civ %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

civ_merge_psnu <- left_join(civ_7_clean, civ_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"))) %>% select(-c(contains("orgunit_7"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(civ_6_clean) %>% distinct()

