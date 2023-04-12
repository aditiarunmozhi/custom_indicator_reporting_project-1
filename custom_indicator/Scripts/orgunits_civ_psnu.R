#psnu level 5
table(civ$orgunit_level) #contains both 6 and 7

#transform and create table
civ_orgs_clean <- orgunit_clean(df_orgs$civ_orgs)

civ_level_6 <- orgunit_level_sep(civ_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

civ_level_7 <- orgunit_level_sep(civ_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

civ_orgunit_table <- orgunit_table_join(civ_level_6, civ_level_7, orgunit_6_uid, orgunit_6)

#merge with data
civ_7_clean <- civ %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
civ_6_clean <- civ %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

civ_merge_psnu <- left_join(civ_7_clean, civ_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"))) %>% select(-c(contains("orgunit_7"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(civ_6_clean) %>% distinct()

