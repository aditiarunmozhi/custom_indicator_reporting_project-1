#psnu level 6
table(gha$orgunit_level)

#transform and create table
gha_orgs_clean <- orgunit_clean(df_orgs$gha_orgs)

gha_level_7 <- orgunit_level_sep(gha_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

gha_level_8 <- orgunit_level_sep(gha_orgs_clean , 8, orgunit_7, orgunit_7_uid, orgunit_8, orgunit_8_uid)

gha_orgunit_table <- orgunit_table_join(gha_level_7, gha_level_8, orgunit_7_uid, orgunit_7)

#merge with data
gha_clean <- gha %>% rename(orgunit_7 = orgunit_parent, orgunit_7_uid = orgunit_parent_uid)

gha_merge_psnu <- left_join(gha_clean, gha_orgunit_table, by = join_by(orgunit_7_uid, orgunit_7), multiple = "all") %>% 
  select(-c(contains("orgunit_7"))) %>% select(-c(contains("orgunit_8"))) %>%  distinct() %>%
  rename(psnu = orgunit_6, psnu_uid = orgunit_6_uid)
