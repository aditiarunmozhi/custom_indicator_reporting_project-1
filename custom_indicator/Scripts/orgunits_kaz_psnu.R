#psnu level 5
table(kaz$orgunit_level)

#transform and create table
kaz_orgs_clean <- orgunit_clean(df_orgs$kaz_orgs)

kaz_level_5 <- orgunit_level_sep(kaz_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

kaz_level_6 <- orgunit_level_sep(kaz_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

kaz_level_7 <- orgunit_level_sep(kaz_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

kaz_orgunit_table <- orgunit_table_join(kaz_level_5, kaz_level_6, orgunit_5_uid, orgunit_5)

#merge with data
kaz_clean <- kaz %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

kaz_merge_psnu <- left_join(kaz_clean, kaz_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)
