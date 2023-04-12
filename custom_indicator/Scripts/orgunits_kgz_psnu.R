#transform and create table
kgz_orgs_clean <- orgunit_clean(df_orgs$kgz_orgs)

kgz_level_5 <- orgunit_level_sep(kgz_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

kgz_level_6 <- orgunit_level_sep(kgz_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

kgz_level_7 <- orgunit_level_sep(kgz_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

kgz_orgunit_table <- orgunit_table_join(kgz_level_5, kgz_level_6, orgunit_5_uid, orgunit_5)

#merge with data
kgz_clean <- kgz %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

kgz_merge_psnu <- left_join(kgz_clean, kgz_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)
