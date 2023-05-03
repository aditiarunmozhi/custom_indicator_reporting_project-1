#psnu level 5
#transform and create table
mli_orgs_clean <- orgunit_clean(df_orgs$mli_orgs)

mli_level_5 <- orgunit_level_sep(mli_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

mli_level_6 <- orgunit_level_sep(mli_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

mli_level_7 <- orgunit_level_sep(mli_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

mli_orgunit_table <- orgunit_table_join(mli_level_5, mli_level_6, orgunit_5_uid, orgunit_5)

#merge with data
mli_clean <- mli %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

mli_merge_psnu <- left_join(mli_clean, mli_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)
