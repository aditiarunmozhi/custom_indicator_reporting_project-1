#psnu level 5
table(tjk$orgunit_level)

#transform and create table
tjk_orgs_clean <- orgunit_clean(df_orgs$tjk_orgs)

tjk_level_5 <- orgunit_level_sep(tjk_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

tjk_level_6 <- orgunit_level_sep(tjk_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

tjk_level_7 <- orgunit_level_sep(tjk_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

tjk_orgunit_table <- orgunit_table_join(tjk_level_5, tjk_level_6, orgunit_5_uid, orgunit_5)

#merge with data
tjk_clean <- tjk %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

tjk_merge_psnu <- left_join(tjk_clean, tjk_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)