#psnu level 5
table(mmr$orgunit_level)

mmr_orgs_clean <- orgunit_clean(df_orgs$mmr_orgs)

mmr_level_5 <- orgunit_level_sep(mmr_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

mmr_level_6 <- orgunit_level_sep(mmr_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

mmr_level_7 <- orgunit_level_sep(mmr_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

mmr_level_8 <- orgunit_level_sep(mmr_orgs_clean , 8, orgunit_7, orgunit_7_uid, orgunit_8, orgunit_8_uid)

mmr_orgunit_table <- orgunit_table_join(mmr_level_5, mmr_level_6, orgunit_5_uid, orgunit_5)
mmr_orgunit_table <- orgunit_table_join(mmr_orgunit_table, mmr_level_7, orgunit_6_uid, orgunit_6)

#merge with data
mmr_8_clean <- mmr %>% filter(orgunit_level == 8) %>% rename(orgunit_7 = orgunit_parent, orgunit_7_uid = orgunit_parent_uid)
mmr_7_clean <- mmr %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

mmr_8_merge <- left_join(mmr_8_clean, mmr_orgunit_table, by = join_by(orgunit_7_uid, orgunit_7), multiple = "all") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)
mmr_7_merge <- left_join(mmr_7_clean, mmr_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

mmr_merge_psnu <- bind_rows(mmr_8_merge, mmr_7_merge)
