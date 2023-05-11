#psnu level 4 (country level)
table(tha$orgunit_level)
psnu_level <- orgunit_levels %>% filter(country_iso == "THA") 

tha_orgs_clean <- orgunit_clean(df_orgs$tha_orgs)

tha_level_4 <- orgunit_level_sep(tha_orgs_clean , 4, orgunit_3, orgunit_3_uid, orgunit_4, orgunit_4_uid)

tha_level_5 <- orgunit_level_sep(tha_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

tha_level_6 <- orgunit_level_sep(tha_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

tha_level_7 <- orgunit_level_sep(tha_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

tha_orgunit_table <- orgunit_table_join(tha_level_4, tha_level_5, orgunit_4_uid, orgunit_4)
tha_orgunit_table <- orgunit_table_join(tha_orgunit_table, tha_level_6, orgunit_5_uid, orgunit_5)

#merge with data
tha_clean <- tha %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

tha_7_merge <- left_join(tha_clean, tha_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)