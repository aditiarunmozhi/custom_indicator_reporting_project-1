#has levels 5, 6, and 7 in data and prioritization level is 4
table(tza$orgunit_level)
#transform and create table
tza_orgs_clean <- orgunit_clean(df_orgs$tza_orgs)

tza_level_4 <- orgunit_level_sep(tza_orgs_clean , 4, orgunit_3, orgunit_3_uid, orgunit_4, orgunit_4_uid)

tza_level_5 <- orgunit_level_sep(tza_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

tza_level_6 <- orgunit_level_sep(tza_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

tza_level_7 <- orgunit_level_sep(tza_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

tza_orgunit_table <- orgunit_table_join(tza_level_4, tza_level_5, orgunit_4_uid, orgunit_4)
tza_orgunit_table <- orgunit_table_join(tza_orgunit_table, tza_level_6, orgunit_5_uid, orgunit_5)

#merge with data
tza_7_clean <- tza %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
tza_6_clean <- tza %>% filter(orgunit_level == 6) %>% rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid)
tza_5_clean <- tza %>% filter(orgunit_level == 5) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

tza_7_merge <- left_join(tza_7_clean, tza_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)
tza_6_merge <- left_join(tza_6_clean, tza_orgunit_table, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)

tza_merge_psnu <- bind_rows(tza_7_merge, tza_6_merge, tza_5_clean)
