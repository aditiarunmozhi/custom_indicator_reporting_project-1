#transform and create table
lbr_orgs_clean <- orgunit_clean(df_orgs$lbr_orgs)

lbr_level_5 <- orgunit_level_sep(lbr_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

lbr_level_6 <- orgunit_level_sep(lbr_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

lbr_level_7 <- orgunit_level_sep(lbr_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

lbr_orgunit_table <- orgunit_table_join(lbr_level_5, lbr_level_6, orgunit_5_uid, orgunit_5)

#merge with data
lbr_7_clean <- lbr %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
lbr_6_clean <- lbr %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

lbr_merge_psnu <- left_join(lbr_7_clean, lbr_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(lbr_6_clean)
