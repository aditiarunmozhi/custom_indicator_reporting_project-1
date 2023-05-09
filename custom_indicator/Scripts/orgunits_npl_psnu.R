#psnu level 5
table(npl$orgunit_level)

npl_orgs_clean <- orgunit_clean(df_orgs$npl_orgs)

npl_level_5 <- orgunit_level_sep(npl_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

npl_level_6 <- orgunit_level_sep(npl_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

npl_level_7 <- orgunit_level_sep(npl_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

npl_orgunit_table <- orgunit_table_join(npl_level_5, npl_level_6, orgunit_5_uid, orgunit_5)

#merge with data
npl_7_clean <- npl %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
npl_6_clean <- npl %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

npl_merge_psnu <- left_join(npl_7_clean, npl_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(npl_6_clean)
