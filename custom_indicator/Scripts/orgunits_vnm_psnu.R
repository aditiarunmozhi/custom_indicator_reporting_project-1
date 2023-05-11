# psnu level 4
table(vnm$orgunit_level)
psnu_level <- orgunit_levels %>% filter(country_iso == "VNM") 

vnm_orgs_clean <- orgunit_clean(df_orgs$vnm_orgs)

vnm_level_4 <- orgunit_level_sep(vnm_orgs_clean , 4, orgunit_3, orgunit_3_uid, orgunit_4, orgunit_4_uid)

vnm_level_5 <- orgunit_level_sep(vnm_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

vnm_level_6 <- orgunit_level_sep(vnm_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

vnm_orgunit_table <- orgunit_table_join(vnm_level_5, vnm_level_6, orgunit_5_uid, orgunit_5)

#merge with data
vnm_6_clean <- vnm %>% filter(orgunit_level == 6) %>% rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid)
vnm_5_clean <- vnm %>% filter(orgunit_level == 5) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

vnm_6_merge <- left_join(vnm_6_clean, vnm_orgunit_table, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_5"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)

vnm_merge_psnu <- bind_rows(vnm_6_merge, vnm_5_clean)
