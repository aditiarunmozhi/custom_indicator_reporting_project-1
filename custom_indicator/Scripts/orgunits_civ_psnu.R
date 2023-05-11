#psnu level 4
table(civ$orgunit_level) #contains both 6 and 7

civ_orgs_clean <- orgunit_clean(df_orgs$civ_orgs)

civ_level_4 <- orgunit_level_sep(civ_orgs_clean , 4, orgunit_3, orgunit_3_uid, orgunit_4, orgunit_4_uid)

civ_level_5 <- orgunit_level_sep(civ_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

civ_level_6 <- orgunit_level_sep(civ_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

civ_level_7 <- orgunit_level_sep(civ_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

civ_orgunit_table <- orgunit_table_join(civ_level_4, civ_level_5, orgunit_4_uid, orgunit_4)
civ_orgunit_table <- orgunit_table_join(civ_orgunit_table, civ_level_6, orgunit_5_uid, orgunit_5)

#merge with data
civ_7_clean <- civ %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
civ_6_clean <- civ %>% filter(orgunit_level == 6) %>% rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid)

civ_7_merge <- left_join(civ_7_clean, civ_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)
civ_6_merge <- left_join(civ_6_clean, civ_orgunit_table, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)

civ_merge_psnu <- bind_rows(civ_7_merge, civ_6_merge)


# future purr usage maybe?
# psnu_level <- orgunit_levels %>% filter(country_iso == "CIV") 
# lev <- (psnu_level$prioritization[1]:max(civ$orgunit_level))
# 
# civ_orgs <- map(lev, orgunit_level_sep(civ_orgs_clean))
# 
# civ_orgs_clean <- orgunit_clean(df_orgs$civ_orgs)
# 
# civ_level_4 <- orgunit_level_sep(civ_orgs_clean , 4)
# 
# civ_level_5 <- orgunit_level_sep(civ_orgs_clean , 5)
# 
# civ_level_6 <- orgunit_level_sep(civ_orgs_clean , 6)
# 
# civ_level_7 <- orgunit_level_sep(civ_orgs_clean , 7)