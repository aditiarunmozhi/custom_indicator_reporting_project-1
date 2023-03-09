#psnu level 5
table(bwa$orgunit_level)

bwa_orgs_clean <- bwa_5_6 %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

bwa_level_5 <- bwa_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)
bwa_level_6 <- bwa_orgs_clean %>% filter(orgunit_level == 6) %>%
  rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid, orgunit_6_uid = orgunit_uid, orgunit_6 = orgunit_name) %>%
  select(-orgunit_level)

bwa_orgunit_table <- full_join(bwa_level_5, bwa_level_6, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all") %>% 
  select(sort(colnames(.))) %>%
  select(orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

bwa_merge_psnu <- bwa %>% rename(psnu = orgunit_parent, psun_uid = orgunit_parent_uid)
