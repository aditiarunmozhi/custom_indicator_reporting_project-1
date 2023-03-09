#psnu level 4
table(bdi$orgunit_level)

bdi_orgs_clean <- bdi_orgs %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

bdi_level_5 <- bdi_orgs_clean %>% filter(orgunit_level == 5) %>% 
  rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid, orgunit_5_uid = orgunit_uid, orgunit_5 = orgunit_name) %>%
  select(-orgunit_level)

#merge with data
bdi_clean <- bdi %>% rename(orgunit_4_uid = orgunit_parent_uid) %>% select(-c(orgunit_parent))

bdi_merge_psnu <- bdi %>% rename(psnu = orgunit_parent) %>% select(-c(orgunit_parent_uid))
 