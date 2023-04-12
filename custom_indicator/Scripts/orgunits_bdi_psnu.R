#psnu level 4
table(bdi$orgunit_level)

bdi_orgs_clean <- orgunit_clean(df_orgs$bdi_orgs)

bdi_level_5 <- orgunit_level_sep(bdi_orgs_clean, 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

#merge with data
bdi_clean <- bdi %>% rename(orgunit_4_uid = orgunit_parent_uid) %>% select(-c(orgunit_parent))

bdi_merge_psnu <- bdi %>% rename(psnu = orgunit_parent) %>% select(-c(orgunit_parent_uid))
 