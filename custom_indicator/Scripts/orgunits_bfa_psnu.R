#psnu level 5
table(bfa$orgunit_level)
bfa_orgs_clean <- orgunit_clean(df_orgs$bfa_orgs)

bfa_level_5 <- orgunit_level_sep(bfa_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

bfa_level_6 <- orgunit_level_sep(bfa_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

bfa_orgunit_table <- orgunit_table_join(bfa_level_5, bfa_level_6, orgunit_5_uid, orgunit_5)

bfa_merge_psnu <- bfa %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)
