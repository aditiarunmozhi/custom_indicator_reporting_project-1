#psnu level 6
table(npl$orgunit_level)

npl_7_clean <- npl %>% filter(orgunit_level == 7) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)
npl_6_clean <- npl %>% filter(orgunit_level == 6) %>% mutate(psnu = orgunit, psnu_uid = orgunituid) %>% select(-c(contains("parent")))

npl_merge_psnu <- bind_rows(npl_7_clean, npl_6_clean)