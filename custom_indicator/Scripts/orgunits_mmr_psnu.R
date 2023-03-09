#psnu level 7
table(mmr$orgunit_level)

mmr_8_clean <- mmr %>% filter(orgunit_level == 8) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)
mmr_7_clean <- mmr %>% filter(orgunit_level == 7) %>% mutate(psnu = orgunit, psnu_uid = orgunituid) %>% select(-c(contains("parent")))

mmr_merge_psnu <- bind_rows(mmr_8_clean, mmr_7_clean)
