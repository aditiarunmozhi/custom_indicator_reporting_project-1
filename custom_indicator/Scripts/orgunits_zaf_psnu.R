#psnu level 5
table(zaf$orgunit_level)

zaf_merge_psnu <- zaf %>% mutate(psnu = orgunit, psnu_uid = orgunituid) %>% select(-c(contains("parent")))
