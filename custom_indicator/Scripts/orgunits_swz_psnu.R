#psnu level 4
table(swz$orgunit_level)

swz_merge_psnu <- swz %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)
