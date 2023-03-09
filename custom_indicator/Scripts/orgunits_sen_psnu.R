#psnu level 5
table(sen$orgunit_level)

sen_merge_psnu <- sen %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)
