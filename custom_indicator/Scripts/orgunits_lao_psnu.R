#psnu level 4

lao_merge_psnu <- lao %>% mutate(psnu = "Laos", psnu_uid = "PcXTNoVUrUc") %>% select(-c(contains("parent")))

#data validation with infolink
lao_data_check <- lao_merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))