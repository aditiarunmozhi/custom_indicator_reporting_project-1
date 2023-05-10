#psnu level 4

lao_merge_psnu <- lao %>% mutate(psnu = "Laos", psnu_uid = "PcXTNoVUrUc") %>% select(-c(contains("parent"))) #ideally this would be pulled from the reference table in case the value changes

#data validation with infolink
lao_data_check <- lao_merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))

# read_csv("https://www.datim.org/api/sqlViews/W5RIpSXU1DP/data.csv")
# check psnu ref tables, sometimes wrong?