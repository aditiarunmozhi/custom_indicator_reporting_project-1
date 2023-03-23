#transform and create table
lao_orgs_clean <- lao_orgs %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))

lao_level_5 <- orgunit_level_sep(lao_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)

lao_level_6 <- orgunit_level_sep(lao_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)

lao_level_7 <- orgunit_level_sep(lao_orgs_clean , 7, orgunit_6, orgunit_6_uid, orgunit_7, orgunit_7_uid)

lao_orgunit_table <- orgunit_table_join(lao_level_5, lao_level_6, orgunit_5_uid, orgunit_5)

#merge with data
lao_clean <- lao %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

lao_merge_psnu <- left_join(lao_clean, lao_orgunit_table, by = join_by(orgunit_6_uid, orgunit_6), multiple = "all") %>% 
  select(-c(contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

#data validation with infolink
lao_data_check <- lao_merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))