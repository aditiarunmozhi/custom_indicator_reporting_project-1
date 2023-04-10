
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# tza_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
#   select(snu_1:snu_4_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
tza_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
  clean_names() %>% print()
#currently commented out because we are merging at a simpler level for now as we test the process




# get orgunit levels to match and join ------------------------------------
tza7op <- df_orgs$tza_orgs %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
tza7uid <- c(tza7op$orgunit_uid)

tza6op <- df_orgs$tza_orgs %>% filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)
tza6uid <- c(tza6op$orgunit_uid)



################################################################################

level7 <- tza_info %>% filter(snu_4!="")
# for level 4 that match_level 7, use snu_4_id -----------------------
tza7<- tza_info %>% filter(snu_4_id %in% tza7uid, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% 
  rename(orgunit_uid  = snu_4_id) %>% inner_join(tza7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
glimpse()
nrow(tza7)

# for level 4 that doesn't match level 7, try match by snu_4 name --------
tza7m1 <- tza_info %>% filter(!snu_4_id %in% tza7uid, snu_4 != "") %>% 
  rename(orgunit_name = snu_4) %>%
        glimpse()
nrow(tza7m1)

#identify and resolve any failed matches
tza7m1 %>%
  anti_join(tza7op) 
#resolve discrepancies

#now match
tza7m <- tza7m1 %>% inner_join(tza7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
tza7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)

#check for unmatched
tza7m %>% filter(is.na(orgunituid)) 

count(level7) - count(tza7) - count(tza7m)
##############################################################################

# check for missing data at snu_level_3
tza6 <- tza_info %>% filter(snu_3 == "", snu_4 == "") %>% print()

##############################################################################


#excluding the above

# at snu_level_match to metadata level, tz 6 ------------------------------
tza6<- tza_info %>% filter(snu_3_id %in% tza6uid, snu_4 == "") %>% 
  rename(orgunit_uid = snu_3_id) %>% inner_join(tza6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  select(-snu_1_id:-snu_2_id, -snu_4, -snu_4_id)
nrow(tza6)

# for level 3 that doesn't match level 6, match by snu_3 to snu_3_id from metadata ----------
tza6m1 <- tza_info %>% filter(!snu_3_id %in% tza6uid, snu_4 == "")
nrow(tza6m1) 

#identify any failed matches, but do not save --> resolve in later stages
tza6m1 %>%
  rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>%
  #add in second join criteria for parent id to statement below
  anti_join(tza6op, by = c("orgunit_name","orgunit_parent")) %>%
  #resolve
  mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>%
  print()

tza6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")
#one orgunit has tow orgunit_uids, arbitrarily remove one below
tza6op <- tza6op %>% filter(orgunit_uid != "gxidqz4F8pv")
#check if resolved
tza6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")


#run inner join
tza6m <- tza6m1 %>% 
  rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>% 
  #fix the issue noted above for TZ
  mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>% 
  #add in second join criteria for parent id to statement below
  inner_join(tza6op, by = c("orgunit_name", "orgunit_parent"), na_matches = "never") %>% # or inner if there are non-matches
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) %>% 
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching

#1 row double matched initially but no longer --> resolved above
test <- tza6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit, orgunit_parent) %>% group_by_all() %>% 
  filter(n()>1)
test



tza <- bind_rows(tza7, tza7m, tza6, tza6m) %>% 
select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(tza) - nrow(tza_info)
glimpse(tza6m)

#later bind country dfs together