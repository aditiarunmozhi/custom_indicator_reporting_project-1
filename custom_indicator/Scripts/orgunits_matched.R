
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
tnz_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
  select(snu_1:snu_4_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
  clean_names() %>% 
  print()

# user purr to create DF for each country, named after each count --------
# tnz_info <- complete_clean_data %>% filter(Country=="Tanzania") %>%
#   clean_names()
#currently commented out because we are merging at a simpler level for now as we test the process




# get orgunit levels to match and join ------------------------------------
tnz7op <- tnz_6_7 %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name)
tnz7 <- c(tnz7op$orgunit_uid )

tnz6op <- tnz_6_7 %>% filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)
tnz6 <- c(tnz6op$orgunit_uid)



# for most level 4 that match_level 7, use snu_4_id -----------------------
tnz7<- tnz_info %>% filter(snu_4_id %in% tnz7, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% rename(orgunit = snu_4, orgunituid = snu_4_id) %>% print()


# for level 4 that doesn't match level 7, match by snu_4 recide sn --------
tnz7m <- tnz_info %>% filter(!snu_4_id %in% tnz7, snu_4 != "") %>% rename(orgunit = snu_4)
nrow(tnz7m)

tnz7m <- tnz7m %>% left_join(tnz7n) %>% # or inner if there are non-matches
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = uid) %>%
  print() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
  
tnz7m %>% filter(is.na(orgunituid)) #check for unmatched


# check for missing data at snu_level_3
tnz6<- tnz_info %>% filter(snu_3 == "", snu_4 == "") %>% print()


# at snu_level_match to metadata level, tz 6 ------------------------------
tnz6<- tnz_info %>% filter(snu_3_id %in% tnz6, snu_4 == "") %>% 
  rename(orgunit = snu_3, orgunituid = snu_3_id) %>% 
  select(-snu_1_id:-snu_2_id, -snu_4, -snu_4_id) %>%
  print()

# for level 3 that doesn't match level 6, match by snu_3 to snu_3_id from metadata ----------
tnz6m <- tnz_info %>% filter(!snu_3_id %in% tnz6, snu_4 == "") %>% print()
nrow(tnz6m) 

tnz6m <- tnz6m %>%
  rename(orgunit = snu_3, parent = snu_2) %>%
  #add in second join criteria for parent id to statement below
  left_join(tz_6n) %>% # or inner if there are non-matches
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = uid) %>%
  print() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#in the case of TZ, there are multiple SNU2s with the same name for snu3. We need to match based on two levels
#to eliminate duplication, but currently our ref file only has 1 level at a time. I submitted a github issue 
#for the grabr package to see if I can find a way to pull the datim metadata table with parent and child levels 
#in same rows 


  tnz6m %>% filter(is.na(orgunituid)) #check for unmatched

  
tnz <- bind_rows(tnz7, tnz7m, tnz6, tnz6m) %>% select(-snu_3, -parent, -snu_4)

#later bind country dfs together
