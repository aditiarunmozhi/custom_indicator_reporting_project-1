
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
tanz_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
  select(snu_1:snu_4_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
  clean_names() %>% 
  print()

# user purr to create DF for each country, named after each count --------
# tanz_info <- complete_clean_data %>% filter(Country=="Tanzania") %>%
#   clean_names()
#currently commented out because we are merging at a simpler level for now as we test the process


# obtain DATIM metadata ---------------------------------------------------
load_secrets()

#obtain ou uid and save
tz <- grabr::get_ouuids(
  add_details = FALSE,
  datim_user(),
  datim_pwd(),
  baseurl = "https://final.datim.org/"
) %>% filter(operatingunit == "Tanzania") %>% 
  select(uid) %>% as.list(uid)

ouuid <- tz

grabr::get_levels(datim_user(), datim_pwd(), baseurl = "https://final.datim.org/")

# get orgunit levels to match and join ------------------------------------
tz_7n <- grabr::get_ouorgs(ouuid,
                           7,  
                           datim_user(),
                           datim_pwd()) 
tz7 <- c(tz_7n$uid)

tz_6n <- grabr::get_ouorgs(ouuid,
                           6,  
                           datim_user(),
                           datim_pwd()) 
tz_6 <- c(tz_6n$uid)



# for most level 4 that match_level 7, use snu_4_id -----------------------
tanz7<- tanz_info %>% filter(snu_4_id %in% tz7, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% rename(orgunit = snu_4, orgunituid = snu_4_id) %>% print()


# for level 4 that doesn't match level 7, match by snu_4 recide sn --------
tanz7m <- tanz_info %>% filter(!snu_4_id %in% tz7, snu_4 != "") %>% rename(orgunit = snu_4)
nrow(tanz7m)

tanz7m <- tanz7m %>% left_join(tz_7n) %>% # or inner if there are non-matches
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = uid) %>%
  print() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
  
tanz7m %>% filter(is.na(orgunituid)) #check for unmatched


# check for missing data at snu_level_3
tanz6<- tanz_info %>% filter(snu_3 == "", snu_4 == "") %>% print()


# at snu_level_match to metadata level, tz 6 ------------------------------
tanz6<- tanz_info %>% filter(snu_3_id %in% tz_6, snu_4 == "") %>% 
  rename(orgunit = snu_3, orgunituid = snu_3_id) %>% 
  select(-snu_1_id:-snu_2_id, -snu_4, -snu_4_id) %>%
  print()

# for level 3 that doesn't match level 6, match by snu_3 to snu_3_id from metadata ----------
tanz6m <- tanz_info %>% filter(!snu_3_id %in% tz_6, snu_4 == "") %>% print()
nrow(tanz6m) 

tanz6m <- tanz6m %>%
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


  tanz6m %>% filter(is.na(orgunituid)) #check for unmatched

  
tnz <- bind_rows(tanz7, tanz7m, tanz6, tanz6m) %>% select(-snu_3, -parent, -snu_4)

#later bind country dfs together
