tha_info <- complete_clean_data %>% filter(country=="Thailand") %>%
  mutate(snu_3 = recode(snu_3,
                        "RSAT Chonburi" = "RSAT – Chonburi",
                        "RSAT Ubon Ratchathani" = "RSAT – Ubon Ratchathani",
                        "RSAT Hatyai" = "Rainbow Sky Association of Thailand, Hat Yai",
                        "RSAT Bangkapi" = "Rainbow Sky Association of Thailand_BKK",
                        "SWING Bangrak" = "Service Workers IN Group Foundation_BKK",
                        "SWING Banglamung" = "Service Workers IN Group Foundation_PTY")
    # snu_3 = str_to_title(snu_3),
  ) %>%
  glimpse()

snu1 <- unique(tha_info$snu_1) %>% print()
snu2 <- c(unique(tha_info$snu_2)) %>% print()
snu3 <- c(unique(tha_info$snu_3)) %>% print()

# get orgunit levels to match and join ------------------------------------
tha7op <- df_orgs$tha_orgs %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
tha7uid <- c(tha7op$orgunit_uid)
tha7org <- c(tha7op$orgunit_name)
unique(tha7op$orgunit_name) 

tha6op <- df_orgs$tha_orgs %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
tha6uid <- c(tha6op$orgunit_uid)
unique(tha6op$orgunit_name)

df_orgs$tha_orgs %>% filter(orgunit_name %in% snu3) #snu3 should match datim level 7
df_orgs$tha_orgs %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 6


################################################################################
# for most level 3 that match_level 7, use snu_3-----------------------
tha7 <- tha_info %>% filter(snu_3 %in% tha7org, snu_3!="") %>% select(-c(snu_1, snu_2)) %>% 
  rename(orgunit_name = snu_3)  %>% inner_join(tha7op) %>%   
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(tha7)/nrow(tha_info))


# # for most level 3 that match_level 7, use snu_3 id-----------------------
# tha7 <- tha_info %>% filter(snu_3_id %in% tha7uid, snu_3!="") %>% select(-c(snu_1_id, snu_2_id)) %>% 
#   rename(orgunit_uid = snu_3_id)  %>% inner_join(tha7op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
# scales::percent(nrow(tha7)/nrow(tha_info))


# for level 3 that doesn't match level 7, match by snu_2 name to level 6--------
tha7m1 <- tha_info %>% filter(!(snu_3 %in% tha7org)) %>% rename(orgunit_name = snu_3)
scales::percent(nrow(tha7m1)/nrow(tha_info))
nrow(tha7m1)

#identify and resolve any failed matches
test <- tha71 <- tha7m1 %>%
  anti_join(tha7op) %>% print()
table(test$orgunit_name)

tha7op %>% filter(str_detect(orgunit_name, "Service Workers")) %>% print(n = 42)

#check for 1:many matches
tha7 %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1) #want to see nothing here


tha <- tha7 %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(tha) - nrow(tha_info)
