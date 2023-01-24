
# Select most recent quarter ----------------------------------------------
period <- "FY22 Q4"

# Data input  ------------------------------------------
#Read in data
infolink_fy22 <- read.csv("~/KP Dashboard/Data Preparation/Data/Intermediate Outputs/infolink/Infolink_FY22_CIGB_for_R.csv", na.strings="~") %>% 
  clean_names() %>% glimpse() #review data structure


tx_indicators <- c("TX_NEW_VERIFY", "TX_CURR_VERIFY", "TX_PVLS_VERIFY")
# Begin Reformatting ------------------------------------------------------
infolink <-   infolink_fy22 %>%
  mutate(numdenom=numerator_denom,
         partner=prime_partner,
         psnu=if_else(psnu_final == "SÃ©gou", "Ségou", psnu_final),
         val=values,
         operatingunit=if_else(operating_unit == "Kyrgyzstan", "Asia Region", operating_unit),
         standardized_disaggregate=if_else(str_starts(standardized_disaggregate, "KeyPop"), "KeyPop", standardized_disaggregate),
         disagg=if_else(str_starts(standardized_disaggregate, "Total"), "Total", standardized_disaggregate),
         orgunit=if_else(is.na(psnu_final), snu1_final, psnu_final),
         orgunit=if_else(psnu_final == "SÃ©gou", "Ségou", orgunit),
         orgunituid=if_else(is.na(psnu_final), snu1uid, psnuuid),
         reportingperiod=recode(quarter,
                                "1/1/2022"  = "FY22 Q1",
                                "4/1/2022"  = "FY22 Q2",
                                "7/1/2022"  = "FY22 Q3",
                                "10/1/2022" = "FY22 Q4",
                                "1/1/2021"  = "FY21 Q1",
                                "4/1/2021"  = "FY21 Q2",
                                "7/1/2021"  = "FY21 Q3",
                                "10/1/2021" = "FY21 Q4"),
         population=recode(key_pops,
                           "FSW" = "Female sex workers (FSW)",
                           "MSM" = "Men who have sex with men (MSM)",
                           "PWID" = "People who inject drugs (PWID)",
                           "TG" = "Transgender people (TG)",
                           "Prisoners" = "People in prisons and other closed settings"),
         age = if_else(str_detect(age, "Years"), str_extract(age, "^.*(?= )"), age),
         age = if_else(indicator %in% tx_indicators, recode(age,
                                                            "<1" = "<20",
                                                            "1-4" = "<20",
                                                            "5-9" = "<20",
                                                            "10-14" = "<20",
                                                            "15-19" = "<20"),
                       recode(age, "Age Unknown" = "Unknown Age")),
         otherdisaggregate=if_else(str_detect(indicator, "PVLS_ELIGIBLE|PrEP"), "", recode(clinical_site_type,
                                                                                           "PEPFAR Supported Site" = "Site Support Type: PEPFAR supported",
                                                                                           "NON-PEPFAR Supported Site" = "Site Support Type: non-PEPFAR supported"))) %>% 
  select(country, disagg, reportingperiod, orgunit, orgunituid, mech_code, partner, operatingunit, psnu, indicator, sex, age, population, otherdisaggregate, numdenom, val) %>% glimpse()

infolink %>% filter(psnu == "") %>% print()
# Isolate KP and age/sex disaggs from totals ------------------------------
table(infolink$disagg)
infolink_minus_nonkp <- infolink %>% filter(!disagg=="Total") %>%   filter(val >= 0)

# subtract KP from totals to get non-KP values ----------------------------
infolink_nonkp <- infolink %>%
  filter(indicator != "TX_PVLS_ELIGIBLE") %>%
  filter(disagg != "Age/Sex") %>%
  mutate(    val=if_else(is.na(population), val, -val),
             population="Non-KP (general population)",
             disagg= "KeyPop") %>%
  group_by(country, disagg, reportingperiod, orgunit, orgunituid, mech_code, partner, operatingunit, psnu, indicator, sex, age, population, otherdisaggregate, numdenom) %>%                         # Specify group indicator
  summarise_at(vars(val),              # Specify column
               list(val = sum)) %>%      # Specify function
  ungroup() %>%
  filter(val > 0)

# append tables -----------------------------------------------------------
infolink_template_pre <- rbind(infolink_minus_nonkp, infolink_nonkp) %>%  #stack KP and non-KP data
  filter(!is.na(population) | !is.na(age)) %>%                            #exclude where sex is reported but not age
  glimpse()

# Exclude countries who do their own reporting ----------------------------
`%!in%` <- Negate(`%in%`)
mission2report <- c("Cote d'Ivoire", "Lesotho", "Burundi", "Malawi", "DRC") # make DRC blank
mission2report
CARims  <- c(81409, 81253, 81240)
# infolink_template_most <- infolink_template_pre %>%
infolink_template <- infolink_template_pre %>%
  filter(operatingunit %!in% mission2report,                 #exclude OUs where missions report themselves
         #mech_code %!in% CARims,                            #exclude CAR IMs - until final export
         reportingperiod  == period,
         !(operatingunit == "DRC" & (indicator == "TX_PVLS_VERIFY" | indicator == "TX_NEW_VERIFY"))) %>% # exclude what DRC mission reported
  select(-country, -disagg) %>%
  glimpse()
unique(infolink_template$indicator)
infolink_template %>% filter(is.na(age), !is.na(sex), !is.na(population))
# check unknown age/sex
# infolink_template_mf <- infolink_template %>% filter(str_detect(sex, "ale$"), is.na(age)) %>% glimpse()
#
# # infolink_template <- union(infolink_template_most, infolink_template_CAR) %>% #stack KP and non-KP data
# #   glimpse()
#
#
# table(infolink_template$operatingunit)
# table(infolink_template_mf$sex)
# table(infolink_template_mf$age)
# table(infolink_template$population)
###############
ous <- c(infolink_template$operatingunit)
table(ous)


glimpse(infolink_template)
#export csv for each ou
for (i in unique(ous)){
  ou_file <- subset(infolink_template, operatingunit %in% i)
  write.csv(    ou_file, paste0("~/KP Dashboard/Data Preparation/data/Output/OHA CIGB/", period, "/", i, ".csv"), na = "", row.names = FALSE)
}
# CAR export for review ---------------------------------------------------
infolink_template_CAR <- infolink_template_pre %>%
  filter(mech_code %in% CARims,
         reportingperiod  == period) %>%
  glimpse()
table(infolink_template_CAR$disagg)
cars <- c(infolink_template_CAR$country)
table(cars)
for (c in unique(cars)){
  country_file <- subset(infolink_template_CAR, country %in% c)
  write.csv(    country_file, paste0("~/KP Dashboard/Data Preparation/data/Output/OHA CIGB/", period, "/CAR/", c, ".csv"), na = "", row.names = FALSE)
}
# Asia Region export for review ---------------------------------------------------
###################################################
###################################################
table(infolink_template_pre$operatingunit)
infolink_asia <- infolink_template_pre %>%
  filter(operatingunit=="Asia Region",
         reportingperiod  == period) %>% glimpse()
countries <- c(infolink_asia$country)
table(infolink_asia$country)
table(countries)
for (i in unique(countries)){
  ou_file <- subset(infolink_asia, country %in% i)
  write.csv(    ou_file, paste0("~/KP Dashboard/Data Preparation/data/Output/OHA CIGB/", period, "/Asia Region/", i, ".csv"), na = "", row.names = FALSE)
}


test <- infolink_template %>% distinct(operatingunit) %>% as.list() %>% unlist()
class(test)
unlist(test)
