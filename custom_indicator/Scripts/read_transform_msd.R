file <- glamr::return_latest("../Data/", "PSNU_IM_FY20") %>% print()
fil3 <- paste0(str_extract(file,".+(?=zip)"),"txt")
mer_df <- read_msd(file, save_rds = TRUE, remove_txt = FALSE)

indicator_list <- c("KP_PREV",  "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "PrEP_CURR", "HTS_TST_POS",
                    "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW")

df <- mer_df %>%  filter(
  str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE) %>%
  mutate(indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
         funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>%
  filter(indicator %in% indicator_list) %>% mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>% glimpse()


check <- df %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(
    cumulative = coalesce(cumulative, 0),
    targets = coalesce(targets, 0),
    fy = fiscal_year,
    partner = prime_partner_name,
    disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
    disagg = recode(disagg, "KeyPop" = "KP"),
    tx_ml_reason = case_when(indicator=="TX_ML" ~ str_extract(otherdisaggregate, "(?<=Outcome\\s-\\s).+")),
    keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
    keypop = recode(keypop, "People in prisons" = "Prisoners")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, tx_ml_reason, keypop, fy, targets, cumulative) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator)