
col_order <- c("reportingperiod", "orgunit", "orgunituid",
               "mech_code", "partner", "ou", "psnu", "indicator",
               "sex", "age", "otherdisaggregate", "population","numdenom", "value")
mission2report <- c("Eswatini", "Cote d'Ivoire")

ci_fhi <- bind_rows(bdi_merge_psnu, bwa_merge_psnu, civ_merge_psnu, cod_merge_psnu, lso_merge_psnu, mwi_merge_psnu, swz_merge_psnu,
                    zaf_merge_psnu, idn_merge_psnu, kaz_merge_psnu, kgz_merge_psnu, tjk_merge_psnu, tza_merge_psnu, lao_merge_psnu, 
                    npl_merge_psnu, mmr_merge_psnu, bfa_merge_psnu, gha_merge_psnu, lbr_merge_psnu, mli_merge_psnu, sen_merge_psnu,
                    tgo_merge_psnu, tha_merge_psnu, vnm_merge_psnu) %>%
  #exclude what country teams report
  filter(!(str_detect(tolower(indicator), "prep") & country == "Botswana"),
         !country %in% mission2report)

#prepare data for export by selecting reporting columns and summarizing by reported columns
ci_fhi360 <-  ci_fhi %>%    select(reportingperiod, orgunit, orgunituid,
         mech_code, partner, ou, psnu, indicator,
         sex, age, otherdisaggregate, population,numdenom, value) %>%
    group_by(across(-c(value))) %>% summarise(value = sum(value), .groups ="drop") %>%
  glimpse()

ci <- ci_fhi360[,col_order]

ous <- c(unique(ci$ou))
reportingperiod <- unique(c(ci$reportingperiod))

for (i in ous){
  ou_file <- subset(ci, ou %in% i)
  write.csv(ou_file, paste0("Dataout/", reportingperiod, "/", i, ".csv"), na = "", row.names = FALSE)
}


# test <- bind_rows(idn, kaz, kgz, tjk, npl, mmr,  phl, bwa, swz, tnz, zaf, bfa, gha, lbr, mli, sen, tgo) %>% 
  # filter(is.na(orgunit_level)) %>% 
  # glimpse()
table(ci$ou, ci$indicator)

# rm(list = ls()[grepl("bwa", ls())])

country <- ci_fhi %>% filter(country == "Botswana")
table(country$partner)
table(country$psnu, country$indicator)

