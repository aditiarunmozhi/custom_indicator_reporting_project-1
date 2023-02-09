
col_order <- c("reportingperiod", "orgunit", "orgunituid",
               "mech_code", "partner", "ou", "orgunit_parent", "indicator",
               "sex", "age", "otherdisaggregate", "population","numdenom", "value")

ci_fhi <- bind_rows(kaz, kgz, tjk, npl, swz, tnz, zaf)
  
countries <- c(unique(ci_fhi$country))

ci_fhi_360 <- ci_fhi %>%
  select(reportingperiod, orgunit, orgunituid,
         mech_code, partner, ou, orgunit_parent, indicator,
         sex, age, otherdisaggregate, population,numdenom, value) %>%
    group_by(across(-c(value))) %>% summarise(value = sum(value), .groups ="drop") %>%
  glimpse()

ci <- ci_fhi_360[,col_order]

ous <- c(unique(ci$ou))
reportingperiod <- unique(c(ci$reportingperiod))

for (i in unique(ous)){
  ou_file <- subset(ci, ou %in% i)
  write.csv(    ou_file, paste0("Dataout/", reportingperiod, "/", i, ".csv"), na = "", row.names = FALSE)
}



