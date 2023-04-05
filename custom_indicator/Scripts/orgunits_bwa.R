
max(df_orgs$bwa_orgs$orgunit_level)
unique(df_orgs$bwa_orgs$regionorcountry_code)

bwa_5_6 <- orgunit_level_list(df_orgs$bwa_orgs, c(5,6))

# ultimately bind rows so that we can merge by country, orgunit_pa --------

