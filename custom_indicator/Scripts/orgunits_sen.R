
max(df_orgs$sen_orgs$orgunit_level)
unique(df_orgs$sen_orgs$regionorcountry_code)

sen_6_7 <- orgunit_level_list(df_orgs$sen_orgs, c(6,7))

# ultimately bind rows so that we can merge by country, orgunit_pa --------

