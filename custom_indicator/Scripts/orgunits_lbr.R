
max(df_orgs$lbr_orgs$orgunit_level)
unique(df_orgs$lbr_orgs$regionorcountry_code)

lbr_6_7 <- orgunit_level_list(df_orgs$lbr_orgs, c(6,7))

# ultimately bind rows so that we can merge by country, orgunit_pa --------

