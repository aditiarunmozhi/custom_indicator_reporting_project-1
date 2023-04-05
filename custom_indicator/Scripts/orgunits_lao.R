
max(df_orgs$lao_orgs$orgunit_level)

lao_6_7 <- orgunit_level_list(df_orgs$lao_orgs, c(6,7))
# ultimately bind rows so that we can merge by country, orgunit_pa --------

# write_csv(df_orgs, "Data/laos_orgunits.csv")


