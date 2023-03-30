
# obtain list of countries from cleaned infolink data ---------------------
countries <- country_list()

# get ou tableD from DATIM -------------------------------------------------

#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES

df_orgs <- orgs_func("Laos")

max(df_orgs$orgunit_level)

lao_6_7 <- orgunit_level_list(c(6,7))
# ultimately bind rows so that we can merge by country, orgunit_pa --------

# write_csv(df_orgs, "Data/laos_orgunits.csv")
