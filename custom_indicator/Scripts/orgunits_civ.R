
# get ou tableD from DATIM -------------------------------------------------

org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"

load_secrets()


df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd()
)
df_ous

#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES

list_orgs <- list_orgs_func("Cote d'Ivoire")
df_orgs <- df_orgs_func()

max(df_orgs$orgunit_level)
unique(df_orgs$regionorcountry_code)

civ_6_7 <-
  df_orgs %>% 
  filter(orgunit_level %in% c(6,7)) %>%
  print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------

