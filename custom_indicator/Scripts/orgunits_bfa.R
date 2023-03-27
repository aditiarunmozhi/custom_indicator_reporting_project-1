
# get ou tableD from DATIM -------------------------------------------------

org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"

load_secrets()


df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd()
)
df_ous

#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES

list_orgs <- list_orgs_func("Burkina Faso")
df_orgs <- df_orgs_func()

max(df_orgs$orgunit_level)
unique(df_orgs$regionorcountry_code)

bfa_5_6 <-
  df_orgs %>% 
  filter(orgunit_level %in% c(5, 6)) %>%
  print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------

