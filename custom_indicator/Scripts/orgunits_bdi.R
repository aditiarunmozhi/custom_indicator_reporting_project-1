
# get ou tableD from DATIM -------------------------------------------------

org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"

load_secrets()


df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd()
)
df_ous

#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES

list_orgs <- list_orgs_func("Burundi")
df_orgs <- df_orgs_func()

max(df_orgs$orgunit_level)
unique(df_orgs$regionorcountry_code)

bdi_5_7 <-
  df_orgs %>% 
  # filter(str_detect(orgunit_name, "H ")) %>%
  filter(orgunit_level %in% c(5,6,7)) %>%
  print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------

