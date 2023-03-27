
# obtain list of countries from cleaned infolink data ---------------------
countries <- complete_clean_data %>% group_by(country) %>% summarise(.groups = "drop")
countries <- c(countries$country)


# get ou tableD from DATIM -------------------------------------------------
            
org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"

load_secrets()


df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd()
)


#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES


list_orgs <- list_orgs_func("Kazakhstan")
df_orgs <- df_orgs_func()

df_orgs

table(df_orgs$orgunit_level)
print(df_orgs, n= 171)

kaz_7_8 <- df_orgs %>% filter(orgunit_level %in% c(7,8)) %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------

