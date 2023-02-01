org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"

df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd()
)

df_ous

list_orgs <- df_ous %>% 
  filter(country %in% c("Zambia")) %>% 
  pull(country_iso) %>% 
  paste0(org_url, "&var=OU:", .) %>% 
  httr::GET(httr::authenticate(user = glamr::datim_user(), 
                               password = glamr::datim_pwd())) %>%
  httr::content("text") %>%
  jsonlite::fromJSON(flatten=TRUE) 



df_orgs <- tibble::as_tibble(list_orgs$listGrid$rows, .name_repair = "unique") %>% 
  setNames(list_orgs$listGrid$headers$name) %>% 
  rename_with(.cols = contains("internal_id"),
              .fn = ~str_replace(., "internal_id", "uid"))

df_orgs %>% print()
