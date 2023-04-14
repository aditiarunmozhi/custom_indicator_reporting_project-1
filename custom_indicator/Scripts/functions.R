
#org unit table functions
orgunit_clean <- function(df) {df %>% select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))}

orgunit_level_sep <- function(df, lev, new_col_1, new_col_uid_1, new_col_2, new_col_uid_2) {
  (df %>% filter(orgunit_level == lev) %>% 
     rename({{new_col_1}} := orgunit_parent, {{new_col_uid_1}} := orgunit_parent_uid, 
            {{new_col_2}} := orgunit_name, {{new_col_uid_2}} := orgunit_uid) %>%
     select(-orgunit_level))}

orgunit_table_join <- function(level_x, level_y, orgunit_x_uid, orgunit_x) {
  full_join(level_x, level_y, by = join_by({{orgunit_x_uid}}, {{orgunit_x}}), multiple = "all") %>%
    select(sort(colnames(.)))}

#org unit list function
org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"
load_secrets()
df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd())

orgs_func <- function(ou) {
  list_orgs <- df_ous %>% 
    filter(country %in% c(ou)) %>% 
    pull(country_iso) %>% 
    paste0(org_url, "&var=OU:", ., "&paging=false") %>% 
    httr::GET(httr::authenticate(user = glamr::datim_user(), 
                                 password = glamr::datim_pwd())) %>% 
    httr::content("text") %>% 
    jsonlite::fromJSON(flatten=TRUE)
  
  df_orgs <- tibble::as_tibble(list_orgs$listGrid$rows, .name_repair = "unique") %>% 
    setNames(list_orgs$listGrid$headers$name) %>% 
    rename_with(.colsa = contains("internal_id"),
                .fn = ~str_replace(., "internal_id", "uid"))
}

orgunit_level_list <- function(df, levels) {
  df %>% filter(orgunit_level %in% levels)
}

#data validation
data_check <- function(merge_psnu) {
  merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))
}
