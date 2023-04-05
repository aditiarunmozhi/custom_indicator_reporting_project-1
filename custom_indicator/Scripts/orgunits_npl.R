
npl_6_7 <- orgunit_level_list(df_orgs$npl_orgs, c(6,7)) %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------


