
table(df_orgs$swz_orgs$orgunit_level)

swz_5_6 <- orgunit_level_list(df_orgs$swz_orgs, c(5,6)) %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print()


# ultimately bind rows so that we can merge by country, orgunit_pa --------


