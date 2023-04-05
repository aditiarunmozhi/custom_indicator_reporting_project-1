
table(df_orgs$kaz_orgs$orgunit_level)
print(df_orgs$kaz_orgs, n= 171)

kaz_7_8 <- orgunit_level_list(df_orgs$kaz_orgs, c(7,8)) %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print()


# ultimately bind rows so that we can merge by country, orgunit_pa --------

