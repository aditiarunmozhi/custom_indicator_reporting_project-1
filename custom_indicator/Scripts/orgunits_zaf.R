

table(df_orgs$zaf_orgs$orgunit_level)
print(df_orgs$zaf_orgs, n= 171)

zaf_4_5 <- orgunit_level_list(df_orgs$zaf_orgs, c(4,5)) %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------

