
table(df_orgs$tjk_orgs$orgunit_level)
print(df_orgs$tjk_orgs, n= 171)

tjk_6_7 <- orgunit_level_list(df_orgs$tjk_orgs, c(6,7)) %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print()

# ultimately bind rows so that we can merge by country, orgunit_pa --------

df_orgs %>% filter(orgunit_name == "Buston Town")
