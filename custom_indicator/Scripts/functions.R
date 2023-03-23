orgunit_level_sep <- function(df, lev, new_col_1, new_col_uid_1, new_col_2, new_col_uid_2) {(df %>% filter(orgunit_level == lev) %>% 
                                                                                               rename({{new_col_1}} := orgunit_parent, {{new_col_uid_1}} := orgunit_parent_uid, {{new_col_2}} := orgunit_name, {{new_col_uid_2}} := orgunit_uid) %>%
                                                                                               select(-orgunit_level))}

orgunit_table_join <- function(level_x, level_y, orgunit_x_uid, orgunit_x) {full_join(level_x, level_y, by = join_by({{orgunit_x_uid}}, {{orgunit_x}}), multiple = "all") %>%
                                                                              select(sort(colnames(.)))}

#merge_psnu <- function(clean, orgunit_table, psnu_uid_merge, psnu_merge) {full_join(clean, orgunit_table, by = join_by({{psnu_uid_merge}}, {{psnu_merge}}), multiple = "all")}
